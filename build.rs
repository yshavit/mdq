use serde::Deserialize;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fs::DirEntry;
use std::path::Path;
use std::{env, fs};

const MD_CASES_PATH: &str = "tests/md_cases/";
const CASES_WRITE: &str = "tests/integ_test_cases.rs";

fn main() -> Result<(), String> {
    println!("cargo::rerun-if-changed={MD_CASES_PATH}");
    let out_dir = env::var("OUT_DIR").unwrap();

    generate_integ_test_cases(&out_dir)?;

    Ok(())
}

fn generate_integ_test_cases(out_dir: &String) -> Result<(), String> {
    let mut out = Writer::new();
    for md_case in fs::read_dir(MD_CASES_PATH).map_err(|e| e.to_string())? {
        let spec_file = DirEntryHelper::new(md_case.map_err(|e| e.to_string())?);
        if !spec_file.run(DirEntry::file_type)?.is_file() {
            return Err(spec_file.err_string::<&str, _>("not a regular file"));
        }

        out.writes(&["mod ", &spec_file.mod_name(), " {"]).nl();
        out.with_indent(|out| {
            out.write("use super::*;").nl().nl();

            let contents = spec_file.run(|f| fs::read_to_string(f.path())).unwrap();
            let spec_file_parsed: TestSpecFile =
                toml::from_str(&contents).map_err(|e| spec_file.err_string(e)).unwrap();

            let chained_needed = spec_file_parsed.chained.map(|ch| ch.needed).unwrap_or(true);

            out.write("const MD: &str = indoc::indoc! {r#\"");
            out.with_indent(|out| {
                let mut iter = spec_file_parsed.given.md.trim().split('\n').peekable();
                while let Some(line) = iter.next() {
                    out.nl().write(line);
                    if iter.peek().is_none() {
                        out.writeln("\"#};");
                    }
                }
            });

            out.write("const FILES: [(&str, &str); ");
            out.write(&format!("{}", &spec_file_parsed.given.get_files_count()));
            out.write("] = [");
            if let Some(files) = &spec_file_parsed.given.files {
                out.with_indent(|out| {
                    for (file_name, file_content) in files {
                        out.writeln(&format!("({:?}, {:?}),", file_name, file_content));
                    }
                });
            }
            out.writeln("];");

            let mut found_chained_case = false;
            for case in spec_file_parsed.get_cases() {
                found_chained_case |= case.case_name.eq("chained");
                case.write_test_fn_to(out);
            }

            match (chained_needed, found_chained_case) {
                (true, false) => Case::write_failing_test(out, "chained", "missing 'chained' test case"),
                (false, true) => Case::write_failing_test(
                    out,
                    "chained__extra",
                    "provided 'chained' test case even though it was marked as not needed",
                ),
                _ => {}
            }

            if !found_chained_case {}
        });

        out.writeln("}");
    }

    let out_path = Path::new(&out_dir).join(CASES_WRITE);
    fs::create_dir_all(out_path.parent().expect("no parent dir found"))
        .map_err(|e| format!("mkdirs on {}: {}", out_path.to_string_lossy(), e))?;
    fs::write(&out_path, out.get()).map_err(|e| format!("writing to {}: {}", out_path.to_string_lossy(), e))?;

    Ok(())
}

struct DirEntryHelper {
    dir_entry: DirEntry,
    path_lossy: String,
}

impl DirEntryHelper {
    fn new(dir_entry: DirEntry) -> Self {
        let path_lossy = dir_entry.path().to_string_lossy().to_string();
        Self { dir_entry, path_lossy }
    }

    fn mod_name(&self) -> String {
        let file_name = self.dir_entry.file_name();
        let p = Path::new(file_name.as_os_str());
        let stem = p
            .file_stem()
            .unwrap_or_else(|| panic!("no file stem for {}", self.dir_entry.path().to_string_lossy()));
        stem.to_string_lossy().to_string()
    }

    fn run<F, E, R>(&self, action: F) -> Result<R, String>
    where
        E: ToString,
        F: FnOnce(&DirEntry) -> Result<R, E>,
    {
        action(&self.dir_entry).map_err(|e| self.err_string(e))
    }

    fn path(&self) -> &str {
        &self.path_lossy
    }

    fn err_string<E: ToString, B: Borrow<E>>(&self, e: B) -> String {
        format!("{}: {}", self.path(), e.borrow().to_string())
    }
}

#[derive(Deserialize)]
struct TestSpecFile {
    given: TestGiven,
    expect: HashMap<String, TestExpect>,
    chained: Option<Chained>,
}

#[derive(Deserialize)]
struct TestGiven {
    md: String,
    files: Option<HashMap<String, String>>,
}

impl TestGiven {
    fn get_files_count(&self) -> usize {
        match &self.files {
            None => 0,
            Some(files) => files.len(),
        }
    }
}

#[derive(Deserialize)]
struct TestExpect {
    cli_args: Vec<String>,
    output: String,
    output_json: Option<bool>,
    expect_success: Option<bool>,
    ignore: Option<String>,
    output_err: Option<String>,
}

#[derive(Deserialize, Copy, Clone)]
struct Chained {
    needed: bool,
}

impl TestSpecFile {
    fn get_cases(self) -> Vec<Case> {
        let mut results = Vec::with_capacity(self.expect.len());
        for (case_name, test_expect) in self.expect {
            results.push(Case {
                case_name,
                cli_args: test_expect.cli_args,
                expect_output: test_expect.output,
                expect_error: test_expect.output_err.unwrap_or_default(),
                output_json: test_expect.output_json.unwrap_or(false),
                expect_success: test_expect.expect_success.unwrap_or(true),
                ignored: test_expect.ignore.is_some(),
            })
        }
        results
    }
}

#[derive(Debug)]
struct Case {
    case_name: String,
    ignored: bool,
    cli_args: Vec<String>,
    expect_output: String,
    expect_error: String,
    output_json: bool,
    expect_success: bool,
}

impl Case {
    fn write_failing_test(out: &mut Writer, name: &str, err_msg: &str) {
        out.writeln("#[test]");
        out.writes(&["fn ", name, "() {"]);
        out.with_indent(|out| {
            out.writes(&["panic!(\"", err_msg, "\");"]);
        });
        out.writeln("}");
    }

    fn write_test_fn_to(&self, out: &mut Writer) {
        let fn_name = self
            .case_name
            .replace(|ch: char| !(ch.is_alphanumeric() || ch.is_whitespace()), "")
            .replace(|ch: char| ch.is_whitespace(), "_");
        if self.ignored {
            // separate out ign-ore to two lines, so that it doesn't trigger the CI check for ignored tests
            out.write("#[ign");
            out.writeln("ore]");
        }
        out.writeln("#[test]");
        out.writes(&["fn ", &fn_name, "() {"]);
        out.with_indent(|out| {
            out.write("Case {");
            out.with_indent(|out| {
                out.writeln(&format!("cli_args: {:?},", &self.cli_args));
                out.writeln(&format!("expect_output_json: {},", self.output_json));
                if self.expect_output.is_empty() {
                    out.writeln("expect_output: \"\",");
                } else {
                    out.write("expect_output: indoc::indoc! {r#\"");
                    out.with_indent(|out| {
                        let mut iter = self.expect_output.split('\n').peekable();
                        while let Some(line) = iter.next() {
                            out.write(line);
                            if iter.peek().is_some() {
                                out.nl();
                            } else {
                                out.write("\"#},");
                            }
                        }
                    });
                }
                if self.expect_error.is_empty() {
                    out.writeln("expect_error: \"\",");
                } else {
                    out.write("expect_error: indoc::indoc! {r#\"");
                    out.with_indent(|out| {
                        let mut iter = self.expect_error.split('\n').peekable();
                        while let Some(line) = iter.next() {
                            out.write(line);
                            if iter.peek().is_some() {
                                out.nl();
                            } else {
                                out.write("\"#},");
                            }
                        }
                    });
                }
                out.write("expect_success: ")
                    .write(&self.expect_success.to_string())
                    .writeln(",");
                out.writeln("md: MD,");
                out.write("files: &FILES,");
            });
            out.write("}.check();");
        });
        out.write("}").nl().nl();
    }
}

struct Writer {
    out: String,
    indent_level: usize,
}

impl Writer {
    fn new() -> Self {
        Self {
            out: String::with_capacity(512),
            indent_level: 0,
        }
    }

    fn with_indent<F>(&mut self, block: F)
    where
        F: FnOnce(&mut Self),
    {
        self.indent_level += 1;
        self.write("\n");
        block(self);
        self.indent_level -= 1;
        self.write("\n");
    }

    fn write(&mut self, text: &str) -> &mut Self {
        let mut iter = text.split('\n').peekable();
        while let Some(line) = iter.next() {
            if !line.is_empty() {
                self.out.push_str(line);
            }
            if iter.peek().is_some() {
                self.out.push('\n');
                for _ in 0..self.indent_level {
                    self.out.push_str("    ");
                }
            }
        }
        self
    }

    fn writes(&mut self, items: &[&str]) -> &mut Self {
        for item in items {
            self.write(item);
        }
        self
    }

    fn writeln(&mut self, text: &str) {
        self.write(text);
        self.write("\n");
    }

    fn nl(&mut self) -> &mut Self {
        self.write("\n");
        self
    }

    fn get(&self) -> &str {
        &self.out
    }
}
