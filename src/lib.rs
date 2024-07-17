use clap::{Parser, ValueEnum};
use output::Output;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::io;
use std::io::{stdin, Read, Write};

use crate::fmt_md::{MdOptions, ReferencePlacement};
use crate::fmt_md_inlines::MdInlinesWriterOptions;
use crate::link_transform::LinkTransform;
use crate::output::Stream;
use crate::select::ParseError;
use crate::tree::{MdElem, ReadOptions};
use crate::tree_ref::MdElemRef;
use crate::tree_ref_serde::SerdeDoc;
use select::MdqRefSelector;

mod fmt_md;
mod fmt_md_inlines;
mod fmt_str;
mod link_transform;
mod matcher;
mod output;
mod parse_common;
mod parsing_iter;
mod select;
mod str_utils;
mod tree;
mod tree_ref;
mod tree_ref_serde;
mod tree_test_utils;
mod utils_for_test;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Where to put link references.
    ///
    /// For links and images of style `[description][1]`, this flag controls where to put the `[1]: https://example.com`
    /// definition.
    #[arg(long, value_enum, default_value_t=ReferencePlacement::Section)]
    pub(crate) link_pos: ReferencePlacement,

    /// Where to put footnote references. Defaults to be same as --link-pos
    #[arg(long, value_enum)]
    pub(crate) footnote_pos: Option<ReferencePlacement>,

    #[arg(long, short, value_enum, default_value_t=LinkTransform::Reference)]
    pub(crate) link_format: LinkTransform,

    /// Output the results as a JSON object, instead of as markdown.
    #[arg(long, short, default_value_t = OutputFormat::Markdown)]
    pub(crate) output: OutputFormat,

    #[arg(
        short = ' ',
        hide = true,
        group = "selectors_group",
        value_name = "selectors starting with list"
    )]
    pub(crate) list_selector: Option<String>,

    /// The selectors string
    #[arg(group = "selectors_group", value_name = "selectors")]
    pub(crate) selectors: Option<String>,

    /// Quiet: do not print anything to stdout. The exit code will still be 0 if-and-only-iff any elements match.
    #[arg(long, short)]
    pub(crate) quiet: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum OutputFormat {
    /// Output results as Markdown.
    Markdown,

    /// Alias for markdown
    Md,

    /// Output results as JSON. Spans of inline elements (like within a single paragraph) will be rendered as a single string of
    /// Markdown, not as separate JSON elements.
    Json,
}

impl Display for OutputFormat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let self_str = match self {
            OutputFormat::Markdown | OutputFormat::Md => "markdown",
            OutputFormat::Json => "json",
        };
        f.write_str(self_str)
    }
}

impl Cli {
    fn selector_string(&self) -> Cow<String> {
        match &self.selectors {
            Some(s) => Cow::Borrowed(s),
            None => match &self.list_selector {
                Some(list_selectors) => {
                    let mut reconstructed = String::with_capacity(list_selectors.len() + 2);
                    reconstructed.push_str("- ");
                    reconstructed.push_str(list_selectors);
                    Cow::Owned(reconstructed)
                }
                None => Cow::Owned(String::new()),
            },
        }
    }
}

pub fn run_in_memory(cli: &Cli, contents: &str) -> (bool, String) {
    let mut out = Vec::with_capacity(256); // just a guess

    let result = run(&cli, contents.to_string(), || &mut out);
    let out_str = String::from_utf8(out).map_err(|_| "UTF-8 decode error").unwrap();
    (result, out_str)
}

pub fn run_stdio(cli: &Cli) -> bool {
    let mut contents = String::new();
    stdin().read_to_string(&mut contents).expect("invalid input (not utf8)");
    run(&cli, contents, || io::stdout().lock())
}

fn run<W, F>(cli: &Cli, contents: String, get_out: F) -> bool
where
    F: FnOnce() -> W,
    W: Write,
{
    let ast = markdown::to_mdast(&contents, &markdown::ParseOptions::gfm()).unwrap();
    let mdqs = MdElem::read(ast, &ReadOptions::default()).unwrap();

    let selectors_str = cli.selector_string();
    let selectors = match MdqRefSelector::parse(&selectors_str) {
        Ok(selectors) => selectors,
        Err(err) => {
            print_select_parse_error(&selectors_str, err);
            return false;
        }
    };

    let mut pipeline_nodes = vec![MdElemRef::Doc(&mdqs)];
    for selector in selectors {
        let new_pipeline = selector.find_nodes(pipeline_nodes);
        pipeline_nodes = new_pipeline;
    }

    let md_options = MdOptions {
        link_reference_placement: cli.link_pos,
        footnote_reference_placement: cli.footnote_pos.unwrap_or(cli.link_pos),
        inline_options: MdInlinesWriterOptions {
            link_format: cli.link_format,
        },
    };

    let found_any = !pipeline_nodes.is_empty();

    if !cli.quiet {
        let mut stdout = get_out();
        match cli.output {
            OutputFormat::Markdown | OutputFormat::Md => {
                let mut out = Output::new(Stream(&mut stdout));
                fmt_md::write_md(&md_options, &mut out, pipeline_nodes.into_iter());
            }
            OutputFormat::Json => {
                serde_json::to_writer(&mut stdout, &SerdeDoc::new(&pipeline_nodes, md_options.inline_options)).unwrap();
            }
        }
    }

    found_any
}

fn print_select_parse_error(original_string: &str, err: ParseError) {
    eprintln!("Syntax error in select specifier:");
    for (line_num, line) in original_string.split('\n').enumerate() {
        if line_num == err.position.line {
            eprintln!("┃ {}", line);
            eprint!("┃ ");
            // Parsers typically throw errors after chars.next(), which advances the stream such that it's looking at
            // the char after the failure. So, subtract 1 so that we're pointing at the right one.
            for _ in 0..(err.position.column - 1) {
                eprint!(" ");
            }
            eprint!("↑ {}", err.reason);
            eprintln!();
        } else {
            eprintln!("⸾ {}", line)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Error;

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        Cli::command().debug_assert();
    }

    #[test]
    fn standard_selectors() {
        let result = Cli::try_parse_from(["mdq", "# hello"]);
        unwrap!(result, Ok(cli));
        assert_eq!(cli.selector_string().as_str(), "# hello");
    }

    #[test]
    fn two_standard_selectors() {
        let result = Cli::try_parse_from(["mdq", "# hello", "# world"]);
        check_err(&result, "unexpected argument '# world' found");
    }

    #[test]
    fn start_with_list_selectors() {
        let result = Cli::try_parse_from(["mdq", "- world"]);
        unwrap!(result, Ok(cli));
        assert_eq!(cli.selector_string().as_str(), "- world");
    }

    #[test]
    fn start_with_list_selectors_twice() {
        let result = Cli::try_parse_from(["mdq", "- hello", "- world"]);
        check_err(
            &result,
            "the argument '-  <selectors starting with list>' cannot be used multiple times",
        );
    }

    #[test]
    fn both_list_and_std_selectors() {
        let result = Cli::try_parse_from(["mdq", "# hello", "- world"]);
        check_err(
            &result,
            "the argument '[selectors]' cannot be used with '-  <selectors starting with list>'",
        )
    }

    fn check_err(result: &Result<Cli, Error>, expect: &str) {
        unwrap!(result, Err(e));
        let e_str = e.to_string();
        let first_line = e_str.split('\n').next().expect("no error string found");
        let mut expect_full = "error: ".to_string();
        expect_full.push_str(expect);
        assert_eq!(first_line, &expect_full);
    }
}
