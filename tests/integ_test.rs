use clap::Parser;
use std::ffi::OsString;
use std::io::ErrorKind;
use std::{env, io};

#[derive(Debug)]
struct Case<const N: usize> {
    cli_args: [&'static str; N],
    expect_output: &'static str,
    expect_error: &'static str,
    expect_output_json: bool,
    md: &'static str,
    files: &'static [(&'static str, &'static str)],
    expect_success: bool,
}

impl<const N: usize> mdq::OsFacade for &Case<N> {
    fn read_stdin(&self) -> io::Result<String> {
        Ok(self.md.to_string())
    }

    fn read_file(&self, path: &str) -> io::Result<String> {
        for (name, content) in self.files {
            if path == *name {
                return Ok(content.to_string());
            }
        }
        Err(io::Error::new(ErrorKind::NotFound, format!("File not found: {}", path)))
    }
}

impl<const N: usize> Case<N> {
    fn check(&self) {
        let (actual_success, actual_out, actual_err) = self.run();
        let (actual_out, expect_out) = if self.expect_output_json {
            let actual_obj = serde_json::from_str::<serde_json::Value>(&actual_out).unwrap();
            let expect_obj = serde_json::from_str::<serde_json::Value>(self.expect_output).unwrap();
            (
                serde_json::to_string_pretty(&actual_obj).unwrap(),
                serde_json::to_string_pretty(&expect_obj).unwrap(),
            )
        } else {
            (actual_out, self.expect_output.to_string())
        };
        assert_eq!(actual_out, expect_out);
        assert_eq!(actual_err, self.expect_error);
        assert_eq!(actual_success, self.expect_success);
    }

    fn run(&self) -> (bool, String, String) {
        let all_cli_args = ["cmd"].iter().chain(&self.cli_args);
        let cli = mdq::cli::Cli::try_parse_from(all_cli_args).unwrap();
        let restore = EnvVarRestore::set_var("MDQ_PORTABLE_ERRORS", "1");
        let result = match mdq::run_in_memory(&cli, self) {
            Ok((found, stdout)) => (found, stdout, String::new()),
            Err(err) => (false, String::new(), err.to_string()),
        };
        drop(restore);
        result
    }
}

struct EnvVarRestore(&'static str, Option<OsString>);

impl EnvVarRestore {
    fn set_var(key: &'static str, value: &str) -> Self {
        let restore = Self(key, env::var_os(key));
        env::set_var(key, value);
        restore
    }
}

impl Drop for EnvVarRestore {
    fn drop(&mut self) {
        match (&self.0, &self.1) {
            (key, None) => env::remove_var(key),
            (key, Some(old)) => env::set_var(key, old),
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/tests/integ_test_cases.rs"));
