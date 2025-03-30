use clap::Parser;

#[derive(Debug)]
struct Case<const N: usize> {
    cli_args: [&'static str; N],
    expect_output: &'static str,
    expect_error: &'static str,
    expect_output_json: bool,
    md: &'static str,
    expect_success: bool,
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
        match mdq::run_in_memory(&cli, self.md) {
            Ok((found, stdout)) => (found, stdout, String::new()),
            Err(err) => (false, String::new(), err.to_string()),
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/tests/integ_test_cases.rs"));
