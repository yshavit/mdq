use clap::Parser;

#[derive(Debug)]
struct Case<const N: usize> {
    cli_args: [&'static str; N],
    expect_output: &'static str,
    expect_output_json: bool,
    md: &'static str,
    expect_success: bool,
}

impl<const N: usize> Case<N> {
    fn check(&self) {
        let all_cli_args = ["cmd"].iter().chain(&self.cli_args);
        let cli = mdq::cli::Cli::try_parse_from(all_cli_args).unwrap();
        let (actual_success, actual_out) = mdq::run_in_memory(&cli, self.md);
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
        assert_eq!(actual_success, self.expect_success);
    }
}

include!(concat!(env!("OUT_DIR"), "/tests/integ_test_cases.rs"));
