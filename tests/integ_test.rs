use clap::Parser;

#[derive(Debug)]
struct Case<const N: usize> {
    cli_args: [&'static str; N],
    expect_output: &'static str,
    md: &'static str,
    expect_success: bool,
}

impl<const N: usize> Case<N> {
    fn check(&self) {
        let all_cli_args = ["cmd"].iter().chain(&self.cli_args);
        let cli = mdq::cli::Cli::try_parse_from(all_cli_args).unwrap();
        let (actual_success, actual_out) = mdq::run_in_memory(&cli, self.md);
        assert_eq!(actual_out, self.expect_output);
        assert_eq!(actual_success, self.expect_success);
    }
}

include!(concat!(env!("OUT_DIR"), "/tests/integ_test_cases.rs"));
