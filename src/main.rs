use clap::Parser;
use mdq::cli::Cli;
use mdq::run_stdio;
use std::process::ExitCode;

fn main() -> ExitCode {
    let cli = Cli::parse();

    if run_stdio(&cli) {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
