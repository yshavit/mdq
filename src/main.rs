use clap::Parser;
use mdq::{run_stdio, Cli};
use std::process::ExitCode;

fn main() -> ExitCode {
    let cli = Cli::parse();

    if run_stdio(&cli) {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
