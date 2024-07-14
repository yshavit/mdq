use clap::Parser;
use mdq::{run_main, Cli};
use std::io::{stdin, Read};
use std::process::ExitCode;

fn main() -> ExitCode {
    let cli = Cli::parse();

    let mut contents = String::new();
    stdin().read_to_string(&mut contents).expect("invalid input (not utf8)");
    if run_main(&cli, contents) {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
