use clap::Parser;
use mdq::cli::Cli;
use mdq::{run_stdio, OsFacade};
use std::io;
use std::io::{stdin, Read};
use std::process::ExitCode;

struct RealOs;

impl OsFacade for RealOs {
    fn read_stdin(&self) -> io::Result<String> {
        let mut contents = String::new();
        stdin().read_to_string(&mut contents)?;
        Ok(contents)
    }

    fn read_file(&self, path: &str) -> io::Result<String> {
        std::fs::read_to_string(path)
    }
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    if run_stdio(&cli, RealOs) {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
