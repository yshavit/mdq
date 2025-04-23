use clap::Parser;
use mdq::run::{CliOptions, Error, OsFacade};
use std::io;
use std::io::{stdin, stdout, Read};
use std::process::ExitCode;

struct RealOs;

#[doc(hidden)]
impl OsFacade for RealOs {
    fn read_stdin(&self) -> io::Result<String> {
        let mut contents = String::new();
        stdin().read_to_string(&mut contents)?;
        Ok(contents)
    }

    fn read_file(&self, path: &str) -> io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn stdout(&mut self) -> impl io::Write {
        stdout().lock()
    }

    fn write_error(&mut self, err: Error) {
        eprint!("{err}")
    }
}

fn main() -> ExitCode {
    let cli = CliOptions::parse();

    if !cli.extra_validation() {
        return ExitCode::FAILURE;
    }

    if mdq::run::run(&cli.into(), &mut RealOs) {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
