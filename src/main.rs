use clap::Parser;
use mdq::cli::Cli;
use mdq::{Error, OsFacade};
use std::io;
use std::io::{stdin, stdout, Read};
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

    fn get_stdout(&mut self) -> impl io::Write {
        stdout().lock()
    }

    fn write_error(&mut self, err: Error) {
        eprint!("{err}")
    }
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    if mdq::run(&cli, &mut RealOs) {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
