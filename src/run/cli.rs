use crate::output;
use crate::output::{LinkTransform, ReferencePlacement};
use clap::error::ErrorKind;
use clap::{CommandFactory, Parser, ValueEnum};
use derive_builder::Builder;
use std::fmt::{Display, Formatter};

macro_rules! create_options_structs {
    (
        $(
            $(#[$meta:meta])*
            clap $clap:tt
            pub $name:ident : $ty:ty
        ),* $(,)?
    ) => {
        #[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Parser)]
        #[command(version, about, long_about = None)]
        #[doc(hidden)]
        pub struct CliOptions {
            $(
            $(#[$meta])*
            #[arg$clap]
            pub(crate) $name: $ty,
            )*

            // clap-only stuff:

            /// Include breaks between elements in plain and markdown output mode.
            ///
            /// For plain, this will add a blank line between elements. For markdown, this will add a thematic break
            /// ("-----") between elements.
            ///
            /// This has no effect in JSON output mode.
            ///
            /// This defaults to true for Markdown output, and false for plain text output.
            // Note: this is a fake arg so we have explicit validation below to ensure it isn't invoked. Clap doesn't let us
            // add boolean flags with 'no-' to disable, so I'm using this trick to fake that out. Basically, this fake arg is
            // only for the help text, and then --br and --no-br are fake but hidden args.
            #[arg(long = "[no]-br", action)]
            pub(crate) br_umbrella: bool,

            /// Negates the --br option.
            #[arg(long, hide = true)]
            pub(crate) br: bool,

            /// Negates the --br option.
            #[arg(long, conflicts_with = "br", hide = true)]
            pub(crate) no_br: bool,

            #[arg(
                short = ' ',
                hide = true,
                group = "selectors_group",
                value_name = "selectors starting with list"
            )]
            pub(crate) list_selector: Option<String>,

            /// The selectors string
            #[arg(group = "selectors_group", value_name = "selectors")]
            pub(crate) selectors: Option<String>,

            /// An optional list of Markdown files to parse, by path. If not provided, standard input will be used.
            ///
            /// If these are provided, mdq will act as if they were all concatenated into a single file. For example, if you
            /// use --link-pos=doc, the link definitions for all input files will be at the very end of the output.
            ///
            /// A path of "-" represents standard input.
            ///
            /// If these are provided, standard input will not be used unless one of the arguments is "-". Files will be
            /// processed in the order you provide them. If you provide the same file twice, mdq will process it twice, unless
            /// that file is "-"; all but the first "-" paths are ignored.
            #[arg()]
            pub(crate) markdown_file_paths: Vec<String>,
        }

        /// Options analogous to the mdq CLI's switches.
        #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Builder)]
        pub struct RunOptions {
            $(
            $(#[$meta])*
            pub $name: $ty,
            )*

            /// Whether to include breaks between elements. This is analogous to the `--[no-]br` option in the CLI
            /// arguments.
            pub add_breaks: Option<bool>,

            pub selectors: String,

            pub markdown_file_paths: Vec<String>
        }

        impl From<CliOptions> for RunOptions {
            fn from(mut value: CliOptions) -> Self {
                let add_breaks = match (value.br, value.no_br) {
                    (false, false) => None,
                    (true, false) => Some(true),
                    (false, true) => Some(false),
                    (true, true) => {
                        // Clap will prevent this from happening. See test [tests::both_br_and_no_br] below.
                        eprintln!("internal error when determining whether to add breaks between elements. will use default settings");
                        None
                    }
                };
                let selectors = match value.selectors.take() {
                    Some(s) => s,
                    None => match &value.list_selector {
                        Some(list_selectors) => {
                            let mut reconstructed = String::with_capacity(list_selectors.len() + 2);
                            reconstructed.push_str("- ");
                            reconstructed.push_str(list_selectors);
                            reconstructed
                        }
                        None => String::new(),
                    },
                };
                Self {
                    $($name: value.$name,)*
                    markdown_file_paths: value.markdown_file_paths,
                    add_breaks,
                    selectors,
                }
            }
        }
    };
}

create_options_structs! {
    /// Where to put link references.
    ///
    /// For links and images of style `[description][1]`, this flag controls where to put the `[1]: https://example.com`
    /// definition.
    clap(long, value_enum, default_value_t=ReferencePlacement::Section)
    pub link_pos: ReferencePlacement,

    /// Where to put footnote references. Defaults to be same as --link-pos
    clap(long, value_enum)
    pub footnote_pos: Option<ReferencePlacement>,

    clap(long, short, value_enum, default_value_t=LinkTransform::Reference)
    pub link_format: LinkTransform,

    clap(long, default_value_t = true, action = clap::ArgAction::Set)
    pub renumber_footnotes: bool,

    /// Specifies the output format. Defaults to markdown.
    clap(long, short, default_value_t = OutputFormat::Markdown)
    pub output: OutputFormat,

    /// The number of characters to wrap text at. This is only valid when the output format is
    /// markdown.
    ///
    /// Certain elements (like section headings and link definitions) will never be wrapped, and the
    /// wrapping will never break a word; it will only ever be along existing whitespace. In
    /// particular, this means the wrapping will never add hyphens, and it will never break URLs.
    clap(long)
    pub wrap_width: Option<usize>,

    /// Quiet: do not print anything to stdout. The exit code will still be 0 if any elements match, and non-0 if none do.
    clap(long, short)
    pub quiet: bool,

    // See: tree.rs > Lookups::unknown_markdown.
    clap(long, hide = true)
    pub allow_unknown_markdown: bool,
}

impl Default for RunOptions {
    fn default() -> Self {
        Self {
            link_pos: ReferencePlacement::Section,
            footnote_pos: None,
            link_format: LinkTransform::Reference,
            renumber_footnotes: true,
            output: OutputFormat::Markdown,
            add_breaks: None,
            wrap_width: None,
            selectors: "".to_string(),
            quiet: false,
            allow_unknown_markdown: false,
            markdown_file_paths: vec![],
        }
    }
}

impl From<&RunOptions> for output::MdWriterOptions {
    fn from(cli: &RunOptions) -> Self {
        output::MdWriterOptions {
            link_reference_placement: cli.link_pos,
            footnote_reference_placement: cli.footnote_pos.unwrap_or(cli.link_pos),
            inline_options: output::InlineElemOptions {
                link_format: cli.link_format,
                renumber_footnotes: cli.renumber_footnotes,
            },
            include_thematic_breaks: cli.should_add_breaks(),
            text_width: cli.wrap_width,
        }
    }
}

impl RunOptions {
    pub fn should_add_breaks(&self) -> bool {
        self.add_breaks.unwrap_or(match self.output {
            OutputFormat::Json => false,
            OutputFormat::Markdown | OutputFormat::Md => true,
            OutputFormat::Plain => false,
        })
    }
}

impl CliOptions {
    pub fn extra_validation(&self) -> bool {
        match self.output {
            OutputFormat::Json => {
                if self.wrap_width.is_some() {
                    let _ = CliOptions::command()
                        .error(
                            ErrorKind::ArgumentConflict,
                            "Can't set text width with JSON output format",
                        )
                        .print();
                    return false;
                }
            }
            OutputFormat::Markdown | OutputFormat::Md => {}
            OutputFormat::Plain => {}
        }
        if self.br_umbrella {
            let _ = CliOptions::command()
                .error(
                    ErrorKind::UnknownArgument,
                    r"invalid argument '--[no]-br'; use '--br' or '--no-br'.",
                )
                .print();
            return false;
        }
        true
    }
}

/// Output formats, analogous to `--output` in the CLI.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, ValueEnum)]
pub enum OutputFormat {
    /// Output results as Markdown.
    Markdown,

    /// Alias for markdown
    Md,

    /// Output results as JSON. Spans of inline elements (like within a single paragraph) will be rendered as a single string of
    /// Markdown, not as separate JSON elements.
    Json,

    /// Outputs just the plain text. This retrains the spacing between paragraphs and paragraph-like blocks (code
    /// blocks, block quotes, etc.) but removes all other formating, including inline formatting. Links are rendered as
    /// just their display text, and footnotes are removed entirely.
    ///
    /// The block:
    ///
    /// ````markdown
    /// hello from [the world](https://example.com)! It's _so easy_ to do do `hello world` in bash[^1]:
    ///
    /// ```bash
    /// echo 'hello world'
    /// ```
    ///
    /// 1. Here's an ordered list
    ///
    /// - Here's an unordered list.
    /// - With multiple items
    ///
    /// [^1]: assuming you have bash installed, of course
    /// ````
    ///
    /// would render as:
    ///
    /// ```text
    /// hello from the world! It's so easy to do do hello world in bash:
    ///
    /// echo 'hello world'
    ///
    /// Here's an ordered list
    /// With multiple items
    ///
    /// Here's an unordered list.
    /// ```
    Plain,
}

impl Default for OutputFormat {
    fn default() -> Self {
        Self::Markdown
    }
}

impl Display for OutputFormat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let self_str = match self {
            OutputFormat::Markdown | OutputFormat::Md => "markdown",
            OutputFormat::Json => "json",
            OutputFormat::Plain => "plain",
        };
        f.write_str(self_str)
    }
}

#[cfg(test)]
mod tests {
    use crate::run::cli::CliOptions;
    use crate::run::RunOptions;
    use crate::util::utils_for_test::*;
    use clap::{Error, Parser};

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        CliOptions::command().debug_assert();
    }

    #[test]
    fn no_args() {
        let result = CliOptions::try_parse_from(["mdq"]);
        unwrap!(result, Ok(cli));
        assert!(cli.markdown_file_paths.is_empty());
        let run_opts: RunOptions = cli.into();
        assert_eq!(run_opts.selectors, "");
    }

    #[test]
    fn no_args_equals_default() {
        let result = CliOptions::try_parse_from(["mdq"]);
        unwrap!(result, Ok(cli));
        let default_run_options = RunOptions::default();
        let from_cli: RunOptions = cli.into();
        assert_eq!(from_cli, default_run_options);
    }

    #[test]
    fn standard_selectors() {
        let result = CliOptions::try_parse_from(["mdq", "# hello"]);
        unwrap!(result, Ok(cli));
        assert!(cli.markdown_file_paths.is_empty());
        let run_opts: RunOptions = cli.into();
        assert_eq!(run_opts.selectors, "# hello");
    }

    #[test]
    fn selector_and_file() {
        let result = CliOptions::try_parse_from(["mdq", "# hello", "file.txt"]);
        unwrap!(result, Ok(cli));
        assert_eq!(cli.markdown_file_paths, ["file.txt"]);
        let run_opts: RunOptions = cli.into();
        assert_eq!(run_opts.selectors, "# hello");
    }

    #[test]
    fn start_with_list_selectors() {
        let result = CliOptions::try_parse_from(["mdq", "- world"]);
        unwrap!(result, Ok(cli));
        let run_opts: RunOptions = cli.into();
        assert_eq!(run_opts.selectors, "- world");
    }

    #[test]
    fn start_with_list_selectors_twice() {
        let result = CliOptions::try_parse_from(["mdq", "- hello", "- world"]);
        check_err(
            &result,
            "the argument '-  <selectors starting with list>' cannot be used multiple times",
        );
    }

    #[test]
    fn both_list_and_std_selectors() {
        let result = CliOptions::try_parse_from(["mdq", "# hello", "- world"]);
        check_err(
            &result,
            "the argument '[selectors]' cannot be used with '-  <selectors starting with list>'",
        )
    }

    #[test]
    fn both_br_and_no_br() {
        let result = CliOptions::try_parse_from(["mdq", "--br", "--no-br"]);
        check_err(&result, "the argument '--br' cannot be used with '--no-br'")
    }

    fn check_err(result: &Result<CliOptions, Error>, expect: &str) {
        unwrap!(result, Err(e));
        let e_str = e.to_string();
        let first_line = e_str.split('\n').next().expect("no error string found");
        let mut expect_full = "error: ".to_string();
        expect_full.push_str(expect);
        assert_eq!(first_line, &expect_full);
    }
}
