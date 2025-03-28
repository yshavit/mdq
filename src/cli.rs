use crate::fmt_md::ReferencePlacement;
use crate::link_transform::LinkTransform;
use clap::error::ErrorKind;
use clap::{CommandFactory, Parser, ValueEnum};
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Where to put link references.
    ///
    /// For links and images of style `[description][1]`, this flag controls where to put the `[1]: https://example.com`
    /// definition.
    #[arg(long, value_enum, default_value_t=ReferencePlacement::Section)]
    pub(crate) link_pos: ReferencePlacement,

    /// Where to put footnote references. Defaults to be same as --link-pos
    #[arg(long, value_enum)]
    pub(crate) footnote_pos: Option<ReferencePlacement>,

    #[arg(long, short, value_enum, default_value_t=LinkTransform::Reference)]
    pub(crate) link_format: LinkTransform,

    #[arg(long, default_value_t = true, action = clap::ArgAction::Set)]
    pub(crate) renumber_footnotes: bool,

    /// Output the results as a JSON object, instead of as markdown.
    #[arg(long, short, default_value_t = OutputFormat::Markdown)]
    pub(crate) output: OutputFormat,

    /// The number of characters to wrap text at. This is only valid when the output format is
    /// markdown.
    ///
    /// Certain elements (like section headings and link definitions) will never be wrapped, and the
    /// wrapping will never break a word; it will only ever be along existing whitespace. In
    /// particular, this means the wrapping will never add hyphens, and it will never break URLs.
    #[arg(long)]
    pub(crate) wrap_width: Option<usize>,

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

    /// Quiet: do not print anything to stdout. The exit code will still be 0 if-and-only-iff any elements match.
    #[arg(long, short)]
    pub(crate) quiet: bool,

    // See: tree.rs > Lookups::unknown_markdown.
    #[arg(long, hide = true)]
    pub(crate) allow_unknown_markdown: bool,
}

impl Cli {
    pub fn selector_string(&self) -> Cow<String> {
        match &self.selectors {
            Some(s) => Cow::Borrowed(s),
            None => match &self.list_selector {
                Some(list_selectors) => {
                    let mut reconstructed = String::with_capacity(list_selectors.len() + 2);
                    reconstructed.push_str("- ");
                    reconstructed.push_str(list_selectors);
                    Cow::Owned(reconstructed)
                }
                None => Cow::Owned(String::new()),
            },
        }
    }

    pub fn extra_validation(&self) -> bool {
        match self.output {
            OutputFormat::Json => {
                if matches!(self.wrap_width, Some(_)) {
                    let _ = Cli::command()
                        .error(
                            ErrorKind::ArgumentConflict,
                            "Can't set text width with JSON output format",
                        )
                        .print();
                    false
                } else {
                    true
                }
            }
            OutputFormat::Markdown | OutputFormat::Md => true,
            OutputFormat::Plain => true,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
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
    use crate::cli::Cli;
    use crate::unwrap;
    use clap::{Error, Parser};

    #[test]
    fn verify_cli() {
        use crate::cli::Cli;
        use clap::CommandFactory;
        Cli::command().debug_assert();
    }

    #[test]
    fn standard_selectors() {
        let result = Cli::try_parse_from(["mdq", "# hello"]);
        unwrap!(result, Ok(cli));
        assert_eq!(cli.selector_string().as_str(), "# hello");
    }

    #[test]
    fn two_standard_selectors() {
        let result = Cli::try_parse_from(["mdq", "# hello", "# world"]);
        check_err(&result, "unexpected argument '# world' found");
    }

    #[test]
    fn start_with_list_selectors() {
        let result = Cli::try_parse_from(["mdq", "- world"]);
        unwrap!(result, Ok(cli));
        assert_eq!(cli.selector_string().as_str(), "- world");
    }

    #[test]
    fn start_with_list_selectors_twice() {
        let result = Cli::try_parse_from(["mdq", "- hello", "- world"]);
        check_err(
            &result,
            "the argument '-  <selectors starting with list>' cannot be used multiple times",
        );
    }

    #[test]
    fn both_list_and_std_selectors() {
        let result = Cli::try_parse_from(["mdq", "# hello", "- world"]);
        check_err(
            &result,
            "the argument '[selectors]' cannot be used with '-  <selectors starting with list>'",
        )
    }

    fn check_err(result: &Result<Cli, Error>, expect: &str) {
        unwrap!(result, Err(e));
        let e_str = e.to_string();
        let first_line = e_str.split('\n').next().expect("no error string found");
        let mut expect_full = "error: ".to_string();
        expect_full.push_str(expect);
        assert_eq!(first_line, &expect_full);
    }
}
