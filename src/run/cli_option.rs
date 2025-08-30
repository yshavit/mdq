use crate::run::{CliOptions, FLAG_BR, FLAG_NO_BR};
use crate::util::enum_maker::make_enum;
use clap::builder::ValueParser;
use clap::Arg;
use std::any::TypeId;

make_enum! {
    /// CLI options. Each corresponds to a flag in [`RunOptions`].
    ///
    /// [`RunOptions`]: crate::run::RunOptions
    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[non_exhaustive]
    pub enum CliOptionName {
        Selectors("selectors"),
        MarkdownFilePaths("markdown_file_paths"),

        AddBreaks("add_breaks"),
        FootnotePos("footnote_pos"),
        LinkFormat("link_format"),
        LinkPos("link_pos"),
        Output("output"),
        Quiet("quiet"),
        RenumberFootnotes("renumber_footnotes"),
        WrapWidth("wrap_width"),
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueFormat {
    /// A boolean argument that doesn't take an argument. Its absence is an implicit `false`, and its presence is a
    /// `true`.
    Flag,
    /// Similar to a [`Flag`], but evaluates to an `Option<bool>` instead of a `bool`. Absence is a value, and presence
    /// is explicit.
    TriFlag {
        flag_true: &'static str,
        flag_false: &'static str,
    },
    /// An integer
    Integer,
    /// An unstructured string
    String,
    /// A value that can be one of a fixed set of possibilities.
    Enum(Vec<EnumVariant>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumVariant {
    pub value: String,
    pub help: String,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CliOption {
    name: CliOptionName,
    cli_switch: Option<String>,
    help: String,
    format: ValueFormat,
}

impl CliOption {
    pub fn name(&self) -> CliOptionName {
        self.name
    }

    pub fn cli_switch(&self) -> &Option<String> {
        &self.cli_switch
    }

    pub fn help(&self) -> &str {
        &self.help
    }

    pub fn format(&self) -> &ValueFormat {
        &self.format
    }
}

impl CliOption {
    pub fn iter() -> impl Iterator<Item = Self> {
        use clap::CommandFactory;
        CliOptions::command()
            .get_arguments()
            .filter(|a| !a.is_hide_set())
            .map(Self::from_arg)
            .collect::<Vec<_>>()
            .into_iter()
    }

    fn from_arg(arg: &Arg) -> Self {
        if arg.is_required_set() {
            panic!(
                "found required arg: {}. need to add \"required\" field to CliOption",
                arg.get_id()
            );
        }
        let name_str = arg.get_id().to_string();
        let name: CliOptionName = name_str.parse().expect(&name_str);
        let help = arg.get_long_help().map(|s| s.to_string()).unwrap_or(String::new());
        let format = match ValueParser::type_id(arg.get_value_parser()) {
            _ if name == CliOptionName::AddBreaks => {
                // "add_breaks" is special, because it's really a dummy that stands in for either --br or --no-br.
                ValueFormat::TriFlag {
                    flag_true: FLAG_BR,
                    flag_false: FLAG_NO_BR,
                }
            }
            p if p == TypeId::of::<usize>() => ValueFormat::Integer,
            p if p == TypeId::of::<bool>() => ValueFormat::Flag,
            p if p == TypeId::of::<String>() => ValueFormat::String,
            _ => {
                let allowed_values: Vec<_> = arg
                    .get_possible_values()
                    .iter()
                    .filter_map(|v| {
                        if v.is_hide_set() {
                            None
                        } else {
                            Some(EnumVariant {
                                value: v.get_name().to_string(),
                                help: v.get_help().map(|s| s.to_string()).unwrap_or(String::new()),
                            })
                        }
                    })
                    .collect();
                if allowed_values.is_empty() {
                    panic!("invalid arg type: {arg}");
                }
                ValueFormat::Enum(allowed_values)
            }
        };
        let cli_switch = match format {
            ValueFormat::TriFlag { .. } => None,
            _ => arg
                .get_long()
                .map(|s| format!("--{s}"))
                .or_else(|| arg.get_short().map(|s| format!("-{s}"))),
        };
        Self {
            name,
            cli_switch,
            help,
            format,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    #[test]
    fn all_args_handled() {
        for opt in CliOption::iter() {
            drop(opt); // we just want to make sure the iterator doesn't panic
        }
    }

    #[test]
    fn no_extra_args() {
        let all_variants: BTreeSet<_> = CliOptionName::iter().collect();
        let option_variants: BTreeSet<_> = CliOption::iter().map(|o| o.name).collect();
        assert_eq!(all_variants, option_variants);
    }
}
