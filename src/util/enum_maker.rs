#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnknownVariantError {
    pub string: String,
}

impl std::fmt::Display for UnknownVariantError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unknown variant: '{}'", self.string)
    }
}

impl std::error::Error for UnknownVariantError {}

macro_rules! make_enum {
    [$(#[$meta:meta])* $vis:vis enum $enum_name:ident { $($variant:ident($as_str:literal),)* }] => {


        $(#[$meta])*
        $vis enum $enum_name {
            $($variant,)*
        }

        impl $enum_name {
            pub fn as_str(&self) -> &'static str {
                match self {
                    $(Self::$variant => $as_str,)*
                }
            }

            pub fn iter() -> impl Iterator<Item = Self> {
                [$(Self::$variant),*].into_iter()
            }
        }

        impl std::fmt::Display for $enum_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(self.as_str())
            }
        }

        impl std::str::FromStr for $enum_name {
            type Err = crate::util::enum_maker::UnknownVariantError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($as_str => Ok(Self::$variant),)*
                    other => Err(crate::util::enum_maker::UnknownVariantError{ string: other.to_string() }),
                }
            }

        }
    };
}

pub(crate) use make_enum;
