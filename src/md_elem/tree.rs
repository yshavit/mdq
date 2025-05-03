use crate::md_elem::concatenate::Concatenate;
use std::backtrace::Backtrace;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Write};
use std::hash::Hash;
use std::vec::IntoIter;

use elem::*;
use markdown::mdast;

/// Additional context for navigating or parsing a Markdown document.
///
/// The main functionality this exposes is [`MdContext::get_footnote`], which lets you look up a footnote's contents by
/// its id. Footnotes can contain loops, such as:
///
/// ```markdown
/// This is my footnote[^1].
///
/// [^1]: Note that it contains a cycle[^2].
/// [^2]: This cycles back to the first footnote[^1].
/// ```
///
/// That means we can't contain them fully within an MdElem without some external state to hold the links. This is that
/// state.
///
/// See [`elem::FootnoteId`](FootnoteId) for an example showing how footnotes get parsed, and how to use the context to
/// fetch them.
#[derive(Clone, Default, Debug, PartialEq)]
pub struct MdContext {
    footnotes: HashMap<FootnoteId, Vec<MdElem>>,

    /// useful as a default value
    empty_md_elems: Vec<MdElem>,
}

impl MdContext {
    /// Gets a footnote's contents, given its id.
    ///
    /// The id does _not_ include the caret: footnote `[^1]` will have id `"1"`.
    ///
    /// See [`elem::FootnoteId`](FootnoteId) for an example showing how footnotes get parsed, and how to use the context
    /// to fetch them.
    pub fn get_footnote(&self, footnote_id: &FootnoteId) -> &Vec<MdElem> {
        self.footnotes.get(footnote_id).unwrap_or(&self.empty_md_elems)
    }

    fn new() -> Self {
        Self {
            footnotes: HashMap::with_capacity(4), // total guess
            empty_md_elems: Vec::new(),
        }
    }
}

/// A fully parsed Markdown document.
///
/// This comprises the root (top-level) [MdElem]s, as well as the [MdContext] for navigating footnotes.
///
/// See [`MdDoc::parse`] how to create one.
#[derive(Clone, Default, Debug, PartialEq)]
pub struct MdDoc {
    pub roots: Vec<MdElem>,
    pub ctx: MdContext,
}

impl MdDoc {
    /// Parse some Markdown into.
    ///
    /// See the various examples in [`elem`] for examples of this parsing in action.
    pub fn parse(text: &str, options: &ParseOptions) -> Result<Self, InvalidMd> {
        parse0(text, options)
    }
}

/// A single node of the parsed Markdown.
///
/// These come in three flavors:
///
/// - block container nodes, which contain other `MdElem`s within them
/// - inline container nodes, which contain [`Inline`]s
/// - leaf nodes, which are scalars and represent the leaf nodes of the tree
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum MdElem {
    /// Block container: A full Markdown document, or contiguous section of one.
    ///
    /// This is distinct from just a `Vec<MdElem>` in that `Doc` represents a single document, while `Vec<MdElem>`
    /// represents a stream of elements. For example, if you had:
    ///
    /// ```markdown
    /// # First section
    ///
    /// Some text
    ///
    /// - a list item
    /// - another list item
    ///
    /// More text.
    ///
    /// # Second section
    ///
    /// And so on.
    /// ```
    ///
    /// Then that whole thing can be represented by a single `Doc` with two elements (both [`MdElem::Section`]s).
    /// Additionally, the body of each section is also a `Doc`. The `Doc` for the first section is:
    ///
    /// ```markdown
    /// Some text
    ///
    /// - a list item
    /// - another list item
    ///
    /// More text.
    /// ```
    ///
    /// On the other hand, if you select list items containing the word "item", you'll get a `Vec<MdElem>` (_not_ a
    /// `Doc`) containing two items, one for each list item.
    ///
    /// Concretely: When mdq outputs the Markdown, it can (if requested) put thematic breaks or newlines between each
    /// element of a `Vec<MdElem>`, but it will not do that for the elements within a `Doc`.
    Doc(Vec<MdElem>),

    // Container blocks
    BlockQuote(BlockQuote),
    List(List),
    Section(Section),

    // Leaf blocks
    CodeBlock(CodeBlock),
    Paragraph(Paragraph),
    Table(Table),
    /// A thematic break:
    ///
    /// ```markdown
    /// -----
    /// ```
    ///
    /// Note that there are several styles of thematic break. mdq unifies them at parse time, which results in some
    /// information loss; you can't necessarily reconstruct the original Markdown.
    ThematicBreak,
    Inline(Inline),
    BlockHtml(BlockHtml),
}

/// Options for parsing Markdown.
///
/// See: [`MdDoc::parse`].
#[derive(Default, Debug)]
pub struct ParseOptions {
    pub(crate) mdast_options: markdown::ParseOptions,
    /// Usually only required for debugging. Defaults to `false`.
    ///
    /// If mdq encounters an unexpected state coming from the underlying parsing library it uses, it can either ignore
    /// it or error out. If this field is set to `true`, mdq will ignore the unexpected state. Otherwise,
    /// [`MdDoc::parse`] will return an `Err` containing [`InvalidMd::UnknownMarkdown`].
    pub allow_unknown_markdown: bool,
}

impl ParseOptions {
    pub fn gfm() -> Self {
        Self {
            mdast_options: markdown::ParseOptions::gfm(),
            allow_unknown_markdown: true,
        }
    }
}

/// Parse some Markdown text.
fn parse0(text: &str, options: &ParseOptions) -> Result<MdDoc, InvalidMd> {
    let ast = markdown::to_mdast(text, &options.mdast_options).map_err(|e| InvalidMd::ParseError(format!("{e}")))?;
    let read_options = ReadOptions {
        validate_no_conflicting_links: false,
        allow_unknown_markdown: options.allow_unknown_markdown,
    };
    MdDoc::read(ast, &read_options)
}

#[derive(Default)]
pub(crate) struct ReadOptions {
    /// For links and images, enforce that reference-style links have at most one definition. If this value is
    /// `false` and a link has multiple definitions, the first one will be picked.
    ///
    /// For example:
    ///
    /// ```md
    /// [ambiguous link][1]
    ///
    /// [1]: https://example.com/one
    /// [1]: https://example.com/conflicting_url
    /// ```
    ///
    /// If this value is `true` and there are multiple _identical_ links, the validation will still pass:
    ///
    /// ```md
    /// [non-ambiguous link][1]
    ///
    /// [1]: https://example.com/one
    /// [1]: https://example.com/one
    /// ```
    pub validate_no_conflicting_links: bool,

    pub allow_unknown_markdown: bool,
}

/// Various error conditions that can come from trying to parse Markdown.
#[derive(Debug, PartialEq, Eq)]
pub enum InvalidMd {
    /// Encountered a Markdown component that mdq doesn't support.
    ///
    /// This is typically an extension for a particular Markdown variant which mdq knows about, but does not handle.
    Unsupported(MarkdownPart),

    /// Internal error. You shouldn't get this.
    NonListItemDirectlyUnderList(MarkdownPart),
    /// Internal error. You shouldn't get this.
    NonRowDirectlyUnderTable(MarkdownPart),
    /// Internal error. You shouldn't get this.
    NonInlineWhereInlineExpected(MdElem),
    /// Internal error. You shouldn't get this.
    MissingReferenceDefinition(String),
    /// Internal error. You shouldn't get this.
    ConflictingReferenceDefinition(String),
    /// Internal error. You shouldn't get this.
    InternalError(UnknownMdParseError),
    /// Internal error. You shouldn't get this.
    ///
    /// See [`ParseOptions::allow_unknown_markdown`].
    UnknownMarkdown(&'static str),
    /// Internal error. You shouldn't get this.
    ParseError(String),
}

impl std::error::Error for InvalidMd {}

/// A portion of unparsable markdown.
///
/// This wraps the AST from the underlying library that mdq uses; the only thing you can really do with it is to use its
/// `Debug`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MarkdownPart {
    node: Box<mdast::Node>,
}

/// An unknown, internal error.
///
/// See [`InvalidMd::InternalError`].
// A wrapper for [Backtrace] that implements [PartialEq] to always return `true`. This lets us use it in a struct
// while still letting us use `#[derive(PartialEq)]`
#[derive(Debug)]
pub struct UnknownMdParseError {
    backtrace: Backtrace,
}

impl Display for UnknownMdParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.backtrace, f)
    }
}

impl std::error::Error for UnknownMdParseError {}

impl PartialEq for UnknownMdParseError {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for UnknownMdParseError {}

impl Display for InvalidMd {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidMd::Unsupported(node) => {
                write!(f, "unsupported node: {:?}", node)
            }
            InvalidMd::NonListItemDirectlyUnderList(node) => {
                write!(f, "expected a list item, but found: {:?}", node)
            }
            InvalidMd::NonRowDirectlyUnderTable(node) => {
                write!(f, "expected a row, but found: {:?}", node)
            }
            InvalidMd::NonInlineWhereInlineExpected(node) => {
                write!(f, "expected an inline element, but found: {:?}", node)
            }
            InvalidMd::MissingReferenceDefinition(id) => {
                write!(f, "couldn't find definition for link/image/footnote: {}", id)
            }
            InvalidMd::ConflictingReferenceDefinition(id) => {
                write!(f, "found multiple definitions for link/image/footnote: {}", id)
            }
            InvalidMd::InternalError(err) => {
                f.write_str("internal error\n")?;
                std::fmt::Display::fmt(&err.backtrace, f)
            }
            InvalidMd::UnknownMarkdown(description) => {
                write!(f, "encountered unknown markdown: {}\n\n", description)?;
                f.write_str("* Please consider reporting this at https://github.com/yshavit/mdq/issues\n")?;
                f.write_str("* You can suppress this error by using --allow-unknown-markdown.")
            }
            InvalidMd::ParseError(s) => {
                write!(f, "encountered when parsing markdown")?;
                f.write_str("* Please consider reporting this at https://github.com/yshavit/mdq/issues\n")?;
                write!(f, "{s}")
            }
        }?;
        f.write_char('\n')
    }
}

/// Inner details of the [MdElem] variants.
///
/// This module includes the types that provide the details of [`md_elem::MdElem`](MdElem) variants, as well as
/// supporting structs. There are four kinds of items in this module:
///
/// - **container markdown** structs, which represent Markdown elements that contain other block or inline structs.
///
/// - **leaf block markdown** structs, which represent Markdown elements that are rendered as blocks, but which cannot
///   contain other blocks. Depending on the specific struct, it may include inline markdown elements, or just plain
///   text.
///
/// - **inline markdown** structs, which represent Markdown elements that are rendered inline, and may contain other
///   inline or terminal markdown elements.
///
/// - **terminal markdown** structs, which represent Markdown elements that cannot contain other inline structs.
///
/// - **supporting items**, which do not map to full Markdown elements, but are instead sub-components of Markdown
///   elements. (These include things like text variants and link definitions).
pub mod elem {
    use super::*;
    use std::mem;

    /// A table row.
    ///
    /// This is just a `Vec` of table cells, but the type alias makes it much easier to reason about.
    pub type TableRow = Vec<TableCell>;

    /// A table cell within a row.
    ///
    /// This is just a `Vec` of [Inline]s, but the type alias makes it much easier to reason about.
    pub type TableCell = Vec<Inline>;

    /// Different kinds of code blocks.
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub enum CodeVariant {
        /// A standard code block
        ///
        /// ````markdown
        /// ```
        /// foo()
        /// ```
        /// ````
        ///
        /// If the `Option<CodeOpts>` is `None`, this block has no metadata in the opening fence (as in the example
        /// directly above). Otherwise, the first word directly after the backticks is the language, and the rest of
        /// the string is the metadata:
        ///
        /// ````markdown
        /// ```rust and some metadata
        /// foo()
        /// ```
        /// ````
        Code(Option<CodeOpts>),
        Math {
            metadata: Option<String>,
        },
        Toml,
        Yaml,
    }

    /// Some inline text.
    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    pub enum Inline {
        /// The marker for a footnote; this is a terminal node in the [MdElem] tree.
        ///
        /// ```markdown
        /// [^1]
        /// ```
        ///
        /// Note that this does not include the content of the footnote. To get that, use [`MdContext::get_footnote`].
        Footnote(FootnoteId),

        /// A span of formatted text (~~deleted~~, _emphasized_, or **strong**).
        ///
        /// See [`Span`].
        Span(Span),

        /// An image; this is a terminal node in the [MdElem] tree.
        ///
        /// ```markdown
        /// ![alt text](https://example.com/hello-world.png)
        /// ```
        Image(Image),

        /// A link
        ///
        /// ```markdown
        /// ![display text](https://example.com/hello-world)
        /// ```
        Link(Link),

        /// Some text; this is a terminal node in the [MdElem] tree.
        Text(Text),
    }

    impl Concatenate for Inline {
        fn try_concatenate(&mut self, mut other: Self) -> Result<(), Self> {
            match (self, &mut other) {
                (Self::Span(my), Self::Span(other)) if my.variant == other.variant => {
                    // Combine span(my) + span(other) into span(my+other)
                    // But my and other are also spans, and now that they're concatenated, we may have elements that
                    // are newly concatenable. So, recurse!
                    my.children.append(&mut other.children);
                    my.children = Concatenate::concatenate_similar(mem::take(&mut my.children));
                    Ok(())
                }
                (Self::Text(my), Self::Text(other))
                    if my.variant == TextVariant::Plain && other.variant == TextVariant::Plain =>
                {
                    // Only combine plaintext; other text elements have semantic meaning that may be lost.
                    // For example, two expressions `x + y` and `1 + 2` don't imply `x + y 1 + 2`
                    my.value.push_str(&other.value);
                    Ok(())
                }
                _ => Err(other),
            }
        }
    }

    /// Leaf block markdown representing block-level HTML.
    ///
    /// These are `<tag>`s that start a line. The exact rules are somewhat involved (see:
    /// <https://spec.commonmark.org/0.31.2/#html-blocks>), but basically these are non-inlined html tags.
    ///
    /// The [`BlockHtml::value`] includes the opening and closing tags, and any text between them.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// <div>
    ///
    /// My div content
    ///
    /// </div>
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// let expected_html_1 = BlockHtml{value: "<div>".to_string()};
    /// let expected_html_2 = BlockHtml{value: "</div>".to_string()};
    ///
    /// let expected_full_md = vec![
    ///     MdElem::BlockHtml(expected_html_1),
    ///     MdElem::Paragraph(Paragraph{
    ///         body: vec![Inline::Text(Text{
    ///             variant: TextVariant::Plain,
    ///             value: "My div content".to_string(),
    ///         })]
    ///     }),
    ///     MdElem::BlockHtml(expected_html_2),
    /// ];
    /// assert_eq!(parsed.roots, expected_full_md);
    /// ```
    ///
    /// c.f. [`TextVariant::InlineHtml`]
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct BlockHtml {
        pub value: String,
    }

    impl Display for BlockHtml {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.value)
        }
    }

    /// Inline markdown representing formatted text (~~deleted~~, _emphasized_, or **strong**).
    ///
    /// The span's body is a `Vec<Inline>`, so you can have nested spans, emphasized link text, etc.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// Some _text **with** ~deletes~_.
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// /// helper
    /// fn plain_text(text: &str) -> Inline {
    ///     Inline::Text(Text{
    ///         variant: TextVariant::Plain,
    ///         value: text.to_string()
    ///     })
    /// }
    ///
    /// let expected_deleted = Span{
    ///     variant: SpanVariant::Delete,
    ///     children: vec![plain_text("deletes")],
    /// };
    /// let expected_strong = Span{
    ///     variant: SpanVariant::Strong,
    ///     children: vec![plain_text("with")],
    /// };
    /// let expected_emphasis = Span{
    ///     variant: SpanVariant::Emphasis,
    ///     children: vec![
    ///         // Unlike the above examples, this one happens to have
    ///         // some nested spans.
    ///         plain_text("text "),
    ///         Inline::Span(expected_strong),
    ///         plain_text(" "),
    ///         Inline::Span(expected_deleted),
    ///     ],
    /// };
    ///
    /// let expected_full_md = vec![
    ///     MdElem::Paragraph(Paragraph{
    ///         body: vec![
    ///             plain_text("Some "),
    ///             Inline::Span(expected_emphasis),
    ///             plain_text("."),
    ///         ]
    ///     }),
    /// ];
    /// assert_eq!(parsed.roots, expected_full_md);
    /// ```
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Span {
        pub variant: crate::md_elem::tree::elem::SpanVariant,
        pub children: Vec<crate::md_elem::tree::elem::Inline>,
    }

    /// Terminal markdown representing an atomic chunk of text.
    ///
    /// This is not just for plain text: inline code, inline HTML, and inline math also get parsed as `Text`. The thing
    /// that defines elements as `Text` (as opposed to [`Span`]) is that they cannot have other elements nested in them.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// Hello, `robots`. <em>And now with gusto!</em>
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// let expected_plain_text = Text{
    ///     variant: TextVariant::Plain,
    ///     value: "Hello, ".to_string(),
    /// };
    /// let expected_inline_code = Text{
    ///     variant: TextVariant::Code,
    ///     value: "robots".to_string(),
    /// };
    /// let expected_inline_html_open = Text{
    ///     variant: TextVariant::InlineHtml,
    ///     value: "<em>".to_string(),
    /// };
    /// let expected_inline_html_contents = Text{
    ///     variant: TextVariant::Plain,
    ///     value: "And now with gusto!".to_string(),
    /// };
    /// let expected_inline_html_close = Text{
    ///     variant: TextVariant::InlineHtml,
    ///     value: "</em>".to_string(),
    /// };
    ///
    /// let expected_full_md = vec![
    ///     MdElem::Paragraph(Paragraph{
    ///         body: vec![
    ///             Inline::Text(expected_plain_text),
    ///             Inline::Text(expected_inline_code),
    ///             Inline::Text(Text{
    ///                 variant: TextVariant::Plain,
    ///                 value: ". ".to_string(),
    ///             }),
    ///             Inline::Text(expected_inline_html_open),
    ///             Inline::Text(expected_inline_html_contents),
    ///             Inline::Text(expected_inline_html_close),
    ///         ],
    ///     })
    /// ];
    /// assert_eq!(parsed.roots, expected_full_md);
    /// ```
    ///
    /// See [`Inline::Text`]
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Text {
        pub variant: crate::md_elem::tree::elem::TextVariant,
        pub value: String,
    }

    /// Inline markdown representing a link.
    ///
    /// (Note that this struct is inline markdown, while the similar [`Image`] is terminal. That's because a link's
    /// display text can include other inline elements, while image alt text is always just plain text.)
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// [the display text](https://example.com/link.html)
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// let expected_link = Link {
    ///     display: vec![Inline::Text(Text{
    ///         variant: TextVariant::Plain,
    ///         value: "the display text".to_string(),
    ///     })],
    ///     link: LinkDefinition{
    ///         url: "https://example.com/link.html".to_string(),
    ///         title: None,
    ///         reference: LinkReference::Inline,
    ///     }
    /// };
    ///
    /// let expected = vec![
    ///     MdElem::Paragraph(Paragraph{
    ///         body: vec![Inline::Link(expected_link)]
    ///     }),
    /// ];
    /// assert_eq!(parsed.roots, expected);
    /// ```
    ///
    /// See [`Inline::Link`]
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Link {
        pub display: Vec<crate::md_elem::tree::elem::Inline>,
        pub link: crate::md_elem::tree::elem::LinkDefinition,
    }

    /// Terminal markdown inline image.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// ![the alt text](https://example.com/image.png)
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// let expected_image = Image {
    ///     alt: "the alt text".to_string(),
    ///     link: LinkDefinition{
    ///         url: "https://example.com/image.png".to_string(),
    ///         title: None,
    ///         reference: LinkReference::Inline,
    ///     }
    /// };
    ///
    /// let expected = vec![
    ///     MdElem::Paragraph(Paragraph{
    ///         body: vec![Inline::Image(expected_image)]
    ///     }),
    /// ];
    /// assert_eq!(parsed.roots, expected);
    /// ```
    ///
    /// See [`Inline::Image`]
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Image {
        pub alt: String,
        pub link: crate::md_elem::tree::elem::LinkDefinition,
    }

    /// Terminal markdown representing the id for a footnote.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// This is[^1] some text.
    ///
    /// [^1]: the footnote text
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// let expected_footnote_id = FootnoteId{id: "1".to_string()}; // Note: does not include the caret
    ///
    /// let expected_footnote_contents = vec![
    ///     MdElem::Paragraph(Paragraph{
    ///         body: vec![Inline::Text(Text{
    ///             variant: TextVariant::Plain,
    ///             value: "the footnote text".to_string(),
    ///         })],
    ///     })
    /// ];
    ///
    /// let expected_full_md = vec![
    ///     MdElem::Paragraph(Paragraph{
    ///         body: vec![
    ///             Inline::Text(Text{
    ///                 variant: TextVariant::Plain,
    ///                 value: "This is".to_string(),
    ///             }),
    ///             Inline::Footnote(expected_footnote_id.clone()),
    ///             Inline::Text(Text{
    ///                 variant: TextVariant::Plain,
    ///                 value: " some text.".to_string(),
    ///             }),
    ///         ]
    ///     }),
    /// ];
    /// assert_eq!(parsed.roots, expected_full_md);
    /// assert_eq!(parsed.ctx.get_footnote(&expected_footnote_id), &expected_footnote_contents);
    /// ```
    ///
    /// See [`Inline::Footnote`] and [`MdContext::get_footnote`]
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct FootnoteId {
        pub id: String,
    }

    impl From<String> for crate::md_elem::tree::elem::FootnoteId {
        fn from(id: String) -> Self {
            Self { id }
        }
    }

    impl crate::md_elem::tree::elem::FootnoteId {
        /// Gets this footnote's reference id as a string.
        ///
        /// For example, given the markdown:
        ///
        /// # Examples
        ///
        /// ```markdown
        /// Hello[^1], world.
        ///
        /// [^1]: this is a standard greeting
        /// ```
        ///
        /// the `FootnoteId`'s `as_str()` would be `"^1"`.
        pub fn as_str(&self) -> &str {
            &self.id
        }

        pub(crate) fn new(id: String, label: Option<String>) -> crate::md_elem::tree::elem::FootnoteId {
            let id = label.unwrap_or(id);
            Self { id }
        }
    }

    /// Supporting struct representing the link in an [`Link`] or [`Image`].
    ///
    /// See those two types for examples.
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct LinkDefinition {
        /// The link's destination.
        pub url: String,
        /// If you have `[1]: https://example.com "my title"`, this is the "my title".
        ///
        /// See: <https://github.github.com/gfm/#link-reference-definitions>
        pub title: Option<String>,
        /// The link's reference style.
        pub reference: LinkReference,
    }

    /// Container markdown representing an item within a list.
    ///
    /// See [`List`] for examples.
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct ListItem {
        /// If `None`, this item is not a task. If `Some`, this is a task, with the bool representing whether it's
        /// marked as completed.
        pub checked: Option<bool>,
        pub item: Vec<MdElem>,
    }

    /// ~~deleted~~, _emphasized_, or **strong**; see [`Span`]
    #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum SpanVariant {
        Delete,
        Emphasis,
        Strong,
    }

    /// Plain, `inline code`, or inline math text; see [`Text`]
    #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum TextVariant {
        /// Plain text
        Plain,

        /// `inline code`
        Code,

        /// $inline math$
        Math,

        /// inline <span>html</span>
        ///
        /// Each tag (and not any text inside matching tags) is its own [`Inline::Text`], and the text's `value`
        /// includes the angle brackets:
        ///
        /// # Examples
        ///
        /// ```
        /// use mdq::md_elem::{*, elem::*};
        /// use mdq::md_elem::elem::{Inline, Paragraph, Text, TextVariant};
        /// let md_elems = MdDoc::parse("inline <span>html</span>", &ParseOptions::gfm()).unwrap();
        /// assert_eq!(1, md_elems.roots.len());
        /// let MdElem::Paragraph(Paragraph{body}) = &md_elems.roots[0] else { panic!() };
        /// assert_eq!(4, body.len());
        /// assert_eq!(&Inline::Text(Text{variant: TextVariant::InlineHtml, value: "<span>".to_string()}), &body[1]);
        /// ```
        ///
        /// c.f. [`BlockHtml`]
        InlineHtml,
    }

    /// Container markdown representing a section title and its body.
    ///
    /// ```markdown
    /// # This is a title with depth 1
    ///
    /// This is its body.
    /// ```
    ///
    /// Note that many Markdown parsers treat Markdown as non-hierarchical: the title is a top-level node, and its
    /// body is another top-level node sibling to it. mdq does _not_ do this: each section's body belongs to that
    /// section.
    ///
    /// Deeper sections will be contained as [`MdElem::Section`]s within this [`Section::body`]. For example, given:
    ///
    /// ```markdown
    /// # This is a title with depth 1
    ///
    /// Hello, world.
    ///
    /// ## This is a title with depth 2
    ///
    /// Alpha, bravo.
    ///
    /// # Back to depth 1
    ///
    /// Foo, bar.
    /// ```
    ///
    /// ... the tree would look roughly like:
    ///
    /// > - This is a title with depth 1
    /// >
    /// >   Hello, world.
    /// >
    /// >   - This is a title with depth 2
    /// >
    /// >   - This is a title with depth 2
    /// >     Alpha, bravo
    /// >
    /// > - Back to depth 1
    /// >
    /// >   Foo, bar.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// ### The section title
    ///
    /// Some contents
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// let expected = vec![
    ///     MdElem::Section(Section{
    ///         depth: 2,
    ///         title: vec![Inline::Text(Text{
    ///             variant: TextVariant::Plain,
    ///             value: "The section title".to_string(),
    ///         })],
    ///         body: vec![MdElem::Paragraph(Paragraph{
    ///             body: vec![Inline::Text(Text{
    ///                 variant: TextVariant::Plain,
    ///                 value: "Some contents".to_string(),
    ///             })],
    ///         })],
    ///     }),
    /// ];
    /// assert_eq!(parsed.roots, expected);
    /// ```
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Section {
        pub depth: u8,
        pub title: Vec<Inline>,
        pub body: Vec<MdElem>,
    }

    /// Leaf block markdown representing a normal paragraph.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// Hello, world
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// let expected = vec![
    ///     MdElem::Paragraph(Paragraph{
    ///         body: vec![Inline::Text(Text{
    ///             variant: TextVariant::Plain,
    ///             value: "Hello, world".to_string(),
    ///         })]
    ///     })
    /// ];
    /// assert_eq!(parsed.roots, expected);
    /// ```
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Paragraph {
        pub body: Vec<Inline>,
    }

    /// Container markdown representing block-quoted text.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// > Hello, world
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// let expected = vec![
    ///     MdElem::BlockQuote(BlockQuote{
    ///         body: vec![
    ///             MdElem::Paragraph(Paragraph{
    ///                 body: vec![Inline::Text(Text{
    ///                     variant: TextVariant::Plain,
    ///                     value: "Hello, world".to_string(),
    ///                 })]
    ///             })
    ///         ]
    ///     })
    /// ];
    /// assert_eq!(parsed.roots, expected);
    /// ```
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct BlockQuote {
        pub body: Vec<MdElem>,
    }

    /// Container markdown representing a list.
    ///
    /// ```markdown
    /// - hello
    /// - world
    /// ```
    ///
    /// or
    ///
    /// ```markdown
    /// 1. hello
    /// 2. world
    /// ```
    ///
    /// If [`List::starting_index`] is `None`, this is an unordered list. If it is `Some`, this is an ordered list,
    /// starting at the specified index.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// - alpha
    ///
    /// 1. bravo
    ///
    /// - [ ] charlie
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// /// helper fn
    /// fn paragraph(text: &str) -> Paragraph {
    ///     Paragraph{
    ///         body: vec![Inline::Text(Text{
    ///             variant: TextVariant::Plain,
    ///             value: text.to_string(),
    ///         })]
    ///     }
    /// }
    ///
    /// let unordered_list = List{
    ///     starting_index: None,
    ///     items: vec![ListItem{
    ///         checked: None,
    ///         item: vec![MdElem::Paragraph(paragraph("alpha"))]
    ///     }],
    /// };
    /// let ordered_list = List{
    ///     starting_index: Some(1),
    ///     items: vec![ListItem{
    ///         checked: None,
    ///         item: vec![MdElem::Paragraph(paragraph("bravo"))]
    ///     }],
    /// };
    /// let task_list = List{
    ///     starting_index: None,
    ///     items: vec![ListItem{
    ///         checked: Some(false),
    ///         item: vec![MdElem::Paragraph(paragraph("charlie"))]
    ///     }],
    /// };
    ///
    /// let expected = vec![
    ///     MdElem::List(unordered_list),
    ///     MdElem::List(ordered_list),
    ///     MdElem::List(task_list),
    /// ];
    /// assert_eq!(parsed.roots, expected);
    /// ```
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct List {
        pub starting_index: Option<u32>,
        pub items: Vec<ListItem>,
    }

    /// Leaf block markdown representing a table.
    ///
    /// Tables will always have at least one row, and the length of [`Table::alignments`] should equal the length of
    /// that first row's `Vec<TableCell>`.
    ///
    /// Note that tables have lots of nested vecs: the table's body is a vec of rows, each of which is a vec of cells,
    /// each of which is a vec of inlines. These can be hard to keep track of, so we provide the [`TableRow`] and
    /// [`TableCell`] type aliases to help keep track of things.
    ///
    /// # Examples
    ///
    /// ```
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r"
    /// | Header 1 | Header 2 |
    /// |:--------:|----------|
    /// |  Hello   | World    |
    /// ";
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// /// helper: note that a cell is a Vec of Inlines!
    /// fn plain_text_cell(text: &str) -> TableCell {
    ///     vec![Inline::Text(Text{
    ///         variant: TextVariant::Plain,
    ///         value: text.to_string()
    ///     })]
    /// }
    ///
    /// let expected_full_md = vec![
    ///     MdElem::Table(Table{
    ///         alignments: vec![Some(ColumnAlignment::Center), None],
    ///         rows: vec![
    ///             vec![plain_text_cell("Header 1"), plain_text_cell("Header 2")],
    ///             vec![plain_text_cell("Hello"), plain_text_cell("World")],
    ///         ],
    ///     }),
    /// ];
    /// assert_eq!(parsed.roots, expected_full_md);
    /// ```
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Table {
        pub alignments: Vec<Option<ColumnAlignment>>,
        /// The table's rows. Each `TableRow` is a `Vec<TableCell>`, and each `TableCell` is a `Vec<Inline>`. It can
        /// get confusing to iterate these nested vecs!
        ///
        /// The first row is the header row.
        pub rows: Vec<TableRow>,
    }

    /// Left, right, or center for table columns.
    ///
    /// This enum does not define "no alignment". It is typically used in an `Option<ColumnAlignment>`, where "no
    /// alignment" is represented as `None`.
    #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum ColumnAlignment {
        Left,
        Right,
        Center,
    }

    /// Leaf block markdown representing a code block.
    ///
    /// This includes "code-like" blocks like yaml and block-level math, which some variants of Markdown support.
    ///
    /// # Examples
    ///
    /// ````
    /// use mdq::md_elem::{*, elem::*};
    /// let md_text = r#"
    /// ```rust title=example.rs
    /// println!("Hello, world!");
    /// ```
    /// "#;
    ///
    /// let parsed = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
    ///
    /// let expected = vec![
    ///     MdElem::CodeBlock(CodeBlock{
    ///         variant: CodeVariant::Code(Some(CodeOpts{
    ///             language: "rust".to_string(),
    ///             metadata: Some("title=example.rs".to_string())
    ///         })),
    ///         value: r#"println!("Hello, world!");"#.to_string(),
    ///     }),
    /// ];
    /// assert_eq!(parsed.roots, expected);
    /// ````
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct CodeBlock {
        /// The type of code block
        pub variant: CodeVariant,
        /// The contents of the code block; everything between the fence lines, or the indented contents.
        pub value: String,
    }

    /// A [LinkDefinition]'s reference style.
    ///
    /// See <https://github.github.com/gfm/#link-reference-definitions>
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum LinkReference {
        /// ```markdown
        /// [hello](https://example.com/world)
        /// ```
        Inline,

        /// ```markdown
        /// [hello][hello-link]
        ///
        /// [hello-link]: https://example.com/world
        /// ```
        ///
        /// This includes numbered links:
        ///
        /// ```markdown
        /// [hello][1]
        ///
        /// [1]: https://example.com/world
        /// ```
        Full(String),

        /// ```markdown
        /// [hello][]
        ///
        /// [hello]: https://example.com/world
        /// ```
        Collapsed,

        /// ```markdown
        /// [hello]
        ///
        /// [hello]: https://example.com/world
        /// ```
        Shortcut,
    }

    /// Supporting struct representing the metadata on a fenced code block.
    ///
    /// Given the markdown:
    ///
    /// ````markdown
    /// ```rust foo
    /// println!("hello, world");
    /// ```
    /// ````
    ///
    /// ... this struct represents the ` ```rust foo ` part.
    ///
    /// See [`CodeVariant::Code`]
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct CodeOpts {
        pub language: String,
        pub metadata: Option<String>,
    }

    macro_rules! from_for_md_elem {
        ($elem:ident ($inner:ident)) => {
            impl From<$inner> for MdElem {
                fn from(value: $inner) -> Self {
                    MdElem::$elem(value)
                }
            }
        };
        ($elem:ident) => {
            from_for_md_elem! {$elem ($elem)}
        };
    }

    from_for_md_elem! { BlockQuote }
    from_for_md_elem! { List }
    from_for_md_elem! { Section }
    from_for_md_elem! { CodeBlock }
    from_for_md_elem! { Paragraph }
    from_for_md_elem! { Table }
    from_for_md_elem! { Inline }
    from_for_md_elem! { BlockHtml }

    impl From<String> for BlockHtml {
        fn from(value: String) -> Self {
            Self { value }
        }
    }

    impl From<Image> for MdElem {
        fn from(value: Image) -> Self {
            MdElem::Inline(Inline::Image(value))
        }
    }

    impl From<Link> for MdElem {
        fn from(value: Link) -> Self {
            MdElem::Inline(Inline::Link(value))
        }
    }

    impl From<Vec<MdElem>> for MdElem {
        fn from(elems: Vec<MdElem>) -> Self {
            Self::Doc(elems)
        }
    }
}

/// Defines all the mdx nodes as match arms. This let us easily mark them as ignored, and in particular makes it so that
/// the prod and test code both marks them as ignored using the same source list (namely, this macro).
macro_rules! mdx_nodes {
    {} => {
        mdast::Node::MdxJsxFlowElement(_)
        | mdast::Node::MdxjsEsm(_)
        | mdast::Node::MdxTextExpression(_)
        | mdast::Node::MdxJsxTextElement(_)
        | mdast::Node::MdxFlowExpression(_)
    };
}

/// Creates a nested enum.
///
/// This macro translates:
/// ```text
/// m_node!(A::B::C { foo: 123 })
/// ```
///
/// into:
///
/// ```text
/// A::B(B::C(C { foo: 123 }))
/// ```
macro_rules! m_node {
    // Terminal cases for Foo{ bar: bazz } in its various configurations
    ($last:ident { $($args:tt)* }) => {
        $last {
            $($args)*
        }
    };

    // Terminal empty struct: Foo::Bar
    ($last:ident :: $next:ident) => {
        $last::$next
    };

    // Recursive case: A::B<tail> -> A::B(B<tail>)
    ($head:ident :: $next:ident $(:: $($tail:ident)::*)? $({ $($args:tt)* })? ) => {
        $head::$next( m_node!($next $(:: $($tail)::*)? $({ $($args)* })?) )
    };
}
pub(crate) use m_node;

impl MdDoc {
    fn read(node: mdast::Node, opts: &ReadOptions) -> Result<Self, InvalidMd> {
        let lookups = Lookups::new(&node, opts)?;
        let mut ctx = MdContext::new();
        let roots = MdElem::from_mdast_0(node, &lookups, &mut ctx)?;
        Ok(Self { roots, ctx })
    }
}

impl MdElem {
    fn from_mdast_0(node: mdast::Node, lookups: &Lookups, ctx: &mut MdContext) -> Result<Vec<Self>, InvalidMd> {
        let result = match node {
            mdast::Node::Root(node) => return MdElem::all(node.children, lookups, ctx),
            mdast::Node::Blockquote(node) => m_node!(MdElem::BlockQuote {
                body: MdElem::all(node.children, lookups, ctx)?,
            }),
            mdast::Node::List(node) => {
                let mut li_nodes = Vec::with_capacity(node.children.len());
                for node in node.children {
                    let mdast::Node::ListItem(li_node) = node else {
                        return Err(InvalidMd::NonListItemDirectlyUnderList(MarkdownPart {
                            node: Box::new(node),
                        }));
                    };
                    let li_mdq = ListItem {
                        checked: li_node.checked,
                        item: MdElem::all(li_node.children, lookups, ctx)?,
                    };
                    li_nodes.push(li_mdq);
                }
                m_node!(MdElem::List {
                    starting_index: node.start,
                    items: li_nodes,
                })
            }
            mdast::Node::Break(_) => MdElem::Inline(Inline::Text(Text {
                variant: TextVariant::Plain,
                value: "\n".to_string(),
            })),
            mdast::Node::InlineCode(node) => MdElem::Inline(Inline::Text(Text {
                variant: TextVariant::Code,
                value: node.value,
            })),
            mdast::Node::InlineMath(node) => MdElem::Inline(Inline::Text(Text {
                variant: TextVariant::Math,
                value: node.value,
            })),
            mdast::Node::Delete(node) => MdElem::Inline(Inline::Span(Span {
                variant: SpanVariant::Delete,
                children: MdElem::inlines(node.children, lookups, ctx)?,
            })),
            mdast::Node::Emphasis(node) => MdElem::Inline(Inline::Span(Span {
                variant: SpanVariant::Emphasis,
                children: MdElem::inlines(node.children, lookups, ctx)?,
            })),
            mdast::Node::Image(node) => MdElem::Inline(Inline::Image(Image {
                alt: node.alt,
                link: LinkDefinition {
                    url: node.url,
                    title: node.title,
                    reference: LinkReference::Inline,
                },
            })),
            mdast::Node::ImageReference(node) => MdElem::Inline(Inline::Image(Image {
                alt: node.alt,
                link: lookups.resolve_link(node.identifier, node.label, node.reference_kind, lookups)?,
            })),
            mdast::Node::Link(node) => MdElem::Inline(Inline::Link(Link {
                display: MdElem::inlines(node.children, lookups, ctx)?,
                link: LinkDefinition {
                    url: node.url,
                    title: node.title,
                    reference: LinkReference::Inline,
                },
            })),
            mdast::Node::LinkReference(node) => MdElem::Inline(Inline::Link(Link {
                display: MdElem::inlines(node.children, lookups, ctx)?,
                link: lookups.resolve_link(node.identifier, node.label, node.reference_kind, lookups)?,
            })),
            mdast::Node::FootnoteReference(node) => {
                MdElem::Inline(Inline::Footnote(FootnoteId::new(node.identifier, node.label)))
            }
            mdast::Node::FootnoteDefinition(node) => {
                let footnote_id = FootnoteId::new(node.identifier, node.label);
                return if ctx.footnotes.contains_key(&footnote_id) {
                    Err(InvalidMd::ConflictingReferenceDefinition(footnote_id.id))
                } else {
                    let children = MdElem::all(node.children, lookups, ctx)?;

                    // Can't use HashMap::entry without cloning footnote_id.
                    #[allow(clippy::map_entry)]
                    if ctx.footnotes.contains_key(&footnote_id) {
                        Err(InvalidMd::ConflictingReferenceDefinition(footnote_id.id))
                    } else {
                        ctx.footnotes.insert(footnote_id, children);
                        Ok(Vec::new())
                    }
                };
            }
            mdast::Node::Strong(node) => MdElem::Inline(Inline::Span(Span {
                variant: SpanVariant::Strong,
                children: MdElem::inlines(node.children, lookups, ctx)?,
            })),
            mdast::Node::Text(node) => MdElem::Inline(Inline::Text(Text {
                variant: TextVariant::Plain,
                value: node.value,
            })),
            mdast::Node::Code(node) => {
                let mdast::Code { value, lang, meta, .. } = node;
                m_node!(MdElem::CodeBlock {
                    value,
                    variant: CodeVariant::Code(lang.map(|lang| CodeOpts {
                        language: lang,
                        metadata: meta,
                    })),
                })
            }
            mdast::Node::Math(node) => {
                let mdast::Math { value, meta, .. } = node;
                m_node!(MdElem::CodeBlock {
                    value,
                    variant: CodeVariant::Math { metadata: meta },
                })
            }
            mdast::Node::Heading(node) => m_node!(MdElem::Section {
                depth: node.depth,
                title: Self::inlines(node.children, lookups, ctx)?,
                body: Vec::new(),
            }),
            mdast::Node::Table(node) => {
                let mdast::Table { children, align, .. } = node;
                let mut rows = Vec::with_capacity(children.len());
                for row_node in children {
                    let mdast::Node::TableRow(mdast::TableRow {
                        children: cell_nodes, ..
                    }) = row_node
                    else {
                        return Err(InvalidMd::NonRowDirectlyUnderTable(MarkdownPart {
                            node: Box::new(row_node),
                        }));
                    };
                    let mut column = Vec::with_capacity(cell_nodes.len());
                    for cell_node in cell_nodes {
                        let mdast::Node::TableCell(table_cell) = cell_node else {
                            return Err(InvalidMd::InternalError(UnknownMdParseError {
                                backtrace: Backtrace::force_capture(),
                            }));
                        };
                        let cell_contents = Self::inlines(table_cell.children, lookups, ctx)?;
                        column.push(cell_contents);
                    }
                    rows.push(column);
                }
                m_node!(MdElem::Table {
                    alignments: align.into_iter().map(Self::convert_alignment).collect(),
                    rows,
                })
            }
            mdast::Node::ThematicBreak(_) => m_node!(MdElem::ThematicBreak),
            mdast::Node::TableRow(_) | mdast::Node::TableCell(_) | mdast::Node::ListItem(_) => {
                // should have been handled by Node::Table
                return Err(InvalidMd::InternalError(UnknownMdParseError {
                    backtrace: Backtrace::force_capture(),
                }));
            }
            mdast::Node::Definition(_) => return Ok(Vec::new()),
            mdast::Node::Paragraph(node) => m_node!(MdElem::Paragraph {
                body: Self::inlines(node.children, lookups, ctx)?,
            }),
            mdast::Node::Toml(node) => m_node!(MdElem::CodeBlock {
                variant: CodeVariant::Toml,
                value: node.value,
            }),
            mdast::Node::Yaml(node) => m_node!(MdElem::CodeBlock {
                variant: CodeVariant::Yaml,
                value: node.value,
            }),
            mdast::Node::Html(node) => m_node!(MdElem::BlockHtml { value: node.value }),
            mdx_nodes! {} => {
                // If you implement this, make sure to remove the mdx_nodes macro. That means you'll also need to
                // adjust the test `nodes_matcher` macro.
                return Err(InvalidMd::Unsupported(MarkdownPart { node: Box::new(node) }));
            }
        };
        Ok(vec![result])
    }

    fn convert_alignment(a: mdast::AlignKind) -> Option<ColumnAlignment> {
        match a {
            mdast::AlignKind::Left => Some(ColumnAlignment::Left),
            mdast::AlignKind::Right => Some(ColumnAlignment::Right),
            mdast::AlignKind::Center => Some(ColumnAlignment::Center),
            mdast::AlignKind::None => None,
        }
    }

    fn all(
        children: Vec<mdast::Node>,
        lookups: &Lookups,
        footnotes_repo: &mut MdContext,
    ) -> Result<Vec<Self>, InvalidMd> {
        Self::all_from_iter(NodeToMdqIter {
            lookups,
            footnotes_repo,
            children: children.into_iter(),
            pending: Vec::new().into_iter(),
        })
    }

    fn all_from_iter<I>(iter: I) -> Result<Vec<Self>, InvalidMd>
    where
        I: Iterator<Item = Result<MdElem, InvalidMd>>,
    {
        // This is just a struct that reflects the struct-variant of MdqNode::Header. If that
        // enum variant used the tuple-style with an explicitly defined struct, we wouldn't need
        // this.
        struct HContainer {
            depth: u8,
            title: Vec<Inline>,
            children: Vec<MdElem>,
        }

        let mut result = Vec::with_capacity(16); // arbitrary capacity guess
        let mut headers: Vec<HContainer> = Vec::with_capacity(result.capacity());
        for child_mdq in iter {
            let child_mdq = child_mdq?;
            if let MdElem::Section(Section {
                depth,
                title,
                body: children,
            }) = child_mdq
            {
                // The new child is a heading. Pop the headers stack until we see a header that's
                // of lower depth, or until there are no more left.
                loop {
                    let Some(prev) = headers.last() else {
                        // There's no previous header, so push this header to the results.
                        headers.push(HContainer { depth, title, children });
                        break;
                    };
                    // There is a header. See if it's lower than ours; if so, we'll just add
                    // ourselves to it, and push our info to the stack
                    if prev.depth < depth {
                        headers.push(HContainer { depth, title, children });
                        break;
                    } else {
                        // We need to pop the previous header. When we do, either add it as a child
                        // to the new previous, or else to the top-level results if there is no new
                        // previous. Then, we'll just loop back around.
                        let HContainer { depth, title, children } = headers.pop().unwrap(); // "let Some(prev)" above guarantees that this works
                        let prev = m_node!(MdElem::Section {
                            depth,
                            title,
                            body: children,
                        });
                        if let Some(grandparent) = headers.last_mut() {
                            grandparent.children.push(prev);
                        } else {
                            result.push(prev);
                        }
                    }
                }
            } else {
                // The new child isn't a heading, so just add it to the last heading, or the top
                // level
                let add_to = if let Some(HContainer { children, .. }) = headers.last_mut() {
                    children
                } else {
                    &mut result
                };
                add_to.push(child_mdq);
            };
        }

        // At this point, we still have our last tree branch of headers. Fold it up into the results.
        while let Some(HContainer { depth, title, children }) = headers.pop() {
            let mdq_header = m_node!(MdElem::Section {
                depth,
                title,
                body: children,
            });
            let add_to = if let Some(HContainer { children, .. }) = headers.last_mut() {
                children
            } else {
                &mut result
            };
            add_to.push(mdq_header);
        }
        headers
            .drain(..)
            .map(|HContainer { depth, title, children }| {
                m_node!(MdElem::Section {
                    depth,
                    title,
                    body: children,
                })
            })
            .for_each(|mdq_node| result.push(mdq_node));

        let result = Concatenate::concatenate_similar(result);
        Ok(result)
    }

    fn inlines(
        children: Vec<mdast::Node>,
        lookups: &Lookups,
        footnotes_repo: &mut MdContext,
    ) -> Result<Vec<Inline>, InvalidMd> {
        let mdq_children = Self::all(children, lookups, footnotes_repo)?;
        let mut result = Vec::with_capacity(mdq_children.len());
        for child in mdq_children {
            // Get this child as an Inline, or complain. HTML can be either inline or block; Self::all will always
            // return it as a block, but we can just extract the String and convert it to an Inline.
            let inline = match child {
                MdElem::Inline(inline) => inline,
                MdElem::BlockHtml(html) => Inline::Text(Text {
                    variant: TextVariant::InlineHtml,
                    value: html.value,
                }),
                _ => return Err(InvalidMd::NonInlineWhereInlineExpected(child)),
            };
            result.push(inline);
        }
        let merged = Concatenate::concatenate_similar(result);
        Ok(merged)
    }
}

impl Concatenate for MdElem {
    fn try_concatenate(&mut self, other: Self) -> Result<(), Self> {
        match (self, other) {
            (Self::Inline(me), Self::Inline(other)) => me.try_concatenate(other).map_err(Self::Inline),
            (_, other) => Err(other),
        }
    }
}

struct NodeToMdqIter<'a, I>
where
    I: Iterator<Item = mdast::Node>,
{
    children: I,
    pending: IntoIter<MdElem>,
    lookups: &'a Lookups,
    footnotes_repo: &'a mut MdContext,
}

impl<I> Iterator for NodeToMdqIter<'_, I>
where
    I: Iterator<Item = mdast::Node>,
{
    type Item = Result<MdElem, InvalidMd>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(pending) = self.pending.next() {
                return Some(Ok(pending));
            }
            let next_node = self.children.next()?;
            match MdElem::from_mdast_0(next_node, self.lookups, self.footnotes_repo) {
                Ok(mdq_node) => {
                    self.pending = mdq_node.into_iter();
                }
                Err(err) => {
                    break Some(Err(err));
                }
            };
        }
    }
}

#[derive(Debug, PartialEq)]
struct Lookups {
    link_definitions: HashMap<String, mdast::Definition>,
    footnote_definitions: HashMap<String, mdast::FootnoteDefinition>,
    allow_unknown_markdown: bool,
}

impl Lookups {
    fn new(node: &mdast::Node, read_opts: &ReadOptions) -> Result<Self, InvalidMd> {
        const DEFAULT_CAPACITY: usize = 8; // random guess

        let mut result = Self {
            link_definitions: HashMap::with_capacity(DEFAULT_CAPACITY),
            footnote_definitions: HashMap::with_capacity(DEFAULT_CAPACITY),
            allow_unknown_markdown: read_opts.allow_unknown_markdown,
        };

        result.build_lookups(node, read_opts)?;

        Ok(result)
    }

    fn unknown_markdown(&self, description: &'static str) -> Result<(), InvalidMd> {
        if self.allow_unknown_markdown {
            Ok(())
        } else {
            Err(InvalidMd::UnknownMarkdown(description))
        }
    }

    fn resolve_link(
        &self,
        identifier: String,
        label: Option<String>,
        reference_kind: mdast::ReferenceKind,
        lookups: &Lookups,
    ) -> Result<LinkDefinition, InvalidMd> {
        if label.is_none() {
            lookups.unknown_markdown("link label was None")?;
        }
        let Some(definition) = self.link_definitions.get(&identifier) else {
            let human_visible_identifier = label.unwrap_or(identifier);
            return Err(InvalidMd::MissingReferenceDefinition(human_visible_identifier));
        };
        let human_visible_identifier = label.unwrap_or(identifier);
        let link_ref = match reference_kind {
            mdast::ReferenceKind::Shortcut => LinkReference::Shortcut,
            mdast::ReferenceKind::Collapsed => LinkReference::Collapsed,
            mdast::ReferenceKind::Full => LinkReference::Full(human_visible_identifier),
        };
        Ok(LinkDefinition {
            url: definition.url.to_owned(),
            title: definition.title.to_owned(),
            reference: link_ref,
        })
    }

    fn build_lookups(&mut self, node: &mdast::Node, read_opts: &ReadOptions) -> Result<(), InvalidMd> {
        let x = format!("{:?}", node);
        let _ = x;
        match node {
            mdast::Node::FootnoteDefinition(def) => {
                Self::add_ref(&mut self.footnote_definitions, &def.identifier, def.clone(), read_opts)
            }
            mdast::Node::Definition(def) => {
                Self::add_ref(&mut self.link_definitions, &def.identifier, def.clone(), read_opts)
            }
            _ => Ok(()),
        }?;
        if let Some(children) = node.children() {
            for child in children {
                self.build_lookups(child, read_opts)?;
            }
        }
        Ok(())
    }

    fn add_ref<V>(
        to: &mut HashMap<String, V>,
        identifier: &String,
        value: V,
        read_options: &ReadOptions,
    ) -> Result<(), InvalidMd>
    where
        V: PartialEq + Debug,
    {
        match to.entry(identifier.to_owned()) {
            Entry::Occupied(other) => {
                if read_options.validate_no_conflicting_links {
                    Err(InvalidMd::ConflictingReferenceDefinition(other.key().to_owned()))
                } else {
                    Ok(())
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(value);
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::md_elem::tree_test_utils::*;
    use crate::util::utils_for_test::*;

    impl MdContext {
        pub fn empty() -> Self {
            Self {
                footnotes: Default::default(),
                empty_md_elems: vec![],
            }
        }

        pub fn with<S: Into<FootnoteId>>(mut self, footnote_id: S, body: Vec<MdElem>) -> Self {
            self.footnotes.insert(footnote_id.into(), body);
            self
        }
    }

    impl From<&str> for FootnoteId {
        fn from(id: &str) -> Self {
            Self { id: id.to_owned() }
        }
    }

    ///  tests of each mdast node type
    ///
    /// The purpose of these is not to test the parser (I trust mdast), but to test my understanding of how it works.
    ///
    /// For example, footnote are `[^a]` in markdown; does that identifier get parsed as `"^a"` or `"a"`?
    mod all_nodes {
        use super::*;
        use indoc::indoc;
        use markdown::mdast::Node;
        use markdown::{mdast, ParseOptions};

        macro_rules! check {
            (error: $enum_value:expr, $enum_variant:pat, $lookups:expr => $err:expr $(, $body:block)? ) => {{
                let node = $enum_value;
                NODES_CHECKER.see(&node);
                unwrap!(node, $enum_variant);
                let node_clone = node.clone();
                let mut ctx = MdContext::new();
                let mdq_err = MdElem::from_mdast_0(node_clone, &$lookups, &mut ctx).err().expect("expected no MdqNode");
                assert_eq!(mdq_err, $err);
                $($body)?
            }};

            (no_node: $enum_value:expr, $enum_variant:pat, $lookups:expr $(=> $ctx_expect:expr)?) => {{
                let node = $enum_value;
                NODES_CHECKER.see(&node);
                unwrap!(node, $enum_variant);
                let node_clone = node.clone();
                let mut ctx = MdContext::new();
                let mdqs = MdElem::from_mdast_0(node_clone, &$lookups, &mut ctx).unwrap();
                assert_eq!(mdqs, Vec::new());
                $(
                assert_eq!(ctx, $ctx_expect);
                )?
            }};

            ($enum_value:expr, $enum_variant:pat, $lookups:expr => $mdq_pat:pat = $mdq_body:block ) => {{
                let node = $enum_value;
                NODES_CHECKER.see(&node);
                unwrap!(node, $enum_variant);
                let node_clone = node.clone();
                let mut ctx = MdContext::new();
                let mut mdqs = MdElem::from_mdast_0(node_clone, &$lookups, &mut ctx).unwrap();
                assert_eq!(&ctx, &MdContext::empty(), "MdContext");
                assert_eq!(mdqs.len(), 1, "expected exactly one element, but found: {:?}", mdqs);
                let mdq = mdqs.pop().unwrap();
                if let $mdq_pat = mdq $mdq_body else {
                    panic!("expected {} but saw {:?}", stringify!($mdq_pat), &mdq)
                }
            }};
        }

        #[test]
        fn root() {
            let (root, _) = parse("hello");
            assert_eq!(root.children.len(), 1);
        }

        #[test]
        fn block_quote() {
            let (root, lookups) = parse("> hello");
            let child = &root.children[0];
            check!(child, Node::Blockquote(_), lookups => m_node!(MdElem::BlockQuote{body}) = {
                assert_eq!(body, md_elems!["hello"]);
            });
        }

        #[test]
        fn footnote() {
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    Cool story [^a]!

                    [^a]: My footnote
                      with two lines."#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[1], Node::FootnoteReference(_), lookups => MdElem::Inline(footnote) = {
                    assert_eq!(footnote, Inline::Footnote("a".into()))
                });
                check!(no_node: &root.children[1], Node::FootnoteDefinition(_), lookups);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    Cool story [^a]!

                    [^a]: - footnote is a list"#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));

                check!(&p.children[1], Node::FootnoteReference(_), lookups => MdElem::Inline(footnote) = {
                    assert_eq!(footnote, Inline::Footnote("a".into()))
                });
                check!(no_node: &root.children[1], Node::FootnoteDefinition(_), lookups);
            }
        }

        #[test]
        fn footnote_cycle() {
            let (root, lookups) = parse_with(
                &ParseOptions::gfm(),
                indoc! {r#"
                Body text[^1][^2].

                [^1]: a footnote that references itself[^1].
                [^2]: a footnote that starts a cycle[^3].
                [^3]: cycles back[^2].
                "#},
            );
            unwrap!(&root.children[0], Node::Paragraph(p));
            assert_eq!(p.children.len(), 4);

            // first child: the paragraph
            check!(&p.children[0], Node::Text(_), lookups => MdElem::Inline(text) = {
                assert_eq!(
                    text,
                    Inline::Text(Text{
                        variant: TextVariant::Plain,
                        value: "Body text".to_string(),
                    })
                );
            });
            check!(&p.children[1], Node::FootnoteReference(_), lookups => MdElem::Inline(footnote) = {
                assert_eq!(footnote, Inline::Footnote("1".into()))
            });
            check!(&p.children[2], Node::FootnoteReference(_), lookups => MdElem::Inline(footnote) = {
                assert_eq!(footnote, Inline::Footnote("2".into()))
            });
            check!(&p.children[3], Node::Text(_), lookups => MdElem::Inline(text) = {
                assert_eq!(
                    text,
                    Inline::Text(Text{
                        variant: TextVariant::Plain,
                        value: ".".to_string(),
                    })
                );
            });

            // second child: [^1]
            check!(no_node: &root.children[1], Node::FootnoteDefinition(_), lookups =>
                MdContext::empty()
                    .with("1", vec![MdElem::Paragraph(Paragraph{
                        body: vec![
                            mdq_inline!("a footnote that references itself"),
                            Inline::Footnote("1".into()),
                            mdq_inline!("."),
                        ],
                    })])
            );

            // third child: [^2]
            check!(no_node: &root.children[2], Node::FootnoteDefinition(_), lookups =>
                MdContext::empty()
                    .with("2", vec![MdElem::Paragraph(Paragraph{
                        body: vec![
                            mdq_inline!("a footnote that starts a cycle"),
                            Inline::Footnote("3".into()),
                            mdq_inline!("."),
                        ],
                    })])
            );

            // fourth child: [^3]
            check!(no_node: &root.children[3], Node::FootnoteDefinition(_), lookups =>
                MdContext::empty()
                    .with("3", vec![MdElem::Paragraph(Paragraph{
                        body: vec![
                            mdq_inline!("cycles back"),
                            Inline::Footnote("2".into()),
                            mdq_inline!("."),
                        ],
                    })])
            );
        }

        #[test]
        fn lists_and_items() {
            let (root, lookups) = parse_with(
                &ParseOptions::gfm(),
                indoc! {r#"
                - First
                - [ ] Second
                - [x] Third
                      With a line break
                4. Fourth
                5. [ ] Fifth
                6. [x] Sixth

                   With a paragraph
                "#},
            );
            assert_eq!(root.children.len(), 2); // unordered list, then ordered

            check!(&root.children[0], Node::List(ul), lookups => m_node!(MdElem::List{starting_index, items}) = {
                for child in &ul.children {
                    check!(error: child, Node::ListItem(_), lookups => internal_error());
                }
                assert_eq!(starting_index, None);
                assert_eq!(items, vec![
                    ListItem {
                        checked: None,
                        item: md_elems!["First"],
                    },
                    ListItem {
                        checked: Some(false),
                        item: md_elems!["Second"],
                    },
                    ListItem {
                        checked: Some(true),
                        item: md_elems!["Third\nWith a line break"],
                    },
                ]);
            });
            check!(&root.children[1], Node::List(ol), lookups => m_node!(MdElem::List{starting_index, items}) = {
                for child in &ol.children {
                    check!(error: child, Node::ListItem(_), lookups => internal_error());
                }
                assert_eq!(starting_index, Some(4));
                assert_eq!(items, vec![
                    ListItem {
                        checked: None,
                        item: md_elems!["Fourth"],
                    },
                    ListItem {
                        checked: Some(false),
                        item: md_elems!["Fifth"],
                    },
                    ListItem {
                        checked: Some(true),
                        item: md_elems![
                            "Sixth",
                            "With a paragraph",
                        ],
                    },
                ]);
            });
        }

        #[test]
        fn text_and_break() {
            let (root, lookups) = parse_with(
                &ParseOptions::gfm(),
                indoc! {r#"
                hello \
                world
                "#},
            );

            check!(&root.children[0], Node::Paragraph(p), lookups => m_node!(MdElem::Paragraph{body}) = {
                assert_eq!(p.children.len(), 3);
                check!(&p.children[0], Node::Text(_), lookups => MdElem::Inline(text) = {
                    assert_eq!(text, Inline::Text(Text{variant: TextVariant::Plain, value: "hello ".to_string()}));
                });
                check!(&p.children[1], Node::Break(_), lookups => MdElem::Inline(text) = {
                    assert_eq!(text, Inline::Text (Text{variant: TextVariant::Plain, value: "\n".to_string()}));
                });
                check!(&p.children[2], Node::Text(_), lookups => MdElem::Inline(text) = {
                    assert_eq!(text, Inline::Text (Text{variant: TextVariant::Plain, value: "world".to_string()}));
                });
                assert_eq!(body, vec![
                    // note: just a single child, which has a two-line string
                    Inline::Text (Text{variant: TextVariant::Plain, value: "hello \nworld".to_string()}),
                ])
            });
        }

        #[test]
        fn inline_code() {
            let (root, lookups) = parse("`foo`");

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::InlineCode(_), lookups => MdElem::Inline(inline) = {
                assert_eq!(inline, Inline::Text (Text{ variant: TextVariant::Code, value: "foo".to_string() }));
            });
        }

        #[test]
        fn inline_math() {
            let mut opts = ParseOptions::gfm();
            opts.constructs.math_text = true;
            let (root, lookups) = parse_with(&opts, r#"$ 0 \ne 1 $"#);

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::InlineMath(_), lookups => MdElem::Inline(inline) = {
                assert_eq!(inline, Inline::Text (Text{ variant: TextVariant::Math, value: r#"0 \ne 1"#.to_string() }));
            });
        }

        #[test]
        fn inline_delete() {
            let (root, lookups) = parse_with(&ParseOptions::gfm(), "~~86 me~~");

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::Delete(_), lookups => MdElem::Inline(inline) = {
                assert_eq!(inline, Inline::Span(Span{
                    variant: SpanVariant::Delete,
                    children: vec![
                        Inline::Text (Text{ variant: TextVariant::Plain, value: "86 me".to_string()}),
                    ]
                }));
            });
        }

        #[test]
        fn inline_emphasis() {
            let (root, lookups) = parse("_86 me_");

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::Emphasis(_), lookups => MdElem::Inline(inline) = {
                assert_eq!(inline, Inline::Span(Span{
                    variant: SpanVariant::Emphasis,
                    children: vec![
                        Inline::Text (Text{ variant: TextVariant::Plain, value: "86 me".to_string()}),
                    ]
                }));
            });
        }

        #[test]
        fn inline_strong() {
            let (root, lookups) = parse("**strongman**");

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::Strong(_), lookups => MdElem::Inline(inline) = {
                assert_eq!(inline, Inline::Span(Span{
                    variant: SpanVariant::Strong,
                    children: vec![
                        Inline::Text (Text{ variant: TextVariant::Plain, value: "strongman".to_string()}),
                    ]
                }));
            });
        }

        #[test]
        fn block_html() {
            {
                let (root, lookups) = parse(indoc! {r#"
                    <div>

                    Hello, world
                "#});

                check!(&root.children[0], Node::Html(_), lookups => MdElem::BlockHtml(html) = {
                    assert_eq!(html, BlockHtml {value: "<div>".to_string() });
                });
                check!(&root.children[1], Node::Paragraph(_), lookups => m_node!(MdElem::Paragraph{body}) = {
                    assert_eq!(body, vec![mdq_inline!("Hello, world")])
                });
                assert_eq!(root.children.len(), 2);
            }
            {
                let (root, lookups) = parse(indoc! {r#"
                    <div Hello
                    world. />
                "#});

                check!(&root.children[0], Node::Html(_), lookups => MdElem::BlockHtml(html) = {
                    assert_eq!(html, BlockHtml{ value: "<div Hello\nworld. />".to_string()});
                });
                assert_eq!(root.children.len(), 1);
            }
            {
                let (root, lookups) = parse("<a href>");

                check!(&root.children[0], Node::Html(_), lookups => MdElem::BlockHtml(inline) = {
                    assert_eq!(inline, BlockHtml{ value: "<a href>".to_string()} );
                });
                assert_eq!(root.children.len(), 1);
            }
        }

        #[test]
        fn inline_html() {
            {
                // Being in a paragraph shows that it can be inline
                let (root, lookups) = parse(indoc! {r#"
                In <em>a paragraph.</em>
                "#});
                check!(&root.children[0], Node::Paragraph(_), lookups => m_node!(MdElem::Paragraph{body}) = {
                    assert_eq!(body.len(), 4);
                    assert_eq!(body, vec![
                        mdq_inline!("In "),
                        Inline::Text (Text{
                            variant: TextVariant::InlineHtml,
                            value: "<em>".to_string()}),
                        mdq_inline!("a paragraph."),
                        Inline::Text (Text{
                            variant: TextVariant::InlineHtml,
                            value: "</em>".to_string()}),
                    ])
                });
            }
            {
                // Being in a paragraph shows that it can be inline
                let (root, lookups) = parse(indoc! {r#"
                In <em
                newline  >a paragraph.</em>
                "#});
                check!(&root.children[0], Node::Paragraph(_), lookups => m_node!(MdElem::Paragraph{body}) = {
                    assert_eq!(body.len(), 4);
                    assert_eq!(body, vec![
                        mdq_inline!("In "),
                        Inline::Text (Text{
                            variant: TextVariant::InlineHtml,
                            value: "<em\nnewline  >".to_string()}),
                        mdq_inline!("a paragraph."),
                        Inline::Text (Text{
                            variant: TextVariant::InlineHtml,
                            value: "</em>".to_string()}),
                    ])
                });
            }
        }

        #[test]
        fn image() {
            {
                let (root, lookups) = parse("![]()");
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Image(_), lookups => MdElem::Inline(img) = {
                    assert_eq!(img, Inline::Image(Image {
                        alt: "".to_string(),
                        link: LinkDefinition{
                            url: "".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }))
                });
            }
            {
                let (root, lookups) = parse("![](https://example.com/foo.png)");
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Image(_), lookups => MdElem::Inline(img) = {
                    assert_eq!(img, Inline::Image(Image {
                        alt: "".to_string(),
                        link: LinkDefinition{
                            url: "https://example.com/foo.png".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }))
                });
            }
            {
                let (root, lookups) = parse("![alt text]()");
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Image(_), lookups => MdElem::Inline(img) = {
                    assert_eq!(img, Inline::Image(Image {
                        alt: "alt text".to_string(),
                        link: LinkDefinition{
                            url: "".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }))
                });
            }
            {
                let (root, lookups) = parse(r#"![](https://example.com/foo.png "my tooltip")"#);
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Image(_), lookups => MdElem::Inline(img) = {
                    assert_eq!(img, Inline::Image(Image {
                        alt: "".to_string(),
                        link: LinkDefinition{
                            url: "https://example.com/foo.png".to_string(),
                            title: Some("my tooltip".to_string()),
                            reference: LinkReference::Inline,
                        },
                    }))
                });
            }
            {
                // This isn't an image, though it almost looks like one
                let (root, lookups) = parse(r#"![]("only a tooltip")"#);
                check!(&root.children[0], Node::Paragraph(_), lookups => p @ m_node!(MdElem::Paragraph{ .. }) = {
                    assert_eq!(p, md_elem!(r#"![]("only a tooltip")"#));
                });
            }
        }

        #[test]
        fn link_node() {
            {
                // inline, no title
                let (root, lookups) = parse("[hello _world_](https://example.com)");
                assert_eq!(root.children.len(), 1);
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Link(_), lookups => MdElem::Inline(link) = {
                    assert_eq!(link, Inline::Link(Link {
                        display: vec![
                            mdq_inline!("hello "),
                            Inline::Span(Span{
                                variant: SpanVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            })
                        ],
                        link: LinkDefinition{
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }))
                });
            }
            {
                // inline, with title
                let (root, lookups) = parse(r#"[hello _world_](https://example.com "the title")"#);
                assert_eq!(root.children.len(), 1);
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Link(_), lookups => MdElem::Inline(link) = {
                    assert_eq!(link, Inline::Link(Link {
                        display: vec![
                            mdq_inline!("hello "),
                            Inline::Span(Span {
                                variant: SpanVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            })
                        ],
                        link: LinkDefinition{
                            url: "https://example.com".to_string(),
                            title: Some("the title".to_string()),
                            reference: LinkReference::Inline,
                        },
                    }))
                });
            }
            {
                // full
                let (root, lookups) = parse_with(
                    &ParseOptions::default(),
                    indoc! {r#"
                    [hello _world_][1]

                    [1]: https://example.com
                    "#},
                );
                assert_eq!(root.children.len(), 2);
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdElem::Inline(link) = {
                    assert_eq!(link, Inline::Link(Link {
                        display: vec![
                            mdq_inline!("hello "),
                            Inline::Span(Span{
                                variant: SpanVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            }),
                        ],
                        link: LinkDefinition{
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        },
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
            {
                // collapsed, with title
                let (root, lookups) = parse_with(
                    &ParseOptions::default(),
                    indoc! {r#"
                    [hello _world_][]

                    [hello _world_]: https://example.com "my title"
                    "#},
                );
                assert_eq!(root.children.len(), 2);
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdElem::Inline(link) = {
                    assert_eq!(link, Inline::Link(Link {
                        display: vec![
                            mdq_inline!("hello "),
                            Inline::Span(Span{
                                variant: SpanVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            }),
                        ],
                        link: LinkDefinition{
                            url: "https://example.com".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Collapsed,
                        },
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
            {
                // shortcut, no title
                let (root, lookups) = parse_with(
                    &ParseOptions::default(),
                    indoc! {r#"
                    [hello _world_]

                    [hello _world_]: https://example.com
                    "#},
                );
                assert_eq!(root.children.len(), 2);
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdElem::Inline(link) = {
                    assert_eq!(link, Inline::Link(Link {
                        display: vec![
                            mdq_inline!("hello "),
                            Inline::Span(Span{
                                variant: SpanVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            }),
                        ],
                        link: LinkDefinition{
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Shortcut,
                        },
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
        }

        #[test]
        fn autolinks() {
            {
                let (root, lookups) = parse("<https://example.com>");
                unwrap!(&root.children[0], Node::Paragraph(p));
                assert_eq!(p.children.len(), 1);
                check!(&p.children[0], Node::Link(_), lookups => m_node!(MdElem::Inline::Link{display, link}) = {
                    assert_eq!(display, vec![mdq_inline!("https://example.com")]);
                    assert_eq!(link, LinkDefinition{
                        url: "https://example.com".to_string(),
                        title: None,reference: LinkReference::Inline,
                    });
                });
            }
            {
                let (root, lookups) = parse("<mailto:md@example.com>");
                unwrap!(&root.children[0], Node::Paragraph(p));
                assert_eq!(p.children.len(), 1);
                check!(&p.children[0], Node::Link(_), lookups => m_node!(MdElem::Inline::Link{display, link}) = {
                    assert_eq!(display, vec![mdq_inline!("mailto:md@example.com")]);
                    assert_eq!(link, LinkDefinition{
                        url: "mailto:md@example.com".to_string(),
                        title: None,reference: LinkReference::Inline,
                    });
                });
            }
            {
                // in default parsing, bare URLs aren't autolink
                let (root, lookups) = parse_with(&ParseOptions::default(), "https://example.com");
                unwrap!(&root.children[0], Node::Paragraph(p));
                assert_eq!(p.children.len(), 1);
                check!(&p.children[0], Node::Text(_), lookups => MdElem::Inline(Inline::Text(Text{variant: TextVariant::Plain, value})) = {
                    assert_eq!(value, "https://example.com".to_string());
                });
            }
            {
                // in GFM parsing, bare URLs *are* autolink
                let (root, lookups) = parse_with(&ParseOptions::gfm(), "https://example.com");
                unwrap!(&root.children[0], Node::Paragraph(p));
                assert_eq!(p.children.len(), 1);
                check!(&p.children[0], Node::Link(_), lookups => m_node!(MdElem::Inline::Link{display, link}) = {
                    assert_eq!(display, vec![mdq_inline!("https://example.com")]);
                    assert_eq!(link, LinkDefinition{
                        url: "https://example.com".to_string(),
                        title: None,reference: LinkReference::Inline,
                    });
                });
            }
        }

        /// Basically the same as [link_ref], but with an exclamation point
        #[test]
        fn image_ref() {
            {
                let (root, lookups) = parse(indoc! {r#"
                    ![][1]

                    [1]: https://example.com/image.png"#});
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::ImageReference(_), lookups => MdElem::Inline(img) = {
                    assert_eq!(img, Inline::Image(Image {
                        alt: "".to_string(),
                        link: LinkDefinition {
                            url: "https://example.com/image.png".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        }
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
            {
                let (root, lookups) = parse(indoc! {r#"
                    ![][1]

                    [1]: https://example.com/image.png "my title""#});
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::ImageReference(_), lookups => MdElem::Inline(img) = {
                    assert_eq!(img, Inline::Image(Image {
                        alt: "".to_string(),
                        link: LinkDefinition {
                            url: "https://example.com/image.png".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Full("1".to_string()),
                        }
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    ![my alt][]

                    [my alt]: https://example.com/image.png "my title""#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::ImageReference(_), lookups => MdElem::Inline(img) = {
                    assert_eq!(img, Inline::Image(Image {
                        alt: "my alt".to_string(),
                        link: LinkDefinition {
                            url: "https://example.com/image.png".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Collapsed,
                        }
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    ![my alt]

                    [my alt]: https://example.com/image.png"#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::ImageReference(_), lookups => MdElem::Inline(img) = {
                    assert_eq!(img, Inline::Image(Image {
                        alt: "my alt".to_string(),
                        link: LinkDefinition {
                            url: "https://example.com/image.png".to_string(),
                            title: None,
                            reference: LinkReference::Shortcut,
                        }
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
        }

        /// Basically the same as [image_ref] but without the exclamation point.
        #[test]
        fn link_ref() {
            {
                let (root, lookups) = parse(indoc! {r#"
                    [][1]

                    [1]: https://example.com/image.png"#});
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdElem::Inline(link) = {
                    assert_eq!(link, Inline::Link(Link {
                        display: vec![],
                        link: LinkDefinition {
                            url: "https://example.com/image.png".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        }
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
            {
                let (root, lookups) = parse(indoc! {r#"
                    [][1]

                    [1]: https://example.com/image.png "my title""#});
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdElem::Inline(link) = {
                    assert_eq!(link, Inline::Link(Link {
                        display: vec![],
                        link: LinkDefinition {
                            url: "https://example.com/image.png".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Full("1".to_string()),
                        }
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    [_my_ text][]

                    [_my_ text]: https://example.com/image.png "my title""#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdElem::Inline(link) = {
                    assert_eq!(link, Inline::Link(Link {
                        display: vec![
                            Inline::Span(Span{
                                variant: SpanVariant::Emphasis,
                                children: vec![
                                    Inline::Text (Text{variant: TextVariant::Plain,value: "my".to_string()})
                                ],
                            }),
                            Inline::Text (Text{variant: TextVariant::Plain,value: " text".to_string()})

                        ],
                        link: LinkDefinition {
                            url: "https://example.com/image.png".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Collapsed,
                        }
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    [my text]

                    [my text]: https://example.com/image.png"#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdElem::Inline(link) = {
                    assert_eq!(link, Inline::Link(Link {
                        display: vec![
                            Inline::Text (Text{variant: TextVariant::Plain,value: "my text".to_string()}),
                        ],
                        link: LinkDefinition {
                            url: "https://example.com/image.png".to_string(),
                            title: None,
                            reference: LinkReference::Shortcut,
                        }
                    }))
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups);
            }
        }

        #[test]
        fn code_block() {
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    ```
                    plain code block
                    ```"#},
                );
                check!(&root.children[0], Node::Code(_), lookups => m_node!(MdElem::CodeBlock{variant, value}) = {
                    assert_eq!(variant, CodeVariant::Code(None));
                    assert_eq!(value, "plain code block");
                })
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    ```rust
                    code block with language
                    ```"#},
                );
                check!(&root.children[0], Node::Code(_), lookups => m_node!(MdElem::CodeBlock{variant, value}) = {
                    assert_eq!(variant, CodeVariant::Code(Some(CodeOpts{
                        language: "rust".to_string(),
                        metadata: None})));
                    assert_eq!(value, "code block with language");
                })
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    ```rust title="example.rs"
                    code block with language and title
                    ```"#},
                );
                check!(&root.children[0], Node::Code(_), lookups => m_node!(MdElem::CodeBlock{variant, value}) = {
                    assert_eq!(variant, CodeVariant::Code(Some(CodeOpts{
                        language: "rust".to_string(),
                        metadata: Some(r#"title="example.rs""#.to_string())})));
                    assert_eq!(value, "code block with language and title");
                })
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    ``` title="example.rs"
                    code block with only title
                    ```"#},
                );
                check!(&root.children[0], Node::Code(_), lookups => m_node!(MdElem::CodeBlock{variant, value}) = {
                    // It's actually just a bogus language!
                    assert_eq!(variant, CodeVariant::Code(Some(CodeOpts{
                        language: r#"title="example.rs""#.to_string(),
                        metadata: None})));
                    assert_eq!(value, "code block with only title");
                })
            }
        }

        #[test]
        fn math_block() {
            let mut opts = ParseOptions::gfm();
            opts.constructs.math_flow = true;
            {
                let (root, lookups) = parse_with(
                    &opts,
                    indoc! {r#"
                    $$
                    x = {-b \pm \sqrt{b^2-4ac} \over 2a}
                    $$"#},
                );
                check!(&root.children[0], Node::Math(_), lookups => m_node!(MdElem::CodeBlock{variant, value}) = {
                    assert_eq!(variant, CodeVariant::Math{metadata: None});
                    assert_eq!(value, r#"x = {-b \pm \sqrt{b^2-4ac} \over 2a}"#);
                })
            }
            {
                let (root, lookups) = parse_with(
                    &opts,
                    indoc! {r#"
                    $$ my metadata
                    x = {-b \pm \sqrt{b^2-4ac} \over 2a}
                    $$"#},
                );
                check!(&root.children[0], Node::Math(_), lookups => m_node!(MdElem::CodeBlock{variant, value}) = {
                    assert_eq!(variant, CodeVariant::Math{metadata: Some("my metadata".to_string())});
                    assert_eq!(value, r#"x = {-b \pm \sqrt{b^2-4ac} \over 2a}"#);
                })
            }
        }

        #[test]
        fn toml_block() {
            let mut opts = ParseOptions::default();
            opts.constructs.frontmatter = true;
            let (root, lookups) = parse_with(
                &opts,
                indoc! {r#"
                +++
                my: toml
                +++"#},
            );
            check!(&root.children[0], Node::Toml(_), lookups => m_node!(MdElem::CodeBlock{variant, value}) = {
                assert_eq!(variant, CodeVariant::Toml);
                assert_eq!(value, r#"my: toml"#);
            })
        }

        #[test]
        fn yaml_block() {
            let mut opts = ParseOptions::default();
            opts.constructs.frontmatter = true;
            let (root, lookups) = parse_with(
                &opts,
                indoc! {r#"
                ---
                my: toml
                ---"#},
            );
            check!(&root.children[0], Node::Yaml(_), lookups => m_node!(MdElem::CodeBlock{variant, value}) = {
                assert_eq!(variant, CodeVariant::Yaml);
                assert_eq!(value, r#"my: toml"#);
            })
        }

        #[test]
        fn header_and_root() {
            let (root, lookups) = parse_with(
                &ParseOptions::gfm(),
                indoc! {r#"
                    ## Header with _emphasis_
                    And some text below it."#},
            );

            let (header_depth, header_title) = check!(&root.children[0], Node::Heading(_), lookups => m_node!(MdElem::Section{depth, title, body}) = {
                assert_eq!(depth, 2);
                assert_eq!(title, vec![
                    Inline::Text (Text{ variant: TextVariant::Plain, value: "Header with ".to_string()}),
                    Inline::Span (Span{
                        variant: SpanVariant::Emphasis,
                        children: vec![
                            Inline::Text (Text{ variant: TextVariant::Plain, value: "emphasis".to_string()}),
                        ]
                    })
                ]);
                assert_eq!(body, vec![
                    // This code path doesn't do recursion; that's done in all_from_iter, which happens at the root
                ]);
                (depth, title)
            });

            let mdast_root = Node::Root(root); // reconstruct it, since parse_with unwrapped it
            NODES_CHECKER.see(&mdast_root);
            let mut footnotes = MdContext::new();
            let mdqs = MdElem::from_mdast_0(mdast_root, &lookups, &mut footnotes).unwrap();

            assert_eq!(
                mdqs,
                vec![m_node!(MdElem::Section {
                    depth: header_depth,
                    title: header_title,
                    body: md_elems!["And some text below it."],
                }),]
            );
        }

        #[test]
        fn thematic_break() {
            let (root, lookups) = parse_with(
                &ParseOptions::gfm(),
                indoc! {r#"
                    Before

                    ---

                    After
                    "#},
            );

            assert_eq!(root.children.len(), 3);
            check!(&root.children[1], Node::ThematicBreak(_), lookups => m_node!(MdElem::ThematicBreak) = {
                // nothing to check
            });
        }

        #[test]
        fn table() {
            // Note that the text in the markdown is contrary to what the headings indicate. For example, the left
            // column is aligned right in the markdown, but the separator (`:---`) means it should be left.
            // I intentionally set it up this way to make it more obvious that the alignment comes from the separator.
            let (root, lookups) = parse_with(
                &ParseOptions::gfm(),
                indoc! {r#"
                    | Header A | Header B | Header C | Header D |
                    |:---------|:--------:|---------:|----------|
                    |        1 | 2        |3         |    4     |
                    "#},
            );
            assert_eq!(root.children.len(), 1);
            check!(&root.children[0], Node::Table(table_node), lookups => m_node!(MdElem::Table{alignments, rows}) = {
                assert_eq!(alignments, vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Center), Some(ColumnAlignment::Right), None]);
                assert_eq!(rows,
                    vec![ // rows
                        vec![// Header row
                            vec![mdq_inline!("Header A")], // cells, each being a spans of inline
                            vec![mdq_inline!("Header B")],
                            vec![mdq_inline!("Header C")],
                            vec![mdq_inline!("Header D")],
                        ],
                        vec![// first (and only) data row
                            vec![mdq_inline!("1")], // cells, each being a spans of inline
                            vec![mdq_inline!("2")],
                            vec![mdq_inline!("3")],
                            vec![mdq_inline!("4")],
                        ],
                    ],
                );
                // Do a spot check for the rows and cells; mainly just so that we'll have called check! on them.
                assert_eq!(table_node.children.len(), 2); // two rows
                check!(error: &table_node.children[0], Node::TableRow(tr), lookups => internal_error(), {
                    assert_eq!(tr.children.len(), 4); // four columns
                    check!(error: &tr.children[0], Node::TableCell(_), lookups => internal_error());
                })
            });
        }

        #[test]
        fn jagged_table() {
            let (root, lookups) = parse_with(
                &ParseOptions::gfm(),
                indoc! {r#"
                    | Header A | Header B |
                    |:---------|---------:|
                    |        1 | 2        |
                    |        3
                    |        4 | 5        | 6 |
                    "#},
            );
            assert_eq!(root.children.len(), 1);
            check!(&root.children[0], Node::Table(_), lookups => m_node!(MdElem::Table{alignments, rows}) = {
                assert_eq!(alignments, vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)]);
                assert_eq!(rows,
                    vec![ // rows
                        vec![// Header row
                            vec![mdq_inline!("Header A")], // cells, each being a spans of inline
                            vec![mdq_inline!("Header B")],
                        ],
                        vec![// data row
                            vec![mdq_inline!("1")], // cells, each being a spans of inline
                            vec![mdq_inline!("2")],
                        ],
                        vec![// data row
                            vec![mdq_inline!("3")], // cells, each being a spans of inline
                        ],
                        vec![// data row
                            vec![mdq_inline!("4")], // cells, each being a spans of inline
                            vec![mdq_inline!("5")],
                            vec![mdq_inline!("6")],
                        ],
                    ],
                );
            });
        }

        fn parse(md: &str) -> (mdast::Root, Lookups) {
            parse_with(&ParseOptions::default(), md)
        }

        fn parse_with(opts: &ParseOptions, md: &str) -> (mdast::Root, Lookups) {
            let doc = markdown::to_mdast(md, opts).unwrap();
            let lookups = Lookups::new(&doc, &ReadOptions::default()).unwrap();
            unwrap!(doc, Node::Root(root));
            (root, lookups)
        }

        variants_checker!(NODES_CHECKER = Node {
            Blockquote(_),
            Break(_),
            Code(_),
            Definition(_),
            Delete(_),
            Emphasis(_),
            FootnoteDefinition(_),
            FootnoteReference(_),
            Heading(_),
            Html(_),
            Image(_),
            ImageReference(_),
            InlineCode(_),
            InlineMath(_),
            Link(_),
            LinkReference(_),
            List(_),
            ListItem(_),
            Math(_),
            Paragraph(_),
            Root(_),
            Strong(_),
            Table(_),
            TableCell(_),
            TableRow(_),
            Text(_),
            ThematicBreak(_),
            Toml(_),
            Yaml(_),
        } ignore {
            MdxJsxFlowElement(_),
            MdxjsEsm(_),
            MdxTextExpression(_),
            MdxJsxTextElement(_),
            MdxFlowExpression(_),
        });
    }

    mod lookups {
        use indoc::indoc;
        use markdown::ParseOptions;

        use super::*;

        #[test]
        fn good_link_ref() {
            let result = lookups_for(
                &ParseOptions::gfm(),
                ReadOptions {
                    validate_no_conflicting_links: true,
                    allow_unknown_markdown: false,
                },
                indoc! {r#"
                Hello [world][1]

                [1]: https://example.com
                "#},
            );
            expect_present(result, |lookups| {
                assert_eq!(1, lookups.link_definitions.len());
                assert_eq!(0, lookups.footnote_definitions.len());
                assert_eq!(
                    lookups.link_definitions.get("1").map(|d| &d.url),
                    Some(&"https://example.com".to_string())
                )
            });
        }

        /// This also covers the "good footnote" case.
        #[test]
        fn link_ref_looks_like_footnote() {
            let result = lookups_for(
                &ParseOptions::gfm(),
                ReadOptions {
                    validate_no_conflicting_links: true,
                    allow_unknown_markdown: false,
                },
                indoc! {r#"
                This [looks like a link][^1], but mdast parses it as a footnote.

                [^1]: https://example.com _What?!_
                "#},
            );

            expect_present(result, |lookups| {
                assert_eq!(0, lookups.link_definitions.len());
                assert_eq!(1, lookups.footnote_definitions.len());
                assert_eq!(
                    lookups
                        .footnote_definitions
                        .get("1")
                        .map(|d| simple_to_string(&d.children)),
                    Some("<p>https://example.com <em>What?!</em></p>".to_string())
                )
            });
        }

        /// mdast doesn't even register this as a link.
        #[test]
        fn link_missing_link_definition() {
            let md = "This [link is broken].";

            let result = lookups_for(
                &ParseOptions::gfm(),
                ReadOptions {
                    validate_no_conflicting_links: true,
                    allow_unknown_markdown: false,
                },
                md,
            );
            expect_present(result, |lookups| {
                assert_eq!(0, lookups.link_definitions.len());
                assert_eq!(0, lookups.footnote_definitions.len());
            });
        }

        /// mdast doesn't even register this as a footnote.
        #[test]
        fn footnote_missing_definition() {
            let md = "This [^a].";

            let result = lookups_for(
                &ParseOptions::gfm(),
                ReadOptions {
                    validate_no_conflicting_links: true,
                    allow_unknown_markdown: false,
                },
                md,
            );
            expect_present(result, |lookups| {
                assert_eq!(0, lookups.link_definitions.len());
                assert_eq!(0, lookups.footnote_definitions.len());
            });
        }

        /// The validation causes this to fail, because the nodes are different: they have different Positions.
        /// I could come up with a more clever comparison algorithm later, but this is good enough for now.
        #[test]
        fn link_has_same_definition_twice() {
            let result = lookups_for(
                &ParseOptions::gfm(),
                ReadOptions {
                    validate_no_conflicting_links: true,
                    allow_unknown_markdown: false,
                },
                indoc! {r#"
                This [link is duplicated][1].

                [1]: https://example.com/one
                [1]: https://example.com/one
                "#},
            );

            expect_absent(result, InvalidMd::ConflictingReferenceDefinition("1".to_string()));
        }

        // See [
        #[test]
        fn link_has_conflicting_definition() {
            fn get(validate_no_conflicting_links: bool) -> Result<Lookups, InvalidMd> {
                lookups_for(
                    &ParseOptions::gfm(),
                    ReadOptions {
                        validate_no_conflicting_links,
                        allow_unknown_markdown: false,
                    },
                    indoc! {r#"
                        This [link is duplicated][1].

                        [1]: https://example.com/one
                        [1]: https://example.com/different
                    "#},
                )
            }

            expect_absent(get(true), InvalidMd::ConflictingReferenceDefinition("1".to_string()));

            expect_present(get(false), |lookups| {
                assert_eq!(1, lookups.link_definitions.len());
                assert_eq!(0, lookups.footnote_definitions.len());
                assert_eq!(
                    lookups.link_definitions.get("1").map(|d| &d.url),
                    Some(&"https://example.com/one".to_string())
                );
            });
        }

        fn lookups_for(parse_opts: &ParseOptions, read_opts: ReadOptions, md: &str) -> Result<Lookups, InvalidMd> {
            let ast = markdown::to_mdast(md, parse_opts).unwrap();
            Lookups::new(&ast, &read_opts)
        }

        fn expect_present<F>(result: Result<Lookups, InvalidMd>, check: F)
        where
            F: FnOnce(Lookups),
        {
            match result {
                Ok(lookups) => check(lookups),
                Err(err) => panic!("expected good Lookups, but got: {:?}", err),
            }
        }

        fn expect_absent(result: Result<Lookups, InvalidMd>, expect: InvalidMd) {
            match result {
                Ok(_) => panic!("expected {:?}, but got good Lookups", expect),
                Err(err) => assert_eq!(err, expect),
            }
        }
    }

    mod nesting {
        use super::*;

        #[test]
        fn h1_with_two_paragraphs() -> Result<(), InvalidMd> {
            let linear = vec![
                m_node!(MdElem::Section {
                    depth: 1,
                    title: vec![mdq_inline!("first")],
                    body: vec![],
                }),
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("aaa")],
                }),
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("bbb")],
                }),
            ];
            let expect = vec![m_node!(MdElem::Section {
                depth: 1,
                title: vec![mdq_inline!("first")],
                body: vec![
                    m_node!(MdElem::Paragraph {
                        body: vec![mdq_inline!("aaa")],
                    }),
                    m_node!(MdElem::Paragraph {
                        body: vec![mdq_inline!("bbb")],
                    }),
                ],
            })];
            let actual = MdElem::all_from_iter(linear.into_iter().map(Ok))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn simple_nesting() -> Result<(), InvalidMd> {
            let linear = vec![
                m_node!(MdElem::Section {
                    depth: 1,
                    title: vec![mdq_inline!("first")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 2,
                    title: vec![mdq_inline!("aaa")],
                    body: vec![],
                }),
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("bbb")],
                }),
            ];
            let expect = vec![m_node!(MdElem::Section {
                depth: 1,
                title: vec![mdq_inline!("first")],
                body: vec![m_node!(MdElem::Section {
                    depth: 2,
                    title: vec![mdq_inline!("aaa")],
                    body: vec![m_node!(MdElem::Paragraph {
                        body: vec![mdq_inline!("bbb")],
                    })],
                })],
            })];
            let actual = MdElem::all_from_iter(linear.into_iter().map(Ok))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn only_headers() -> Result<(), InvalidMd> {
            let linear = vec![
                m_node!(MdElem::Section {
                    depth: 1,
                    title: vec![mdq_inline!("first")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 2,
                    title: vec![mdq_inline!("second")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 3,
                    title: vec![mdq_inline!("third")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 3,
                    title: vec![mdq_inline!("fourth")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 2,
                    title: vec![mdq_inline!("fifth")],
                    body: vec![],
                }),
            ];
            let expect = vec![m_node!(MdElem::Section {
                depth: 1,
                title: vec![mdq_inline!("first")],
                body: vec![
                    m_node!(MdElem::Section {
                        depth: 2,
                        title: vec![mdq_inline!("second")],
                        body: vec![
                            m_node!(MdElem::Section {
                                depth: 3,
                                title: vec![mdq_inline!("third")],
                                body: vec![],
                            }),
                            m_node!(MdElem::Section {
                                depth: 3,
                                title: vec![mdq_inline!("fourth")],
                                body: vec![],
                            }),
                        ],
                    }),
                    m_node!(MdElem::Section {
                        depth: 2,
                        title: vec![mdq_inline!("fifth")],
                        body: vec![],
                    }),
                ],
            })];
            let actual = MdElem::all_from_iter(linear.into_iter().map(Ok))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn no_headers() -> Result<(), InvalidMd> {
            let linear = vec![
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("one")],
                }),
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("two")],
                }),
            ];
            let expect = vec![
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("one")],
                }),
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("two")],
                }),
            ];
            let actual = MdElem::all_from_iter(linear.into_iter().map(Ok))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn header_skips() -> Result<(), InvalidMd> {
            let linear = vec![
                m_node!(MdElem::Section {
                    depth: 1,
                    title: vec![mdq_inline!("one")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 5,
                    title: vec![mdq_inline!("five")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 2,
                    title: vec![mdq_inline!("two")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 3,
                    title: vec![mdq_inline!("three")],
                    body: vec![],
                }),
            ];
            let expect = vec![m_node!(MdElem::Section {
                depth: 1,
                title: vec![mdq_inline!("one")],
                body: vec![
                    m_node!(MdElem::Section {
                        depth: 5,
                        title: vec![mdq_inline!("five")],
                        body: vec![],
                    }),
                    m_node!(MdElem::Section {
                        depth: 2,
                        title: vec![mdq_inline!("two")],
                        body: vec![m_node!(MdElem::Section {
                            depth: 3,
                            title: vec![mdq_inline!("three")],
                            body: vec![],
                        })],
                    }),
                ],
            })];
            let actual = MdElem::all_from_iter(linear.into_iter().map(Ok))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn backwards_order() -> Result<(), InvalidMd> {
            let linear = vec![
                m_node!(MdElem::Section {
                    depth: 3,
                    title: vec![mdq_inline!("three")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 2,
                    title: vec![mdq_inline!("two")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 1,
                    title: vec![mdq_inline!("one")],
                    body: vec![],
                }),
            ];
            let expect = vec![
                m_node!(MdElem::Section {
                    depth: 3,
                    title: vec![mdq_inline!("three")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 2,
                    title: vec![mdq_inline!("two")],
                    body: vec![],
                }),
                m_node!(MdElem::Section {
                    depth: 1,
                    title: vec![mdq_inline!("one")],
                    body: vec![],
                }),
            ];
            let actual = MdElem::all_from_iter(linear.into_iter().map(Ok))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn paragraph_before_and_after_header() -> Result<(), InvalidMd> {
            let linear = vec![
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("before")],
                }),
                m_node!(MdElem::Section {
                    depth: 3,
                    title: vec![mdq_inline!("the header")],
                    body: vec![],
                }),
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("after")],
                }),
            ];
            let expect = vec![
                m_node!(MdElem::Paragraph {
                    body: vec![mdq_inline!("before")],
                }),
                m_node!(MdElem::Section {
                    depth: 3,
                    title: vec![mdq_inline!("the header")],
                    body: vec![m_node!(MdElem::Paragraph {
                        body: vec![mdq_inline!("after")],
                    })],
                }),
            ];
            let actual = MdElem::all_from_iter(linear.into_iter().map(Ok))?;
            assert_eq!(expect, actual);
            Ok(())
        }
    }

    mod link_descriptions {
        use super::*;
        use markdown::ParseOptions;

        #[test]
        fn simple() {
            check("the text", "the text");
        }

        #[test]
        fn matched_text_brackets() {
            check("link [foo [bar]]", "link [foo [bar]]");
        }

        #[test]
        fn escaped_text_brackets() {
            check("link \\[foo bar", "link [foo bar")
        }

        fn check(in_description: &str, expected: &str) {
            let md_str = format!("[{in_description}](https://example.com)");
            let nodes = markdown::to_mdast(&md_str, &ParseOptions::default()).unwrap();
            let root_elems = MdDoc::read(nodes, &ReadOptions::default()).unwrap().roots;

            assert_eq!(
                root_elems,
                vec![MdElem::Paragraph(Paragraph {
                    body: vec![Inline::Link(Link {
                        display: vec![Inline::Text(Text {
                            variant: TextVariant::Plain,
                            value: expected.to_string(),
                        })],
                        link: LinkDefinition {
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    })],
                })]
            );
        }
    }

    mod concats {
        use super::*;
        use SpanVariant::*;
        use TextVariant::*;

        #[test]
        fn em_span_to_em_span() {
            check(
                [span(Emphasis, "hello, "), span(Emphasis, "world")],
                [span(Emphasis, "hello, world")],
            );
        }

        #[test]
        fn strong_span_to_strong_span() {
            check(
                [span(Strong, "hello, "), span(Strong, "world")],
                [span(Strong, "hello, world")],
            );
        }

        #[test]
        fn strong_span_to_em_span() {
            check(
                [span(Strong, "hello, "), span(Emphasis, "world")],
                [span(Strong, "hello, "), span(Emphasis, "world")],
            );
        }

        #[test]
        fn plain_text_to_plain_text() {
            check(
                [text(Plain, "hello, "), text(Plain, "world")],
                [text(Plain, "hello, world")],
            );
        }

        #[test]
        fn code_to_code() {
            // don't concat code
            check(
                [text(Code, "hello, "), text(Code, "world")],
                [text(Code, "hello, "), text(Code, "world")],
            );
        }

        #[test]
        fn math_to_math() {
            // don't concat math
            check(
                [text(Math, "hello, "), text(Math, "world")],
                [text(Math, "hello, "), text(Math, "world")],
            );
        }

        #[test]
        fn plain_text_to_code() {
            check(
                [text(Plain, "hello, "), text(Code, "world")],
                [text(Plain, "hello, "), text(Code, "world")],
            );
        }

        #[test]
        fn empty() {
            check([], []);
        }

        #[test]
        fn multiple() {
            check(
                [
                    span(Emphasis, "a"),
                    span(Emphasis, "b"),
                    span(Emphasis, "c"),
                    span(Strong, "d"),
                    span(Strong, "e"),
                    span(Emphasis, "f"),
                ],
                [span(Emphasis, "abc"), span(Strong, "de"), span(Emphasis, "f")],
            );
        }

        fn check<const N: usize, const M: usize>(incoming: [Inline; N], expected: [Inline; M]) {
            let actual = Concatenate::concatenate_similar(incoming.into_iter().collect());
            let expected: Vec<_> = expected.into_iter().collect();
            assert_eq!(actual, expected);
        }

        fn text(variant: TextVariant, value: &str) -> Inline {
            Inline::Text(Text {
                variant,
                value: value.to_string(),
            })
        }

        fn span(variant: SpanVariant, value: &str) -> Inline {
            Inline::Span(Span {
                variant,
                children: vec![text(Plain, value)],
            })
        }
    }

    /// A simple representation of some nodes. Very non-exhaustive, just for testing.
    fn simple_to_string(nodes: &[mdast::Node]) -> String {
        fn build(out: &mut String, node: &mdast::Node) {
            let (tag, text) = match node {
                mdast::Node::Text(text_node) => ("", text_node.value.as_str()),
                mdast::Node::Emphasis(_) => ("em", ""),
                mdast::Node::Paragraph(_) => ("p", ""),
                _ => ("", ""),
            };
            if !tag.is_empty() {
                out.push_str(&format!("<{}>", tag))
            }
            out.push_str(text);
            if let Some(children) = node.children() {
                children.iter().for_each(|c| build(out, c));
            }
            if !tag.is_empty() {
                out.push_str(&format!("</{}>", tag))
            }
        }
        let mut s = String::with_capacity(32);
        nodes.iter().for_each(|n| build(&mut s, n));
        s
    }

    fn internal_error() -> InvalidMd {
        InvalidMd::InternalError(UnknownMdParseError {
            backtrace: Backtrace::force_capture(),
        })
    }
}
