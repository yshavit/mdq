use std::backtrace::Backtrace;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Write};
use std::hash::{Hash, Hasher};
use std::vec::IntoIter;

use markdown::mdast;

#[derive(Debug, PartialEq)]
pub enum MdElem {
    // Container blocks
    BlockQuote(BlockQuote),
    List(List),
    Section(Section),

    // Leaf blocks
    CodeBlock(CodeBlock),
    Paragraph(Paragraph),
    Table(Table),
    ThematicBreak,

    Inline(Inline),
    Html(String),
}

#[derive(Debug, PartialEq)]
pub struct Section {
    pub depth: u8,
    pub title: Vec<Inline>,
    pub body: Vec<MdElem>,
}

#[derive(Debug, PartialEq)]
pub struct Paragraph {
    pub body: Vec<Inline>,
}

#[derive(Debug, PartialEq)]
pub struct BlockQuote {
    pub body: Vec<MdElem>,
}

#[derive(Debug, PartialEq)]
pub struct List {
    pub starting_index: Option<u32>,
    pub items: Vec<ListItem>,
}

#[derive(Debug, PartialEq)]
pub struct Table {
    pub alignments: Vec<mdast::AlignKind>,
    pub rows: Vec<TableRow>,
}

#[derive(Debug, PartialEq)]
pub struct CodeBlock {
    pub variant: CodeVariant,
    pub value: String,
}

/// See https://github.github.com/gfm/#link-reference-definitions
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LinkReference {
    Inline,
    Full(String),
    Collapsed,
    Shortcut,
}

pub struct ReadOptions {
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

pub type TableRow = Vec<Line>;
pub type Line = Vec<Inline>;

#[derive(Debug, PartialEq, Hash)]
pub enum CodeVariant {
    Code(Option<CodeOpts>),
    Math { metadata: Option<String> },
    Toml,
    Yaml,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Inline {
    Footnote(Footnote),
    Formatting(Formatting),
    Image(Image),
    Link(Link),
    Text(Text),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Formatting {
    pub variant: FormattingVariant,
    pub children: Vec<Inline>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Text {
    pub variant: TextVariant,
    pub value: String,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Link {
    pub text: Vec<Inline>,
    pub link_definition: LinkDefinition,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Image {
    pub alt: String,
    pub link: LinkDefinition,
}

#[derive(Debug)]
pub struct Footnote {
    pub label: String,
    pub text: Vec<MdElem>,
}

/// Note that [Footnote]'s [Eq] and [Hash] only key off of its label, _not_ its text content.
impl Hash for Footnote {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.label.hash(state);
    }
}

/// Note that [Footnote]'s [Eq] and [Hash] only key off of its label, _not_ its text content.
impl PartialEq for Footnote {
    fn eq(&self, other: &Self) -> bool {
        self.label == other.label
    }
}

impl Eq for Footnote {}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LinkDefinition {
    pub url: String,
    /// If you have `[1]: https://example.com "my title"`, this is the "my title".
    ///
    /// See: https://github.github.com/gfm/#link-reference-definitions
    pub title: Option<String>,
    pub reference: LinkReference,
}

#[derive(Debug, PartialEq)]
pub struct ListItem {
    pub checked: Option<bool>,
    pub item: Vec<MdElem>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum FormattingVariant {
    Delete,
    Emphasis,
    Strong,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TextVariant {
    Plain,
    Code,
    Math,
    Html,
}

#[derive(Debug, PartialEq)]
pub enum InvalidMd {
    Unsupported(mdast::Node),
    NonListItemDirectlyUnderList(mdast::Node),
    NonRowDirectlyUnderTable(mdast::Node),
    NonInlineWhereInlineExpected(MdElem),
    MissingReferenceDefinition(String),
    ConflictingReferenceDefinition(String),
    InternalError(PartialEqBacktrace),
    UnknownMarkdown(&'static str),
}

/// A wrapper for [Backtrace] that implements [PartialEq] to always return `true`. This lets us use it in a struct
/// while still letting us use `#[derive(PartialEq)]`
#[derive(Debug)]
pub struct PartialEqBacktrace(Backtrace);

impl PartialEq for PartialEqBacktrace {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

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
            InvalidMd::InternalError(backtrace) => {
                f.write_str("internal error\n")?;
                std::fmt::Display::fmt(&backtrace.0, f)
            }
            InvalidMd::UnknownMarkdown(description) => {
                write!(f, "encountered unknown markdown: {}\n\n", description)?;
                f.write_str("* Please consider reporting this at https://github.com/yshavit/mdq/issues\n")?;
                f.write_str("* You can suppress this error by using --allow-unknown-markdown.")
            }
        }?;
        f.write_char('\n')
    }
}

#[derive(Debug, PartialEq, Hash)]
pub struct CodeOpts {
    pub language: String,
    pub metadata: Option<String>,
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
/// ```compile_fail
/// m_node!(A::B::C { foo: 123 }
/// ```
///
/// into:
///
/// ```compile_fail
/// A::B(B::C(C { foo: 123 }))
/// ```
#[macro_export]
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

impl MdElem {
    pub fn read(node: mdast::Node, opts: &ReadOptions) -> Result<Vec<Self>, InvalidMd> {
        let lookups = Lookups::new(&node, opts)?;
        Self::from_mdast_0(node, &lookups)
    }

    fn from_mdast_0(node: mdast::Node, lookups: &Lookups) -> Result<Vec<Self>, InvalidMd> {
        let result = match node {
            mdast::Node::Root(node) => return MdElem::all(node.children, lookups),
            mdast::Node::BlockQuote(node) => m_node!(MdElem::BlockQuote {
                body: MdElem::all(node.children, lookups)?,
            }),
            mdast::Node::FootnoteDefinition(_) => return Ok(Vec::new()),
            mdast::Node::List(node) => {
                let mut li_nodes = Vec::with_capacity(node.children.len());
                for node in node.children {
                    let mdast::Node::ListItem(li_node) = node else {
                        return Err(InvalidMd::NonListItemDirectlyUnderList(node));
                    };
                    let li_mdq = ListItem {
                        checked: li_node.checked,
                        item: MdElem::all(li_node.children, lookups)?,
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
            mdast::Node::Delete(node) => MdElem::Inline(Inline::Formatting(Formatting {
                variant: FormattingVariant::Delete,
                children: MdElem::inlines(node.children, lookups)?,
            })),
            mdast::Node::Emphasis(node) => MdElem::Inline(Inline::Formatting(Formatting {
                variant: FormattingVariant::Emphasis,
                children: MdElem::inlines(node.children, lookups)?,
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
                text: MdElem::inlines(node.children, lookups)?,
                link_definition: LinkDefinition {
                    url: node.url,
                    title: node.title,
                    reference: LinkReference::Inline,
                },
            })),
            mdast::Node::LinkReference(node) => MdElem::Inline(Inline::Link(Link {
                text: MdElem::inlines(node.children, lookups)?,
                link_definition: lookups.resolve_link(node.identifier, node.label, node.reference_kind, lookups)?,
            })),
            mdast::Node::FootnoteReference(node) => {
                let definition = lookups.resolve_footnote(&node.identifier, &node.label, lookups)?;
                MdElem::Inline(Inline::Footnote(Footnote {
                    label: node.label.unwrap_or(node.identifier),
                    text: MdElem::all(definition.children.clone(), lookups)?,
                }))
            }
            mdast::Node::Strong(node) => MdElem::Inline(Inline::Formatting(Formatting {
                variant: FormattingVariant::Strong,
                children: MdElem::inlines(node.children, lookups)?,
            })),
            mdast::Node::Text(node) => MdElem::Inline(Inline::Text(Text {
                variant: TextVariant::Plain,
                value: node.value,
            })),
            mdast::Node::Code(node) => {
                let mdast::Code { value, lang, meta, .. } = node;
                m_node!(MdElem::CodeBlock {
                    value,
                    variant: CodeVariant::Code(match lang {
                        None => None,
                        Some(lang) => Some(CodeOpts {
                            language: lang,
                            metadata: meta,
                        }),
                    }),
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
                title: Self::inlines(node.children, lookups)?,
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
                        return Err(InvalidMd::NonRowDirectlyUnderTable(row_node));
                    };
                    let mut column = Vec::with_capacity(cell_nodes.len());
                    for cell_node in cell_nodes {
                        let mdast::Node::TableCell(table_cell) = cell_node else {
                            return Err(InvalidMd::InternalError(PartialEqBacktrace(Backtrace::force_capture())));
                        };
                        let cell_contents = Self::inlines(table_cell.children, lookups)?;
                        column.push(cell_contents);
                    }
                    rows.push(column);
                }
                m_node!(MdElem::Table {
                    alignments: align,
                    rows,
                })
            }
            mdast::Node::ThematicBreak(_) => m_node!(MdElem::ThematicBreak),
            mdast::Node::TableRow(_) | mdast::Node::TableCell(_) | mdast::Node::ListItem(_) => {
                // should have been handled by Node::Table
                return Err(InvalidMd::InternalError(PartialEqBacktrace(Backtrace::force_capture())));
            }
            mdast::Node::Definition(_) => return Ok(Vec::new()),
            mdast::Node::Paragraph(node) => m_node!(MdElem::Paragraph {
                body: Self::inlines(node.children, lookups)?,
            }),
            mdast::Node::Toml(node) => m_node!(MdElem::CodeBlock {
                variant: CodeVariant::Toml,
                value: node.value,
            }),
            mdast::Node::Yaml(node) => m_node!(MdElem::CodeBlock {
                variant: CodeVariant::Yaml,
                value: node.value,
            }),
            mdast::Node::Html(node) => MdElem::Html(node.value),

            mdx_nodes! {} => {
                // If you implement this, make sure to remove the mdx_nodes macro. That means you'll also need to
                // adjust the test `nodes_matcher` macro.
                return Err(InvalidMd::Unsupported(node));
            }
        };
        Ok(vec![result])
    }

    fn all(children: Vec<mdast::Node>, lookups: &Lookups) -> Result<Vec<Self>, InvalidMd> {
        Self::all_from_iter(NodeToMdqIter {
            lookups,
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

        result.shrink_to_fit();
        Ok(result)
    }

    fn inlines(children: Vec<mdast::Node>, lookups: &Lookups) -> Result<Vec<Inline>, InvalidMd> {
        let mdq_children = Self::all(children, lookups)?;
        let mut result = Vec::with_capacity(mdq_children.len());
        for child in mdq_children {
            // Get this child as an Inline, or complain. HTML can be either inline or block; Self::all will always
            // return it as a block, but we can just extract the String and convert it to an Inline.
            let inline = match child {
                MdElem::Inline(inline) => inline,
                MdElem::Html(html) => Inline::Text(Text {
                    variant: TextVariant::Html,
                    value: html,
                }),
                _ => return Err(InvalidMd::NonInlineWhereInlineExpected(child)),
            };
            // If both this and the previous were plain text, then just combine the texts. This can happen if there was
            // a Node::Break between them.
            if let (
                Some(Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: prev_text,
                })),
                Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: new_text,
                }),
            ) = (result.last_mut(), &inline)
            {
                prev_text.push_str(new_text)
            } else {
                result.push(inline);
            }
        }
        Ok(result)
    }
}

struct NodeToMdqIter<'a, I>
where
    I: Iterator<Item = mdast::Node>,
{
    children: I,
    pending: IntoIter<MdElem>,
    lookups: &'a Lookups,
}

impl<'a, I> Iterator for NodeToMdqIter<'a, I>
where
    I: Iterator<Item = mdast::Node>,
{
    type Item = Result<MdElem, InvalidMd>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(pending) = self.pending.next() {
                return Some(Ok(pending));
            }
            let Some(next_node) = self.children.next() else {
                return None;
            };
            match MdElem::from_mdast_0(next_node, self.lookups) {
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

        result.build_lookups(node, &read_opts)?;

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
        if let None = label {
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

    fn resolve_footnote(
        &self,
        identifier: &String,
        label: &Option<String>,
        lookups: &Lookups,
    ) -> Result<&mdast::FootnoteDefinition, InvalidMd> {
        if label.is_none() {
            lookups.unknown_markdown("footnote label was None")?;
        }
        let Some(definition) = self.footnote_definitions.get(identifier) else {
            let human_visible_identifier = label.to_owned().unwrap_or_else(|| identifier.to_string());
            return Err(InvalidMd::MissingReferenceDefinition(human_visible_identifier));
        };
        Ok(definition)
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
    use crate::md_elem;
    use crate::md_elems;
    use crate::mdq_inline;

    ///  tests of each mdast node type
    ///
    /// The purpose of these is not to test the parser (I trust mdast), but to test my understanding of how it works.
    ///
    /// For example, footnote are `[^a]` in markdown; does that identifier get parsed as `"^a"` or `"a"`?
    mod all_nodes {
        use super::*;
        use crate::unwrap;
        use indoc::indoc;
        use markdown::mdast::Node;
        use markdown::{mdast, ParseOptions};

        macro_rules! check {
            (error: $enum_value:expr, $enum_variant:pat, $lookups:expr => $err:expr $(, $body:block)? ) => {{
                let node = $enum_value;
                NODES_CHECKER.see(&node);
                unwrap!(node, $enum_variant);
                let node_clone = node.clone();
                let mdq_err = MdElem::from_mdast_0(node_clone, &$lookups).err().expect("expected no MdqNode");
                assert_eq!(mdq_err, $err);
                $($body)?
            }};

            (no_node: $enum_value:expr, $enum_variant:pat, $lookups:expr) => {{
                let node = $enum_value;
                NODES_CHECKER.see(&node);
                unwrap!(node, $enum_variant);
                let node_clone = node.clone();
                let mdqs = MdElem::from_mdast_0(node_clone, &$lookups).unwrap();
                assert_eq!(mdqs, Vec::new());
            }};

            ($enum_value:expr, $enum_variant:pat, $lookups:expr => $mdq_pat:pat = $mdq_body:block ) => {{
                let node = $enum_value;
                NODES_CHECKER.see(&node);
                unwrap!(node, $enum_variant);
                let node_clone = node.clone();
                let mut mdqs = MdElem::from_mdast_0(node_clone, &$lookups).unwrap();
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
            check!(child, Node::BlockQuote(_), lookups => m_node!(MdElem::BlockQuote{body}) = {
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
                    assert_eq!(footnote, Inline::Footnote(Footnote{
                        label: "a".to_string(),
                        text: md_elems!["My footnote\nwith two lines."],
                    }))
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
                    assert_eq!(footnote, Inline::Footnote(Footnote{
                        label: "a".to_string(),
                        text: vec![
                            m_node!(MdElem::List{
                                starting_index: None,
                                items: vec![
                                    ListItem{
                                        checked: None,
                                        item: md_elems!["footnote is a list"],
                                    }
                                ],
                            }),
                        ],
                    }))
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
                assert_eq!(footnote, Inline::Footnote(Footnote{
                    label: "1".to_string(),
                    text: vec![
                        MdElem::Paragraph(Paragraph {
                            body: vec![
                                mdq_inline!("Body text"),
                                Inline::Footnote(Footnote{
                                    label: "1".to_string(),
                                    text: vec![
                                        MdElem::Paragraph(Paragraph{
                                            body: vec![
                                                mdq_inline!("a footnote that references itself")
                                            ],
                                        }),
                                        MdElem::Inline(Inline::Footnote(Footnote{
                                            label: "1".to_string(),
                                            text: vec![], // TODO
                                        }))
                                    ],
                                }),
                            ]
                        })
                    ]
                }))
            });
            todo!()
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
                assert_eq!(inline, Inline::Formatting(Formatting{
                    variant: FormattingVariant::Delete,
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
                assert_eq!(inline, Inline::Formatting(Formatting{
                    variant: FormattingVariant::Emphasis,
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
                assert_eq!(inline, Inline::Formatting(Formatting{
                    variant: FormattingVariant::Strong,
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

                check!(&root.children[0], Node::Html(_), lookups => MdElem::Html(html) = {
                    assert_eq!(html, "<div>");
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

                check!(&root.children[0], Node::Html(_), lookups => MdElem::Html(html) = {
                    assert_eq!(html, "<div Hello\nworld. />");
                });
                assert_eq!(root.children.len(), 1);
            }
            {
                let (root, lookups) = parse("<a href>");

                check!(&root.children[0], Node::Html(_), lookups => MdElem::Html(inline) = {
                    assert_eq!(inline, "<a href>");
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
                            variant: TextVariant::Html,
                            value: "<em>".to_string()}),
                        mdq_inline!("a paragraph."),
                        Inline::Text (Text{
                            variant: TextVariant::Html,
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
                            variant: TextVariant::Html,
                            value: "<em\nnewline  >".to_string()}),
                        mdq_inline!("a paragraph."),
                        Inline::Text (Text{
                            variant: TextVariant::Html,
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
                        text: vec![
                            mdq_inline!("hello "),
                            Inline::Formatting(Formatting{
                                variant: FormattingVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            })
                        ],
                        link_definition: LinkDefinition{
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
                        text: vec![
                            mdq_inline!("hello "),
                            Inline::Formatting(Formatting {
                                variant: FormattingVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            })
                        ],
                        link_definition: LinkDefinition{
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
                        text: vec![
                            mdq_inline!("hello "),
                            Inline::Formatting(Formatting{
                                variant: FormattingVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            }),
                        ],
                        link_definition: LinkDefinition{
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
                        text: vec![
                            mdq_inline!("hello "),
                            Inline::Formatting(Formatting{
                                variant: FormattingVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            }),
                        ],
                        link_definition: LinkDefinition{
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
                        text: vec![
                            mdq_inline!("hello "),
                            Inline::Formatting(Formatting{
                                variant: FormattingVariant::Emphasis,
                                children: vec![mdq_inline!("world")],
                            }),
                        ],
                        link_definition: LinkDefinition{
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
                check!(&p.children[0], Node::Link(_), lookups => m_node!(MdElem::Inline::Link{text, link_definition}) = {
                    assert_eq!(text, vec![mdq_inline!("https://example.com")]);
                    assert_eq!(link_definition, LinkDefinition{
                        url: "https://example.com".to_string(),
                        title: None,reference: LinkReference::Inline,
                    });
                });
            }
            {
                let (root, lookups) = parse("<mailto:md@example.com>");
                unwrap!(&root.children[0], Node::Paragraph(p));
                assert_eq!(p.children.len(), 1);
                check!(&p.children[0], Node::Link(_), lookups => m_node!(MdElem::Inline::Link{text, link_definition}) = {
                    assert_eq!(text, vec![mdq_inline!("mailto:md@example.com")]);
                    assert_eq!(link_definition, LinkDefinition{
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
                check!(&p.children[0], Node::Link(_), lookups => m_node!(MdElem::Inline::Link{text, link_definition}) = {
                    assert_eq!(text, vec![mdq_inline!("https://example.com")]);
                    assert_eq!(link_definition, LinkDefinition{
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
                        text: vec![],
                        link_definition: LinkDefinition {
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
                        text: vec![],
                        link_definition: LinkDefinition {
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
                        text: vec![
                            Inline::Formatting(Formatting{
                                variant: FormattingVariant::Emphasis,
                                children: vec![
                                    Inline::Text (Text{variant: TextVariant::Plain,value: "my".to_string()})
                                ],
                            }),
                            Inline::Text (Text{variant: TextVariant::Plain,value: " text".to_string()})

                        ],
                        link_definition: LinkDefinition {
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
                        text: vec![
                            Inline::Text (Text{variant: TextVariant::Plain,value: "my text".to_string()}),
                        ],
                        link_definition: LinkDefinition {
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
                    Inline::Formatting (Formatting{
                        variant: FormattingVariant::Emphasis,
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
            let mdqs = MdElem::from_mdast_0(mdast_root, &lookups).unwrap();

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
                assert_eq!(alignments, vec![mdast::AlignKind::Left, mdast::AlignKind::Center, mdast::AlignKind::Right, mdast::AlignKind::None]);
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
                assert_eq!(alignments, vec![mdast::AlignKind::Left, mdast::AlignKind::Right]);
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

        crate::variants_checker!(NODES_CHECKER = Node {
            BlockQuote(_),
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
            let actual = MdElem::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
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
            let actual = MdElem::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
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
            let actual = MdElem::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
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
            let actual = MdElem::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
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
            let actual = MdElem::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
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
            let actual = MdElem::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
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
            let actual = MdElem::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
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
            let root_elems = MdElem::read(nodes, &ReadOptions::default()).unwrap();

            assert_eq!(
                root_elems,
                vec![MdElem::Paragraph(Paragraph {
                    body: vec![Inline::Link(Link {
                        text: vec![Inline::Text(Text {
                            variant: TextVariant::Plain,
                            value: expected.to_string(),
                        })],
                        link_definition: LinkDefinition {
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    })],
                })]
            );
        }
    }

    /// A simple representation of some nodes. Very non-exhaustive, just for testing.
    fn simple_to_string(nodes: &Vec<mdast::Node>) -> String {
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
        InvalidMd::InternalError(PartialEqBacktrace(Backtrace::force_capture()))
    }

    impl Default for ReadOptions {
        fn default() -> Self {
            Self {
                validate_no_conflicting_links: false,
                allow_unknown_markdown: false,
            }
        }
    }
}
