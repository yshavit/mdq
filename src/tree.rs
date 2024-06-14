use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

use markdown::mdast::{AlignKind, Code, Definition, FootnoteDefinition, Math, Node, ReferenceKind, Table, TableRow};

#[derive(Debug, PartialEq)]
pub enum MdqNode {
    // root
    Root {
        body: Vec<MdqNode>,
    },

    // paragraphs with child nodes
    Header {
        depth: u8,
        title: Vec<Inline>,
        body: Vec<MdqNode>,
    },
    Paragraph {
        body: Vec<Inline>,
    },
    BlockQuote {
        body: Vec<MdqNode>,
    },
    List {
        starting_index: Option<u32>,
        items: Vec<ListItem>,
    },
    Table {
        alignments: Vec<AlignKind>,
        rows: Vec<Tr>,
    },

    ThematicBreak,

    // blocks that contain strings (as opposed to nodes)
    CodeBlock {
        variant: CodeVariant,
        value: String,
    },

    // inline spans
    Inline(Inline), // TODO rename to "span"?
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
    /// ```
    /// [ambiguous link][1]
    ///
    /// [1]: https://example.com/one
    /// [1]: https://example.com/conflicting_url
    /// ```
    ///
    /// If this value is `true` and there are multiple _identical_ links, the validation will still pass:
    ///
    /// ```
    /// [non-ambiguous link][1]
    ///
    /// [1]: https://example.com/one
    /// [1]: https://example.com/one
    /// ```
    pub validate_no_conflicting_links: bool,
}

impl Default for ReadOptions {
    fn default() -> Self {
        Self {
            validate_no_conflicting_links: false,
        }
    }
}

pub type Tr = Vec<Line>; // TODO rename to TableRow
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
    Span {
        // TODO rename to Composite or something? Or Formatting?
        variant: SpanVariant,
        children: Vec<Inline>,
    },
    Text {
        variant: InlineVariant,
        value: String,
    },
    Link {
        text: Vec<Inline>,
        link: Link,
    },
    Image {
        alt: String,
        link: Link,
    },
    Footnote(Footnote),
}

#[derive(Debug)]
pub struct Footnote {
    pub label: String,
    pub text: Vec<MdqNode>,
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
pub struct Link {
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
    pub item: Vec<MdqNode>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SpanVariant {
    Delete,
    Emphasis,
    Strong,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum InlineVariant {
    Text,
    Code,
    Math,
    Html,
}

#[derive(Debug, PartialEq)]
pub enum InvalidMd {
    Unsupported(Node),
    NonListItemDirectlyUnderList(Node),
    NonRowDirectlyUnderTable(Node),
    NonInlineWhereInlineExpected,
    MissingReferenceDefinition(String),
    ConflictingReferenceDefinition(String),
    InternalError,
}

#[derive(Debug, PartialEq, Hash)]
pub struct CodeOpts {
    pub language: String,
    pub metadata: Option<String>,
}

#[derive(Debug, PartialEq)]
enum NoNode {
    Skipped,
    Invalid(InvalidMd),
}

impl From<InvalidMd> for NoNode {
    fn from(value: InvalidMd) -> Self {
        NoNode::Invalid(value)
    }
}

impl TryFrom<Node> for MdqNode {
    type Error = InvalidMd;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        Self::read(value, &ReadOptions::default())
    }
}

/// Defines all the mdx nodes as match arms. This let us easily mark them as TODOs, and in particular makes it so that
/// the prod and test code both marks them as TODOs using the same source list (namely, this macro).
macro_rules! mdx_nodes {
    {} => {
        Node::MdxJsxFlowElement(_)
        | Node::MdxjsEsm(_)
        | Node::MdxTextExpression(_)
        | Node::MdxJsxTextElement(_)
        | Node::MdxFlowExpression(_)
    };
}

impl MdqNode {
    fn read(node: Node, opts: &ReadOptions) -> Result<Self, InvalidMd> {
        let lookups = Lookups::new(&node, opts)?;
        match Self::from_mdast_0(node, &lookups) {
            Ok(result) => Ok(result),
            Err(NoNode::Skipped) => Ok(MdqNode::Root { body: Vec::new() }),
            Err(NoNode::Invalid(err)) => Err(err),
        }
    }

    fn from_mdast_0(node: Node, lookups: &Lookups) -> Result<Self, NoNode> {
        let result = match node {
            Node::Root(node) => MdqNode::Root {
                body: MdqNode::all(node.children, lookups)?,
            },
            Node::BlockQuote(node) => MdqNode::BlockQuote {
                body: MdqNode::all(node.children, lookups)?,
            },
            Node::FootnoteDefinition(_) => return Err(NoNode::Skipped),
            Node::List(node) => {
                let mut li_nodes = Vec::with_capacity(node.children.len());
                for node in node.children {
                    let Node::ListItem(li_node) = node else {
                        return Err(NoNode::Invalid(InvalidMd::NonListItemDirectlyUnderList(node)));
                    };
                    let li_mdq = ListItem {
                        checked: li_node.checked,
                        item: MdqNode::all(li_node.children, lookups)?,
                    };
                    li_nodes.push(li_mdq);
                }
                MdqNode::List {
                    starting_index: node.start,
                    items: li_nodes,
                }
            }
            Node::Break(_) => MdqNode::Inline(Inline::Text {
                variant: InlineVariant::Text,
                value: "\n".to_string(),
            }),
            Node::InlineCode(node) => MdqNode::Inline(Inline::Text {
                variant: InlineVariant::Code,
                value: node.value,
            }),
            Node::InlineMath(node) => MdqNode::Inline(Inline::Text {
                variant: InlineVariant::Math,
                value: node.value,
            }),
            Node::Delete(node) => MdqNode::Inline(Inline::Span {
                variant: SpanVariant::Delete,
                children: MdqNode::inlines(node.children, lookups)?,
            }),
            Node::Emphasis(node) => MdqNode::Inline(Inline::Span {
                variant: SpanVariant::Emphasis,
                children: MdqNode::inlines(node.children, lookups)?,
            }),
            Node::Image(node) => MdqNode::Inline(Inline::Image {
                alt: node.alt,
                link: Link {
                    url: node.url,
                    title: node.title,
                    reference: LinkReference::Inline,
                },
            }),
            Node::ImageReference(node) => MdqNode::Inline(Inline::Image {
                alt: node.alt,
                link: lookups.resolve_link(node.identifier, node.label, node.reference_kind)?,
            }),
            Node::Link(node) => MdqNode::Inline(Inline::Link {
                text: MdqNode::inlines(node.children, lookups)?,
                link: Link {
                    url: node.url,
                    title: node.title,
                    reference: LinkReference::Inline,
                },
            }),
            Node::LinkReference(node) => MdqNode::Inline(Inline::Link {
                text: MdqNode::inlines(node.children, lookups)?,
                link: lookups.resolve_link(node.identifier, node.label, node.reference_kind)?,
            }),
            Node::FootnoteReference(node) => {
                let definition = lookups.resolve_footnote(&node.identifier, &node.label)?;
                MdqNode::Inline(Inline::Footnote(Footnote {
                    label: node.label.unwrap_or(node.identifier),
                    text: MdqNode::all(definition.children.clone(), lookups)?,
                }))
            }
            Node::Strong(node) => MdqNode::Inline(Inline::Span {
                variant: SpanVariant::Strong,
                children: MdqNode::inlines(node.children, lookups)?,
            }),
            Node::Text(node) => MdqNode::Inline(Inline::Text {
                variant: InlineVariant::Text,
                value: node.value,
            }),
            Node::Code(node) => {
                let Code { value, lang, meta, .. } = node;
                MdqNode::CodeBlock {
                    value,
                    variant: CodeVariant::Code(match lang {
                        None => None,
                        Some(lang) => Some(CodeOpts {
                            language: lang,
                            metadata: meta,
                        }),
                    }),
                }
            }
            Node::Math(node) => {
                let Math { value, meta, .. } = node;
                MdqNode::CodeBlock {
                    value,
                    variant: CodeVariant::Math { metadata: meta },
                }
            }
            Node::Heading(node) => MdqNode::Header {
                depth: node.depth,
                title: Self::inlines(node.children, lookups)?,
                body: Vec::new(),
            },
            Node::Table(node) => {
                let Table { children, align, .. } = node;
                let mut rows = Vec::with_capacity(children.len());
                for row_node in children {
                    let Node::TableRow(TableRow {
                        children: cell_nodes, ..
                    }) = row_node
                    else {
                        return Err(NoNode::Invalid(InvalidMd::NonRowDirectlyUnderTable(row_node)));
                    };
                    let mut column = Vec::with_capacity(cell_nodes.len());
                    for cell_node in cell_nodes {
                        let Node::TableCell(table_cell) = cell_node else {
                            return Err(NoNode::Invalid(InvalidMd::InternalError));
                        };
                        let cell_contents = Self::inlines(table_cell.children, lookups)?;
                        column.push(cell_contents);
                    }
                    rows.push(column);
                }
                MdqNode::Table {
                    alignments: align,
                    rows,
                }
            }
            Node::ThematicBreak(_) => MdqNode::ThematicBreak,
            Node::TableRow(_) | Node::TableCell(_) | Node::ListItem(_) => {
                return Err(NoNode::Invalid(InvalidMd::InternalError)); // should have been handled by Node::Table
            }
            Node::Definition(_) => return Err(NoNode::Skipped),
            Node::Paragraph(node) => MdqNode::Paragraph {
                body: Self::inlines(node.children, lookups)?,
            },
            Node::Toml(node) => MdqNode::CodeBlock {
                variant: CodeVariant::Toml,
                value: node.value,
            },
            Node::Yaml(node) => MdqNode::CodeBlock {
                variant: CodeVariant::Yaml,
                value: node.value,
            },
            Node::Html(node) => MdqNode::Inline(Inline::Text {
                variant: InlineVariant::Html,
                value: node.value,
            }),

            mdx_nodes! {} => {
                // If you implement this, make sure to remove the mdx_nodes macro. That means you'll also need to
                // adjust the test `nodes_matcher` macro.
                return Err(NoNode::Invalid(InvalidMd::Unsupported(node)));
            }
        };
        Ok(result)
    }

    fn all(children: Vec<Node>, lookups: &Lookups) -> Result<Vec<Self>, InvalidMd> {
        Self::all_from_iter(NodeToMdqIter {
            children: children.into_iter(),
            lookups,
        })
    }

    fn all_from_iter<I>(iter: I) -> Result<Vec<Self>, InvalidMd>
    where
        I: Iterator<Item = Result<MdqNode, InvalidMd>>,
    {
        // This is just a struct that reflects the struct-variant of MdqNode::Header. If that
        // enum variant used the tuple-style with an explicitly defined struct, we wouldn't need
        // this.
        struct HContainer {
            depth: u8,
            title: Vec<Inline>,
            children: Vec<MdqNode>,
        }

        let mut result = Vec::with_capacity(16); // arbitrary capacity guess
        let mut headers: Vec<HContainer> = Vec::with_capacity(result.capacity());
        for child_mdq in iter {
            let child_mdq = child_mdq?;
            if let MdqNode::Header {
                depth,
                title,
                body: children,
            } = child_mdq
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
                        let prev = MdqNode::Header {
                            depth,
                            title,
                            body: children,
                        };
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
            let mdq_header = MdqNode::Header {
                depth,
                title,
                body: children,
            };
            let add_to = if let Some(HContainer { children, .. }) = headers.last_mut() {
                children
            } else {
                &mut result
            };
            add_to.push(mdq_header);
        }
        headers
            .drain(..)
            .map(|HContainer { depth, title, children }| MdqNode::Header {
                depth,
                title,
                body: children,
            })
            .for_each(|mdq_node| result.push(mdq_node));

        result.shrink_to_fit();
        Ok(result)
    }

    fn inlines(children: Vec<Node>, lookups: &Lookups) -> Result<Vec<Inline>, InvalidMd> {
        let mdq_children = Self::all(children, lookups)?;
        let mut result = Vec::with_capacity(mdq_children.len());
        for child in mdq_children {
            let MdqNode::Inline(inline) = child else {
                return Err(InvalidMd::NonInlineWhereInlineExpected);
            };
            // If both this and the previous were plain text, then just combine the texts. This can happen if there was
            // a Node::Break between them.
            if let (
                Some(Inline::Text {
                    variant: InlineVariant::Text,
                    value: prev_text,
                }),
                Inline::Text {
                    variant: InlineVariant::Text,
                    value: new_text,
                },
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
    I: Iterator<Item = Node>,
{
    children: I,
    lookups: &'a Lookups,
}

impl<'a, I> Iterator for NodeToMdqIter<'a, I>
where
    I: Iterator<Item = Node>,
{
    type Item = Result<MdqNode, InvalidMd>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let Some(next_node) = self.children.next() else {
                return None;
            };
            match MdqNode::from_mdast_0(next_node, self.lookups) {
                Ok(mdq_node) => {
                    break Some(Ok(mdq_node));
                }
                Err(NoNode::Skipped) => continue,
                Err(NoNode::Invalid(err)) => {
                    break Some(Err(err));
                }
            };
        }
    }
}

#[derive(Debug, PartialEq)]
struct Lookups {
    link_definitions: HashMap<String, Definition>,
    footnote_definitions: HashMap<String, FootnoteDefinition>,
}

impl Lookups {
    fn new(node: &Node, read_opts: &ReadOptions) -> Result<Self, InvalidMd> {
        const DEFAULT_CAPACITY: usize = 8; // random guess

        let mut result = Self {
            link_definitions: HashMap::with_capacity(DEFAULT_CAPACITY),
            footnote_definitions: HashMap::with_capacity(DEFAULT_CAPACITY),
        };

        result.build_lookups(node, &read_opts)?;

        Ok(result)
    }

    fn resolve_link(
        &self,
        identifier: String,
        label: Option<String>,
        reference_kind: ReferenceKind,
    ) -> Result<Link, NoNode> {
        if let None = label {
            todo!("What is this case???");
        }
        let Some(definition) = self.link_definitions.get(&identifier) else {
            let human_visible_identifier = label.unwrap_or(identifier);
            return Err(NoNode::Invalid(InvalidMd::MissingReferenceDefinition(
                human_visible_identifier,
            )));
        };
        let human_visible_identifier = label.unwrap_or(identifier);
        let link_ref = match reference_kind {
            ReferenceKind::Shortcut => LinkReference::Shortcut,
            ReferenceKind::Collapsed => LinkReference::Collapsed,
            ReferenceKind::Full => LinkReference::Full(human_visible_identifier),
        };
        Ok(Link {
            url: definition.url.to_owned(),
            title: definition.title.to_owned(),
            reference: link_ref,
        })
    }

    fn resolve_footnote(&self, identifier: &String, label: &Option<String>) -> Result<&FootnoteDefinition, NoNode> {
        if label.is_none() {
            todo!("What is this case???");
        }
        let Some(definition) = self.footnote_definitions.get(identifier) else {
            let human_visible_identifier = label.to_owned().unwrap_or_else(|| identifier.to_string());
            return Err(NoNode::Invalid(InvalidMd::MissingReferenceDefinition(
                human_visible_identifier,
            )));
        };
        Ok(definition)
    }

    fn build_lookups(&mut self, node: &Node, read_opts: &ReadOptions) -> Result<(), InvalidMd> {
        let x = format!("{:?}", node);
        let _ = x;
        match node {
            Node::FootnoteDefinition(def) => {
                Self::add_ref(&mut self.footnote_definitions, &def.identifier, def.clone(), read_opts)
            }
            Node::Definition(def) => Self::add_ref(&mut self.link_definitions, &def.identifier, def.clone(), read_opts),
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

    ///  tests of each mdast node type
    ///
    /// The purpose of these is not to test the parser (I trust mdast), but to test my understanding of how it works.
    ///
    /// For example, footnote are `[^a]` in markdown; does that identifier get parsed as `"^a"` or `"a"`?
    mod all_nodes {
        use std::collections::HashSet;
        use std::sync::{Arc, Mutex};
        use std::{thread, time};

        use indoc::indoc;
        use lazy_static::lazy_static;
        use markdown::mdast::Node;
        use markdown::{mdast, ParseOptions};
        use regex::Regex;

        use super::*;

        /// Turn a pattern match into an `if let ... { else panic! }`.
        macro_rules! unwrap {
            // TODO intellij errors if these are in the other order. file a ticket.
            ($enum_value:expr, $enum_variant:pat) => {
                let node = $enum_value;
                let node_debug = format!("{:?}", node);
                let $enum_variant = node else {
                    panic!("Expected {} but saw {}", stringify!($enum_variant), node_debug);
                };
            };
        }

        macro_rules! check {
            (no_node: $enum_value:expr, $enum_variant:pat, $lookups:expr => $no_node:expr $(, $body:block)? ) => {{
                let node = $enum_value;
                NODES_CHECKER.see(&node);
                unwrap!(node, $enum_variant);
                let node_clone = node.clone();
                let mdq_err = MdqNode::from_mdast_0(node_clone, &$lookups).err().expect("expected no MdqNode");
                assert_eq!(mdq_err, $no_node);
                $($body)?
            }};

            ( $enum_value:expr, $enum_variant:pat, $lookups:expr => $mdq_pat:pat = $mdq_body:block ) => {{
                let node = $enum_value;
                NODES_CHECKER.see(&node);
                unwrap!(node, $enum_variant);
                let node_clone = node.clone();
                let mdq = MdqNode::from_mdast_0(node_clone, &$lookups).unwrap();
                if let $mdq_pat = mdq $mdq_body else {
                    panic!("expected {} but saw {:?}", stringify!($mdq_pat), &mdq)
                }
            }};
        }

        /// Creates a matcher against [Node] with the given variants, and returns the variant names as a collection.
        ///
        /// If you see a compilation failure here, it means the call site is missing variants (or has an unknown
        /// variant).
        ///
        /// This macro assumes that each variant can match a pattern `Node::TheVariant(_)`.
        macro_rules! nodes_matcher {
            [$($variant:ident),* $(,)?] => {
                {
                    None.map(|n: Node| match n {
                        $(
                        Node::$variant(_) => {}
                        )*
                        mdx_nodes!{} => {
                            // If you implement mdx nodes, you should also remove the mdx_nodes macro. That will
                            // (correctly) break this macro. You should add those MDX arms to the get_mdast_node_names
                            // function, to ensure that we have tests for them.
                        }
                    });
                    vec![$(stringify!($variant).to_string(),)*].into_iter().collect()
                }
            };
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
            check!(child, Node::BlockQuote(_), lookups => MdqNode::BlockQuote { body } = {
                assert_eq!(&body, &vec![text_paragraph("hello")]);
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
                check!(&p.children[1], Node::FootnoteReference(_), lookups => MdqNode::Inline(footnote) = {
                    assert_eq!(footnote, Inline::Footnote(Footnote{
                        label: "a".to_string(),
                        text: vec![
                            text_paragraph("My footnote\nwith two lines.")
                        ],
                    }))
                });
                check!(no_node: &root.children[1], Node::FootnoteDefinition(_), lookups => NoNode::Skipped);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    Cool story [^a]!

                    [^a]: - footnote is a list"#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));

                check!(&p.children[1], Node::FootnoteReference(_), lookups => MdqNode::Inline(footnote) = {
                    assert_eq!(footnote, Inline::Footnote(Footnote{
                        label: "a".to_string(),
                        text: vec![
                            MdqNode::List {
                                starting_index: None,
                                items: vec![
                                    ListItem{
                                        checked: None,
                                        item: vec![text_paragraph("footnote is a list")],
                                    }
                                ],
                            },
                        ],
                    }))
                });
                check!(no_node: &root.children[1], Node::FootnoteDefinition(_), lookups => NoNode::Skipped);
            }
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

            check!(&root.children[0], Node::List(ul), lookups => MdqNode::List{starting_index, items} = {
                for child in &ul.children {
                    check!(no_node: child, Node::ListItem(_), lookups => NoNode::Invalid(InvalidMd::InternalError));
                }
                assert_eq!(starting_index, None);
                assert_eq!(items, vec![
                    ListItem {
                        checked: None,
                        item: vec![text_paragraph("First")],
                    },
                    ListItem {
                        checked: Some(false),
                        item: vec![text_paragraph("Second")],
                    },
                    ListItem {
                        checked: Some(true),
                        item: vec![text_paragraph("Third\nWith a line break")],
                    },
                ]);
            });
            check!(&root.children[1], Node::List(ol), lookups => MdqNode::List{starting_index, items} = {
                for child in &ol.children {
                    check!(no_node: child, Node::ListItem(_), lookups => NoNode::Invalid(InvalidMd::InternalError));
                }
                assert_eq!(starting_index, Some(4));
                assert_eq!(items, vec![
                    ListItem {
                        checked: None,
                        item: vec![text_paragraph("Fourth")],
                    },
                    ListItem {
                        checked: Some(false),
                        item: vec![text_paragraph("Fifth")],
                    },
                    ListItem {
                        checked: Some(true),
                        item: vec![
                            text_paragraph("Sixth"),
                            text_paragraph("With a paragraph"),
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

            check!(&root.children[0], Node::Paragraph(p), lookups => MdqNode::Paragraph{body} = {
                assert_eq!(p.children.len(), 3);
                check!(&p.children[0], Node::Text(_), lookups => MdqNode::Inline(text) = {
                    assert_eq!(text, Inline::Text {variant: InlineVariant::Text, value: "hello ".to_string()});
                });
                check!(&p.children[1], Node::Break(_), lookups => MdqNode::Inline(text) = {
                    assert_eq!(text, Inline::Text {variant: InlineVariant::Text, value: "\n".to_string()});
                });
                check!(&p.children[2], Node::Text(_), lookups => MdqNode::Inline(text) = {
                    assert_eq!(text, Inline::Text {variant: InlineVariant::Text, value: "world".to_string()});
                });
                assert_eq!(body, vec![
                    // note: just a single child, which has a two-line string
                    Inline::Text {variant: InlineVariant::Text, value: "hello \nworld".to_string()},
                ])
            });
        }

        #[test]
        fn inline_code() {
            let (root, lookups) = parse("`foo`");

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::InlineCode(_), lookups => MdqNode::Inline(inline) = {
                assert_eq!(inline, Inline::Text { variant: InlineVariant::Code, value: "foo".to_string() });
            });
        }

        #[test]
        fn inline_math() {
            let mut opts = ParseOptions::gfm();
            opts.constructs.math_text = true;
            let (root, lookups) = parse_with(&opts, r#"$ 0 \ne 1 $"#);

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::InlineMath(_), lookups => MdqNode::Inline(inline) = {
                assert_eq!(inline, Inline::Text { variant: InlineVariant::Math, value: r#" 0 \ne 1 "#.to_string() });
            });
        }

        #[test]
        fn inline_delete() {
            let (root, lookups) = parse_with(&ParseOptions::gfm(), "~~86 me~~");

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::Delete(_), lookups => MdqNode::Inline(inline) = {
                assert_eq!(inline, Inline::Span {
                    variant: SpanVariant::Delete,
                    children: vec![
                        Inline::Text { variant: InlineVariant::Text, value: "86 me".to_string()},
                    ]
                });
            });
        }

        #[test]
        fn inline_emphasis() {
            let (root, lookups) = parse("_86 me_");

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::Emphasis(_), lookups => MdqNode::Inline(inline) = {
                assert_eq!(inline, Inline::Span {
                    variant: SpanVariant::Emphasis,
                    children: vec![
                        Inline::Text { variant: InlineVariant::Text, value: "86 me".to_string()},
                    ]
                });
            });
        }

        #[test]
        fn inline_strong() {
            let (root, lookups) = parse("**strongman**");

            unwrap!(&root.children[0], Node::Paragraph(p));
            check!(&p.children[0], Node::Strong(_), lookups => MdqNode::Inline(inline) = {
                assert_eq!(inline, Inline::Span {
                    variant: SpanVariant::Strong,
                    children: vec![
                        Inline::Text { variant: InlineVariant::Text, value: "strongman".to_string()},
                    ]
                });
            });
        }

        #[test]
        fn inline_html() {
            {
                let (root, lookups) = parse("<a href>");

                check!(&root.children[0], Node::Html(_), lookups => MdqNode::Inline(inline) = {
                    assert_eq!(inline, Inline::Text {
                        variant: InlineVariant::Html,
                        value: "<a href>".to_string(),
                    });
                });
            }
            {
                // Being in a paragraph shows that it can be inline
                let (root, lookups) = parse(indoc! {r#"
                In <em>a paragraph.</em>
                "#});
                check!(&root.children[0], Node::Paragraph(_), lookups => MdqNode::Paragraph{body} = {
                    assert_eq!(body.len(), 4);
                    assert_eq!(body, vec![
                        inline_text("In "),
                        Inline::Text {
                            variant: InlineVariant::Html,
                            value: "<em>".to_string()},
                        inline_text("a paragraph."),
                        Inline::Text {
                            variant: InlineVariant::Html,
                            value: "</em>".to_string()},
                    ])
                });
            }
            {
                // Being in a paragraph shows that it can be inline
                let (root, lookups) = parse(indoc! {r#"
                In <em
                newline  >a paragraph.</em>
                "#});
                check!(&root.children[0], Node::Paragraph(_), lookups => MdqNode::Paragraph{body} = {
                    assert_eq!(body.len(), 4);
                    assert_eq!(body, vec![
                        inline_text("In "),
                        Inline::Text {
                            variant: InlineVariant::Html,
                            value: "<em\nnewline  >".to_string()},
                        inline_text("a paragraph."),
                        Inline::Text {
                            variant: InlineVariant::Html,
                            value: "</em>".to_string()},
                    ])
                });
            }
        }

        #[test]
        fn image() {
            {
                let (root, lookups) = parse("![]()");
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Image(_), lookups => MdqNode::Inline(img) = {
                    assert_eq!(img, Inline::Image {
                        alt: "".to_string(),
                        link: Link{
                            url: "".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    })
                });
            }
            {
                let (root, lookups) = parse("![](https://example.com/foo.png)");
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Image(_), lookups => MdqNode::Inline(img) = {
                    assert_eq!(img, Inline::Image {
                        alt: "".to_string(),
                        link: Link{
                            url: "https://example.com/foo.png".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    })
                });
            }
            {
                let (root, lookups) = parse("![alt text]()");
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Image(_), lookups => MdqNode::Inline(img) = {
                    assert_eq!(img, Inline::Image {
                        alt: "alt text".to_string(),
                        link: Link{
                            url: "".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    })
                });
            }
            {
                let (root, lookups) = parse(r#"![](https://example.com/foo.png "my tooltip")"#);
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Image(_), lookups => MdqNode::Inline(img) = {
                    assert_eq!(img, Inline::Image {
                        alt: "".to_string(),
                        link: Link{
                            url: "https://example.com/foo.png".to_string(),
                            title: Some("my tooltip".to_string()),
                            reference: LinkReference::Inline,
                        },
                    })
                });
            }
            {
                // This isn't an image, though it almost looks like one
                let (root, lookups) = parse(r#"![]("only a tooltip")"#);
                check!(&root.children[0], Node::Paragraph(_), lookups => p @ MdqNode::Paragraph{ .. } = {
                    assert_eq!(p, text_paragraph(r#"![]("only a tooltip")"#));
                });
            }
        }

        #[test]
        fn link() {
            {
                // inline, no title
                let (root, lookups) = parse("[hello _world_](https://example.com)");
                assert_eq!(root.children.len(), 1);
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Link(_), lookups => MdqNode::Inline(link) = {
                    assert_eq!(link, Inline::Link {
                        text: vec![
                            inline_text("hello "),
                            Inline::Span {
                                variant: SpanVariant::Emphasis,
                                children: vec![inline_text("world")],
                            }
                        ],
                        link: Link{
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    })
                });
            }
            {
                // inline, with title
                let (root, lookups) = parse(r#"[hello _world_](https://example.com "the title")"#);
                assert_eq!(root.children.len(), 1);
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::Link(_), lookups => MdqNode::Inline(link) = {
                    assert_eq!(link, Inline::Link {
                        text: vec![
                            inline_text("hello "),
                            Inline::Span {
                                variant: SpanVariant::Emphasis,
                                children: vec![inline_text("world")],
                            }
                        ],
                        link: Link{
                            url: "https://example.com".to_string(),
                            title: Some("the title".to_string()),
                            reference: LinkReference::Inline,
                        },
                    })
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
                check!(&p.children[0], Node::LinkReference(_), lookups => MdqNode::Inline(link) = {
                    assert_eq!(link, Inline::Link {
                        text: vec![
                            inline_text("hello "),
                            Inline::Span {
                                variant: SpanVariant::Emphasis,
                                children: vec![inline_text("world")],
                            },
                        ],
                        link: Link{
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        },
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
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
                check!(&p.children[0], Node::LinkReference(_), lookups => MdqNode::Inline(link) = {
                    assert_eq!(link, Inline::Link {
                        text: vec![
                            inline_text("hello "),
                            Inline::Span {
                                variant: SpanVariant::Emphasis,
                                children: vec![inline_text("world")],
                            },
                        ],
                        link: Link{
                            url: "https://example.com".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Collapsed,
                        },
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
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
                check!(&p.children[0], Node::LinkReference(_), lookups => MdqNode::Inline(link) = {
                    assert_eq!(link, Inline::Link {
                        text: vec![
                            inline_text("hello "),
                            Inline::Span {
                                variant: SpanVariant::Emphasis,
                                children: vec![inline_text("world")],
                            },
                        ],
                        link: Link{
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Shortcut,
                        },
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
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
                check!(&p.children[0], Node::ImageReference(_), lookups => MdqNode::Inline(img) = {
                    assert_eq!(img, Inline::Image {
                        alt: "".to_string(),
                        link: Link {
                            url: "https://example.com/image.png".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        }
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
            }
            {
                let (root, lookups) = parse(indoc! {r#"
                    ![][1]

                    [1]: https://example.com/image.png "my title""#});
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::ImageReference(_), lookups => MdqNode::Inline(img) = {
                    assert_eq!(img, Inline::Image {
                        alt: "".to_string(),
                        link: Link {
                            url: "https://example.com/image.png".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Full("1".to_string()),
                        }
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    ![my alt][]

                    [my alt]: https://example.com/image.png "my title""#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::ImageReference(_), lookups => MdqNode::Inline(img) = {
                    assert_eq!(img, Inline::Image {
                        alt: "my alt".to_string(),
                        link: Link {
                            url: "https://example.com/image.png".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Collapsed,
                        }
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    ![my alt]

                    [my alt]: https://example.com/image.png"#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::ImageReference(_), lookups => MdqNode::Inline(img) = {
                    assert_eq!(img, Inline::Image {
                        alt: "my alt".to_string(),
                        link: Link {
                            url: "https://example.com/image.png".to_string(),
                            title: None,
                            reference: LinkReference::Shortcut,
                        }
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
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
                check!(&p.children[0], Node::LinkReference(_), lookups => MdqNode::Inline(link) = {
                    assert_eq!(link, Inline::Link {
                        text: vec![],
                        link: Link {
                            url: "https://example.com/image.png".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        }
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
            }
            {
                let (root, lookups) = parse(indoc! {r#"
                    [][1]

                    [1]: https://example.com/image.png "my title""#});
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdqNode::Inline(link) = {
                    assert_eq!(link, Inline::Link {
                        text: vec![],
                        link: Link {
                            url: "https://example.com/image.png".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Full("1".to_string()),
                        }
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    [_my_ text][]

                    [_my_ text]: https://example.com/image.png "my title""#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdqNode::Inline(link) = {
                    assert_eq!(link, Inline::Link {
                        text: vec![
                            Inline::Span{
                                variant: SpanVariant::Emphasis,
                                children: vec![
                                    Inline::Text {variant: InlineVariant::Text,value: "my".to_string()}
                                ],
                            },
                            Inline::Text {variant: InlineVariant::Text,value: " text".to_string()}

                        ],
                        link: Link {
                            url: "https://example.com/image.png".to_string(),
                            title: Some("my title".to_string()),
                            reference: LinkReference::Collapsed,
                        }
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
            }
            {
                let (root, lookups) = parse_with(
                    &ParseOptions::gfm(),
                    indoc! {r#"
                    [my text]

                    [my text]: https://example.com/image.png"#},
                );
                unwrap!(&root.children[0], Node::Paragraph(p));
                check!(&p.children[0], Node::LinkReference(_), lookups => MdqNode::Inline(link) = {
                    assert_eq!(link, Inline::Link {
                        text: vec![
                            Inline::Text {variant: InlineVariant::Text,value: "my text".to_string()},
                        ],
                        link: Link {
                            url: "https://example.com/image.png".to_string(),
                            title: None,
                            reference: LinkReference::Shortcut,
                        }
                    })
                });
                check!(no_node: &root.children[1], Node::Definition(_), lookups => NoNode::Skipped);
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
                check!(&root.children[0], Node::Code(_), lookups => MdqNode::CodeBlock{variant, value} = {
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
                check!(&root.children[0], Node::Code(_), lookups => MdqNode::CodeBlock{variant, value} = {
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
                check!(&root.children[0], Node::Code(_), lookups => MdqNode::CodeBlock{variant, value} = {
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
                check!(&root.children[0], Node::Code(_), lookups => MdqNode::CodeBlock{variant, value} = {
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
                check!(&root.children[0], Node::Math(_), lookups => MdqNode::CodeBlock{variant, value} = {
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
                check!(&root.children[0], Node::Math(_), lookups => MdqNode::CodeBlock{variant, value} = {
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
            check!(&root.children[0], Node::Toml(_), lookups => MdqNode::CodeBlock{variant, value} = {
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
            check!(&root.children[0], Node::Yaml(_), lookups => MdqNode::CodeBlock{variant, value} = {
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

            let (header_depth, header_title) = check!(&root.children[0], Node::Heading(_), lookups => MdqNode::Header{depth, title, body} = {
                assert_eq!(depth, 2);
                assert_eq!(title, vec![
                    Inline::Text { variant: InlineVariant::Text, value: "Header with ".to_string()},
                    Inline::Span {
                        variant: SpanVariant::Emphasis,
                        children: vec![
                            Inline::Text { variant: InlineVariant::Text, value: "emphasis".to_string()},
                        ]
                    }
                ]);
                assert_eq!(body, vec![
                    // This code path doesn't do recursion; that's done in all_from_iter, which happens at the root
                ]);
                (depth, title)
            });

            check!(&Node::Root(root), Node::Root(_), lookups => MdqNode::Root{body} = {
                assert_eq!(body, vec![
                    MdqNode::Header{
                        depth: header_depth,
                        title: header_title,
                        body: vec![
                            text_paragraph("And some text below it.")
                        ]
                    },
                ])
            });
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
            check!(&root.children[1], Node::ThematicBreak(_), lookups => MdqNode::ThematicBreak = {
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
            check!(&root.children[0], Node::Table(table_node), lookups => MdqNode::Table{alignments, rows} = {
                assert_eq!(alignments, vec![AlignKind::Left, AlignKind::Center, AlignKind::Right, AlignKind::None]);
                assert_eq!(rows,
                    vec![ // rows
                        vec![// Header row
                            vec![inline_text("Header A")], // cells, each being a spans of inline
                            vec![inline_text("Header B")],
                            vec![inline_text("Header C")],
                            vec![inline_text("Header D")],
                        ],
                        vec![// first (and only) data row
                            vec![inline_text("1")], // cells, each being a spans of inline
                            vec![inline_text("2")],
                            vec![inline_text("3")],
                            vec![inline_text("4")],
                        ],
                    ],
                );
                // Do a spot check for the rows and cells; mainly just so that we'll have called check! on them.
                assert_eq!(table_node.children.len(), 2); // two rows
                check!(no_node: &table_node.children[0], Node::TableRow(tr), lookups => NoNode::Invalid(InvalidMd::InternalError), {
                    assert_eq!(tr.children.len(), 4); // four columns
                    check!(no_node: &tr.children[0], Node::TableCell(_), lookups => NoNode::Invalid(InvalidMd::InternalError));
                })
            });
        }

        #[test]
        fn all_variants_tested() {
            let timeout = time::Duration::from_millis(500);
            let retry_delay = time::Duration::from_millis(50);
            let start = time::Instant::now();
            loop {
                if NODES_CHECKER.all_were_seen() {
                    break;
                }
                if start.elapsed() >= timeout {
                    let remaining = NODES_CHECKER.remaining_as_copy();
                    panic!(
                        "Timed out, and missing {} variants:\n- {}",
                        remaining.len(),
                        remaining.join("\n- ")
                    )
                }
                thread::sleep(retry_delay);
            }
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

        struct NodesChecker {
            require: Arc<Mutex<HashSet<String>>>,
        }

        lazy_static! {
            static ref NODES_CHECKER: NodesChecker = NodesChecker::new();
        }

        impl NodesChecker {
            fn new() -> Self {
                Self {
                    require: Self::get_mdast_node_names(),
                }
            }

            fn see(&self, node: &Node) {
                let node_debug = format!("{:?}", node);
                let re = Regex::new(r"^\w+").unwrap();
                let node_name = re.find(&node_debug).unwrap().as_str();
                self.require.lock().map(|mut set| set.remove(node_name)).unwrap();
            }

            fn all_were_seen(&self) -> bool {
                self.require.lock().map(|set| set.is_empty()).unwrap()
            }

            fn remaining_as_copy(&self) -> Vec<String> {
                let mut result: Vec<String> = self
                    .require
                    .lock()
                    .map(|set| set.iter().map(|s| s.to_owned()).collect())
                    .unwrap();
                result.sort();
                result
            }

            /// Returns how many variants of [Node] there are.
            ///
            /// We can't use strum to do this, because we don't own the Node code. Instead, we rely on a bit of
            /// trickery. First, we create a `match` over all the variants, making sure each one is on its own line.
            /// Then, we use [line!] to get the line counts right before and after that `match`, and do some basic
            /// arithmetic to figure out how many variants there are.
            ///
            /// This isn't 100% fool-proof (it requires manually ensuring that each variant is on its own line, though
            /// `cargo fmt` helps with that), but it should be good enough in practice.
            fn get_mdast_node_names() -> Arc<Mutex<HashSet<String>>> {
                let all_node_names = nodes_matcher![
                    Root,
                    BlockQuote,
                    FootnoteDefinition,
                    List,
                    Toml,
                    Yaml,
                    Break,
                    InlineCode,
                    InlineMath,
                    Delete,
                    Emphasis,
                    FootnoteReference,
                    Html,
                    Image,
                    ImageReference,
                    Link,
                    LinkReference,
                    Strong,
                    Text,
                    Code,
                    Math,
                    Heading,
                    Table,
                    ThematicBreak,
                    TableRow,
                    TableCell,
                    ListItem,
                    Definition,
                    Paragraph,
                ];
                Arc::new(Mutex::new(all_node_names))
            }
        }
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
            fn get<'a>(validate_no_conflicting_links: bool) -> Result<Lookups, InvalidMd> {
                lookups_for(
                    &ParseOptions::gfm(),
                    ReadOptions {
                        validate_no_conflicting_links,
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
                MdqNode::Header {
                    depth: 1,
                    title: vec![inline_text("first")],
                    body: vec![],
                },
                MdqNode::Paragraph {
                    body: vec![inline_text("aaa")],
                },
                MdqNode::Paragraph {
                    body: vec![inline_text("bbb")],
                },
            ];
            let expect = vec![MdqNode::Header {
                depth: 1,
                title: vec![inline_text("first")],
                body: vec![
                    MdqNode::Paragraph {
                        body: vec![inline_text("aaa")],
                    },
                    MdqNode::Paragraph {
                        body: vec![inline_text("bbb")],
                    },
                ],
            }];
            let actual = MdqNode::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn simple_nesting() -> Result<(), InvalidMd> {
            let linear = vec![
                MdqNode::Header {
                    depth: 1,
                    title: vec![inline_text("first")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 2,
                    title: vec![inline_text("aaa")],
                    body: vec![],
                },
                MdqNode::Paragraph {
                    body: vec![inline_text("bbb")],
                },
            ];
            let expect = vec![MdqNode::Header {
                depth: 1,
                title: vec![inline_text("first")],
                body: vec![MdqNode::Header {
                    depth: 2,
                    title: vec![inline_text("aaa")],
                    body: vec![MdqNode::Paragraph {
                        body: vec![inline_text("bbb")],
                    }],
                }],
            }];
            let actual = MdqNode::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn only_headers() -> Result<(), InvalidMd> {
            let linear = vec![
                MdqNode::Header {
                    depth: 1,
                    title: vec![inline_text("first")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 2,
                    title: vec![inline_text("second")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 3,
                    title: vec![inline_text("third")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 3,
                    title: vec![inline_text("fourth")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 2,
                    title: vec![inline_text("fifth")],
                    body: vec![],
                },
            ];
            let expect = vec![MdqNode::Header {
                depth: 1,
                title: vec![inline_text("first")],
                body: vec![
                    MdqNode::Header {
                        depth: 2,
                        title: vec![inline_text("second")],
                        body: vec![
                            MdqNode::Header {
                                depth: 3,
                                title: vec![inline_text("third")],
                                body: vec![],
                            },
                            MdqNode::Header {
                                depth: 3,
                                title: vec![inline_text("fourth")],
                                body: vec![],
                            },
                        ],
                    },
                    MdqNode::Header {
                        depth: 2,
                        title: vec![inline_text("fifth")],
                        body: vec![],
                    },
                ],
            }];
            let actual = MdqNode::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn no_headers() -> Result<(), InvalidMd> {
            let linear = vec![
                MdqNode::Paragraph {
                    body: vec![inline_text("one")],
                },
                MdqNode::Paragraph {
                    body: vec![inline_text("two")],
                },
            ];
            let expect = vec![
                MdqNode::Paragraph {
                    body: vec![inline_text("one")],
                },
                MdqNode::Paragraph {
                    body: vec![inline_text("two")],
                },
            ];
            let actual = MdqNode::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn header_skips() -> Result<(), InvalidMd> {
            let linear = vec![
                MdqNode::Header {
                    depth: 1,
                    title: vec![inline_text("one")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 5,
                    title: vec![inline_text("five")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 2,
                    title: vec![inline_text("two")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 3,
                    title: vec![inline_text("three")],
                    body: vec![],
                },
            ];
            let expect = vec![MdqNode::Header {
                depth: 1,
                title: vec![inline_text("one")],
                body: vec![
                    MdqNode::Header {
                        depth: 5,
                        title: vec![inline_text("five")],
                        body: vec![],
                    },
                    MdqNode::Header {
                        depth: 2,
                        title: vec![inline_text("two")],
                        body: vec![MdqNode::Header {
                            depth: 3,
                            title: vec![inline_text("three")],
                            body: vec![],
                        }],
                    },
                ],
            }];
            let actual = MdqNode::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn backwards_order() -> Result<(), InvalidMd> {
            let linear = vec![
                MdqNode::Header {
                    depth: 3,
                    title: vec![inline_text("three")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 2,
                    title: vec![inline_text("two")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 1,
                    title: vec![inline_text("one")],
                    body: vec![],
                },
            ];
            let expect = vec![
                MdqNode::Header {
                    depth: 3,
                    title: vec![inline_text("three")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 2,
                    title: vec![inline_text("two")],
                    body: vec![],
                },
                MdqNode::Header {
                    depth: 1,
                    title: vec![inline_text("one")],
                    body: vec![],
                },
            ];
            let actual = MdqNode::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
            assert_eq!(expect, actual);
            Ok(())
        }

        #[test]
        fn paragraph_before_and_after_header() -> Result<(), InvalidMd> {
            let linear = vec![
                MdqNode::Paragraph {
                    body: vec![inline_text("before")],
                },
                MdqNode::Header {
                    depth: 3,
                    title: vec![inline_text("the header")],
                    body: vec![],
                },
                MdqNode::Paragraph {
                    body: vec![inline_text("after")],
                },
            ];
            let expect = vec![
                MdqNode::Paragraph {
                    body: vec![inline_text("before")],
                },
                MdqNode::Header {
                    depth: 3,
                    title: vec![inline_text("the header")],
                    body: vec![MdqNode::Paragraph {
                        body: vec![inline_text("after")],
                    }],
                },
            ];
            let actual = MdqNode::all_from_iter(linear.into_iter().map(|n| Ok(n)))?;
            assert_eq!(expect, actual);
            Ok(())
        }
    }

    fn inline_text(text: &str) -> Inline {
        Inline::Text {
            value: text.to_string(),
            variant: InlineVariant::Text,
        }
    }

    /// Helper for creating a [MdqNode::Paragraph] with plain text.
    fn text_paragraph(text: &str) -> MdqNode {
        MdqNode::Paragraph {
            body: vec![Inline::Text {
                variant: InlineVariant::Text,
                value: text.to_string(),
            }],
        }
    }

    /// A simple representation of some nodes. Very non-exhaustive, just for testing.
    fn simple_to_string(nodes: &Vec<Node>) -> String {
        fn build(out: &mut String, node: &Node) {
            let (tag, text) = match node {
                Node::Text(text_node) => ("", text_node.value.as_str()),
                Node::Emphasis(_) => ("em", ""),
                Node::Paragraph(_) => ("p", ""),
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
}
