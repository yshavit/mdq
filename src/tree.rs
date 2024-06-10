use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;

use markdown::mdast::{
    AlignKind, Code, Definition, FootnoteDefinition, Math, Node, ReferenceKind, Table, TableRow,
};

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
    Inline(Inline),
}

/// See https://github.github.com/gfm/#link-reference-definitions
#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum CodeVariant {
    Code(Option<CodeOpts>),
    Math { metadata: Option<String> },
    Toml,
    Yaml,
}

#[derive(Debug, PartialEq)]
pub enum Inline {
    Span {
        variant: SpanVariant,
        children: Vec<Inline>,
    },
    Text {
        variant: InlineVariant,
        value: String,
    },
    Link {
        url: String,
        text: Vec<Inline>,

        /// If you have `[1]: https://example.com "my title"`, this is the "my title".
        ///
        /// See: https://github.github.com/gfm/#link-reference-definitions
        title: Option<String>,

        reference: LinkReference,
    },
    Image {
        url: String,
        alt: String,
        title: Option<String>,
    },
}

#[derive(Debug, PartialEq)]
pub struct ListItem {
    pub checked: Option<bool>,
    pub item: Vec<MdqNode>,
}

#[derive(Debug, PartialEq)]
pub enum SpanVariant {
    Delete,
    Emphasis,
    Strong,
}

#[derive(Debug, PartialEq)]
pub enum InlineVariant {
    Text,
    Code,
    Math,
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

#[derive(Debug, PartialEq)]
pub struct CodeOpts {
    pub language: String,
    pub metadata: Option<String>,
}

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
                        return Err(NoNode::Invalid(InvalidMd::NonListItemDirectlyUnderList(
                            node,
                        )));
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
                url: node.url,
                alt: node.alt,
                title: node.title,
            }),
            Node::ImageReference(_) => {
                return Err(NoNode::Skipped);
            }
            Node::Link(node) => MdqNode::Inline(Inline::Link {
                url: node.url,
                text: MdqNode::inlines(node.children, lookups)?,
                title: node.title,
                reference: LinkReference::Inline,
            }),
            Node::LinkReference(node) => {
                if let Some(node_label) = node.label {
                    if node_label != node.identifier {
                        todo!("What is this case?");
                    }
                }

                let Some(definition) = lookups.link_definitions.get(&node.identifier) else {
                    return Err(NoNode::Invalid(InvalidMd::MissingReferenceDefinition(
                        node.identifier,
                    )));
                };
                if let Some(definition_label) = &definition.label {
                    if definition_label != &node.identifier {
                        todo!("What is this case?");
                    }
                }
                let link_ref = match node.reference_kind {
                    ReferenceKind::Shortcut => LinkReference::Shortcut,
                    ReferenceKind::Collapsed => LinkReference::Collapsed,
                    ReferenceKind::Full => LinkReference::Full(node.identifier),
                };
                MdqNode::Inline(Inline::Link {
                    url: definition.url.to_owned(),
                    text: MdqNode::inlines(node.children, lookups)?,
                    title: definition.title.to_owned(),
                    reference: link_ref,
                })
            }
            Node::FootnoteReference(_) => {
                todo!()
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
                let Code {
                    value, lang, meta, ..
                } = node;
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
                let Table {
                    children, align, ..
                } = node;
                let mut rows = Vec::with_capacity(children.len());
                for row_node in children {
                    let Node::TableRow(TableRow {
                        children: cell_nodes,
                        ..
                    }) = row_node
                    else {
                        return Err(NoNode::Invalid(InvalidMd::NonRowDirectlyUnderTable(
                            row_node,
                        )));
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
                return Err(NoNode::Invalid(InvalidMd::InternalError)); // should have been handled explicitly!
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

            Node::MdxJsxFlowElement(_)
            | Node::MdxjsEsm(_)
            | Node::MdxTextExpression(_)
            | Node::MdxJsxTextElement(_)
            | Node::MdxFlowExpression(_)
            | Node::Html(_) => return Err(NoNode::Invalid(InvalidMd::Unsupported(node))),
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
                        headers.push(HContainer {
                            depth,
                            title,
                            children,
                        });
                        break;
                    };
                    // There is a header. See if it's lower than ours; if so, we'll just add
                    // ourselves to it, and push our info to the stack
                    if prev.depth < depth {
                        headers.push(HContainer {
                            depth,
                            title,
                            children,
                        });
                        break;
                    } else {
                        // We need to pop the previous header. When we do, either add it as a child
                        // to the new previous, or else to the top-level results if there is no new
                        // previous. Then, we'll just loop back around.
                        let HContainer {
                            depth,
                            title,
                            children,
                        } = headers.pop().unwrap(); // "let Some(prev)" above guarantees that this works
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
        while let Some(HContainer {
            depth,
            title,
            children,
        }) = headers.pop()
        {
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
            .map(
                |HContainer {
                     depth,
                     title,
                     children,
                 }| MdqNode::Header {
                    depth,
                    title,
                    body: children,
                },
            )
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
            result.push(inline);
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

    fn build_lookups(&mut self, node: &Node, read_opts: &ReadOptions) -> Result<(), InvalidMd> {
        let x = format!("{:?}", node);
        let _ = x;
        match node {
            Node::FootnoteDefinition(def) => Self::add_ref(
                &mut self.footnote_definitions,
                &def.identifier,
                def.clone(),
                read_opts,
            ),
            Node::Definition(def) => Self::add_ref(
                &mut self.link_definitions,
                &def.identifier,
                def.clone(),
                read_opts,
            ),
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
                    Err(InvalidMd::ConflictingReferenceDefinition(
                        other.key().to_owned(),
                    ))
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

    mod lookups {
        use indoc::indoc;
        use markdown::ParseOptions;

        use super::*;

        #[test]
        fn playground() {
            let md = indoc! {r#"
                Hello [world]

                [world]: https://example.com "MY TITLE"
                "#};

            let ast = markdown::to_mdast(md, &ParseOptions::gfm()).unwrap();
            eprintln!("AST: {:?}", ast);
            let mdq = MdqNode::read(ast, &ReadOptions::default()).unwrap();
            eprintln!("MDQ: {:?}", mdq);
            todo!("END OF PLAYGROUND");
        }

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
                    Some("https://example.com What?!".to_string())
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

            expect_absent(
                result,
                InvalidMd::ConflictingReferenceDefinition("1".to_string()),
            );
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

            expect_absent(
                get(true),
                InvalidMd::ConflictingReferenceDefinition("1".to_string()),
            );

            expect_present(get(false), |lookups| {
                assert_eq!(1, lookups.link_definitions.len());
                assert_eq!(0, lookups.footnote_definitions.len());
                assert_eq!(
                    lookups.link_definitions.get("1").map(|d| &d.url),
                    Some(&"https://example.com/one".to_string())
                );
            });
        }

        fn lookups_for(
            parse_opts: &ParseOptions,
            read_opts: ReadOptions,
            md: &str,
        ) -> Result<Lookups, InvalidMd> {
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

    /// A simple representation of some nodes. Very non-exhaustive, just for testing.
    fn simple_to_string(nodes: &Vec<Node>) -> String {
        fn build(out: &mut String, node: &Node) {
            match node {
                Node::Text(text_node) => out.push_str(&text_node.value),
                _ => {
                    if let Some(children) = node.children() {
                        children.iter().for_each(|c| build(out, c))
                    }
                }
            }
        }
        let mut s = String::with_capacity(32);
        nodes.iter().for_each(|n| build(&mut s, n));
        s
    }
}
