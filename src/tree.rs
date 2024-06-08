use markdown::mdast::{AlignKind, Code, Math, Node, Table, TableRow};

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
        // this really should be an Either<Ol, Ul> with different list item types
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
}

#[derive(Debug, PartialEq)]
pub struct ListItem {
    pub checked: Option<bool>,
    pub children: Vec<MdqNode>,
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

    fn try_from(node: Node) -> Result<Self, InvalidMd> {
        let lookups = Lookups::new(&node)?;
        match Self::from_mdast_0(node, &lookups) {
            Ok(result) => Ok(result),
            Err(NoNode::Skipped) => Ok(MdqNode::Root { body: Vec::new() }),
            Err(NoNode::Invalid(err)) => Err(err),
        }
    }
}

impl MdqNode {
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
                        children: MdqNode::all(li_node.children, lookups)?,
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
            Node::Image(_) => {
                todo!()
            }
            Node::ImageReference(_) => {
                todo!()
            }
            Node::Link(_) => {
                todo!()
            }
            Node::LinkReference(_) => {
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
            | Node::FootnoteReference(_)
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

struct Lookups {
    // todo
}

impl Lookups {
    fn new(_node: &Node) -> Result<Self, InvalidMd> {
        // validations:
        // - all references are used (maybe? maybe we just ignore this)
        // - all required references are present
        // - no dupes
        // todo!()
        Ok(Lookups {})
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tree::Inline::Text;

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

    fn inline_text(text: &str) -> Inline {
        Text {
            value: text.to_string(),
            variant: InlineVariant::Text,
        }
    }
}