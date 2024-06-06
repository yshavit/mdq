use markdown::mdast::{AlignKind, Code, Math, Node, Table, TableRow};

pub enum MdqNode {
    // root
    Root {
        children: Vec<MdqNode>
    },

    // paragraphs with child nodes
    Heading {
        depth: u8,
        title: Vec<Inline>,
    },
    Paragraph {
        children: Vec<MdqNode>,
    },
    BlockQuote {
        children: Vec<MdqNode>
    },
    List {
        starting_index: Option<u32>,
        children: Vec<ListItem>,
    },
    Table {
        align: Vec<AlignKind>,
        rows: Vec<Vec<Inline>>,
    },
    ThematicBreak,

    // paragraphs with fixed text
    CodeBlock {
        value: String,
        opts: Option<CodeOpts>,
    },
    MathBlock {
        value: String,
        metadata: Option<String>,
    },

    // inline spans
    Inline(Inline),
}

pub enum Inline {
    Span {
        variant: SpanVariant,
        children: Vec<MdqNode>,
    },
    Text {
        variant: InlineVariant,
        value: String,
    },

}

pub struct ListItem {
    checked: Option<bool>,
    children: Vec<MdqNode>,
}

pub enum SpanVariant {
    Delete,
    Emphasis,
    Strong,
}

pub enum InlineVariant {
    Text,
    Code,
    Math,
}

pub enum InvalidMd {
    Unsupported(Node),
    NonListItemDirectlyUnderList(Node),
    NonRowDirectlyUnderTable(Node),
    NonInlineWhereInlineExpected,
    InternalError,
}

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
        todo!("need to reorganize into headers");
        match Self::from_mdast_0(node, &lookups) {
            Ok(result) => Ok(result),
            Err(NoNode::Skipped) => Ok(MdqNode::Root { children: Vec::new() }),
            Err(NoNode::Invalid(err)) => Err(err),
        }
    }
}

impl MdqNode {
    fn from_mdast_0(node: Node, lookups: &Lookups) -> Result<Self, NoNode> {
        let result = match node {
            Node::Root(node) =>
                MdqNode::Root {
                    children: MdqNode::all(node.children, lookups)?,
                },
            Node::BlockQuote(node) =>
                MdqNode::BlockQuote { children: MdqNode::all(node.children, lookups)? },
            Node::FootnoteDefinition(_) =>
                return Err(NoNode::Skipped),
            Node::List(node) => {
                let mut li_nodes = Vec::with_capacity(node.children.len());
                for node in node.children {
                    let Node::ListItem(li_node) = node else {
                        return Err(NoNode::Invalid(InvalidMd::NonListItemDirectlyUnderList(node)));
                    };
                    let li_mdq = ListItem {
                        checked: li_node.checked,
                        children: MdqNode::all(li_node.children, lookups)?,
                    };
                    li_nodes.push(li_mdq);
                }
                MdqNode::List {
                    starting_index: node.start,
                    children: li_nodes,
                }
            }
            Node::Break(_) =>
                MdqNode::Inline(Inline::Text { variant: InlineVariant::Text, value: "\n".to_string() }),
            Node::InlineCode(node) =>
                MdqNode::Inline(Inline::Text { variant: InlineVariant::Code, value: node.value }),
            Node::InlineMath(node) =>
                MdqNode::Inline(Inline::Text { variant: InlineVariant::Math, value: node.value }),
            Node::Delete(node) =>
                MdqNode::Inline(Inline::Span { variant: SpanVariant::Delete, children: MdqNode::all(node.children, lookups)? }),
            Node::Emphasis(node) =>
                MdqNode::Inline(Inline::Span { variant: SpanVariant::Emphasis, children: MdqNode::all(node.children, lookups)? }),
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
            Node::Strong(node) => {
                MdqNode::Inline(Inline::Span { variant: SpanVariant::Strong, children: MdqNode::all(node.children, lookups)? })
            }
            Node::Text(node) => {
                MdqNode::Inline(Inline::Text { variant: InlineVariant::Text, value: node.value })
            }
            Node::Code(node) => {
                let Code { value, lang, meta, .. } = node;
                MdqNode::CodeBlock {
                    value,
                    opts: match lang {
                        None => None,
                        Some(lang) => {
                            Some(CodeOpts {
                                language: lang,
                                metadata: meta,
                            })
                        }
                    },
                }
            }
            Node::Math(node) => {
                let Math { value, meta, .. } = node;
                MdqNode::MathBlock {
                    value,
                    metadata: meta,
                }
            }
            Node::Heading(node) => {
                MdqNode::Heading {
                    depth: node.depth,
                    title: Self::inline(node.children, lookups)?,
                }
            }
            Node::Table(node) => {
                let Table { children, align, .. } = node;
                let mut rows = Vec::with_capacity(children.len());
                for row_node in children {
                    let Node::TableRow(TableRow { children: cell_nodes, .. }) = row_node else {
                        return Err(NoNode::Invalid(InvalidMd::NonRowDirectlyUnderTable(row_node)));
                    };
                    let mut cells = Vec::with_capacity(cell_nodes.len());
                    for cell_node in cell_nodes {
                        let MdqNode::Inline(cell) = Self::from_mdast_0(cell_node, lookups)? else {
                            return Err(NoNode::Invalid(InvalidMd::NonInlineWhereInlineExpected));
                        };
                        cells.push(cell);
                    }
                    rows.push(cells);
                }
                MdqNode::Table {
                    align,
                    rows,
                }
            }
            Node::ThematicBreak(_) => {
                MdqNode::ThematicBreak
            }
            Node::TableRow(_)
            | Node::TableCell(_)
            | Node::ListItem(_)
            => {
                return Err(NoNode::Invalid(InvalidMd::InternalError)); // should have been handled explicitly!
            }
            Node::Definition(_) => return Err(NoNode::Skipped),
            Node::Paragraph(node) => {
                MdqNode::Paragraph {
                    children: Self::all(node.children, lookups)?,
                }
            }

            Node::MdxJsxFlowElement(_)
            | Node::MdxjsEsm(_)
            | Node::MdxTextExpression(_)
            | Node::MdxJsxTextElement(_)
            | Node::MdxFlowExpression(_)
            | Node::Toml(_)
            | Node::Yaml(_)
            | Node::FootnoteReference(_)
            | Node::Html(_)
            => return Err(NoNode::Invalid(InvalidMd::Unsupported(node)))
        };
        Ok(result)
    }

    fn all(children: Vec<Node>, lookups: &Lookups) -> Result<Vec<Self>, InvalidMd> {
        let mut result = Vec::with_capacity(children.len());
        for child_node in children {
            let child_mdq = match Self::from_mdast_0(child_node, lookups) {
                Ok(n) => n,
                Err(NoNode::Skipped) => continue,
                Err(NoNode::Invalid(err)) => return Err(err),
            };
            result.push(child_mdq);
        }
        Ok(result)
    }

    fn inline(children: Vec<Node>, lookups: &crate::normalized_ast::Lookups) -> Result<Vec<Inline>, InvalidMd> {
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

struct Lookups {
    // todo
}

impl Lookups {
    fn new(_node: &Node) -> Result<Self, InvalidMd> {
        // validations:
        // - all references are used (maybe? maybe we just ignore this)
        // - all required references are present
        // - no dupes
        todo!()
    }
}
