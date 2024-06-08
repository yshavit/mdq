use crate::tree::{CodeVariant, Inline, MdqNode};
use crate::tree_to_json;

#[allow(dead_code)]
pub enum Selector {
    // List(Matcher, ListType, Selected),  // should this be list or list item?
    Image { text: Matcher, href: Matcher },
    Link { text: Matcher, href: Matcher },
    CodeBlock(Matcher),
    Heading(Matcher),
    // TODO I need an "any", or maybe just a "paragraph", or maybe both
    // TODO does it make sense to select on a block quote?
}

#[allow(dead_code)]
pub enum ListType {
    Ordered,
    Unordered,
}

pub type Selected = Option<bool>;

#[allow(dead_code)]
pub enum Matcher {
    Any,
    Substring {
        look_for: String,
        anchored_left: bool,
        anchored_right: bool,
    },
    Regex(regex::Regex),
}

#[allow(dead_code)]
enum Resolver {
    // TODO need better name
    Current,
    Next,
    Until(fn(&MdqNode, &MdqNode) -> bool),
}

impl Matcher {
    fn matches(&self, text: &str) -> bool {
        match self {
            Matcher::Any => true,
            Matcher::Substring {
                look_for,
                anchored_left,
                anchored_right,
            } => {
                // TODO we can do this more efficiently, but this is good for now.
                // In the future, we could take an approach of comparing each char of look_for to
                // the next char in the &strs list.
                match (anchored_left, anchored_right) {
                    (false, false) => text.contains(look_for),
                    (true, false) => text.starts_with(look_for),
                    (false, true) => text.ends_with(look_for),
                    (true, true) => text.eq(look_for),
                }
            }
            Matcher::Regex(pattern) => pattern.is_match(&text),
        }
    }
}

impl Selector {
    pub fn find<'a>(&'a self, node: &'a MdqNode) -> Vec<&MdqNode> {
        match node {
            MdqNode::Root { body } => {
                self.find_in_children(body)
            }
            MdqNode::Header { depth, title, body } => {
                if let Selector::Heading(matcher) = self {
                    if matcher.matches(&Self::line_to_string(title)) {
                        body.iter().map(|elem| elem).collect()
                    } else {
                        Vec::new()
                    }
                } else {
                    self.find_in_children(body)
                }
            }
            MdqNode::Paragraph { .. } => {
                Vec::new() // see TODO on Selector
            }
            MdqNode::BlockQuote { body } => {
                self.find_in_children(body)
            }
            MdqNode::List { starting_index, items } => {
                let _is_ordered = starting_index.is_some(); // TODO use in selected
                items.iter().flat_map(|li| {
                    // TODO check selected
                    self.find_in_children(&li.children)
                }).collect()
            }
            MdqNode::Table { align, rows } => {
                Vec::new() // todo()
            }
            MdqNode::ThematicBreak => {
                Vec::new() // can't be selected, doesn't have children
            }
            MdqNode::CodeBlock { variant, value } => {
                let matched = match (self, variant) {
                    (Selector::CodeBlock(matcher), CodeVariant::Code(_)) => matcher.matches(value),
                    (_, _) => false,
                };
                if matched { vec!(node) } else { Vec::new() }
            }
            MdqNode::Inline(inline) => {
                let matched = match inline {
                    Inline::Span { variant, children } => {
                        match (self, variant) {
                            (_, _) => false, // TODO links, etc
                        }
                    }
                    Inline::Text { .. } => false,
                };
                if matched { vec!(node) } else { Vec::new() }
            }
        }

    }

    fn find_in_children<'a>(&'a self, children: &'a Vec<MdqNode>) -> Vec<&MdqNode> {
        children.iter().flat_map(|elem| self.find(elem)).collect()
    }

    fn line_to_string(line: &[Inline]) -> String {
        tree_to_json::TextOnly::line_to_string(line)
    }
}