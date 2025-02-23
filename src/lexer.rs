use std::collections::HashMap;
use std::fmt;


#[derive(Default)]
pub struct Trie {
    is_end_of_word: bool,
    children:       HashMap<char, Trie>
}

impl Trie {
    pub fn insert(&mut self, word: &str) {
        let mut current = self;

        for ch in word.chars() {
            current = current.children.entry(ch).or_insert_with(Trie::default);
        }

        current.is_end_of_word = true;
    }

    // Returns how many characters in Trie matched `input`
    pub fn common_prefix_length(&self, input: &str) -> usize {
        let mut current_node = self;
        let mut matched = 0;

        for c in input.chars() {
            match current_node.children.get(&c) {
                Some(node) => {
                    current_node = node;
                    matched += 1;
                }
                None => break
            }
        }

        matched
    }

    pub fn find_longest_word<'a>(&self, input: &'a str) -> Option<&'a str> {
        let mut current_node = self;
        let mut longest_match: Option<usize> = None;

        for (position, current_character) in input.chars().enumerate() {
            match current_node.children.get(&current_character) {
                Some(node) => {
                    current_node = node;

                    if current_node.is_end_of_word {
                        longest_match = Some(position + 1);
                    }
                }
                None => break
            }
        }

        longest_match.map(|end_position| &input[0 .. end_position])
    }
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Operator,
    Comment,
    Keyword,
    Delimiter,
    NumericValue,
    StringLiteral,
    Function,
    Identifier,
    Other,
    Whitespace
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Operator => write!(f, "operator"),
            TokenType::Comment => write!(f, "comment"),
            TokenType::Keyword => write!(f, "keyword"),
            TokenType::Delimiter => write!(f, "delimiter"),
            TokenType::NumericValue => write!(f, "numeric-value"),
            TokenType::StringLiteral => write!(f, "string-literal"),
            TokenType::Function => write!(f, "function"),
            TokenType::Identifier => write!(f, "identifier"),
            TokenType::Whitespace => write!(f, "whitespace"),
            TokenType::Other => write!(f, "other")
        }
    }
}


/// TO-DO: Add a `line_number` field to the `Token` struct
#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub token_type: TokenType,
    // Measured in bytes
    pub start:      usize,
    pub end:        usize,
    pub value:      &'a str
}
