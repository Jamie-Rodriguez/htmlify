use std::collections::HashSet;
use std::sync::LazyLock;

use phf::{Set, phf_set};
use regex::Regex;

use crate::lexer::{Token, TokenType, Trie};
#[cfg(test)]
use crate::render::{
    PresentationTokenType, RenderToken, RenderTokenType, add_line_nums, decimate, to_html,
    tokenize_html
};


const KEYWORD_LIST: [&'static str; 46] =
    ["break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete",
     "do", "else", "export", "extends", "false", "finally", "for", "function", "if", "import",
     "in", "instanceof", "new", "null", "return", "super", "switch", "this", "throw", "true",
     "try", "typeof", "var", "void", "while", "with", "let", "static", "yield", "await", "enum",
     "implements", "interface", "package", "private", "protected", "public"];

// Technically the comma (,) is an operator, but also a separator depending on
// the context -  excluded from operators and instead classifying it as a
// separator. See
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_operators#comma_operator
//
// `delete`, `typeof` and `void` are also (unary) operators, but they are being
// classified as keywords
//
// Same for `in` and `instanceof`
const OPERATOR_LIST: [&'static str; 49] =
    ["=", "+=", "-=", "*=", "/=", "%=", "**=", "<<=", ">>=", ">>>=", "&=", "^=", "|=", "&&=",
     "||=", "??=", "==", "!=", "===", "!==", ">", ">=", "<", "<=", "%", "++", "--", "-", "+", "/",
     "*", "**", "&", "|", "^", "~", "<<", ">>", ">>>", "&&", "||", "??", "!", "?", ":", "?.",
     "...", ",", "=>"];

const DELIMITER_LIST: [&'static str; 8] = ["(", ")", "{", "}", "[", "]", ".", ";"];

const COMMENT_DELIMITER_LIST: [&'static str; 2] = ["/*", "//"];

const STRING_DELIMITER_LIST: [&'static str; 3] = ["\"", "'", "`"];

static KEYWORD_SET: Set<&'static str> = phf_set! {
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceof",
    "new",
    "null",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "let",
    "static",
    "yield",
    "await",
    "enum",
    "implements",
    "interface",
    "package",
    "private",
    "protected",
    "public"
};

static KEYWORD_TRIE: LazyLock<Trie> = LazyLock::new(|| {
    let mut root = Trie::default();

    for &keyword in KEYWORD_LIST.iter() {
        root.insert(keyword);
    }

    root
});

static OPERATOR_TRIE: LazyLock<Trie> = LazyLock::new(|| {
    let mut root = Trie::default();

    for &operator in OPERATOR_LIST.iter() {
        root.insert(operator);
    }

    root
});

static DELIMITER_TRIE: LazyLock<Trie> = LazyLock::new(|| {
    let mut root = Trie::default();

    for &delimiter in DELIMITER_LIST.iter() {
        root.insert(delimiter);
    }

    root
});

static COMMENT_DELIMITERS: phf::Map<&'static str, &'static str> = phf::phf_map! {
    "//" => "\n",
    "/*" => "*/"
};

static COMMENT_DELIMITER_TRIE: LazyLock<Trie> = LazyLock::new(|| {
    let mut root = Trie::default();

    for &delimiter in COMMENT_DELIMITER_LIST.iter() {
        root.insert(delimiter);
    }

    root
});

static STRING_DELIMITERS: phf::Map<&'static str, &'static str> = phf::phf_map! {
    "\"" => "\"",
    "'" => "'",
    "`" => "`"
};

static STRING_DELIMITER_TRIE: LazyLock<Trie> = LazyLock::new(|| {
    let mut root = Trie::default();

    for &delimiter in STRING_DELIMITER_LIST.iter() {
        root.insert(delimiter);
    }

    root
});


fn find_delimited_end(text: &str,
                      start: usize,
                      opening: &str,
                      delimiters: &phf::Map<&str, &str>)
                      -> usize {
    let closing = delimiters.get(opening).expect("Invalid delimiter");

    text[start + opening.len() ..].find(closing)
                                  .map(|position| start + opening.len() + position + closing.len())
                                  .unwrap_or(text.len())
}


pub fn tokenize(source_code: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::with_capacity(source_code.len());
    let mut i: usize = 0;

    while i < source_code.len() {
        let start = i;

        // Get current character (handling multi-byte chars correctly)
        let current_char = source_code[i ..].chars().next().expect("Error parsing str to char");

        // Comments
        if let Some(delimiter) = COMMENT_DELIMITER_TRIE.find_longest_word(&source_code[i ..]) {
            let end = find_delimited_end(source_code, i, delimiter, &COMMENT_DELIMITERS);

            tokens.push(Token { token_type: TokenType::Comment,
                                start: i,
                                end,
                                value: &source_code[i .. end] });

            i = end;
            continue;
        }

        // Strings
        if let Some(delimiter) = STRING_DELIMITER_TRIE.find_longest_word(&source_code[i ..]) {
            let end = find_delimited_end(source_code, i, delimiter, &STRING_DELIMITERS);

            tokens.push(Token { token_type: TokenType::StringLiteral,
                                start: i,
                                end,
                                value: &source_code[i .. end] });

            i = end;
            continue;
        }

        // Whitespace
        if current_char.is_whitespace() {
            let mut end = i;

            while end < source_code.len() {
                let next_char =
                    source_code[end ..].chars().next().expect("Error parsing str to char");
                if !next_char.is_whitespace() {
                    break;
                }
                end += next_char.len_utf8();
            }

            tokens.push(Token { token_type: TokenType::Whitespace,
                                start,
                                end,
                                value: &source_code[start .. end] });

            i = end;
            continue;
        }

        // Identifiers
        if let Some(identifier) =
            Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap().find(&source_code[i ..])
        {
            let potential_identifier = &source_code[i .. i + identifier.end()];

            if !KEYWORD_SET.contains(potential_identifier) {
                tokens.push(Token { token_type: TokenType::Identifier,
                                    start:      i,
                                    end:        i + identifier.end(),
                                    value:      potential_identifier });

                i += identifier.end();
                continue;
            }
        }

        // Keywords
        if let Some(word) = KEYWORD_TRIE.find_longest_word(&source_code[i ..]) {
            tokens.push(Token { token_type: TokenType::Keyword,
                                start,
                                end: start + word.len(),
                                value: word });

            i += word.len();
            continue;
        }

        // Numeric values
        if let Some(numeric_string) =
            Regex::new(r"^(\.[0-9]+|[0-9]+(\.[0-9]*)?)").unwrap().find(&source_code[i ..])
        {
            tokens.push(Token { token_type: TokenType::NumericValue,
                                start,
                                end: start + numeric_string.end(),
                                value: &source_code[start .. start + numeric_string.end()] });

            i += numeric_string.end();
            continue;
        }

        // Operators
        if let Some(word) = OPERATOR_TRIE.find_longest_word(&source_code[i ..]) {
            tokens.push(Token { token_type: TokenType::Operator,
                                start,
                                end: start + word.len(),
                                value: word });

            i += word.len();
            continue;
        }

        // Delimiters
        if let Some(word) = DELIMITER_TRIE.find_longest_word(&source_code[i ..]) {
            tokens.push(Token { token_type: TokenType::Delimiter,
                                start,
                                end: start + word.len(),
                                value: word });

            i += word.len();
            continue;
        }

        // Unknown token - should we consume a single character?
        tokens.push(Token { token_type: TokenType::Other,
                            start:      i,
                            end:        i + current_char.len_utf8(),
                            value:      &source_code[i .. i + current_char.len_utf8()] });

        i += current_char.len_utf8();
    }

    // Post-process tokens to identify functions
    // There are other ways that functions can be defined in JavaScript, maybe
    // later...
    let mut found_functions: HashSet<&str> = HashSet::new();
    i = 0;
    while i < tokens.len() {
        // Defined by "function" keyword
        if tokens[i].token_type == TokenType::Keyword
           && tokens[i].value == "function"
           && tokens.get(i + 2).map_or(false, |token| token.token_type == TokenType::Identifier)
        {
            found_functions.insert(tokens[i + 2].value);
            tokens[i + 2].token_type = TokenType::Function;
        } else if tokens[i].token_type == TokenType::Identifier
                  && (tokens.get(i + 1).map_or(false, |t| t.value == "(")
                      || (tokens.get(i + 1)
                                .map_or(false, |t| t.token_type == TokenType::Whitespace)
                          && tokens.get(i + 2).map_or(false, |t| t.value == "(")))
        {
            // Function invocation
            found_functions.insert(tokens[i].value);
            tokens[i].token_type = TokenType::Function;
        } else if tokens[i].value == "=>" {
            let preceding_tokens = &tokens[.. i];

            // First find the parentheses immediately before the arrow
            let found_paren = if !preceding_tokens.is_empty() {
                let last_idx = preceding_tokens.len() - 1;

                if preceding_tokens[last_idx].value == ")" {
                    // Immediate previous token is paren
                    Some(0)
                } else if preceding_tokens.len() >= 2
                          && preceding_tokens[last_idx].token_type == TokenType::Whitespace
                          && preceding_tokens[last_idx - 1].value == ")"
                {
                    // Previous token is whitespace and token before that is paren
                    Some(1)
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(closing_paren_idx) = found_paren {
                let mut parens_stack = Vec::<&str>::with_capacity(preceding_tokens.len());
                let mut idx = preceding_tokens.len() - 1 - closing_paren_idx;

                // Start with the closing parenthesis we found
                parens_stack.push(")");

                while idx > 0 {
                    idx -= 1;
                    match preceding_tokens[idx].value {
                        "(" | "[" => {
                            if let Some(top) = parens_stack.last() {
                                if (*top == ")" && preceding_tokens[idx].value == "(")
                                   || (*top == "]" && preceding_tokens[idx].value == "[")
                                {
                                    parens_stack.pop();
                                    if parens_stack.is_empty() {
                                        // Found matching parenthesis - now look for equals sign

                                        idx -= 1;
                                        let maybe_equals_idx =
                                            if preceding_tokens.get(idx).is_some_and(|token| {
                                                                            token.token_type
                                                                            == TokenType::Operator
                                                                            && token.value == "="
                                                                        })
                                            {
                                                Some(idx)
                                            } else if preceding_tokens.get(idx)
                                                                      .is_some_and(|token| {
                                                                          token.token_type
                                                                          == TokenType::Whitespace
                                                                      })
                                                      && preceding_tokens.get(idx - 1)
                                                                         .is_some_and(|token| {
                                                                             token.token_type
                                                                             == TokenType::Operator
                                                                             && token.value == "="
                                                                         })
                                            {
                                                Some(idx - 1)
                                            } else {
                                                None
                                            };

                                        // Finally, we can search for the next identifier, which
                                        // should be a function!!
                                        if let Some(equals_idx) = maybe_equals_idx {
                                            idx = equals_idx - 1;

                                            let maybe_func_idx = if preceding_tokens
                                                .get(idx)
                                                .is_some_and(|token| {
                                                    token.token_type == TokenType::Identifier
                                                }) {
                                                Some(idx)
                                            } else if preceding_tokens.get(idx).is_some_and(
                                                |token| token.token_type == TokenType::Whitespace,
                                            ) && preceding_tokens
                                                .get(idx - 1)
                                                .is_some_and(|token| {
                                                    token.token_type == TokenType::Identifier
                                                })
                                            {
                                                Some(idx - 1)
                                            } else {
                                                None
                                            };

                                            // TokenType::Identifier found - set to
                                            // TokenType::Function
                                            if let Some(func_idx) = maybe_func_idx {
                                                found_functions
                                                    .insert(preceding_tokens[func_idx].value);
                                                tokens[func_idx].token_type = TokenType::Function;
                                            }
                                        }

                                        break;
                                    }
                                }
                            }
                        }
                        ")" | "]" => parens_stack.push(preceding_tokens[idx].value),
                        _ => continue
                    }
                }
            } else {
                // No parentheses found, check for single parameter without parentheses
                let maybe_param_index = if !preceding_tokens.is_empty() {
                    let token_index = preceding_tokens.len() - 1;

                    if preceding_tokens[token_index].token_type == TokenType::Identifier {
                        Some(token_index)
                    } else if preceding_tokens.len() >= 2
                              && preceding_tokens[token_index].token_type == TokenType::Whitespace
                              && preceding_tokens[token_index - 1].token_type
                                 == TokenType::Identifier
                    {
                        Some(token_index - 1)
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some(param_index) = maybe_param_index {
                    // Found a parameter identifier, now look for equals sign
                    let mut index = param_index;

                    // Skip backward to find equals sign
                    while index > 0 {
                        index -= 1;

                        if preceding_tokens[index].token_type == TokenType::Whitespace {
                            continue;
                        }

                        if preceding_tokens[index].token_type == TokenType::Operator
                           && preceding_tokens[index].value == "="
                        {
                            // Found equals sign, now look for function name
                            index -= 1;

                            // Skip any whitespace before equals sign
                            while index > 0
                                  && preceding_tokens[index].token_type == TokenType::Whitespace
                            {
                                index -= 1;
                            }

                            // If we found an identifier before the equals, that's the function name
                            if preceding_tokens[index].token_type == TokenType::Identifier {
                                found_functions.insert(preceding_tokens[index].value);
                                tokens[index].token_type = TokenType::Function;
                            }

                            break;
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        i += 1;
    }

    // Have to post-process once more for the case that a function is defined
    // *after* it is invoked Known as *hoisting* in JavaScript
    for token in &mut tokens {
        if found_functions.contains(token.value) && token.token_type == TokenType::Identifier {
            token.token_type = TokenType::Function;
        }
    }

    tokens
}


#[cfg(test)]
mod tests {
    use prop::string::string_regex;
    use proptest::prelude::*;

    use super::*;


    static OPERATOR_SET: Set<&'static str> = phf_set! {
        "=",
        "+=",
        "-=",
        "*=",
        "/=",
        "%=",
        "**=",
        "<<=",
        ">>=",
        ">>>=",
        "&=",
        "^=",
        "|=",
        "&&=",
        "||=",
        "??=",
        "==",
        "!=",
        "===",
        "!==",
        ">",
        ">=",
        "<",
        "<=",
        "%",
        "++",
        "--",
        "-",
        "+",
        "/",
        "*",
        "**",
        "&",
        "|",
        "^",
        "~",
        "<<",
        ">>",
        ">>>",
        "&&",
        "||",
        "??",
        "!",
        "?",
        ":",
        "?.",
        "...",
        ",",
        "=>"
    };

    static DELIMITER_SET: Set<&'static str> = phf_set! {
        "(",
        ")",
        "{",
        "}",
        "[",
        "]",
        ".",
        ";"
    };

    #[derive(Debug)]
    enum FunctionDecoration {
        Keyword,    // "function" before
        Arrow,      // "=>" after
        Invocation  // "(" after
    }


    prop_compose! {
        fn arbitrary_token_type()(i in 0usize..16) -> TokenType {
            match i {
                0 => TokenType::Operator,
                1 => TokenType::Comment,
                2 => TokenType::Keyword,
                3 => TokenType::Delimiter,
                4 => TokenType::NumericValue,
                5 => TokenType::StringLiteral,
                6 => TokenType::Function,
                7 => TokenType::Identifier,
                8..=14 => TokenType::Whitespace,
                _ => TokenType::Other,
            }
        }
    }

    fn set_to_keyword_regex(s: &Set<&str>) -> String {
        s.iter().map(|s| *s).collect::<Vec<&str>>().join("|")
    }

    fn set_to_escaped_regex(s: &Set<&str>) -> String {
        s.iter().map(|s| regex::escape(s)).collect::<Vec<String>>().join("|")
    }

    fn arbitrary_token_value(token_type: TokenType) -> impl Strategy<Value = String> {
        match token_type {
            TokenType::Operator => string_regex(&set_to_escaped_regex(&OPERATOR_SET))
                .unwrap()
                .boxed(),
            // Single-line comments *include* the newline (and carriage return) character(s)
            // TokenType::Comment => string_regex("//[^\n]*(\n|\r\n)|/\\*.*\\*/").unwrap(),
            TokenType::Comment => string_regex("//[^\n]*\n|/\\*[^(*/)]*\\*/").unwrap().boxed(),
            TokenType::Keyword => string_regex(&set_to_keyword_regex(&KEYWORD_SET))
                .unwrap()
                .prop_filter(
                    "Don't *explicitly* generate the keyword \"function\"",
                    |k| k != "function",
                )
                .boxed(),
            TokenType::Delimiter => string_regex(&set_to_escaped_regex(&DELIMITER_SET))
                .unwrap()
                .boxed(),
            TokenType::NumericValue => string_regex("(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)")
                .unwrap()
                .boxed(),
            TokenType::StringLiteral => string_regex("\"[^\"]*\"|'[^']*'|`[^`]*`").unwrap().boxed(),
            TokenType::Function => string_regex("[a-zA-Z_][a-zA-Z0-9_]*")
                .unwrap()
                .prop_filter("Function names cannot be keywords!", |s| {
                    !KEYWORD_SET.contains(s)
                })
                .boxed(),
            TokenType::Identifier => string_regex("[a-zA-Z_][a-zA-Z0-9_]*").unwrap().boxed(),
            TokenType::Whitespace => string_regex("[ \t\r\n]+").unwrap().boxed(),
            TokenType::Other => string_regex("#|@").unwrap().boxed(),
        }
    }

    prop_compose! {
        fn arbitrary_token()(
            token_type in arbitrary_token_type()
        )(
            value in arbitrary_token_value(token_type.clone()),
            start in 0usize..10000,
            token_type in Just(token_type)
        ) -> Token<'static> {
            let end = start + value.len();

            // Because we can't specify excluding words via Regex
            // (unsupported), correct token_type here
            Token {
                token_type: if KEYWORD_SET.contains(&value) { TokenType::Keyword } else { token_type },
                start,
                end,
                value: Box::leak(value.into_boxed_str()), // Convert to 'static str
            }
        }
    }

    prop_compose! {
        fn arbitrary_function_decoration()(choice in 0..=2) -> FunctionDecoration {
            match choice {
                0 => FunctionDecoration::Keyword,
                1 => FunctionDecoration::Arrow,
                _ => FunctionDecoration::Invocation,
            }
        }
    }

    prop_compose! {
        // TO-DO: Clean this up!
        fn arbitrary_token_vec(min_tokens: usize, max_tokens: usize)(
            mut tokens in proptest::collection::vec(arbitrary_token(), min_tokens..=max_tokens),
            function_decorations in proptest::collection::vec(arbitrary_function_decoration(), max_tokens..=max_tokens)
        ) -> Vec<Token<'static>> {
            // Sort tokens by start position and adjust positions to be sequential
            tokens.sort_by_key(|t| t.start);

            // ========== Ambiguous scenarios ==========
            fn ambiguous_token_sequence(tokens: &Vec<Token<'static>>, i: usize) -> bool {
                /*
                  If we see neighbouring Operator tokens, remove one.
                  Have to remove one rather generate and insert a new random,
                  non-Operator token in between because the generator doesn't allow
                  for generating new tokens after the fact
                */
                if i + 1 < tokens.len() && tokens[i].token_type == TokenType::Operator && tokens[i + 1].token_type == TokenType::Operator {
                    return true;
                }

                // "..." ambiguity: two "." Delimiters + NumericValue starting with "."
                if i + 2 < tokens.len() &&
                   tokens[i].token_type == TokenType::Delimiter && tokens[i].value == "." &&
                   tokens[i + 1].token_type == TokenType::Delimiter && tokens[i + 1].value == "." &&
                   tokens[i + 2].token_type == TokenType::NumericValue && tokens[i + 2].value.starts_with(".") {
                    return true;
                }

                // "." Delimiter next to a NumericValue
                if i + 1 < tokens.len() && (
                   (tokens[i].token_type == TokenType::Delimiter && tokens[i].value == "." &&
                   tokens[i + 1].token_type == TokenType::NumericValue) ||
                   (tokens[i].token_type == TokenType::NumericValue && !tokens[i].value.contains(".") &&
                   tokens[i + 1].token_type == TokenType::Delimiter && tokens[i + 1].value == ".")
                ) {
                    return true;
                }

                // "." Delimiter next to a "..." Operator
                /*
                  Source code "...." is ambiguous - can be correctly interpreted as
                  either [".", "..."] or ["...", "."].
                  As by design, the lexer greedily consumes characters,
                  it will *always* interpret "...." as ["...", "."].
                  Because of this, we remove the scenario of generating [".", "..."]
                */
                if i + 1 < tokens.len() &&
                   tokens[i].token_type == TokenType::Delimiter && tokens[i].value == "." &&
                   tokens[i + 1].token_type == TokenType::Operator && tokens[i + 1].value == "..." {
                    return true;
                }

                // NumericValue next to a "..." Operator
                /*
                  E.g.
                  Input
                      [{ token_type: NumericValue, start: 0, end: 1, value: "0" },
                       { token_type: Operator,     start: 1, end: 4, value: "..." }]
                  Gets parsed as
                      [{ token_type: NumericValue, start: 0, end: 2, value: "0." },
                       { token_type: Delimiter,    start: 2, end: 3, value: "." },
                       { token_type: Delimiter,    start: 3, end: 4, value: "." }]
                */
                if i + 1 < tokens.len() &&
                   tokens[i].token_type == TokenType::NumericValue && !tokens[i].value.ends_with(".") &&
                   tokens[i + 1].token_type == TokenType::Operator && tokens[i + 1].value == "..." {
                    return true;
                }

                // "?" Operator next to a NumericValue starting with "."
                if i + 1 < tokens.len() &&
                   tokens[i].value == "?" &&
                   tokens[i + 1].token_type == TokenType::NumericValue && tokens[i + 1].value.starts_with(".") {
                    return true;
                }

                // Keyword or Identifier or Function followed by a NumericValue
                if i + 1 < tokens.len() &&
                   (tokens[i].token_type == TokenType::Keyword || tokens[i].token_type == TokenType::Identifier || tokens[i].token_type == TokenType::Function ) &&
                   (tokens[i + 1].token_type == TokenType::NumericValue) {
                    return true;
                }

                false
            }

            // ========== Combining Operators ==========
            let mut i = 0;
            while i < tokens.len() - 1 {
                // In case tokens.len() < 4...
                let max_length = 4.min(tokens.len() - i);
                let mut combined_length = 0;

                // Try possible combinations from longest to shortest
                for length in (2..=max_length).rev() {
                    if i + length <= tokens.len() {
                        let combined = tokens[i..i + length]
                            .iter()
                            .map(|t| t.value)
                            .collect::<String>();

                        if OPERATOR_SET.contains(combined.as_str()) {
                            tokens[i].token_type = TokenType::Operator;
                            tokens[i].value = Box::leak(combined.into_boxed_str());
                            tokens[i].end = tokens[i + length - 1].end;

                            for _ in 1..length {
                                tokens.remove(i + 1);
                            }

                            combined_length = length;
                            break;
                        }
                    }
                }

                if combined_length > 0 {
                    continue;
                } else if tokens[i].token_type == TokenType::Operator && tokens[i].value == "/" && i + 1 < tokens.len() && (tokens[i + 1].value.starts_with("/") || tokens[i + 1].value.starts_with("*")) {
                    /*
                      Sometimes the `/` `Operator` precedes the `//` or `/* */` `Comment`s,
                      or even another `/` `Operator`!
                      Solution: Remove `/` token
                    */
                    tokens.remove(i);
                } else {
                    i += 1;
                }
            }

            // ==========  Function declarations ==========
            i = 0;
            let mut decoration_iter = function_decorations.iter();
            // Insert relevant surrounding tokens to correctly declare function tokens
            while i < tokens.len() {
                if tokens[i].token_type == TokenType::Function {
                    match decoration_iter.next().unwrap() {
                        FunctionDecoration::Keyword => {
                            let position = tokens[i].start;

                            tokens.insert(i, Token {
                                token_type: TokenType::Keyword,
                                start: position,
                                end: position + "function".len(),
                                value: "function"
                            });
                            tokens.insert(i + 1, Token {
                                token_type: TokenType::Whitespace,
                                start: position + "function".len(),
                                end: position + "function".len() + 1,
                                value: " "
                            });

                            i += 3;
                        },
                        FunctionDecoration::Arrow => {
                            let position = tokens[i].end;

                            tokens.insert(i + 1, Token {
                                token_type: TokenType::Operator,
                                start: position,
                                end: position + "=".len(),
                                value: "="
                            });
                            tokens.insert(i + 2, Token {
                                token_type: TokenType::Delimiter,
                                start: position + "=".len(),
                                end: position + "=".len() + "(".len(),
                                value: "("
                            });
                            tokens.insert(i + 3, Token {
                                token_type: TokenType::Delimiter,
                                start: position + "=".len() + "(".len(),
                                end: position + "=".len() + "(".len() + ")".len(),
                                value: ")"
                            });
                            tokens.insert(i + 4, Token {
                                token_type: TokenType::Operator,
                                start: position + "=".len() + "(".len() + ")".len(),
                                end: position + "=".len() + "(".len() + ")".len() + "=>".len(),
                                value: "=>"
                            });

                            i += 5;
                        },
                        FunctionDecoration::Invocation => {
                            let position = tokens[i].end;

                            tokens.insert(i + 1, Token {
                                token_type: TokenType::Delimiter,
                                start: position,
                                end: position + "(".len(),
                                value: "("
                            });

                            i += 2;
                        }
                    }
                } else {
                    i += 1;
                }
            }

            // Merge neighbouring Whitespace tokens
            i = 0;
            while i < tokens.len() - 1 {
                if tokens[i].token_type == TokenType::Whitespace &&
                   tokens[i + 1].token_type == TokenType::Whitespace {
                    tokens[i].value = Box::leak((tokens[i].value.to_owned() + tokens[i + 1].value).into_boxed_str());
                    tokens[i].end = tokens[i + 1].end;
                    tokens.remove(i + 1);
                } else {
                    i += 1;
                }
            }

            // There is a case that an `Identifier` token is followed by a
            // "(" `Delimiter` token, which should be a `Function` token
            for position in 0..tokens.len() - 1 {
                if tokens[position].token_type == TokenType::Identifier &&
                    (tokens[position + 1].value == "(" ||
                     (tokens[position + 1].token_type == TokenType::Whitespace && tokens.get(position + 2).map_or(false, |t| t.value == "("))) {
                    tokens[position].token_type = TokenType::Function;
                }
            }

            // Fix up any Identifiers that have already been marked as Functions
            let function_names: HashSet<&str> = tokens.iter().filter_map(|t| {
                match t.token_type {
                    TokenType::Function => Some(t.value),
                    _ => None
                }
            }).collect();

            for token in tokens.iter_mut() {
                if token.token_type == TokenType::Identifier && function_names.contains(token.value)  {
                    token.token_type = TokenType::Function;
                }
            }

            // Insert whitespace between invalid token sequences
            i = 0;
            while i + 1 < tokens.len() {
                if matches!(tokens[i].token_type,
                        TokenType::Keyword |
                        TokenType::Identifier |
                        TokenType::NumericValue |
                        TokenType::Function) &&
                matches!(tokens[i + 1].token_type,
                        TokenType::Keyword |
                        TokenType::Identifier |
                        TokenType::NumericValue |
                        TokenType::Function) {
                    let position = tokens[i].end;

                    tokens.insert(i + 1, Token {
                        token_type: TokenType::Whitespace,
                        start: position,
                        end: position + " ".len(),
                        value: " "
                    });

                    i += 2;
                } else {
                    i += 1;
                }
            }

            i = 0;
            while i < tokens.len() {
                if ambiguous_token_sequence(&tokens, i) {
                    let position = tokens[i].end;
                    tokens.insert(i + 1, Token {
                        token_type: TokenType::Whitespace,
                        start: position,
                        end: position + " ".len(),
                        value: " "
                    });
                    i += 2;
                } else {
                    i += 1;
                }
            }

            // Fix up the start and end positions
            let mut current_pos = 0;
            for token in tokens.iter_mut() {
                token.start = current_pos;
                current_pos += token.value.len();
                token.end = current_pos;
            }

            tokens
        }
    }

    fn create_source_from_tokens(tokens: &Vec<Token>) -> String {
        tokens.iter().map(|t| t.value).collect::<String>()
    }


    proptest! {
    #![proptest_config(ProptestConfig {
        cases: 10000, verbose: 0, .. ProptestConfig::default()
    })]

    #[test]
    fn test_source_creation(generated_tokens in arbitrary_token_vec(1, 30)) {
            let source_code = create_source_from_tokens(&generated_tokens);
            let parsed_tokens = tokenize(&source_code);

            prop_assert_eq!(generated_tokens.len(), parsed_tokens.len());

            for (generated, parsed) in generated_tokens.iter().zip(parsed_tokens.iter()) {
                prop_assert_eq!(generated.token_type, parsed.token_type);
                prop_assert_eq!(generated.start, parsed.start);
                prop_assert_eq!(generated.end, parsed.end);
                prop_assert_eq!(generated.value, parsed.value);
            };
        }
    }

    proptest! {
            #![proptest_config(ProptestConfig {
                verbose: 2, .. ProptestConfig::default()
            })]

        #[test]
        fn test_decimate(generated_tokens in arbitrary_token_vec(1, 30)) {
            let lines: usize = generated_tokens.iter().map(|token| if token.value.ends_with('\n') { token.value.matches('\n').count() - 1 } else { token.value.matches('\n').count() }).sum();

            let decimated_tokens = decimate(&generated_tokens);

            prop_assert_eq!(decimated_tokens.len(), generated_tokens.len() + lines);
        }

        #[test]
        fn test_add_line_nums(generated_tokens in arbitrary_token_vec(1, 30)) {
            let lines: usize = generated_tokens.iter().map(|token| token.value.matches('\n').count()).sum::<usize>() + 1;

            let decimated_tokens = decimate(&generated_tokens);
            let rendered_tokens = add_line_nums(&decimated_tokens);

            let line_num_tokens: Vec<&RenderToken> = rendered_tokens.iter().filter(|token| matches!(token.token_type, RenderTokenType::Presentation(PresentationTokenType::LineNumber))).collect();
            let spacer_tokens: Vec<&RenderToken> = rendered_tokens.iter().filter(|token| matches!(token.token_type, RenderTokenType::Presentation(PresentationTokenType::LineNumberSpacer))).collect();

            // Calculate how many spacer tokens are needed
            let max_num_digits = (lines as f64).log10().floor() as usize + 1;
            let mut num_alignment_spacers = 0;

            for d in 1..max_num_digits {
                // How many numbers exist with exactly d digits
                let num_with_d_digits = 9 * 10_usize.pow((d - 1) as u32);
                // Each of these needs (max_digits - d) alignment spaces
                num_alignment_spacers += num_with_d_digits * (max_num_digits - d);
            }

            prop_assert_eq!(line_num_tokens.len(), lines);
            prop_assert_eq!(spacer_tokens.len(), lines + num_alignment_spacers);
            let mut line_num = 1;
            for token in line_num_tokens {
                prop_assert_eq!(token.value, line_num.to_string());
                line_num += 1;
            }
        }
    }

    proptest! {
    #![proptest_config(ProptestConfig {
        cases: 10000, verbose: 0, .. ProptestConfig::default()
    })]

    #[test]
    fn test_to_html(generated_tokens in arbitrary_token_vec(1, 30)) {
            let decimated_tokens = decimate(&generated_tokens);
            let rendered_tokens = add_line_nums(&decimated_tokens);
            let html = to_html(&rendered_tokens);

            let retokenised_html = tokenize_html(&html);

            // Newlines are rendered "natively" in the resulting HTML string,
            // (i.e. outside of the HTML tags) and so won't be present when
            // retokenised
            let rendered_tokens_sans_newlines = rendered_tokens.iter().filter(|token| token.value != "\n").collect::<Vec<&RenderToken>>();

            prop_assert_eq!(rendered_tokens_sans_newlines.len(), retokenised_html.len());

            for (rendered, retokenised) in rendered_tokens_sans_newlines.iter().zip(retokenised_html.iter()) {
                prop_assert_eq!(&rendered.token_type, &retokenised.token_type);
                prop_assert_eq!(rendered.value
                                    .replace("\n", "")
                                    .replace('&', "&amp;")
                                    .replace('<', "&lt;")
                                    .replace('>', "&gt;"),
                                retokenised.value);
            };
        }
    }

    #[test]
    fn test_tokenize_whitespace() {
        let input = "   \n\t  ";
        let tokens = tokenize(input);

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, TokenType::Whitespace);
        assert_eq!(tokens[0].value, input);
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, input.len());
    }


    #[test]
    fn test_tokenize_mixed_whitespace() {
        let input = " : \n \t .>>";
        let tokens = tokenize(input);
        let reconstructed: String = tokens.iter().map(|token| token.value).collect();

        assert_eq!(tokens.len(), 5);
        assert_eq!(reconstructed, input);
        assert_eq!(tokens[0].token_type, TokenType::Whitespace);
        assert_eq!(tokens[1].token_type, TokenType::Operator);
        assert_eq!(tokens[2].token_type, TokenType::Whitespace);
        assert_eq!(tokens[3].token_type, TokenType::Delimiter);
        assert_eq!(tokens[4].token_type, TokenType::Operator);
    }

    #[test]
    fn test_tokenize_single_line_comment() {
        let input = "let // This is a comment\n >>";
        let tokens = tokenize(input);

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].token_type, TokenType::Keyword);
        assert_eq!(tokens[0].value, "let");
        assert_eq!(tokens[1].token_type, TokenType::Whitespace);
        assert_eq!(tokens[1].value, " ");
        assert_eq!(tokens[2].token_type, TokenType::Comment);
        assert_eq!(tokens[2].value, "// This is a comment\n");
        assert_eq!(tokens[3].token_type, TokenType::Whitespace);
        assert_eq!(tokens[3].value, " ");
        assert_eq!(tokens[4].token_type, TokenType::Operator);
        assert_eq!(tokens[4].value, ">>");
    }

    #[test]
    fn test_tokenize_multi_line_comment() {
        let input = "const /* This is a\nmulti-line comment */ =";
        let tokens = tokenize(input);

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].token_type, TokenType::Keyword);
        assert_eq!(tokens[0].value, "const");
        assert_eq!(tokens[1].token_type, TokenType::Whitespace);
        assert_eq!(tokens[1].value, " ");
        assert_eq!(tokens[2].token_type, TokenType::Comment);
        assert_eq!(tokens[2].value, "/* This is a\nmulti-line comment */");
        assert_eq!(tokens[3].token_type, TokenType::Whitespace);
        assert_eq!(tokens[3].value, " ");
        assert_eq!(tokens[4].token_type, TokenType::Operator);
        assert_eq!(tokens[4].value, "=");
    }

    #[test]
    fn test_tokenize_string_literals() {
        let input = r#"let x = "hello" + 'world' + `template`;"#;
        let tokens = tokenize(input);

        assert_eq!(tokens.len(), 16);
        assert_eq!(tokens[0].token_type, TokenType::Keyword);
        assert_eq!(tokens[0].value, "let");
        assert_eq!(tokens[1].token_type, TokenType::Whitespace);
        assert_eq!(tokens[1].value, " ");
        assert_eq!(tokens[2].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].value, "x");
        assert_eq!(tokens[3].token_type, TokenType::Whitespace);
        assert_eq!(tokens[3].value, " ");
        assert_eq!(tokens[4].token_type, TokenType::Operator);
        assert_eq!(tokens[4].value, "=");
        assert_eq!(tokens[5].token_type, TokenType::Whitespace);
        assert_eq!(tokens[5].value, " ");
        assert_eq!(tokens[6].token_type, TokenType::StringLiteral);
        assert_eq!(tokens[6].value, "\"hello\"");
        assert_eq!(tokens[7].token_type, TokenType::Whitespace);
        assert_eq!(tokens[7].value, " ");
        assert_eq!(tokens[8].token_type, TokenType::Operator);
        assert_eq!(tokens[8].value, "+");
        assert_eq!(tokens[9].token_type, TokenType::Whitespace);
        assert_eq!(tokens[9].value, " ");
        assert_eq!(tokens[10].token_type, TokenType::StringLiteral);
        assert_eq!(tokens[10].value, "'world'");
        assert_eq!(tokens[11].token_type, TokenType::Whitespace);
        assert_eq!(tokens[11].value, " ");
        assert_eq!(tokens[12].token_type, TokenType::Operator);
        assert_eq!(tokens[12].value, "+");
        assert_eq!(tokens[13].token_type, TokenType::Whitespace);
        assert_eq!(tokens[13].value, " ");
        assert_eq!(tokens[14].token_type, TokenType::StringLiteral);
        assert_eq!(tokens[14].value, "`template`");
        assert_eq!(tokens[15].token_type, TokenType::Delimiter);
        assert_eq!(tokens[15].value, ";");
    }

    #[test]
    fn test_tokenize_unclosed_multi_line_comment() {
        let input = "/* This comment never ends";
        let tokens = tokenize(input);

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, TokenType::Comment);
        assert_eq!(tokens[0].value, input);
    }

    #[test]
    fn test_tokenize_numeric_values() {
        let input = "42 3.14 100 0.1 1. .2";
        let tokens = tokenize(input);

        // assert_eq!(tokens.len(), 9);
        assert_eq!(tokens[0].token_type, TokenType::NumericValue);
        assert_eq!(tokens[0].value, "42");
        assert_eq!(tokens[1].token_type, TokenType::Whitespace);
        assert_eq!(tokens[2].token_type, TokenType::NumericValue);
        assert_eq!(tokens[2].value, "3.14");
        assert_eq!(tokens[3].token_type, TokenType::Whitespace);
        assert_eq!(tokens[4].token_type, TokenType::NumericValue);
        assert_eq!(tokens[4].value, "100");
        assert_eq!(tokens[5].token_type, TokenType::Whitespace);
        assert_eq!(tokens[6].token_type, TokenType::NumericValue);
        assert_eq!(tokens[6].value, "0.1");
        assert_eq!(tokens[7].token_type, TokenType::Whitespace);
        assert_eq!(tokens[8].token_type, TokenType::NumericValue);
        assert_eq!(tokens[8].value, "1.");
        assert_eq!(tokens[9].token_type, TokenType::Whitespace);
        assert_eq!(tokens[10].token_type, TokenType::NumericValue);
        assert_eq!(tokens[10].value, ".2");
    }

    #[test]
    fn test_tokenize_identifiers() {
        let input = "myVar _test test123 $illegal 12.3foo 456bar";
        let tokens = tokenize(input);

        assert_eq!(tokens.len(), 14);
        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].value, "myVar");
        assert_eq!(tokens[1].token_type, TokenType::Whitespace);
        assert_eq!(tokens[2].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].value, "_test");
        assert_eq!(tokens[3].token_type, TokenType::Whitespace);
        assert_eq!(tokens[4].token_type, TokenType::Identifier);
        assert_eq!(tokens[4].value, "test123");
        assert_eq!(tokens[5].token_type, TokenType::Whitespace);
        assert_eq!(tokens[6].token_type, TokenType::Other);
        assert_eq!(tokens[6].value, "$");
        assert_eq!(tokens[7].token_type, TokenType::Identifier);
        assert_eq!(tokens[7].value, "illegal");
        assert_eq!(tokens[8].token_type, TokenType::Whitespace);
        assert_eq!(tokens[9].token_type, TokenType::NumericValue);
        assert_eq!(tokens[9].value, "12.3");
        assert_eq!(tokens[10].token_type, TokenType::Identifier);
        assert_eq!(tokens[10].value, "foo");
        assert_eq!(tokens[11].token_type, TokenType::Whitespace);
        assert_eq!(tokens[12].token_type, TokenType::NumericValue);
        assert_eq!(tokens[12].value, "456");
        assert_eq!(tokens[13].token_type, TokenType::Identifier);
        assert_eq!(tokens[13].value, "bar");
    }

    #[test]
    fn test_tokenize_mixed_numeric_and_identifiers() {
        let input = "count1 + 123 + price99.99";
        let tokens = tokenize(input);

        assert_eq!(tokens.len(), 10);
        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].value, "count1");
        assert_eq!(tokens[1].token_type, TokenType::Whitespace);
        assert_eq!(tokens[2].token_type, TokenType::Operator);
        assert_eq!(tokens[2].value, "+");
        assert_eq!(tokens[3].token_type, TokenType::Whitespace);
        assert_eq!(tokens[4].token_type, TokenType::NumericValue);
        assert_eq!(tokens[4].value, "123");
        assert_eq!(tokens[5].token_type, TokenType::Whitespace);
        assert_eq!(tokens[6].token_type, TokenType::Operator);
        assert_eq!(tokens[6].value, "+");
        assert_eq!(tokens[7].token_type, TokenType::Whitespace);
        assert_eq!(tokens[8].token_type, TokenType::Identifier);
        assert_eq!(tokens[8].value, "price99");
        assert_eq!(tokens[9].token_type, TokenType::NumericValue);
        assert_eq!(tokens[9].value, ".99");
    }

    #[test]
    fn test_y_combinator() {
        let input = "".to_owned()
                    + "// Classical Y-combinator implementation\n"
                    + "const Y = f => (x => x(x))(x => f(y => x(x)(y)));";

        let tokens = tokenize(&input);

        assert_eq!(tokens.len(), 42);

        for token in &tokens {
            match token.value {
                "Y" | "f" | "x" => assert_eq!(token.token_type, TokenType::Function),
                "y" => assert_eq!(token.token_type, TokenType::Identifier),
                "const" => assert_eq!(token.token_type, TokenType::Keyword),
                "=" | "=>" => assert_eq!(token.token_type, TokenType::Operator),
                "(" | ")" | ";" => assert_eq!(token.token_type, TokenType::Delimiter),
                " " => assert_eq!(token.token_type, TokenType::Whitespace),
                s if s.starts_with("//") => assert_eq!(token.token_type, TokenType::Comment),
                _ => {}
            }
        }
    }

    #[test]
    fn test_fibonacci_sequence() {
        let input = "".to_owned()
                    + "/**\n"
                    + " * Basic recursive implementation of the Fibonacci Sequence\n"
                    + " * F[n] = F[n-1] + F[n-2] where F[0] = 0 and F[1] = 1\n"
                    + " */\n"
                    + "function fibonacci(n) {\n"
                    + "    // Base cases\n"
                    + "    if (n === 0) return 0;\n"
                    + "    if (n === 1) return 1;\n"
                    + "\n"
                    + "    // Recursive case directly implements Fn = Fn-1 + Fn-2\n"
                    + "    return fibonacci(n - 1) + fibonacci(n - 2);\n"
                    + "};\n";
        let tokens = tokenize(&input);

        assert_eq!(tokens.len(), 71);
    }
}
