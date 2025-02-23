use std::fmt;

use crate::lexer::{Token, TokenType};


#[derive(Debug, PartialEq)]
pub enum PresentationTokenType {
    LineNumberSpacer,
    LineNumber
}

impl fmt::Display for PresentationTokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PresentationTokenType::LineNumberSpacer => write!(f, "line-number-spacer"),
            PresentationTokenType::LineNumber => write!(f, "line-number")
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RenderTokenType {
    Presentation(PresentationTokenType),
    Lexer(TokenType)
}

impl fmt::Display for RenderTokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RenderTokenType::Presentation(presentation_token) => {
                write!(f, "{}", presentation_token)
            }
            RenderTokenType::Lexer(lexer_token) => write!(f, "{}", lexer_token)
        }
    }
}

#[derive(Debug)]
pub struct RenderToken<'a> {
    pub token_type: RenderTokenType,
    pub value:      &'a str
}


#[cfg(test)]
pub fn tokenize_html(xml: &str) -> Vec<RenderToken> {
    let mut tokens = Vec::new();
    let mut i = 0;

    // Process the content by tag pairs
    while i < xml.len() {
        let c = xml[i ..].chars().next().unwrap();

        // Skip whitespace between tags
        if c.is_whitespace() {
            i += c.len_utf8();
            continue;
        }

        // Opening tag - find tag attributes and content
        if c == '<' && i + 1 < xml.len() && xml[i + 1 ..].chars().next().unwrap() != '/' {
            // Find tag name
            let mut tag_end = i + 1;
            while tag_end < xml.len()
                  && !xml[tag_end ..].chars().next().unwrap().is_whitespace()
                  && xml[tag_end ..].chars().next().unwrap() != '>'
            {
                tag_end += xml[tag_end ..].chars().next().unwrap().len_utf8();
            }

            let tag_name = &xml[i + 1 .. tag_end];

            // Extract class attribute
            let mut class_value = None;
            if let Some(class_pos) = xml[tag_end ..].find("class=\"") {
                let start_pos = tag_end + class_pos + 7; // "class=\"" is 7 chars
                if let Some(end_pos) = xml[start_pos ..].find('"') {
                    class_value = Some(&xml[start_pos .. start_pos + end_pos]);
                }
            } else if let Some(class_pos) = xml[tag_end ..].find("class='") {
                let start_pos = tag_end + class_pos + 7; // "class='" is 7 chars
                if let Some(end_pos) = xml[start_pos ..].find('\'') {
                    class_value = Some(&xml[start_pos .. start_pos + end_pos]);
                }
            }

            // Skip to end of opening tag
            let mut content_start = tag_end;
            while content_start < xml.len() {
                if xml[content_start ..].chars().next().unwrap() == '>' {
                    content_start += 1; // Move past '>'
                    break;
                }
                content_start += xml[content_start ..].chars().next().unwrap().len_utf8();
            }

            // Find closing tag
            let closing_tag = format!("</{}>", tag_name);
            let content_end = if let Some(pos) = xml[content_start ..].find(&closing_tag) {
                content_start + pos
            } else {
                // No closing tag found, treat rest as content
                xml.len()
            };

            // Extract content
            let content = &xml[content_start .. content_end];

            // Create token based on class value
            if let Some(class_name) = class_value {
                match class_name {
                    "operator" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::Operator),
                        value: content,
                    }),
                    "comment" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::Comment),
                        value: content,
                    }),
                    "keyword" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::Keyword),
                        value: content,
                    }),
                    "delimiter" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::Delimiter),
                        value: content,
                    }),
                    "numeric-value" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::NumericValue),
                        value: content,
                    }),
                    "string-literal" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::StringLiteral),
                        value: content,
                    }),
                    "function" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::Function),
                        value: content,
                    }),
                    "identifier" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::Identifier),
                        value: content,
                    }),
                    "whitespace" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::Whitespace),
                        value: content,
                    }),
                    "line-number" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Presentation(
                            PresentationTokenType::LineNumber,
                        ),
                        value: content,
                    }),
                    "line-number-spacer" => tokens.push(RenderToken {
                        token_type: RenderTokenType::Presentation(
                            PresentationTokenType::LineNumberSpacer,
                        ),
                        value: content,
                    }),
                    _ => tokens.push(RenderToken {
                        token_type: RenderTokenType::Lexer(TokenType::Other),
                        value: content,
                    }),
                }
            } else {
                // No class attribute found, default to Other
                tokens.push(RenderToken { token_type: RenderTokenType::Lexer(TokenType::Other),
                                          value:      content });
            }

            // Skip past the closing tag
            i = content_end + closing_tag.len();
        } else if c == '<' && i + 1 < xml.len() && xml[i + 1 ..].chars().next().unwrap() == '/' {
            // Skip closing tags (already processed)
            while i < xml.len() && xml[i ..].chars().next().unwrap() != '>' {
                i += xml[i ..].chars().next().unwrap().len_utf8();
            }
            if i < xml.len() {
                i += 1; // Skip '>'
            }
        } else {
            // Skip any text outside of tags
            i += c.len_utf8();
        }
    }

    tokens
}


pub fn decimate<'a>(tokens: &'a Vec<Token<'a>>) -> Vec<Token<'a>> {
    let mut new_tokens: Vec<Token<'a>> = Vec::with_capacity(2 * tokens.len());

    for token in tokens {
        let mut current_position = token.start;
        let mut remaining_slice = token.value;

        while let Some(offset) = remaining_slice.find('\n').map(|w| w + 1) {
            let current_line = &remaining_slice[.. offset];

            new_tokens.push(Token { token_type: token.token_type,
                                    start:      current_position,
                                    end:        current_position + offset,
                                    value:      current_line });

            remaining_slice = &remaining_slice[offset ..];
            current_position += offset;
        }

        if !remaining_slice.is_empty() {
            new_tokens.push(Token { token_type: token.token_type,
                                    start:      current_position,
                                    end:        token.end,
                                    value:      remaining_slice });
        }
    }

    new_tokens
}

/// TO-DO: If we capture the line-number in the token generation, we can use
/// that to determine the line-number spacer size without having to iterate
/// through the tokens again.
///
/// Question: Should we add a line-number offset as a parameter?
/// Assumes that the tokens are already decimated
/// i.e. no token spans multiple lines
pub fn add_line_nums<'a>(tokens: &'a Vec<Token<'a>>) -> Vec<RenderToken<'a>> {
    let mut render_tokens: Vec<RenderToken> = Vec::with_capacity(2 * tokens.len());
    let mut line_num: usize = 1;

    render_tokens.push(RenderToken {
        token_type: RenderTokenType::Presentation(PresentationTokenType::LineNumber),
        value: Box::leak(line_num.to_string().into_boxed_str()),
    });
    render_tokens.push(RenderToken {
        token_type: RenderTokenType::Presentation(PresentationTokenType::LineNumberSpacer),
        value: Box::leak(" ".to_string().into_boxed_str()),
    });

    for token in tokens {
        render_tokens.push(RenderToken { token_type: RenderTokenType::Lexer(token.token_type),
                                         value:      token.value });

        if token.value.contains('\n') {
            line_num += 1;

            render_tokens.push(RenderToken {
                token_type: RenderTokenType::Presentation(PresentationTokenType::LineNumber),
                value: Box::leak(line_num.to_string().into_boxed_str()),
            });
            render_tokens.push(RenderToken {
                token_type: RenderTokenType::Presentation(PresentationTokenType::LineNumberSpacer),
                value: Box::leak(" ".to_string().into_boxed_str()),
            });
        }
    }

    // We could also do (line_num as f64).log10().floor() as usize + 1
    let max_line_num_chars = line_num.to_string().len();

    let mut i = 0;

    while i < render_tokens.len() {
        if render_tokens[i].token_type
           == RenderTokenType::Presentation(PresentationTokenType::LineNumber)
        {
            let num_chars = render_tokens[i].value.to_string().len();
            let num_spaces = max_line_num_chars - num_chars;

            if num_spaces > 0 {
                render_tokens.insert(
                    i,
                    RenderToken {
                        token_type: RenderTokenType::Presentation(
                            PresentationTokenType::LineNumberSpacer,
                        ),
                        value: Box::leak(" ".repeat(num_spaces).into_boxed_str()),
                    },
                );
            }

            i += 2;
        } else {
            i += 1;
        }
    }

    render_tokens
}

pub fn to_html(tokens: &Vec<RenderToken>) -> String {
    let mut html = String::new();

    for token in tokens {
        let class_name = token.token_type.to_string();

        let escaped_value = if token.value.ends_with('\n') {
            token.value[.. token.value.len() - 1].to_string()
        } else {
            token.value.to_string()
        };

        let escaped_content =
            escaped_value.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;");

        if !escaped_content.is_empty() {
            html.push_str(&format!("<span class=\"{}\">{}</span>", class_name, escaped_content));
        }

        if token.value.ends_with('\n') {
            html.push_str("\n");
        }
    }

    html
}

pub fn htmlify_with_line_nums<F>(source_code: &str, tokenize: F) -> String
    where F: Fn(&str) -> Vec<Token> {
    let tokens = tokenize(source_code);
    let decimated_tokens = decimate(&tokens);

    let rendered_tokens = add_line_nums(&decimated_tokens);

    to_html(&rendered_tokens)
}

pub fn htmlify<F>(source_code: &str, tokenize: F) -> String
    where F: Fn(&str) -> Vec<Token> {
    let tokens = tokenize(source_code);
    let decimated_tokens = decimate(&tokens);

    let rendered_tokens =
        decimated_tokens.iter()
                        .map(|token| RenderToken { token_type:
                                                       RenderTokenType::Lexer(token.token_type),
                                                   value:      token.value })
                        .collect();

    to_html(&rendered_tokens)
}
