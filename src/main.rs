use std::fs;

use clap::{Parser, ValueEnum};

pub mod javascript;
pub mod lexer;
pub mod render;


#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[clap(short = 'l', long, value_enum, help = "Specify the language of the source-code")]
    language: Language,

    #[clap(short = 'n', long, help = "Render line-numbers in the HTML output")]
    line_nums: bool,

    #[clap(short, long, help = "Source file to read")]
    source_file: String,

    #[clap(short,
           long,
           help = "Output file to write to. If not provided, the output will be written to a \
                   file with the same name as the source file, but with the extension \".html\"")]
    output_file: Option<String>
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum Language {
    #[clap(name = "js", alias = "javascript", alias = "ecmascript")]
    JavaScript
}


fn main() {
    let Args { language: lang, line_nums, source_file, output_file: provided_output_file } =
        Args::parse();

    let source_code = fs::read_to_string(&source_file).expect(&format!("Failed to read source \
                                                                        file: {source_file}"));

    let html = match lang {
        Language::JavaScript =>
            if line_nums {
                render::htmlify_with_line_nums(&source_code, javascript::tokenize)
            } else {
                render::htmlify(&source_code, javascript::tokenize)
            },
    };

    let output_file = match provided_output_file {
        Some(path) => path,
        None => {
            let path = std::path::Path::new(&source_file);
            let output = path.with_extension("html");
            output.to_string_lossy().into_owned()
        }
    };

    fs::write(&output_file, &html).expect(&format!("Failed to write to output file: \
                                                    {output_file}"));
    println!("Output written to: {output_file}");
}
