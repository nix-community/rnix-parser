use rnix::parser::ParseError;
use std::{env, fs};

fn main() {
    let file = match env::args().skip(1).next() {
        Some(file) => file,
        None => {
            eprintln!("Usage: error-report <file>");
            return;
        }
    };
    let content = match fs::read_to_string(&file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("error reading file: {}", err);
            return;
        }
    };
    let ast = rnix::parse(&content);
    for error in ast.errors() {
        let range = match error {
            ParseError::Unexpected(range) => range,
            ParseError::UnexpectedExtra(range) => range,
            ParseError::UnexpectedWanted(_, range, _) => range,
            ParseError::UnexpectedDoubleBind(range) => range,
            err => {
                eprintln!("error: {}", err);
                continue;
            }
        };
        eprintln!("----- ERROR -----");
        eprintln!("{}", error);
        let start = range.start().to_usize();
        let start_row = content[..start].lines().count() - 1;
        let start_line = content[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let start_col = content[start_line..start].chars().count();
        let end = range.end().to_usize();
        let end_row = content[..end].lines().count() - 1;
        let end_line = content[..end].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let end_col = content[end_line..end].chars().count();

        let mut line_len = 1;
        let mut line = end_row;
        while line >= 10 {
            line /= 10;
            line_len += 1;
        }

        let i = start_row.saturating_sub(1);
        for (i, line) in content.lines().enumerate().skip(i).take(end_row - i + 1) {
            println!("{:line_len$} {}", i + 1, line, line_len = line_len);
            if i >= start_row && i <= end_row {
                print!("{:line_len$} ", "", line_len = line_len);
                let mut end_col = if i == end_row { end_col } else { line.chars().count() };
                if i == start_row {
                    print!("{:indent$}", "", indent = start_col);
                    end_col -= start_col;
                }
                for _ in 0..end_col {
                    print!("^");
                }
                println!();
            }
        }
    }
}
