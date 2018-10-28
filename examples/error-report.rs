extern crate rnix;

use rnix::Error as NixError;
use std::{env, fs, io::{self, Write}};

fn main() {
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

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
    let errors = match rnix::parse(&content) {
        Ok(ast) => {
            ast.errors()
                .map(|node| {
                    let (span, err) = node.error();
                    (*span, err.to_string())
                })
                .collect()
        },
        Err(err) => match err {
            NixError::TokenizeError(span, err) => vec![(Some(span), err.to_string())],
            NixError::ParseError(span, err) => vec![(span, err.to_string())],
        }
    };

    for (i, (span, err)) in errors.iter().enumerate() {
        if i > 0 {
            writeln!(stdout).unwrap();
        }
        writeln!(stdout, "error: {}", err).unwrap();

        let span = match span {
            Some(span) => span,
            None => continue
        };

        let start = span.start as usize;
        let end = span.end.map(|i| i as usize).unwrap_or(start+1);

        let prev_line_end = content[..start].rfind('\n');
        let prev_line = content[..prev_line_end.unwrap_or(0)].rfind('\n').map(|n| n+1).unwrap_or(0);
        let next_line = content[end..].find('\n').map(|n| n + 1 + end).unwrap_or(content.len());
        let next_line_end = content[next_line..].find('\n').map(|n| n + next_line).unwrap_or(content.len());

        let line = content[..start].chars().filter(|c| *c == '\n').count() + 1;
        let col = start - prev_line_end.map(|n| n+1).unwrap_or(0) + 1;

        writeln!(stdout, "At {}:{}:{}", file, line, col);
        writeln!(stdout).unwrap();

        let mut pos = prev_line;
        loop {
            let line = content[pos..].find('\n').map(|n| n + pos).unwrap_or(content.len() - 1);

            writeln!(stdout, "{}", &content[pos..line]).unwrap();
            if pos >= prev_line_end.map(|n| n + 1).unwrap_or(0) && line < next_line {
                for i in pos..line {
                    if i >= start && i < end {
                        stdout.write_all(&[b'^']).unwrap();
                    } else {
                        stdout.write_all(&[b' ']).unwrap();
                    }
                }
                writeln!(stdout).unwrap();
            }

            pos = line+1;
            if pos >= next_line_end {
                break;
            }
        }
    }
}
