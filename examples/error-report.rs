extern crate rnix;

use rnix::Error as NixError;
use std::{env, fs, io::{self, Write}};

fn main() {
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    let file = match env::args().skip(1).next() {
        Some(file) => file,
        None => {
            eprintln!("Usage: dump-ast <file>");
            return;
        }
    };
    let content = match fs::read_to_string(file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("error reading file: {}", err);
            return;
        }
    };
    let span = match rnix::parse(&content) {
        Ok(_) => { println!("success"); return; },
        Err(err) => {
            writeln!(stdout, "error: {}", err).unwrap();
            match err {
                NixError::TokenizeError(span, _) => span,
                NixError::ParseError(Some(span), _) => span,
                NixError::ParseError(None, _) => return
            }
        }
    };

    writeln!(stdout, "{:?}", span);
    writeln!(stdout).unwrap();

    let start = span.start;
    let end = span.end.unwrap_or(start+1);

    let start_line = content[..start].rfind('\n').unwrap_or(0);
    let prev_line = content[..start_line].rfind('\n').map(|n| n+1).unwrap_or(0);
    let end_line = end + content[end..].find('\n').map(|n| n + 1).unwrap_or(content.len());
    let next_line = end_line + content[end_line..].find('\n').unwrap_or(content.len());

    let mut pos = prev_line;
    loop {
        let line = pos + content[pos..].find('\n').unwrap_or(content.len());

        writeln!(stdout, "{}", &content[pos..line]).unwrap();
        if pos >= start_line && line <= end_line {
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
        if pos >= next_line {
            break;
        }
    }
}
