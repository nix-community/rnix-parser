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
                _ => return
            }
        }
    };

    println!("{:?}", span);

    writeln!(stdout).unwrap();

    let start = span.start;
    let end = span.end.unwrap_or(start);

    for (row, line) in content.lines().enumerate() {
        let row = row as u64;
        if row < start.0.saturating_sub(1) || row > end.0 + 1 {
            continue;
        }

        writeln!(stdout, "{}", line).unwrap();

        if row == start.0 {
            draw_line(&mut stdout, line, Some(start.1), Some(end.1).into_iter().filter(|_| start.0 == end.0).next());
        } else if row == end.0 {
            draw_line(&mut stdout, line, Some(start.1).into_iter().filter(|_| start.0 == end.0).next(), Some(end.1));
        } else if row > start.0 && row < end.0 {
            draw_line(&mut stdout, line, None, None);
        }
    }
}
fn draw_line<W: Write>(stdout: &mut W, line: &str, start: Option<u64>, end: Option<u64>) {
    let start = start.unwrap_or(0);
    let end = end.unwrap_or(line.len() as u64);
    for col in 0..line.len() as u64 {
        if col < start || (col != start && col >= end) {
            stdout.write_all(&[b' ']).unwrap();
        } else {
            stdout.write_all(&[b'^']).unwrap();
        }
    }
    writeln!(stdout).unwrap();
}
