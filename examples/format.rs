extern crate rnix;

use std::{env, fs, io::{self, Write}};
use rnix::parser::*;

fn main() {
    let file = match env::args().skip(1).next() {
        Some(file) => file,
        None => {
            eprintln!("Usage: format <file>");
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
    match rnix::parse(&content) {
        Ok(ast) => { print(&mut io::stdout(), 0, &ast).unwrap(); },
        Err(err) => eprintln!("error: {}", err)
    }
}

fn pad<W: Write>(w: &mut W, indent: usize) -> io::Result<()> {
    for _ in 0..indent {
        w.write_all(&[b' '])?;
    }
    Ok(())
}

fn print<W: Write>(w: &mut W, indent: usize, ast: &AST) -> io::Result<()> {
    for comment in &ast.0.comments {
        writeln!(w, "# {}", comment.trim())?;
        pad(w, indent)?;
    }
    match &ast.1 {
        ASTType::Value(val) => write!(w, "{}", val)?,
        ASTType::Var(name) => write!(w, "{}", name)?,
        ASTType::Set { recursive, values } => {
            if *recursive {
                write!(w, "rec ")?;
            }
            writeln!(w, "{{")?;
            for entry in values {
                let indent = indent+2;
                pad(w, indent)?;
                match entry {
                    SetEntry::Assign(key, val) => {
                        for (i, part) in key.iter().enumerate() {
                            if i > 0 {
                                write!(w, ".")?;
                            }
                            print(w, indent, &part)?;
                        }
                        write!(w, " = ")?;
                        print(w, indent, &val)?;
                        write!(w, ";")?;
                    },
                    SetEntry::Inherit(from, values) => {
                        write!(w, "inherit")?;
                        if let Some(from) = from {
                            write!(w, "(")?;
                            print(w, indent, &from)?;
                            write!(w, ")")?;
                        }
                        for val in values {
                            write!(w, " {}", val)?;
                        }
                    }
                }
                writeln!(w)?;
            }
            pad(w, indent)?;
            write!(w, "}}")?;
        },
        ASTType::Lambda(arg, body) => {
            match arg {
                FnArg::Ident(name) => write!(w, "{}", name)?,
                FnArg::Pattern { args, bind, exact } => {
                    if let Some(bind) = bind {
                        write!(w, "{} @ ", bind)?;
                    }
                    write!(w, "{{ ")?;
                    for (i, PatEntry(name, default)) in args.iter().enumerate() {
                        if i > 0 {
                            write!(w, ", ")?;
                        }
                        write!(w, "{}", name)?;
                        if let Some(default) = default {
                            write!(w, "? ")?;
                            print(w, indent, default)?;
                        }
                    }
                    if !exact {
                        if !args.is_empty() {
                            write!(w, ", ")?;
                        }
                        write!(w, "...")?;
                    }
                    write!(w, " }}")?;
                }
            }
            write!(w, ": ")?;
            print(w, indent, body)?;
        },
        ASTType::List(list) => {
            writeln!(w, "[")?;
            for item in list {
                pad(w, indent+2)?;
                print(w, indent+2, item)?;
                writeln!(w)?;
            }
            pad(w, indent)?;
            write!(w, "]");
        },
        _ => write!(w, "TODO")?,
    }
    Ok(())
}
