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
        Ok(ast) => { ast.print(&mut io::stdout(), 0).unwrap(); },
        Err(err) => eprintln!("error: {}", err)
    }
}

fn pad<W: Write>(w: &mut W, indent: usize) -> io::Result<()> {
    for _ in 0..indent {
        w.write_all(&[b' '])?;
    }
    Ok(())
}

trait Print {
    fn print<W: Write>(&self, w: &mut W, indent: usize) -> io::Result<()>;
}

impl Print for AST {
    fn print<W: Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        for comment in &self.0.comments {
            writeln!(w, "# {}", comment.trim())?;
            pad(w, indent)?;
        }
        match &self.1 {
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
                                part.print(w, indent)?;
                            }
                            write!(w, " = ")?;
                            val.print(w, indent)?;
                            write!(w, ";")?;
                        },
                        SetEntry::Inherit(from, values) => {
                            write!(w, "inherit")?;
                            if let Some(from) = from {
                                write!(w, "(")?;
                                from.print(w, indent)?;
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
                                default.print(w, indent)?;
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
                body.print(w, indent)?;
            },
            ASTType::List(list) => {
                writeln!(w, "[")?;
                for item in list {
                    pad(w, indent+2)?;
                    item.print(w, indent+2)?;
                    writeln!(w)?;
                }
                pad(w, indent)?;
                write!(w, "]");
            },
            _ => write!(w, "TODO")?,
        }
        Ok(())
    }
}
