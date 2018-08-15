#![feature(box_patterns)]

extern crate rnix;

use std::{env, fs, io::{self, Write}};
use rnix::{parser::*, value::*};

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

fn escape(input: &str, multiline: bool) -> String {
    let mut output = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '$' => match chars.peek() {
                Some('$') => { chars.next(); output.push_str("$$") },
                Some('{') if multiline => { chars.next(); output.push_str("''${") },
                Some('{') => { chars.next(); output.push_str("\\${") },
                _ => output.push('$'),
            },
            '\'' if multiline => match chars.peek() {
                Some('\'') => { chars.next(); output.push_str("\'\'\'"); },
                _ => output.push('\''),
            },
            '"' if !multiline => output.push_str("\\\""),
            '\r' if multiline => output.push_str("''r"),
            '\t' if multiline => output.push_str("''t"),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            c => output.push(c)
        }
    }
    output
}

fn print<W: Write>(w: &mut W, indent: usize, ast: &AST) -> io::Result<()> {
    //if !ast.0.comments.is_empty() {
    //    writeln!(w)?;
    //    pad(w, indent)?;
    //}
    for comment in &ast.0.comments {
        let mut leading = 0;
        while comment.bytes().nth(leading) == Some(b'#') {
            leading += 1;
        }
        writeln!(w, "#{} {}", &comment[..leading], comment[leading..].trim())?;
        pad(w, indent)?;
    }
    macro_rules! math {
        ($($op:path => $str:expr),*) => {
            match &ast.1 {
                $($op(box (one, two)) => {
                    print(w, indent+2, one)?;
                    write!(w, concat!(' ', $str, ' '))?;
                    print(w, indent+2, two)?;
                    return Ok(());
                }),*
                _ => ()
            }
        }
    }
    math! (
        ASTType::Add => "+",
        ASTType::Sub => "-",
        ASTType::Mul => "*",
        ASTType::Div => "/"
    );
    match &ast.1 {
        ASTType::Value(Value::Bool(val)) => write!(w, "{}", val)?,
        ASTType::Value(Value::Integer(val)) => write!(w, "{}", val)?,
        ASTType::Value(Value::Float(val)) => write!(w, "{}", val)?,
        ASTType::Value(Value::Path(Anchor::Home, val)) => write!(w, "~/{}", val)?,
        ASTType::Value(Value::Path(Anchor::Store, val)) => write!(w, "<{}>", val)?,
        ASTType::Value(Value::Path(_, val)) => write!(w, "{}", val)?,
        ASTType::Value(Value::Str { multiline, content }) => {
            if !multiline {
                write!(w, "\"{}\"", escape(content, false))?
            } else {
                writeln!(w, "''");
                for line in content.lines() {
                    pad(w, indent+2)?;
                    writeln!(w, "{}", escape(line, true))?;
                }
                pad(w, indent)?;
                write!(w, "''");
            }
        },
        ASTType::Var(name) => write!(w, "{}", name)?,
        ASTType::Interpol { multiline, parts } => {
            if *multiline {
                writeln!(w, "''")?;
                pad(w, indent+2)?;
            } else {
                write!(w, "\"")?;
            }
            for part in parts {
                match part {
                    Interpol::Literal(literal) if *multiline => for (i, line) in literal.lines().enumerate() {
                        if i > 0 {
                            writeln!(w)?;
                            pad(w, indent+2)?;
                        }
                        write!(w, "{}", escape(line, true))?;
                    },
                    Interpol::Literal(literal) => write!(w, "{}", escape(literal, false))?,
                    Interpol::AST(ast) => {
                        write!(w, "${{")?;
                        print(w, indent+2, &ast)?;
                        write!(w, "}}")?;
                    }
                }
            }
            if *multiline {
                writeln!(w)?;
                pad(w, indent)?;
                write!(w, "''")?;
            } else {
                write!(w, "\"")?;
            }
        },
        ASTType::Set { recursive, values } => {
            if *recursive {
                write!(w, "rec ")?;
            }
            if values.is_empty() {
                write!(w, "{{}}")?;
                return Ok(());
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
                        write!(w, ";")?;
                    }
                }
                writeln!(w)?;
            }
            pad(w, indent)?;
            write!(w, "}}")?;
        },
        ASTType::Lambda(arg, body) => {
            write!(w, "(")?;
            match arg {
                LambdaArg::Ident(name) => write!(w, "{}", name)?,
                LambdaArg::Pattern { args, bind, exact } => {
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
            write!(w, ")")?;
        },
        ASTType::List(list) => {
            writeln!(w, "[")?;
            for item in list {
                pad(w, indent+2)?;
                print(w, indent+2, item)?;
                writeln!(w)?;
            }
            pad(w, indent)?;
            write!(w, "]")?;
        },
        ASTType::IndexSet(box (set, index)) => {
            print(w, indent+2, set)?;
            write!(w, ".")?;
            print(w, indent+2, index)?;
        },
        ASTType::Apply(box (f, arg)) => {
            write!(w, "(")?;
            print(w, indent, f)?;
            write!(w, " ")?;
            print(w, indent, arg)?;
            write!(w, ")")?;
        },
        ASTType::With(box (namespace, body)) => {
            write!(w, "with ")?;
            print(w, indent, namespace)?;
            write!(w, "; ")?;
            print(w, indent, body)?;
        },
        ASTType::Import(path) => {
            write!(w, "import ")?;
            print(w, indent+2, path)?;
        },
        ast => write!(w, "TODO: {:?}", ast)?,
    }
    Ok(())
}
