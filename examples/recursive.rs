#![feature(rust_2018_preview, box_patterns)]

#[macro_use]
extern crate failure;
extern crate rnix;

use failure::Error;
use rnix::{parser::*, value::*};
use std::{env, fs};

fn main() -> Result<(), Error> {
    let mut nixpkgs = env::var("NIX_PATH")?.split(':')
        .find(|s| s.starts_with("nixpkgs="))
        .ok_or_else(|| format_err!("no store path found"))?
        .to_string();
    nixpkgs.drain(0.."nixpkgs=".len());

    println!("Nix store path: {}", nixpkgs);

    let default = nixpkgs.clone() + "/lib/default.nix";
    App { nixpkgs }.parse(&default)
}

struct App {
    nixpkgs: String
}

impl App {
    fn parse(&self, file: &str) -> Result<(), Error> {
        print!("Trying {}... ", file);
        let content = fs::read_to_string(file)?;
        let ast = rnix::parse(&content)?.into(); // Drop all metadata
        println!("success!");

        self.resolve(&ast)
    }
    fn resolve(&self, ast: &AST) -> Result<(), Error> {
        match &ast.1 {
            ASTType::Import(path) => {
                match &path.1 {
                    ASTType::Value(Value::Path(Anchor::Store, path)) => {
                        self.parse(&(self.nixpkgs.clone() + &path))?;
                    },
                    _ => bail!("tried to import something that wasn't a store path")
                }
            },
            ASTType::Set { values, .. } => {
                for entry in values {
                    if let SetEntry::Assign(_, value) = entry {
                        self.resolve(value)?;
                    }
                }
            },
            ASTType::With(box (one, two)) => { self.resolve(one)?; self.resolve(two)?; }
            _ => ()
        }
        Ok(())
    }
}
