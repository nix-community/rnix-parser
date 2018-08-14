#![feature(rust_2018_preview, box_patterns)]

#[macro_use]
extern crate failure;
extern crate rnix;

use failure::Error;
use rnix::{nometa::*, value::*};
use std::{env, fs, path::{PathBuf, Path}};

fn main() -> Result<(), Error> {
    let nixpkgs = PathBuf::from(
        &env::var("NIX_PATH")?.split(':')
            .find(|s| s.starts_with("nixpkgs="))
            .ok_or_else(|| format_err!("no store path found"))?
            ["nixpkgs=".len()..]
    );

    println!("Nix store path: {}", nixpkgs.display());

    let default = nixpkgs.join("lib/default.nix");
    if let Err(err) = (App { nixpkgs }.parse(&default)) {
        println!("failure: {}", err);
    }
    Ok(())
}

struct App {
    nixpkgs: PathBuf
}

impl App {
    fn parse(&self, file: &Path) -> Result<(), Error> {
        print!("Trying {}... ", file.display());
        let content = fs::read_to_string(file)?;
        let ast = rnix::parse(&content)?.into(); // Drop all metadata
        println!("success!");

        self.resolve(file, &ast)
    }
    fn parse_file_from_ast(&self, file: &Path, ast: &AST) -> Result<(), Error> {
        match ast {
            AST::Value(Value::Path(Anchor::Store, path)) => {
                self.parse(&self.nixpkgs.join(&path))?;
            },
            AST::Value(Value::Path(Anchor::Relative, path)) => {
                self.parse(&file.parent().unwrap().join(path))?;
            },
            ast => bail!("importing on something that's not a good path: {:?}", ast)
        }
        Ok(())
    }
    fn resolve(&self, file: &Path, ast: &AST) -> Result<(), Error> {
        match ast {
            AST::Apply(box (name, arg)) => {
                if let AST::Var(name) = name {
                    if name == "callLibs" {
                        self.parse_file_from_ast(file, arg)?;
                    }
                }
            },
            AST::Import(path) => {
                self.parse_file_from_ast(file, path)?;
            },
            AST::Set { values, .. } => {
                for entry in values {
                    if let SetEntry::Assign(_, value) = entry {
                        self.resolve(file, value)?;
                    }
                }
            },
            AST::LetIn(values, body) => {
                self.resolve(file, body)?;
                for entry in values {
                    if let SetEntry::Assign(_, value) = entry {
                        self.resolve(file, value)?;
                    }
                }
            },
            AST::With(box (one, two)) => { self.resolve(file, one)?; self.resolve(file, two)?; }
            _ => ()
        }
        Ok(())
    }
}
