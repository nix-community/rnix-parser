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

    let app = App {
        nixpkgs: nixpkgs.clone()
    };

    for path in &["lib/default.nix", "pkgs/top-level/all-packages.nix"] {
        let default = nixpkgs.join(&path);
        if let Err(err) = app.parse(&default) {
            println!("failure: {}", err);
            break;
        }
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
        let mut file = match ast {
            AST::Value(Value::Path(Anchor::Store, path)) => {
                self.nixpkgs.join(&path)
            },
            AST::Value(Value::Path(Anchor::Relative, path)) => {
                file.parent().unwrap().join(path)
            },
            ast => bail!("importing on something that's not a good path: {:?}", ast)
        };
        if file.metadata()?.is_dir() {
            file.push("default.nix");
        }
        self.parse(&file)?;
        Ok(())
    }
    fn resolve(&self, file: &Path, ast: &AST) -> Result<(), Error> {
        match ast {
            AST::Apply(box (name, arg)) => {
                if let AST::Var(name) = name {
                    if name == "callLibs" || name == "callPackage" {
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
            AST::With(box (namespace, body)) => {
                self.resolve(file, namespace)?;
                self.resolve(file, body)?;
            },
            AST::Lambda(arg, body) => {
                if let LambdaArg::Pattern { args, .. } = arg {
                    for PatEntry(_, default) in args {
                        if let Some(default) = default {
                            self.resolve(file, default)?;
                        }
                    }
                }
                self.resolve(file, body)?;
            },
            _ => ()
        }
        Ok(())
    }
}
