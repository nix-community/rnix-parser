#![feature(rust_2018_preview, box_patterns)]

#[macro_use]
extern crate failure;
extern crate rnix;

use failure::Error;
use rnix::{parser::nometa::*, value::*};
use std::{
    collections::HashSet,
    env,
    fs,
    path::{PathBuf, Path}
};

fn main() -> Result<(), Error> {
    let nixpkgs = PathBuf::from(
        &env::var("NIX_PATH")?.split(':')
            .find(|s| s.starts_with("nixpkgs="))
            .ok_or_else(|| format_err!("no store path found"))?
            ["nixpkgs=".len()..]
    );

    println!("Nix store path: {}", nixpkgs.display());

    let mut app = App {
        nixpkgs: nixpkgs.clone(),
        checked: HashSet::new()
    };

    for path in &["lib/default.nix", "pkgs/top-level/all-packages.nix"] {
        let default = nixpkgs.join(&path);
        if let Err(err) = app.parse(&default) {
            println!("error: {}", err);
            break;
        }
    }
    Ok(())
}

struct App {
    nixpkgs: PathBuf,
    checked: HashSet<PathBuf>
}

impl App {
    fn parse(&mut self, file: &Path) -> Result<(), Error> {
        if self.checked.contains(file) {
            return Ok(());
        }
        self.checked.insert(file.to_path_buf());
        print!("Trying {}... ", file.display());
        let content = fs::read_to_string(file)?;
        let ast = rnix::parse(&content)?.into(); // Drop all metadata
        println!("success!");

        self.resolve(file, &ast)
    }
    fn parse_file_from_ast(&mut self, file: &Path, ast: &AST) -> Result<(), Error> {
        let mut file = match ast {
            AST::Value(Value::Path(Anchor::Store, path)) => {
                println!("{:?}", path);
                self.nixpkgs.join(&path)
            },
            AST::Value(Value::Path(Anchor::Relative, path)) => {
                println!("{:?}", path);
                file.parent().unwrap().join(path)
            },
            //ast => bail!("importing on something that's not a good path: {:?}", ast)
            _ => return Ok(())
        };
        if file.metadata()?.is_dir() {
            file.push("default.nix");
        }
        self.parse(&file)?;
        self.resolve(&file, ast)
    }
    fn resolve(&mut self, file: &Path, ast: &AST) -> Result<(), Error> {
        ast.recurse(&mut |ast| -> Result<(), Error> {
            match ast {
                AST::Apply(box (name, arg)) => {
                    if let AST::Var(name) = name {
                        if name == "callLibs" || name == "callPackage" {
                            self.parse_file_from_ast(&file, arg)?;
                        }
                    }
                },
                AST::Import(path) => {
                    self.parse_file_from_ast(&file, path)?;
                },
                _ => ()
            }
            Ok(())
        })
    }
}
