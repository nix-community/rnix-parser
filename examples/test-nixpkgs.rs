use std::{env, error::Error, fs, path::Path};

fn main() -> Result<(), Box<dyn Error>> {
    let path = env::var("NIX_PATH")?;
    let nixpkgs =
        path.split(':').find(|s| s.starts_with("nixpkgs=")).ok_or("no store path found")?;

    println!("Nix store path: {}", nixpkgs);

    recurse(Path::new(&nixpkgs["nixpkgs=".len()..]))
}
fn recurse(path: &Path) -> Result<(), Box<dyn Error>> {
    if path.metadata()?.is_file() {
        if path.extension().and_then(|s| s.to_str()) != Some("nix") {
            return Ok(());
        }

        println!("Checking {}", path.display());
        let original = fs::read_to_string(path)?;
        if original.trim().is_empty() {
            return Ok(());
        }

        let parsed = rnix::Root::parse(&original).tree().to_string();
        if original != parsed {
            eprintln!("Original input does not match parsed output!");
            println!("Input:");
            println!("----------");
            println!("{}", original);
            println!("----------");
            println!("Output:");
            println!("----------");
            println!("{}", parsed);
            println!("----------");
            return Err("parsing error".into());
        }
        return Ok(());
    } else {
        for entry in path.read_dir()? {
            let entry = entry?;
            if entry.file_type()?.is_symlink() {
                continue;
            }
            recurse(&entry.path())?;
        }
    }
    Ok(())
}
