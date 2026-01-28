# Migration Guide

## Migrating to 0.13.0

### Path types are now distinguished at the type level

Previously, all Nix paths were parsed into a single `TOKEN_PATH` / `NODE_PATH` type, requiring runtime string inspection to determine the path variant. Now, paths are distinguished at the type level:

| Path Type | Example | Token | Node |
|-----------|---------|-------|------|
| Absolute | `/foo/bar` | `TOKEN_PATH_ABS` | `NODE_PATH_ABS` |
| Relative | `./foo`, `../bar`, `foo/bar` | `TOKEN_PATH_REL` | `NODE_PATH_REL` |
| Home-relative | `~/foo` | `TOKEN_PATH_HOME` | `NODE_PATH_HOME` |
| Search | `<nixpkgs>` | `TOKEN_PATH_SEARCH` | `NODE_PATH_SEARCH` |

#### Updating `SyntaxKind` matches

```rust
// Before:
match kind {
    SyntaxKind::TOKEN_PATH => { /* ... */ }
    SyntaxKind::NODE_PATH => { /* ... */ }
}

// After:
match kind {
    SyntaxKind::TOKEN_PATH_ABS
    | SyntaxKind::TOKEN_PATH_REL
    | SyntaxKind::TOKEN_PATH_HOME
    | SyntaxKind::TOKEN_PATH_SEARCH => { /* ... */ }

    SyntaxKind::NODE_PATH_ABS
    | SyntaxKind::NODE_PATH_REL
    | SyntaxKind::NODE_PATH_HOME
    | SyntaxKind::NODE_PATH_SEARCH => { /* ... */ }
}
```

#### Updating `Expr` matches

The `Expr` enum no longer has a `Path` variant. Instead, there are four separate variants:

```rust
// Before:
match expr {
    ast::Expr::Path(p) => {
        // Had to inspect the string to determine path type
    }
    // ...
}

// After:
match expr {
    ast::Expr::PathAbs(p) => { /* absolute path like /foo/bar */ }
    ast::Expr::PathRel(p) => { /* relative path like ./foo */ }
    ast::Expr::PathHome(p) => { /* home path like ~/foo */ }
    ast::Expr::PathSearch(p) => { /* search path like <nixpkgs> */ }
    // ...
}
```

#### Using the `Path` enum

If you need to handle all path types uniformly, use the `ast::Path` enum:

```rust
use rnix::ast::{AstNode, Path};

// Cast any path node to the Path enum
if let Some(path) = Path::cast(node) {
    match path {
        Path::PathAbs(p) => { /* ... */ }
        Path::PathRel(p) => { /* ... */ }
        Path::PathHome(p) => { /* ... */ }
        Path::PathSearch(p) => { /* ... */ }
    }

    // Or use helper methods
    if path.is_search() {
        // Search paths don't support interpolation
    }

    // Get parts from any path type
    let parts = path.parts();
}
```

#### `Path::parts()` return type change

The `parts()` method now returns `Vec<InterpolPart<PathContent>>` instead of `impl Iterator<...>`:

```rust
// Before:
let mut parts = path.parts();
let first = parts.next();

// After:
let parts = path.parts();
let first = parts.first();
```

### Double slashes in paths are now syntax errors

Paths containing `//` (e.g., `foo//bar`) are now tokenized as `TOKEN_ERROR`. This matches Nix's behavior where such paths are invalid.
