# Pura
`shell.nix` generator.


If you have the file named `hs.yaml` (or `hs.yml`) like below,

```yaml
packages:
  - ghc
  - cabal-install
  - haskell-language-server
shellAliases:
  b: "cabal build"
  c: "cabal clean"
  r: "cabal run"
  repl: "cabal repl"
shellHook: |
  function hi() {
    echo "hi"
  }

```

Pura recognizes this file as a boilerplate of `shell.nix` named `hs`.

After you executed `pura hs`, Pura generated a `shell.nix` like below


```nix
with import <nixpkgs> {};
mkShell {
  packages = [
    ghc
    cabal-install
    haskell-language-server
  ];
  shellHook = ''
    alias b="cabal build"
    alias c="cabal clean"
    alias r="cabal run"
    alias repl="cabal repl"
    function hi() {
      echo "hi"
    }
  '';
}
```

# Usage
1. Prepare template file
1. Move to a project root directory
1. Execute `pura <name>`
1. Done

Pura supposes the location of template files is `~/.config/pura` but you can specify it your choice by using `--template-root` option.


# Template file format

| name | type | default |
| ---  | ---- | -------
| packages | Array<String> | [] |
| shellAliases | Array<KeyValue<Name,Value>> | [] |
| shellHook | String | "" |

`shellAliases` are merged with `shellHook` as shell aliases.
