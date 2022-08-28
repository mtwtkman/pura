# Usage

---

If you have the file named `hs.yaml` like below,

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
shellHooks: |
  function hi() {
    echo "hi"
  }

```

you can call `pura hs` and it generates a `shell.nix` file like below

```nix
with import <nixpkgs> {};
mkShell {
  packages = [
    ghc
    cabal-install
    haskell-language-server
  ];
  shellHooks = ''
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

# Command
`pura --config-root <filepath/.config/pura> <name>`

or

`pura --config <path-to-config-file>`

# Config file format

---

| name | type | default |
| ---  | ---- | ------- |
| packages | Array<String> | [] |
| shellAliases | Array<KeyValue<Name,Value>> | [] |
| shellHooks | String | "" |

`shellAliases` are included to `shellHooks` as shell alias definitions.

If all fields are absent or `pura` was called without a config name, `pura` will generate a skelton `shell.nix` like below.

```nix
with import <nixpkgs> {};
mkShell {
  packages = [
  ];
  shellHooks = ''
  '';
};
```
