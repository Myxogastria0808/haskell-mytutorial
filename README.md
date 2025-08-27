# Haskell Tutorial

## Setup

0. set `flake.nix` and `.envrc`

- flake.nix

```nix
{
  description = "haskell-mytutorial";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            stack
            ghc
            haskell-language-server
          ];
        };
      }
    );
}
```

- .envrc

```
use flake
```

1. setup devShell

```sh
direnv allow
```

2. set `.vscode/settings.json`

`"haskell.manageHLS": "PATH",` is the most important setting.

```json
{
  "[haskell]": {
    "editor.defaultFormatter": "haskell.haskell",
    "editor.formatOnSave": true,
    "editor.formatOnPaste": true
  },
  "haskell.manageHLS": "PATH",
  "haskell.plugin.semanticTokens.globalOn": true,
  "nixEnvSelector.nixFile": "${workspaceFolder}/flake.nix"
}
```

3. create a stack project

```sh
stack new <project-name>
cd <project-name>
```

4. update `.gitignore`

```
# Nix
/.direnv

# Haskell
.stack-work/
*~

```

## Build and Run

`stack exec` command require `<project-name>-exe`.

```sh
stack build
stack exec <project-name>-exe
```

## Test

```sh
stack test
```
