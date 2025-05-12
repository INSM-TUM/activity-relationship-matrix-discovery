{pkgs ? import ./pkgs.nix {}}:
with pkgs;
  mkShell {
    nativeBuildInputs = [
      # Rust
      rustc
      rustc-wasm32
      rust-analyzer
      cargo
      gcc
      llvmPackages.bintools
      rustfmt
      clippy
      trunk
    ];
    # Don't set rpath for native addons
    NIX_DONT_SET_RPATH = true;
    NIX_NO_SELF_RPATH = true;
    RUST_SRC_PATH = "${rustPlatform.rustLibSrc}";
    CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "lld";
    shellHook = ''
    '';
  }
