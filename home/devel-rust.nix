{pkgs, ...}:

{
  home.packages = with pkgs; [
    rls
  ];

  programs.emacs.extraPackages = e: with e; [
    cargo-mode
    flycheck-rust
    rustic
  ];
}
