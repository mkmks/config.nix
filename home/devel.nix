{config, pkgs, ...}:

{
  home = {    
    packages = with pkgs; [
      apktool
      cmake-language-server
      gh
      unstable.devenv
    ];
  };

  programs = {

    # tools

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    git = {
      enable = true;
      lfs.enable = true;
      settings.user = {
        email = "nf@mkmks.org";
        name = "Nikita Frolov";
      };
    };

    # editors

    emacs.extraPackages = e: with e; [
      agent-shell
      company
      direnv
      ein
      envrc
      flycheck
      flycheck-eglot
      magit
      projectile
      projectile-ripgrep
      nix-buffer
      # programming languages
      capnp-mode
      cmake-mode
      dockerfile-mode
      elm-mode
      nix-mode
      protobuf-mode
      sql-clickhouse
      terraform-mode
      toml-mode
      typescript-mode
      yaml-mode
      zig-mode
      ## haskell
      flycheck-haskell
      haskell-mode
      ## ocaml
      merlin
      tuareg
      ## rust
      cargo-mode
      flycheck-rust
      rustic
      ## scala
	    sbt-mode
	    scala-mode
      ## solidity
      solidity-flycheck
      solidity-mode
    ];
    
    helix = {
      enable = true;
    };
    
    vscode = {
      enable = true;
      profiles.default.extensions = with pkgs.vscode-extensions; [
        justusadam.language-haskell
        mkhl.direnv
        ms-vsliveshare.vsliveshare
        ocamllabs.ocaml-platform
        rust-lang.rust-analyzer
        scala-lang.scala
        scalameta.metals
      ];
    };

    # agents

    codex = {
      enable = true;
      package = pkgs.unstable.codex;
      settings = {
#        model = "gpt-oss:20b";
        model_reasoning_effort = "high";
#        model_provider = "llama-cpp";
        model_providers = {
          llama-cpp = {
            name = "llama-cpp";
            base_url = "http://127.0.0.1:11435/v1";
          };
        };
        projects = {
          "/home/viv/repos/chess-hs-codex".trust_level = "trusted";
          "/home/viv/repos/zama/kms".trust_level = "trusted"; 
        };
        approval_policy = "on-request";
        sandbox_mode = "workspace-write";
        web_search = "disabled";
      };
    };

    opencode = {
      enable = true;
      settings = {
        provider = {
          llama-cpp = {
            name = "llama-server (local)";
            npm = "@ai-sdk/openai-compatible";
            options = {
              baseURL = "http://localhost:11435/v1";
            };
            models = {
              "gpt-oss:20b" = {
                name = "gpt-oss:20b";
              };
              "gpt-oss:120b" = {
                name = "gpt-oss:120b";
              };
              "glm-4.7-flash" = {
                name = "glm-4.7-flash";
              };
              "qwen3-coder-next" = {
                name = "qwen3-coder-next";
              };
              "qwen3.6-35b-a3b" = {
                name = "qwen3.6-35b-a3b";
              };
              "qwen3.8-27b" = {
                name = "qwen3.8-27b";
              };
            };
          };
        };
      };
    };
  };

  services.lorri.enable = true;
}
