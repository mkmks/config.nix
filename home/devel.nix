{config, pkgs, ...}:

{
  home = {    
    packages = with pkgs; [
      apktool
      cmake-language-server
      codex-acp
      gh
      jadx
      unstable.devenv
    ];
    sessionVariables = {
      ANTHROPIC_AUTH_TOKEN = "dummy";
      ANTHROPIC_API_KEY = "";
      ANTHROPIC_BASE_URL = "http://localhost:11435";
      ANTHROPIC_MODEL = "gpt-oss:20b";
      ANTHROPIC_DEFAULT_HAIKU_MODEL = config.home.sessionVariables.ANTHROPIC_MODEL;
      ANTHROPIC_DEFAULT_SONNET_MODEL = config.home.sessionVariables.ANTHROPIC_MODEL;
      ANTHROPIC_DEFAULT_OPUS_MODEL = config.home.sessionVariables.ANTHROPIC_MODEL;
    };
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
        projects."/home/viv/repos/chess-hs-codex".trust_level = "trusted";
        approval_policy = "on-request";
        sandbox_mode = "workspace-write";
        web_search = "disabled";
      };
    };

    opencode = {
      enable = true;
      package = pkgs.unstable.opencode;
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
              "qwen3.6-27b" = {
                name = "qwen3.6-27b";
              };
              "qwen3.6-35b-a3b" = {
                name = "qwen3.6-35b-a3b";
              };
              "glm-4.7-flash" = {
                name = "glm-4.7-flash";
              };
            };
          };
        };
      };
    };
  };

  services.lorri.enable = true;
}
