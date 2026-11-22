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
      package = pkgs.unstable.vscode;
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
      package = pkgs.opencode;
      settings = {
        "$schema" = "https://opencode.ai/config.json";
        compaction = {
          auto = true;
          keep.tokens = 3000;
          buffer = 6000;
        };
        tool_output = {
          max_lines = 300;
          max_bytes = 12000;
        };
        instructions = [
          (toString (pkgs.writeText "opencode-context-budget.md" ''
            When using local llama-cpp models, conserve their small context windows:
            - Delegate substantial discovery to Explore with a narrow, self-contained task.
              Include essential constraints and request paths, symbols, and a summary under
              300 words rather than whole files. Do not delegate trivial work.
            - Run local subagents sequentially in the foreground: the local server has one slot.
              Avoid loading different local models concurrently.
            - Search before reading files. Read relevant sections and keep tool output bounded.
              Summarize test failures instead of dumping full logs.
            - Keep durable decisions in concise project documentation when requested.
              Do not create documentation or handoff files without authorization.
            - At task boundaries, offer a short handoff for a fresh session: goal, constraints,
              changed files, verification results, and remaining work.
          ''))
        ];
        commands.handoff = {
          description = "Summarize this task for a fresh, small-context session";
          template = "Provide a concise handoff in the conversation, without writing files: objective and constraints, decisions, changed files, tests and outcomes, blockers, and next steps. Include essential paths and commands. Stay under 500 words.";
        };
        providers = {
          llama-cpp = {
            name = "llama-server (local)";
            package = "@opencode/ai/providers/openai-compatible";
            settings = {
              baseURL = "http://localhost:11435/v1";
            };
            models = {
              "inkling-2-mini" = {
                name = "inkling-2-mini";
                modelID = "inkling-2-mini";
                capabilities = {
                  tools = true;
                  input = [ "text" "image" ];
                  output = [ "text" ];
                };
                limit = { context = 65536; output = 8192; };
              };
              "gpt-oss:20b" = {
                name = "gpt-oss:20b";
                modelID = "gpt-oss-20b";
                capabilities = {
                  tools = true;
                  input = [ "text" ];
                  output = [ "text" ];
                };
                limit = { context = 131072; output = 8192; };
              };
              "qwen3.6-35b-a3b" = {
                name = "qwen3.6-35b-a3b";
                modelID = "qwen3.6-35b-a3b";
                capabilities = {
                  tools = true;
                  input = [ "text" "image" ];
                  output = [ "text" ];
                };
                limit = { context = 262144; output = 8192; };
              };
              "qwen3.8-flash-next" = {
                name = "qwen3.8-flash-next";
                modelID = "qwen3.8-flash-next";
                capabilities = {
                  tools = true;
                  input = [ "text" "image" ];
                  output = [ "text" ];
                };
                limit = { context = 65536; output = 8192; };
              };
              "qwen3.8-flash-next-abliterated" = {
                name = "qwen3.8-flash-next-abliterated";
                modelID = "qwen3.8-flash-next-abliterated";
                capabilities = {
                  tools = true;
                  input = [ "text" "image" ];
                  output = [ "text" ];
                };
                limit = { context = 65536; output = 8192; };
              };
            };
          };
        };
      };
    };
  };

  services.lorri.enable = true;
}
