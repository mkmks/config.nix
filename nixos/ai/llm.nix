{ lib, ... }:

let
  # llama-server preset shared by all Qwen3.8-Flash-Next variants; only the
  # model source (hf-repo) and serving name (alias) differ.
  flashNextPreset = { alias, hf-repo }: {
    inherit alias hf-repo;
    # Keep expert backing weights on CPU and cache selected experts on GPU.
    load-mode = "mmap";
    lazy-mode = "on";
    gpu-layers = "all";
    cpu-moe = "true";
    moe-cache-mib = "7168";
    # Keep the n-gram lookup table mmap-backed for on-demand reads.
    override-tensor = "per_layer_token_embd.weight=CPU";
    ctx-size = "65536";
    parallel = "1";
    flash-attn = "on";
    # Larger batches: CPU experts re-read weights once per batch, so bigger
    # prompt chunks amortize RAM bandwidth. KV in q8_0 halves attention-side
    # memory traffic; the freed VRAM goes into the MoE cache.
    batch-size = "1024";
    cache-type-k = "q8_0";
    cache-type-v = "q8_0";
    ubatch-size = "512";
    threads = "24";
    threads-batch = "24";
    temp = "1.0";
    top-p = "0.95";
    min-p = "0.0";
    top-k = "20";
    repeat-penalty = "1.0";
    presence-penalty = "0.0";
    reasoning-effort = "medium";
    mmproj-auto = "false";
  };
in
{
  # The server binary comes from services.llama-cpp.package: the pinned CUDA
  # build of the llama-cpp flake input on hivemind, set there in flake.nix.
  services.llama-cpp = {
    enable = true;
    openFirewall = true;
    port = 11435;
    modelsPreset = {
      "gpt-oss-20b" = {
        alias = "gpt-oss-20b";
        hf-repo = "unsloth/gpt-oss-20b-GGUF:UD-Q4_K_XL";
        ctx-size = "131072";
        temp = "1.0";
        top-p = "1.0";
        top-k = "0";
      };
      "qwen3.6-35b-a3b" = {
        alias = "qwen3.6-35b-a3b";
        hf-repo = "unsloth/Qwen3.6-35B-A3B-GGUF:UD-Q8_K_XL";
        load-mode = "none";
        ctx-size = "262144";
        temp = "0.6";
        top-p = "0.95";
        min-p = "0.0";
        top-k = "20";
        repeat-penalty = "1.0";
        presence-penalty = "0.0";
      };
      "qwen3.8-flash-next" = flashNextPreset {
        alias = "qwen3.8-flash-next";
        hf-repo = "unsloth/Qwen3.8-Flash-Next-GGUF:UD-Q4_K_XL";
      };
      # huihui-ai's v2 abliteration of the same base model (ablation
      # emphasis adjusted, no experts ablated). :UD-Q4_K_XL resolves to
      # their newest unsloth-UD-Q4_K_XL-V2 calibration: same quant, shard
      # count, and size as the stock preset above.
      "qwen3.8-flash-next-abliterated" = flashNextPreset {
        alias = "qwen3.8-flash-next-abliterated";
        hf-repo = "huihui-ai/Huihui-Qwen3.8-Flash-Next-abliterated-v2-GGUF:UD-Q4_K_XL";
      };
    };
  };

  # Download GGUFs into a dedicated directory rather than the default
  # /var/cache/llama-cpp, so other tooling can share them. StateDirectory
  # creates the name relative to /var/lib.
  systemd.services.llama-cpp.serviceConfig = {
    StateDirectory = lib.mkForce [ "llama-cpp" "models" ];

    Environment = lib.mkForce [
      "LLAMA_CACHE=/var/lib/models"
    ];
  };
}
