{ pkgs, ... }:

let
  # Use the flake's locked unstable package set, including its CUDA configuration.
  sd = pkgs.unstable.stable-diffusion-cpp.override { cudaSupport = true; };

  diffusion = pkgs.fetchurl {
    url = "https://huggingface.co/unsloth/Qwen-Image-2.1-Turbo-GGUF/resolve/main/qwen-image-2.1-turbo-Q8_0.gguf";
    hash = "sha256-OZ0t3DY282RW4vW/pXGH2k44chvARkMDF/QqnqtbFxI=";
  };

  # Qwen-Image-2.1 requires its own VAE, not the Qwen-Image 1.0 / Wan 2.2 VAE.
  vae = pkgs.fetchurl {
    url = "https://huggingface.co/Comfy-Org/Qwen-Image-2.1/resolve/main/vae/qwen_image_2.1_vae_bf16.safetensors";
    hash = "sha256-uyH3RzBR4aw2hRXdPy4VzUTXoRdI7ogj4d3KPkh2t8k=";
  };

  # Abliterated Qwen3-VL-8B encoder; keep it in system RAM to save VRAM.
  textEncoder = pkgs.fetchurl {
    url = "https://huggingface.co/AtomicChat/Qwen-Image-2.1-Turbo-Uncensored-GGUF/resolve/main/Qwen-Image-2.1-Turbo-Uncensored-Q8_0.gguf";
    hash = "sha256-+Pn2J2tybbUpH0pTQwiWEstf0IqoHo8Hkdxm0sA9B8o=";
  };

  # Only the editing wrapper loads the vision projector for -r reference images.
  visionProjector = pkgs.fetchurl {
    url = "https://huggingface.co/AtomicChat/Qwen-Image-2.1-Turbo-Uncensored-GGUF/resolve/main/mmproj-Qwen-Image-2.1-Turbo-Uncensored-F16.gguf";
    hash = "sha256-2YcKHxaNs9gUWWIWqvNXAhVBR7RDVyx5OazKGAJiGsY=";
  };

  # No trailing newline: that would end sd-cli before the wrapper's arguments.
  commonArgs = builtins.concatStringsSep " " [
    "--diffusion-model ${diffusion}"
    "--vae ${vae}"
    "--llm ${textEncoder}"
    "--params-backend te=cpu"
    "--sigmas qwen-image-2.1-turbo"
    "--steps 8"
    "--cfg-scale 1.0"
    "--sampling-method euler"
  ];
in
{
  # Dimensions must be divisible by 32. All arguments are forwarded to sd-cli.
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "qwen-image" ''
      exec ${sd}/bin/sd-cli \
        ${commonArgs} \
        "$@"
    '')
    (pkgs.writeShellScriptBin "qwen-image-edit" ''
      exec ${sd}/bin/sd-cli \
        ${commonArgs} \
        --llm_vision ${visionProjector} \
        "$@"
    '')
  ];
}
