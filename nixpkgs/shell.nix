{pkgs, ...}:

{
  home = {
    packages = with pkgs; [
      # base
      bc
      dtach
      fdupes
      file
      jq
      mc
      mg
      p7zip
      psmisc
      sdcv
      silver-searcher
      unrar
      unzip
      xdg-utils

      # net
      dnsutils
      inetutils
      lftp
      nmap
      picocom

      # img
      pkgs.exif
      exiftool
      pkgs.imagemagick
      pdftk
      poppler_utils
      
      # snd
      pamixer
    ];

    sessionVariables = {
      ALTERNATIVE_EDITOR = "mg -n";
      SDCV_PAGER = "less -R";      
    };
  };
  
  programs = {
    fish = {
      enable = true;

      functions = {
        d = ''
set -l dict
# 35/53/64
switch $argv[1]
	# ENGLISH (7/8)
	case ee
	    set dict 'OxfordDictionary (En-En)'
	case eg
	    set dict 'Collins (En-De)'
	case ed
	    set dict 'Van Dale (En-Nl)'
	case ef
	    set dict 'Universal (En-Fr)'
	case ei
	    set dict 'Universal (En-It)'
	case es
	    set dict 'Collins (En-Es)'
	case er
	    set dict 'LingvoUniversal (En-Ru)'
	# GERMAN (6/8)
	case ge
	    set dict 'Collins (De-En)'
	case gg
	    set dict 'Duden (De-De)'
	case gf
	    set dict 'CompactVerlag (De-Fr)'
	case gi
	    set dict 'CompactVerlag (De-It)'
	case gs
	    set dict 'CompactVerlag (De-Es)'
	case gr
	    set dict 'Universal (De-Ru)'
	# DUTCH (2/8)
	case de
	    set dict 'Van Dale (Nl-En)'	    
	case dr
	    set dict 'Part Ne-Ru'
	# FRENCH (4/8)
	case fe
	    set dict 'Universal (Fr-En)'    
	case fg
	    set dict 'CompactVerlag (Fr-De)'	    	    
	case ff
	    set dict 'Le Grand Robert (Fr-Fr)'
	case fr
	    set dict 'Universal (Fr-Ru)'	    
	# ITALIAN (3/8)
	case ie
	    set dict 'Universal (It-En)'    
	case ig
	    set dict 'CompactVerlag (It-De)'	    
	case ir
	    set dict 'Universal (It-Ru)'	    	    
	# SPANISH (4/8)
	case se
	    set dict 'Collins (Es-En)'
	case sg
	    set dict 'CompactVerlag (Es-De)'
	case ss
	    set dict 'VOX (Spa-Spa)'
	case sr
	    set dict 'Universal (Es-Ru)'	    
	# PORTUGUESE (1/8)
	case pr
	    set dict 'Universal (Pt-Ru)'
	# RUSSIAN (8/8)
	case re
	    set dict 'LingvoUniversal (Ru-En)'
	case rg
	    set dict 'Universal (Ru-De)'
	case rd
	    set dict 'Part Ru-Ne'
	case rf
	    set dict 'Universal (Ru-Fr)'
	case ri
	    set dict 'Universal (Ru-It)'
	case rs
	    set dict 'Universal (Ru-Es)'
	case rp
	    set dict 'Universal (Ru-Pt)'
	case rr
	    set dict 'ExplanatoryBTS (Ru-Ru)'
end
sdcv -0c -u $dict $argv[2]          
        '';

        my_fish_prompt = ''
set -l nix_shell_info (
  if test -n "$IN_NIX_SHELL"
    echo -n "<nix-shell> "
  end
)
echo -n -s $nix_shell_info '$ '
        '';

        fish_prompt = ''
# Remove the trailing newline from the original prompt. This is done
# using the string builtin from fish, but to make sure any escape codes
# are correctly interpreted, use %b for printf.
printf "%b" (string join "\n" (my_fish_prompt))
vterm_prompt_end
'';

        fish_title = ''
hostname
echo ":"
pwd
        '';

        vterm_printf = ''
if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
  # tell tmux to pass the escape sequences through
  printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
else if string match -q -- "screen*" "$TERM"
       # GNU screen (screen, screen-256color, screen-256color-bce)
       printf "\eP\e]%s\007\e\\" "$argv"
     else
       printf "\e]%s\e\\" "$argv"
     end
        '';

        vterm_prompt_end = "vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)";
      };
      
      interactiveShellInit = ''
set fish_greeting

if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
end

gpg-connect-agent -q updatestartuptty /bye > /dev/null
      '';
      
      shellAliases = {
        ec = "emacsclient -n";
        u = "udiskie-umount";
      };
    };

    ncmpcpp.enable = true;
    ncspot.enable = true;
  };
}
