{config, pkgs, ...}:

{
  programs.fish = {
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
    };
    
    interactiveShellInit = ''
if test "$TERM" != "dumb"
  ${config.home.profileDirectory}/bin/starship init fish | source
  source ${pkgs.emacsPackages.vterm}/share/emacs/site-lisp/elpa/vterm-*/etc/emacs-vterm.fish
end
      '';
    
    shellAliases = {
      ec = "emacsclient -n";
      u = "udiskie-umount";
    };
  };
}
