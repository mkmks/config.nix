{pkgs, ...}:

{
  accounts.email = {
    certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
    maildirBasePath = "Mail";
    
    accounts.fastmail = {
      primary = true;

      address = "nf@mkmks.org";
      realName = "Nikita Frolov";
      userName = "nf@mkmks.org";
      passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup email nf@mkmks.org";
      
      imap.host = "imap.fastmail.com";      
      smtp.host = "smtp.fastmail.com";

      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
      };
      msmtp.enable = true;
      mu.enable = true;
    };
  };

  home.packages = with pkgs; [
    libsecret
  ];

  programs = {
    mbsync.enable = true;
    msmtp.enable = true;
    mu.enable = true;

    emacs.extraPackages = e: with e; [
      mu4e
      mu4e-conversation
      #mu4e-maildirs-extension      
    ];
  };
  
  services.mbsync.enable = true;
}
