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
      passwordCommand = "${pkgs.gnome3.libsecret}/bin/secret-tool lookup email nf@mkmks.org";
      
      imap.host = "imap.fastmail.com";      
      smtp.host = "smtp.fastmail.com";

      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
      };
      msmtp.enable = true;
    };  
  };

  home.packages = with pkgs; [
    gnome3.libsecret    
  ];

  programs = {
    mbsync.enable = true;
    msmtp.enable = true;
    mu.enable = true;
  };
  
  services.mbsync.enable = true;
}