{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.timeout = null;
  boot.plymouth.enable = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "mikolaj";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces = {
      enp1s0.useDHCP = true;
      wlp2s0.useDHCP = true;
    };
  };

  time.timeZone = "Europe/Paris";

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services.xserver = {
    enable = true;
    layout = "us";
    autorun = false;
    libinput.enable = true;
    displayManager.startx.enable = true;
    windowManager.dwm.enable = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "osu-lazer"
    ];

  users.users.wafelack = {
    isNormalUser = true;
    home = "/home/wafelack";
    extraGroups = [ "wheel" "networkmanager" ];
    packages = with pkgs; [
      firefox
      neofetch
      stow
      git
      pinentry
      ghc
      xorg.xmodmap
      xclip
      dmenu
      flameshot
      kitty
      pavucontrol
      tmux
      ruby
      inkscape
      xmobar
      stack
      cabal-install
      sbcl
      feh
      osu-lazer
    ];
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
    gnupg
    pinentry-curses
    gnumake
    gcc
    arandr
  ];

  nixpkgs.overlays = [
    (self: super: {
      dwm = super.dwm.overrideAttrs (oldAttrs: rec {
        configFile = super.writeText "config.def.h" (builtins.readFile /home/wafelack/.dotfiles/misc/dwm_config.h);
        postPatch = oldAttrs.postPatch or "" + "\necho 'Using own config file...'\ncp ${configFile} config.def.h"; 
      });
    })
  ];

  fonts.fonts = with pkgs; [
    unifont
    iosevka
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
