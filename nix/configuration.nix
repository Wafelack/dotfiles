{ config, pkgs, ... }:

{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
    ];

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  time.timeZone = "Europe/Paris";

  networking = {
    hostName = "mikolaj";
    useDHCP = false;
    interfaces = {
      enp1s0.useDHCP = true;
      wlp2s0.useDHCP = true;
    };
    networkmanager.enable = true;
  };

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
    windowManager.stumpwm.enable = true;
    desktopManager.xterm.enable = false;
    displayManager = {
      startx.enable = true;
      sessionCommands = "${pkgs.xorg.setxkmap}/bin/setxkmap -option caps:escape";
    };
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users.wafelack = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "audio" "sound" "video" ];
    shell = "/run/current-system/sw/bin/fish";
    packages = with pkgs; [
      neofetch
      flameshot
      emacs
      pavucontrol
      stow
      firefox
      gitAndTools.gitFull
      ghc
      sbcl
      clisp
      feh
      ctags
      man-pages
      man-pages-posix
      multimc
      vlc
      unzip
      arandr
      synapse
      tor-browser-bundle-bin
    ];
  };

  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [
      { users = [ "wafelack" ]; persist = true; keepEnv = true; }  
    ];
  };

  environment.systemPackages = with pkgs; [
    (st.overrideAttrs (oldAttrs: rec {
      configFile = writeText "config.def.h" (builtins.readFile "/home/wafelack/.dotfiles/misc/st_config.h");
      postPatch = "${oldAttrs.postPatch}\ncp ${configFile} config.def.h";
    }))
    vim
    wget
    w3m
    fish
    xorg.setxkbmap
    xorg.xmodmap
    nixpkgs-fmt
    pinentry-curses
  ];

  programs = {
    gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
    };
    slock.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
