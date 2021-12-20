{ config, pkgs, lib, ... }:

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
    displayManager.startx.enable = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "steam-original"
    "steam"
    "steam-runtime"
  ];
  programs.steam.enable = true;


  users.users.wafelack = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "audio" "sound" "video" "docker" ];
    shell = "/run/current-system/sw/bin/fish";
    packages = with pkgs;
      let
        tectonic-fixed = with pkgs; rustPlatform.buildRustPackage rec {
          pname = "tectonic";
          version = "0.8.0";

          src = fetchFromGitHub {
            owner = "tectonic-typesetting";
            repo = "tectonic";
            rev = "09b15758b41fe25d6a8a1e3b6f475336fa72a667";
            fetchSubmodules = true;
            sha256 = "sha256-FY8/2F1HE/I6/NiK7rulYFZjerYho0yJi7sCXl5aR44=";
          };

          cargoSha256 = "sha256-P2j5VloAX5k4YizbdyyLO7+0dBeDtEGg5NLeIrwKYt0=";

          nativeBuildInputs = [ pkg-config makeWrapper ];

          buildInputs = [ fontconfig harfbuzz openssl icu ]
            ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ ApplicationServices Cocoa Foundation ]);

          # Tectonic runs biber when it detects it needs to run it, see:
          # https://github.com/tectonic-typesetting/tectonic/releases/tag/tectonic%400.7.0
          postInstall = ''
            wrapProgram $out/bin/tectonic \
              --prefix PATH "${lib.getBin biber}/bin"
          '' + lib.optionalString stdenv.isLinux ''
            substituteInPlace dist/appimage/tectonic.desktop \
              --replace Exec=tectonic Exec=$out/bin/tectonic
            install -D dist/appimage/tectonic.desktop -t $out/share/applications/
            install -D dist/appimage/tectonic.svg -t $out/share/icons/hicolor/scalable/apps/
          '';

          doCheck = true;

          meta = with lib; {
            description = "Modernized, complete, self-contained TeX/LaTeX engine, powered by XeTeX and TeXLive";
            homepage = "https://tectonic-typesetting.github.io/";
            changelog = "https://github.com/tectonic-typesetting/tectonic/blob/tectonic@${version}/CHANGELOG.md";
            license = with licenses; [ mit ];
            maintainers = [ maintainers.lluchs maintainers.doronbehar ];
          };
        }; 
        torBrowserWithAudio = pkgs.tor-browser-bundle-bin.override {
          mediaSupport = true;
          pulseaudioSupport = true;
        }; in
      [
        tectonic-fixed
        anki
        kdenlive
        drawio
        himalaya
        pass
        openscad
        steam
        irssi
        killall
        acpi
        clang
        screenkey
        inkscape
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
        torBrowserWithAudio
        screen
        universal-ctags
        redshift
        simplescreenrecorder
        tree
	qbittorrent
      ];
  };

  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [
      { users = [ "wafelack" ]; persist = true; keepEnv = true; }
    ];
  };
   
nix.nixPath = [
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
    "nixos-config=/etc/nixos/configuration.nix"
    "/nix/var/nix/profiles/per-user/root/channels"
  ];

  environment.systemPackages = with pkgs; [
    (st.overrideAttrs (oldAttrs: rec {
      configFile = writeText "config.def.h" (builtins.readFile "/home/wafelack/.dotfiles/st/config.h");
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
    pinentry-qt
  ];

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    slock.enable = true;
  };

  fonts.fonts =
    let
      dunda = pkgs.stdenv.mkDerivation {
        name = "dunda-zlm";
        version = "0.1";
        src = pkgs.fetchFromGitHub {
          owner = "jackhumbert";
          repo = "zbalermorna";
          rev = "920b28d798ae1c06885c674bbf02b08ffed12b2f";
          sha256 = "00sl3f1x4frh166mq85lwl9v1f5r3ckkfg8id5fibafymick5vyp";
        };
        installPhase =
          let
            fontsDir  = "$out/share/fonts";
          in
          ''
            mkdir -p "${fontsDir}"
            cp "$src/fonts/dunda-regular.otf" "${fontsDir}"
          '';
      };
      crisa = pkgs.stdenv.mkDerivation {
        name = "crisa-zlm";
        version = "0.1";
        src = pkgs.fetchFromGitHub {
          owner = "jackhumbert";
          repo = "zbalermorna";
          rev = "920b28d798ae1c06885c674bbf02b08ffed12b2f";
          sha256 = "00sl3f1x4frh166mq85lwl9v1f5r3ckkfg8id5fibafymick5vyp";
        };
        installPhase =
          let
            fontsDir  = "$out/share/fonts";
          in
          ''
            mkdir -p "${fontsDir}"
            cp "$src/fonts/crisa-regular.otf" "${fontsDir}"
          '';
      };
    in
    with pkgs; [
      unifont
      iosevka
      dunda
      crisa
      libertine
    ];

  virtualisation.docker.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
