{
  description = "Wayfinder — autonomous agentic framework";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    {
      nixosModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.services.wayfinder;
        in
        {
          options.services.wayfinder = {
            enable = lib.mkEnableOption "Wayfinder";

            configFile = lib.mkOption {
              type = lib.types.path;
              default = "/etc/wayfinder/wayfinder.edn";
            };

            promptsDir = lib.mkOption {
              type = lib.types.path;
              default = "${self}/prompts";
            };

            stateDir = lib.mkOption {
              type = lib.types.path;
              default = "/var/lib/wayfinder";
            };

            user = lib.mkOption {
              type = lib.types.str;
              default = "wayfinder";
            };
          };

          config = lib.mkIf cfg.enable {
            users.users.${cfg.user} = {
              isNormalUser = true;
              group = cfg.user;
              home = "/home/wayfinder";
              createHome = true;

              packages = with pkgs; [
                git
                zsh
                coreutils
                python3
                jq
                pandoc
              ];
            };
            users.groups.${cfg.user} = {};

            systemd.services.wayfinder = {
              wantedBy = [ "multi-user.target" ];
              after = [ "network-online.target" ];
              wants = [ "network-online.target" ];

              path = [
                # Wayfinder's own scripts land in ~/bin so it can extend
                # itself at runtime.
                "/home/wayfinder/bin"
                pkgs.git
                pkgs.zsh
                pkgs.coreutils
                pkgs.nix
                # kiwix-search: full-text search against the wikipedia zim
                # mounted at /srv/wikipedia/latest.
                pkgs.kiwix-tools
              ];

              environment = {
                SHELL_PATH = "${pkgs.zsh}/bin/zsh";
                WAYFINDER_CONFIG = cfg.configFile;
              };

              serviceConfig = {
                Type = "simple";
                User = cfg.user;
                Group = cfg.user;
                WorkingDirectory = "${self}";
                ExecStart = "${pkgs.clojure}/bin/clojure -M:run";
                Restart = "on-failure";
                RestartSec = "10";
                StateDirectory = "wayfinder";

                # Sandbox
                ProtectSystem = "strict";
                ReadWritePaths = [
                  cfg.stateDir
                  "/home/wayfinder"
                  "/srv/wayfinder"
                  # The Nix daemon socket lives here; the nix client needs RW
                  # to connect when wayfinder runs nix-shell / nix develop.
                  "/nix/var/nix/daemon-socket"
                ];
                NoNewPrivileges = true;
                PrivateTmp = true;
                ProtectHome = "read-only";
                # AF_UNIX: required to reach the nix daemon socket.
                RestrictAddressFamilies = [
                  "AF_UNIX"
                  "AF_INET"
                  "AF_INET6"
                ];
                ProtectKernelTunables = true;
                ProtectKernelModules = true;
                ProtectClock = true;
                CapabilityBoundingSet = [ "" ];
                LockPersonality = true;
                ProtectHostname = true;
                ProtectControlGroups = true;
                RestrictRealtime = true;
                RestrictSUIDSGID = true;
                SystemCallFilter = [ "@system-service" "~@privileged" ];
                SystemCallErrorNumber = "EPERM";
              };
            };

            systemd.tmpfiles.rules = [
              "d /srv/wayfinder 0755 ${cfg.user} ${cfg.user} -"
              "d /srv/wayfinder/www 0755 ${cfg.user} ${cfg.user} -"
            ];
            services.nginx = {
              enable = true;
              virtualHosts."wayfinder.doppel.moe" = {
                root = "/srv/wayfinder/www"; 
                enableACME = true;
                forceSSL = true;
                locations."/" = {
                  index = "index.html";
                  tryFiles = "$uri $uri/ =404";
                };
              };
            };
          };
        };

      devShells.x86_64-linux.default =
        let
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
        in
        pkgs.mkShell {
          buildInputs = with pkgs; [ clojure jdk21 git zsh ];
        };
    };
}
