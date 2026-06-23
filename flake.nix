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
              isSystemUser = true;
              group = cfg.user;
              home = cfg.stateDir;
              createHome = true;
            };
            users.groups.${cfg.user} = {};

            systemd.services.wayfinder = {
              wantedBy = [ "multi-user.target" ];
              after = [ "network-online.target" ];
              wants = [ "network-online.target" ];

              path = [ pkgs.git pkgs.zsh pkgs.coreutils ];
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
