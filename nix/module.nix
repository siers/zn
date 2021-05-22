{ pkgs, ... }:

let
  zn = import ~/code/haskell/zn/nix/default.nix {};
  conf = ./zn.conf;

  config = {
    users.users = {
      zn = {
        uid = 1001;
      };
    };

    systemd.services.zn = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      description = "Start zn irc bot";
      serviceConfig = {
        Type = "simple";
        User = "zn";
        Environment = "conf=${conf}";
        ExecStart = ''${zn}/bin/zn'';
        KillSignal = "SIGINT";
      };
    };
  };
in {
  users.users = {
    zn = {
      uid = 1001;
    };
  };

  containers.zn = {
    autoStart = true;
    ephemeral = true;

    bindMounts = {
      "/data" = {
        hostPath = "/home/zn/zn/data";
        isReadOnly = false;
      };
    };

    inherit config;
  };

  virtualisation.docker.enable = true;

  virtualisation.oci-containers = {
    backend = "docker";

    containers = {
      opennsfw = {
        image = "siers/caffe-open-nsfw-server:latest";
        ports = ["127.0.0.1:80:80"];
        volumes = [ "/home/zn/zn/data/down:/data" ];
      };
    };
  };
}
