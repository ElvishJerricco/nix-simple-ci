{ haskellPackages, lib }:

let

  cleanSource = builtins.filterSource (path: type:
    lib.cleanSourceFilter path type
    && baseNameOf path != "dist"
  );

in haskellPackages.callCabal2nix "nix-simple-ci" (cleanSource ./.) {}
