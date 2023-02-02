{ pkgs, ... }:
  let
    pythonPackages = (ps: [
      # ps.duckdb
      ps.pandas
      ps.seaborn
    ]);
    python = with pkgs; rec {
      python = pkgs.python311.withPackages pythonPackages;
      mypyPath = "${python}/bin";
    };
  in python
