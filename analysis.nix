{ pkgs, ... }:
  let
    pythonPackages = (ps: [
      # ps.duckdb
      ps.pandas
      ps.seaborn
    ]);
    python = with pkgs; {
      out = pkgs.python311.withPackages pythonPackages;
    };
  in python
