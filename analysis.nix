{ pkgs, ... }:
let
  pythonPackages = (ps: [
    ps.black
    ps.jinja2
    ps.pandas
    ps.seaborn
    ps.tabulate # markdown support for pandas
  ]);
  python = with pkgs; rec {
    python = pkgs.python311.withPackages pythonPackages;
  };
in
python
