`ifsc-client`
====

This repo holds a small client CLI and analysis scripts for asking questions
above the history of [IFSC] results.

Development
-----

tl;dr:

```bash
nix develop
npm install
# choose a year to write results to csv
purs-nix run season-to-csv -y 2021 -u https://components.ifsc-climbing.org
```

The CLI program is written in PureScript. All source for the CLI program can be
found in `./src`. Analysis scripts are in Python. All Python source can be found
in `./analysis`. If you'd like to run the CLI program or the Python scripts, your
best bet is getting set up with [`nix`], enabling [flakes], and running
[`nix develop`] to drop into a dev shell with everything available.

PureScript dependencies can be found in the `flake.nix` `ps = ...` block. Python
dependencies can be found in `analysis.nix`. You're free to get things set up
however you want, but the 

Disclaimers
-----

I can't re-publish the downloaded data because the [IFSC terms and conditions] explicitly
forbid it. I'm choosing to use the data gathered here under the terms that I've downloaded
it for caching purposes. If you choose to use the CLI program to download the data for your
own purposes, you should consider whether you think the activity is forbidden by those terms
and conditions. I believe that use here is not automated data collection / data harvesting
since it's not automated and the interaction with IFSC is restricted to well-formed HTTP
requests to an unauthenticated REST API. You should make your own judgments on that front.

[flakes]: https://nixos.wiki/wiki/Flakes
[`nix`]: https://nixos.org/download.html
[`nix develop`]: https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-develop.html
[IFSC]: https://www.ifsc-climbing.org/
[IFSC terms and conditions]: https://www.ifsc-climbing.org/index.php/2-uncategorised/67-terms-and-conditions
