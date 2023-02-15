`ifsc-client`
====

This repo holds a small client CLI and analysis scripts for asking questions
above the history of [IFSC] results.

Organization
-----

There are two components to the repository: a CLI client for fetching IFSC data and writing
it to CSV and a few python scripts for creating plots and figures from those data.

### CLI client

The CLI client is written in [PureScript]. Its source files live in [`./src`](./src) and [`./test`](./test).

Tests depend on having a running mock of some of the IFSC endpoints. That mocking and source
JSON files to serve at specific endpoints can be found in [`wiremock-mappings.json`](./wiremock-mappings.json),
and the test server can be started with `docker compose -f docker-compose.test.yml up -d`.

The easiest way to run the CLI program is with [`nix`]. To do so, you can run:

```bash
nix develop .#cli-client # 1
npm install # 2
# choose a year to write results to csv
purs-nix run season-to-csv -y 2021 -u https://components.ifsc-climbing.org # 3

docker compose -f docker-compose.test.yml up -d # 4
purs-nix test # 5
```

This series of commands will:

1. enter the `cli-client` development shell configured in this repo's `flake.nix`
2. install node modules specified in the `package-lock.json` file
3. run the `season-to-csv` command for year 2021, using the live IFSC service as a base URL
4. start a mock server to run tests against
5. run all of the client's tests

You could also technically run the CLI with [`spago`]. To do so, you'd need to
copy project configuration over to your `package.dhall` and `spago.dhall` files.
If you've attempted this and believe it should have worked but it didn't for some
reason, please open an issue.

### Analysis scripts

Once you have all of the data, you can analyze it. I threw all of the CSVs into a
folder at the top of this directory called `ard` (for "analysis-ready data").
You can see the whole workflow in the `main` function defined in
[`analysis/questions.py`](./analysis/questions.py). In short, it's:

1. read the data
2. make the difficulty figures
3. make the separation figures

More information is available in the [`analysis` README`](./analysis/README.md).

Development
-----

### With `nix`

tl;dr:

```bash
nix develop .#cli-client
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
however you want, but you'll have a much easier time with `nix`.

### Without `nix`

If you'd like this, please leave a thumbs up on #11.

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
[PureScript]: https://www.purescript.org/
[`spago`]: https://github.com/purescript/spago
