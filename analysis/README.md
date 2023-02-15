`analysis`
=====

This directory contains analysis scripts for IFSC data.
The data are assumed to follow the format output by the
CLI client in the root of this repository.

These scripts require tabular data, where rows represent
a climber's attempts on a boulder, and columns are:

- `year`: (`str`) an integer representing the year of the competition
- `eventName`: (`str`) the name of the event
- `competitorName`: (`str`) the competitor's name
- `rank`: (`int`) the competitor's overall rank within the competition
- `round`: (`enum`) one of `Qualification`, `SemiFinal`, or `Final`
- `top`: (`int`, but only one or zero) whether the climber topped the boulder
- `topTries`: (`int`) how many tries it took the climber to top the boulder
- `zone`: (`int`, but only 1 or 0) whether the climber achieved the zone hold on the boulder
- `zoneTries`: (`int`) how many tries it took the climber to reach the zone on the boulder
- `competitionCategory`: (`enum`) one of `Men` or `Women`

If you want to run these scripts, then, from the root of the repository, you can run:

```bash
mkdir ./figures # 1
nix develop .#analysis # 2
python3 analysis/questions.py # 3
```

This snippet:

1. makes a `figures/` directory to hold output
2. enters the `analysis` development environment specified in this repository's
   [`flake.nix`](../flake.nix).
3. runs the [`questions.py`](./questions.py) script

This will work _if and ony if_:

- you run it from the root of the repository
- you've already downloaded some data to the `ard` directory

You can change the directory that the script looks in by editing the
`load_csvs` call in `questions.py`.

The scripts depend on:

```
pandas==1.5.2
seaborn==0.12.1
jinja2==3.1.2
tabulate==0.9.0
```

These provide:

1. `pandas`: general purpose data munging
2. `seaborn`: pretty plots that work well with `pandas` `DataFrames`
3. `jinja2`: templating functionality, required by pandas to write HTML output
4. `tabulate`: markdown support for pandas

You can choose whatever method you want to set up a python environment with these dependencies,
and it Should Work:tm:.
