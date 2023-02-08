from pathlib import Path

import pandas as pd  # type: ignore

from difficulty import difficulty_plots
from separation import separation_figures


def validate_load_csv(p: Path) -> pd.DataFrame:
    head = pd.read_csv(p, nrows=1)
    expected_header = [
        "year",
        "eventName",
        "competitorName",
        "rank",
        "round",
        "top",
        "topTries",
        "zone",
        "zoneTries",
        "competitionCategory",
    ]
    assert head.columns.tolist() == expected_header
    return pd.read_csv(p)


def load_csvs(dir_rel_path: str) -> pd.DataFrame:
    directory = Path(dir_rel_path)
    csvs = directory.glob("*.csv")
    return pd.concat(validate_load_csv(f) for f in csvs)


def main() -> None:
    df = load_csvs("./ard")
    separation_figures(df)
    difficulty_plots(df)


if __name__ == "__main__":
    main()
