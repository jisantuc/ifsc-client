from pathlib import Path
from typing import List

import pandas as pd  # type: ignore


from difficulty import difficulty_plots
from separation import score_df


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


def get_unique_years(df: pd.DataFrame) -> List[int]:
    return df["year"].unique().tolist()


def main():
    df = load_csvs("./ard")
    scored = score_df(df)
    scored.to_csv("ranks-sanity-check.csv")
    # difficulty_plots(df)


if __name__ == "__main__":
    main()
