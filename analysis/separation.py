import pandas as pd  # type: ignore


def _ranks_for_sorted_ser(df: pd.DataFrame) -> pd.Series:
    last_place = len(df)
    out = pd.Series(
        [x + 1 for x in range(last_place)],
        name="withinRoundRank",
    )
    out.index = df.index.get_level_values("competitorName")
    return out


def _separated_by(diff_series: pd.Series):
    top_diff = diff_series["top"]
    zone_diff = diff_series["zone"]
    top_attempts_diff = diff_series["topTries"]
    zone_attempts_diff = diff_series["zoneTries"]

    if pd.isna(top_diff):
        return "last"
    elif top_diff > 0:
        return "top"
    elif zone_diff > 0:
        return "zone"
    elif top_attempts_diff < 0:
        return "top tries"
    elif zone_attempts_diff < 0:
        return "zone tries"
    else:
        return "countback"


def score_df(df: pd.DataFrame) -> pd.DataFrame:
    grouped_by_competitor = df.groupby(
        ["year", "eventName", "competitionCategory", "round", "competitorName"]
    )
    tops = grouped_by_competitor["top"].sum()
    zones = grouped_by_competitor["zone"].sum()
    attempt_scores = grouped_by_competitor[["topTries", "zoneTries"]].sum()
    round_scores = pd.concat([tops, zones, attempt_scores], axis=1)
    sorted = round_scores.sort_values(
        by=[
            "year",
            "eventName",
            "competitionCategory",
            "round",
            "top",
            "zone",
            "topTries",
            "zoneTries",
        ],
        ascending=[True, True, True, True, False, False, True, True],
    )
    ranks = sorted.groupby(
        level=["year", "eventName", "competitionCategory", "round"]
    ).apply(_ranks_for_sorted_ser)
    score_ranks = pd.concat([sorted, ranks], axis=1)
    next_rank_scores = score_ranks.groupby(
        level=["year", "eventName", "competitionCategory", "round"]
    ).apply(lambda x: x.shift(-1))
    separation = score_ranks - next_rank_scores
    separated_by = separation.apply(_separated_by, axis=1)
    separated_by.name = "separatedBy"
    return pd.concat([score_ranks, separated_by], axis=1)


def separation_by_rank(df: pd.DataFrame) -> pd.DataFrame:
    # procedure --
    # grouping by year, show frequency with which each rank one through seven
    # is decided by each kind of separation
    pass
