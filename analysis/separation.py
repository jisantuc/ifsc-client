import matplotlib.pyplot as plt  # type: ignore
import pandas as pd  # type: ignore
from pandas.io.formats.style import Styler  # type: ignore
import seaborn as sns  # type: ignore

separation_priority_order = ["top", "zone", "top tries", "zone tries", "countback"]


def _ranks_for_sorted_ser(df: pd.DataFrame) -> pd.Series:
    last_place = len(df)
    out = pd.Series(
        [x + 1 for x in range(last_place)],
        name="withinRoundRank",
    )
    out.index = df.index.get_level_values("competitorName")
    return out


def _separated_by(diff_series: pd.Series) -> str:
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


def separation_by_rank(df: pd.DataFrame, round: str) -> pd.DataFrame:
    return (
        df.xs(round, level="round")
        .groupby("withinRoundRank")["separatedBy"]
        .value_counts()
        .rename(None)
        .reset_index(level="separatedBy")
        .pivot(columns="separatedBy")
        .droplevel(0, axis=1)[separation_priority_order]
    )


def separation_by_year_rank(df: pd.DataFrame, round: str) -> pd.DataFrame:
    return (
        df.xs(round, level="round")
        .groupby(["year", "withinRoundRank"])["separatedBy"]
        .value_counts()
        .rename(None)
        .reset_index(level="separatedBy")
        .pivot(columns="separatedBy")
        .droplevel(0, axis=1)[separation_priority_order]
    )


def separation_by_year_rank_category(df: pd.DataFrame, round: str) -> pd.DataFrame:
    return (
        df.xs(round, level="round")
        .groupby(["year", "competitionCategory", "withinRoundRank"])["separatedBy"]
        .value_counts()
        .rename(None)
        .reset_index(level="separatedBy")
        .pivot(columns="separatedBy")
        .droplevel(0, axis=1)[separation_priority_order]
    )


def style_table(style: Styler) -> Styler:
    style.background_gradient(axis=1, vmin=0, cmap="Greens")
    style.set_table_styles(
        [
            {
                "selector": "th",
                "props": "font-weight: normal; padding-left: 0.5rem; padding-right: 0.5rem;",
            }
        ]
    )
    style.set_properties(**{"text-align": "center"})
    style.format(int)
    return style


def write_table_html(df: pd.DataFrame, filename: str) -> None:
    with open(filename, "w") as outf:
        outf.write(
            df.fillna(0).style.pipe(style_table).to_html(na_rep=0, bold_headers=False)
        )


def separation_tables(df: pd.DataFrame) -> None:
    final_separation = separation_by_rank(df, "Final")
    womens_results = df.xs("Women", level="competitionCategory")
    mens_results = df.xs("Men", level="competitionCategory")
    write_table_html(final_separation.loc[:5], "figures/final-separation.html")
    women_final_separation = separation_by_rank(womens_results, "Final")
    write_table_html(
        women_final_separation.loc[:5], "figures/final-separation-women.html"
    )
    men_final_separation = separation_by_rank(mens_results, "Final")
    write_table_html(men_final_separation.loc[:5], "figures/final-separation-men.html")

    semifinal_separation = separation_by_rank(df, "SemiFinal")
    write_table_html(semifinal_separation.loc[:6], "figures/semifinal-separation.html")
    women_semifinal_separation = separation_by_rank(womens_results, "SemiFinal")
    write_table_html(
        women_semifinal_separation.loc[:6], "figures/semifinal-separation-women.html"
    )
    men_semifinal_separation = separation_by_rank(mens_results, "SemiFinal")
    write_table_html(
        men_semifinal_separation.loc[:6], "figures/semifinal-separation-men.html"
    )


def separation_plots(df: pd.DataFrame) -> None:
    idx = pd.IndexSlice
    semifinal_separation = pd.DataFrame(
        separation_by_year_rank(df, "SemiFinal").stack().loc[idx[:, 1:6, :]]
    )
    semifinal_separation["round"] = "SemiFinal"
    semifinal_separation.set_index("round", append=True, inplace=True)
    final_separation = pd.DataFrame(
        separation_by_year_rank(df, "Final").stack().loc[idx[:, 1:5, :]]
    )
    final_separation["round"] = "Final"
    final_separation.set_index("round", append=True, inplace=True)
    long_form_data = (
        pd.concat([final_separation, semifinal_separation])
        .drop(2020, level="year")
        .groupby(level=["year", "round", "separatedBy"])
        .sum()
    )
    sns.relplot(
        long_form_data,
        x="year",
        y=long_form_data[0].tolist(),
        hue="separatedBy",
        col="round",
        kind="line",
        errorbar=None,
        height=3,
        aspect=1.3,
    )
    plt.savefig("figures/separation-type-over-time.svg")


def separation_figures(df: pd.DataFrame) -> None:
    scored = score_df(df)
    separation_tables(scored)
    separation_plots(scored)
