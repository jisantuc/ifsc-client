import matplotlib.pyplot as plt  # type: ignore
import pandas as pd  # type: ignore

from filters import get_finalist_climbs, get_finalist_tops, get_finalist_zones
from plotting import stripplot_by_round


def event_difficulty_tops(df: pd.DataFrame) -> None:
    stripplot_by_round(
        df,
        get_finalist_climbs,
        lambda df: df.groupby(["eventName", "round", "competitionCategory"]),
        "top",
        None,
    )
    plt.savefig("figures/difficulty-top-percentage-round-event.svg")
    plt.clf()


def year_difficulty_tops(df: pd.DataFrame) -> None:
    stripplot_by_round(
        df,
        get_finalist_climbs,
        lambda df: df.groupby(["round", "year", "competitionCategory"]),
        "top",
        None,
    )
    plt.savefig("figures/difficulty-top-percentage-round-year.svg")
    plt.clf()


def year_difficulty_tops_category(df: pd.DataFrame) -> None:
    stripplot_by_round(
        df,
        get_finalist_climbs,
        lambda df: df.groupby(["round", "year", "competitionCategory"]),
        "top",
        "competitionCategory",
    )
    plt.savefig("figures/difficulty-top-percentage-round-year-category.svg")
    plt.clf()


def alt_difficulty(df: pd.DataFrame) -> None:
    _, axes = plt.subplots(1, 3, figsize=(8, 3))
    axes[1].set_xlim(0.8, 4)
    axes[2].set_xlim(axes[1].get_xlim())

    stripplot_by_round(
        df,
        get_finalist_tops,
        lambda df: df.groupby(["round", "year", "competitionCategory"]),
        "topTries",
        ax=axes[1],
    )
    stripplot_by_round(
        df,
        get_finalist_climbs,
        lambda df: df.groupby(["round", "year", "competitionCategory"]),
        "zone",
        ax=axes[0],
    )
    stripplot_by_round(
        df,
        get_finalist_zones,
        lambda df: df.groupby(["round", "year", "competitionCategory"]),
        "zoneTries",
        ax=axes[2],
    )
    for ax in axes[1:]:
        ax.set_yticklabels([])
        ax.set_ylabel(None)
        plt.tight_layout()
    plt.savefig("figures/difficulty-alt-measures-round-year.svg")
    plt.clf()


def difficulty_plots(df: pd.DataFrame) -> None:
    event_difficulty_tops(df)
    year_difficulty_tops(df)
    alt_difficulty(df)
    year_difficulty_tops_category(df)
