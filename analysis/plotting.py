from typing import Callable, Optional


import matplotlib.pyplot as plt  # type: ignore
import pandas as pd  # type: ignore
from pandas.core.groupby import GroupBy
import seaborn as sns  # type: ignore


_round_order = ["Qualification", "SemiFinal", "Final"]


def stripplot_by_round(
    df: pd.DataFrame,
    data_filter: Callable[[pd.DataFrame], pd.DataFrame],
    grouper: Callable[[pd.DataFrame], GroupBy],
    column: str,
    facet_col: Optional[str] = None,
    ax: Optional[plt.Axes] = None,
) -> None:
    filtered = data_filter(df)
    grouped = grouper(filtered)
    plottable = grouped[column].mean().reset_index()
    sns.stripplot(
        plottable, x=column, y="round", hue=facet_col, order=_round_order[::-1], ax=ax
    )
    plt.tight_layout()
