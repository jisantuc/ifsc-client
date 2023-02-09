import pandas as pd  # type: ignore


def get_finalist_climbs(df: pd.DataFrame) -> pd.DataFrame:
    event_grouped = df.groupby(["eventName", "competitorName"])
    return event_grouped.filter(lambda x: "Final" in x["round"].unique())


def get_finalist_tops(df: pd.DataFrame) -> pd.DataFrame:
    finalist_climbs = get_finalist_climbs(df)
    return finalist_climbs[finalist_climbs["top"]]


def get_finalist_zones(df: pd.DataFrame) -> pd.DataFrame:
    finalist_climbs = get_finalist_climbs(df)
    return finalist_climbs[finalist_climbs["zone"]]
