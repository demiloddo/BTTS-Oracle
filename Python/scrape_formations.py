#!/usr/bin/env python3

import datetime
import unicodedata
from typing import List, Tuple

import bs4
import requests


def normalize_nb_space(s: str) -> str:
    return unicodedata.normalize("NFKD", s).replace("  ", " ")


def rm_newline(s: str) -> str:
    return s.lstrip().rstrip()


def aggregate_roles(players: List[Tuple[str, ...]]) -> List[Tuple[str, ...]]:
    return [(player, role, role[0], team) for player, role, team in players]


def write_lineups(players: List[Tuple[str, ...]]):
    # default to write lineup on a new file each time to avoid (maybe harmful)
    # override
    fname = f"lineups_{datetime.datetime.now().isoformat()}.csv"
    with open(fname, "w") as fd:
        header = ",".join(("name", "role", "aggregate_role", "team"))
        fd.write(header + "\n")
        for el in players:
            content = ",".join(el)
            fd.write(content + "\n")
    print(f"Wrote available lineups to {fname}")


# NOTE this website also gives:
# - players that are not in the lineup due to injury or suspension
# - weather information, where available
LINEUPS_URL = "https://www.rotowire.com/soccer/lineups.php?league=SERI"

n_lineups, lineups = 0, []

print(f"Requesting lineups from {LINEUPS_URL}")
req = requests.get(LINEUPS_URL)
soup = bs4.BeautifulSoup(req.content, "html.parser")

lineups_panel = soup.find("div", {"class": "lineups"})

for lineup_anchor in lineups_panel.find_all("div", {"class": "lineup is-soccer"}):
    lineup_ready = True

    match_panel = lineup_anchor.find("div", {"class": "lineup__box"})
    # playing teams
    for team_sfx in ["home", "visit"]:

        # lineups
        lineup_panel = lineup_anchor.find("div", {"class": "lineup__main"})
        players_list = lineup_panel.find("ul", {"class": f"lineup__list is-{team_sfx}"})

        status = rm_newline(players_list.find("li", {"class": "lineup__status"}).text)
        # skip matches that didn't announce lineup yet
        if status == "Unknown Lineup":
            lineup_ready = False
            break

        team_name = rm_newline(
            match_panel.find("div", {"class": f"lineup__team is-{team_sfx}"}).div.text
        )
        print(f"Team: {team_name}")

        players = players_list.find_all("li", {"class": "lineup__player"})
        # filter injured players
        players = [
            player_li
            for player_li in players
            if not player_li.find("span", {"class": "lineup__inj"})
        ]
        for player_li in players:
            position = rm_newline(player_li.find("div", {"class": "lineup__pos"}).text)
            name = rm_newline(player_li.find("a").text)
            print(f"Player: {name:<25} :: {position}")
            lineups.append((name, position, team_name))

    # time of match
    if lineup_ready:
        n_lineups += 1
        timestamp = rm_newline(
            normalize_nb_space(lineup_anchor.find("div", "lineup__time").text)
        )
        print(f"Time: {timestamp}")
        print("----------------------------")

print(f"# announced lineups: {n_lineups}")

lineups_aggr = aggregate_roles(lineups)


if __name__ == "__main__":
    write_lineups(lineups_aggr)
