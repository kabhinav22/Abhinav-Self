html = read_html("https://www.icc-cricket.com/rankings/womens/player-rankings/odi/batting")
ranking = html_elements(html, ".rankings-table__pos-number")
ranking = html_text(ranking)
ranking
as.numeric(ranking)

html = read_html("https://www.icc-cricket.com/rankings/womens/player-rankings/odi/batting")
name = html_elements(html, ".table-body__cell rankings-table__name name a")
name = html_text(name)
name


html = read_html("https://www.icc-cricket.com/rankings/womens/player-rankings/odi/batting")
team = html_elements(html, ".table-body__logo-text")
team = html_text(team)
team


html = read_html("https://www.icc-cricket.com/rankings/womens/player-rankings/odi/batting")
rating = html_elements(html, ".table-body__cell rating")
rating = html_text(rating)
rating

icc_rank = data.frame(ranking, name, team, rating)


html = read_html("https://www.icc-cricket.com/rankings/womens/player-rankings/odi/batting")
tt = html_elements(html, ".table-body__cell rankings-table__name name a")
tt = html_text(tt)
tt
