# NBA Boxscore Dashboard

Shiny Dashboard tracking the 2022-23 NBA Season through the scope of boxscore numbers.

Get quick and easy insights into different aspects of a team's or players' performance. Measures such as...
- How a team/player is performing this season
    - If a team/player is playing above or below expectation
- Which stats relate to wins and losses
- A player's or team's rank compared to others

### Motivation:
I am an active follower in the NBA communities on both Twitter and Reddit (As a Kings fan, this season has been much more bearable to watch lol). It is very common to see an argument for a player or team, good or bad, backed up with statistics as it provides good discussion and banter. Seeing this inspired me to design a dashboard that easily tracks these commonly used stats I saw people bring up. I thought it would be nice to connect my hobby for sports (more recently the NBA) with my data skills.

### Technology Used:
- Python: Pandas, Selenium, BeautifulSoup
- R: tidyverse, plotly, ggplot, rpart, shiny/shinydashboard

### Future Improvements:
1. Ranking according to position:
At the moment, I have league-wide rankings for teams (reasonable) and players (not so reasonable). More recently, I have extracted data from NBA.com about player positions. In the future, I plan to implement position rankings, so that it is easier to understand how a player compares to others in their position.

2. Comparison tab:
I would like to eventually add a comparison tab. In a previous iteration of this dashboard, I implemented a team comparison between offense and defense (before removing it). As I see more discussion on Twitter and Reddit comparing teams and players, I would eventually like to bring back a more engaging implementation. 

3. League-Wide Stat Leaders:
Likewise to improvement (2), league-wide stat leaders are another popular topic among the NBA community. I had previously implemented this idea but eventually scrapped it since I felt the graphics were too redundant. Bringing this idea back in a more intuitive manner I something I will eventually complete.