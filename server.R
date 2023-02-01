library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(rpart)
library(rpart.plot)

function(input, output, session) {
  ##### SETUP ######
  rv = reactiveValues(
    boxscore = read_csv("Regular_Season_H2H_Boxscore_2022-23.csv") %>%
      mutate(`GAME DATE`=as.Date(`GAME DATE`, format="%m/%d/%Y")),
    teams = c("BOS","MEM","DAL","ORL","HOU","UTA","LAC","DEN","ATL",
              "SAS","NOP","OKC","PHI","CLE","MIN","CHA","LAL","CHI",
              "NYK","MIL","BKN","IND","TOR","POR","DET","SAC","PHX",
              "MIA","GSW","WAS"), # delete if not needed
    team_color = c("BOS"="#008348","MEM"="#5d76a9","DAL"="#0053bc","ORL"="#0077c0","HOU"="#ce1141","UTA"="#0d2240","LAC"="#c8102e","DEN"="#244289","ATL"="#e03a3e",
                   "SAS"="#c4ced4","NOP"="#b4975a","OKC"="#007ac1","PHI"="#006bb6","CLE"="#6f263d","MIN"="#236192","CHA"="#00788c","LAL"="#552583","CHI"="#ce1141",
                   "NYK"="#f58426","MIL"="#00471b","BKN"="black","IND"="#fdbb30","TOR"="#b4975a","POR"="#e03a3e","DET"="#1d428a","SAC"="#5a2b81","PHX"="#1d1160",
                   "MIA"="#98002e","GSW"="#006bb6","WAS"="#002b5c"),
    box_color = c("BOS"="green","MEM"="light-blue","DAL"="blue","ORL"="light-blue","HOU"="red","UTA"="yellow","LAC"="red","DEN"="navy","ATL"="red",
                       "SAS"="black","NOP"="navy","OKC"="aqua","PHI"="blue","CLE"="maroon","MIN"="light-blue","CHA"="teal","LAL"="yellow","CHI"="red",
                       "NYK"="blue","MIL"="olive","BKN"="black","IND"="yellow","TOR"="red","POR"="red","DET"="red","SAC"="purple","PHX"="orange",
                       "MIA"="red","GSW"="blue","WAS"="navy"),
    players = read_csv("Regular_Season_Player_2022-23.csv") %>%
      mutate(`GAME DATE`=as.Date(`GAME DATE`, format="%m/%d/%Y")),
    current_player = NULL,
    cum_current_player = NULL,
    nba_player_avg = NULL,
    current_team = NULL,
    cum_current_team = NULL,
    nba_team_avg = NULL
  )
  observeEvent(input$team, {
    # update current team
    rv$current_team = rv$boxscore %>% filter(`TEAM` == input$team)
    # store cumulative team boxscore (for convergence)
    rv$cum_current_team = rv$current_team %>%
      arrange(`GAME DATE`) %>% # oldest to newest for each player
      mutate(across(where(is.numeric), cummean)) %>%
      mutate(`WIN%`=cumsum(`W/L` == "W")/1:n()) %>%
      arrange(desc(`GAME DATE`)) # newest at top (easier subsetting)
  })
  observeEvent(input$player, {
    # update player boxscore
    rv$current_player = rv$players[rv$players$`PLAYER` == input$player,]
    # store cumulative current player boxscore (for convergence)
    rv$cum_current_player = rv$current_player %>%
      arrange(`GAME DATE`) %>% # oldest to newest for each player
      mutate(across(where(is.numeric), cummean)) %>%
      arrange(desc(`GAME DATE`)) # newest at top (easier subsetting)
  })
  observeEvent(input$player_date_range, {
    # update player boxscore
    rv$current_player = rv$players[rv$players$PLAYER == input$player,] %>%
      filter(`GAME DATE` >= input$player_date_range[1]) %>%
      filter(`GAME DATE` <= input$player_date_range[2])
    # store cumulative current player boxscore
    rv$cum_current_player = rv$current_player %>%
      arrange(`GAME DATE`) %>% # oldest to newest for each player
      mutate(across(where(is.numeric), cummean)) %>%
      arrange(desc(`GAME DATE`)) # newest at top (easier subsetting)
  })
  observe({
    # store player league averages
    rv$nba_player_avg = rv$players %>%
      summarise(across(where(is.numeric), mean, na.rm=T))
    # store team league averages
    rv$nba_team_avg = rv$boxscore %>%
      summarise(across(where(is.numeric), mean, na.rm=T))
  })
  get_thumbs_icon = function(value, col, high_good=T, team=F) {
    league = rv$nba_player_avg[1,col] %>% pull(col)
    if (team) {league = rv$nba_team_avg[1,col]}
    temp_icon = icon("fa-solid fa-thumbs-up", verify_fa=F)
    if (high_good) {
      if (value < league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa=F)}
    } else {
      if (value >= league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa=F)}
    }
    return(list(icon=temp_icon, league=league))
  }
  
  
  ##### TEAM DASHBOARD ######
  get_team_rank = function(col, descend=T, returnTable=F) {
    x = rv$boxscore %>% group_by(`TEAM`) %>%
      summarize_at(c(col), mean, na.rm=T)
    x = x[order(x[,col], decreasing=descend),] %>% mutate("Rank"=1:n())
    if (returnTable) {
      x = x[x$`TEAM` == input$team,] %>% mutate(`Stat`=col) %>% rename("Value"=col) %>%
        mutate_at(vars("Value"), funs(round(., 1))) %>% select(4,2,3)
      return(x)
    }
    return(x[x$`TEAM` == input$team,"Rank"][[1]])
  }
  # Box for Logo and Record
  output$team_logo = renderImage({
    list(src=sprintf(".\\logos\\%s.png", input$team),
         width="35%")
  }, deleteFile=F)
  output$team_record = renderUI({
    last10 = table(head(rv$cum_current_team$`W/L`, 10)) # [1] L [2] W
    counts = rv$current_team %>% count(`W/L`) %>% pull(`n`)
    record = sprintf("Record: %d - %d (%.3f)", counts[2], counts[1], counts[2]/(counts[1]+counts[2]))
    last10_record = sprintf("%d - %d)", last10[2], last10[1])
    HTML(paste("<center><b>", record, "</b>(Last 10: ", last10_record, "</center>"))
  })
  # Team Plot 11 and Plot 12
  observeEvent(input$team_box11, {
    if (input$team_box11 == "Shooting") {
      output$team_shoot = renderPlotly({
        fig = rv$cum_current_team %>% arrange(`GAME DATE`) %>% # oldest to newest
          plot_ly(x=~`GAME DATE`, y=~`3P%`, type="scatter", mode="lines+markers",
                  name="3P%") %>%
          add_trace(y=~`2P%`, type="scatter", mode="lines+markers", name="2P%") %>%
          add_trace(y=~`FT%`, type="scatter", mode="lines+markers", name="FT%") %>%
          add_trace(y=~`WIN%`, name="Win %", mode="lines", yaxis="y2", 
                    line=list(shape="hv", width=3), name="WIN%") %>% 
          layout(
            hovermode = "x unified",
            xaxis = list(title="Date"),
            yaxis = list(title="Shot %"),
            yaxis2 = list(overlaying="y", side="right", title="Win %", showgrid=F, zeroline=F,
                          automargin=T, range = list(0, 1)),
            legend = list(x=0.15, y=1.1, orientation="h", bgcolor="#00000000"),
            margin = list(l=20, r=20, t=10, b=10)
          )
        return(fig)
      })
      output$team_table12 = DT::renderDataTable({
        cols_desc = c("FG%","FGA","3P%","3PA","FT%","FTA","2P%","2PA")
        x = lapply(cols_desc, get_team_rank, returnTable=T)
        return(do.call(rbind.data.frame, x))
      }, options=list(dom='t', scrollY="260px", pageLength=20), rownames=F)
    } 
    if (input$team_box11 == "Rebounding") {
      output$team_rebound = renderPlotly({
        fig = rv$cum_current_team %>% arrange(`GAME DATE`) %>% # oldest to newest
          plot_ly(x=~`GAME DATE`, y=~`DREB`, type="scatter", mode="lines+markers",
                  name="DREB") %>%
          add_trace(y=~`OREB`, type="scatter", mode="lines+markers", name="OREB") %>%
          add_trace(y=~`WIN%`, name="Win %", mode="lines", yaxis="y2", 
                    line=list(shape="hv", width=3), name="WIN%") %>% 
          layout(
            hovermode = "x unified",
            xaxis = list(title="Date"),
            yaxis = list(title="Rebounds"),
            yaxis2 = list(overlaying="y", side="right", title="Win %", showgrid=F, zeroline=F,
                          automargin=T),
            legend = list(x=0.15, y=1.1, orientation="h", bgcolor="#00000000"),
            margin = list(l=20, r=20, t=10, b=10)
          )
        return(fig)
      })
      output$team_table12 = DT::renderDataTable({
        cols_desc = c("OREB","OREB%","DREB","DREB%","REB%")
        cols_asc = c("OREB OPP","OREB% OPP","DREB OPP","DREB% OPP","REB OPP","REB% OPP")
        x = lapply(cols_desc, get_team_rank, returnTable=T)
        x2 = lapply(cols_asc, get_team_rank, returnTable=T, descend=F)
        df = do.call(rbind.data.frame, x)
        df2 = do.call(rbind.data.frame, x2)
        return(bind_rows(df, df2))
      }, options=list(dom='t', scrollY="260px", pageLength=20), rownames=F)
    } 
    if (input$team_box11 == "Defense") {
      output$team_defense = renderPlotly({
        fig = rv$cum_current_team %>% arrange(`GAME DATE`) %>% # oldest to newest
          plot_ly(x=~`GAME DATE`, y=~`3P% OPP`, type="scatter", mode="lines+markers",
                  name="OPP 3P%") %>%
          add_trace(y=~`2P% OPP`, type="scatter", mode="lines+markers", name="OPP 2P%") %>%
          add_trace(y=~`WIN%`, name="Win %", mode="lines", yaxis="y2", 
                    line=list(shape="hv", width=3), name="WIN%") %>% 
          layout(
            hovermode = "x unified",
            xaxis = list(title="Date"),
            yaxis = list(title="Opponent Shot %"),
            yaxis2 = list(overlaying="y", side="right", title="Win %", showgrid=F, zeroline=F,
                          automargin=T, range = list(0, 1)),
            legend = list(x=0.15, y=1.1, orientation="h", bgcolor="#00000000"),
            margin = list(l=20, r=20, t=10, b=10)
          )
        return(fig)
      })
      output$team_table12 = DT::renderDataTable({
        cols_desc = c("STL","BLK","TOV OPP")
        cols_asc = c("FG% OPP","3P% OPP","2P% OPP","PTSOFF TO OPP","2NDPTS OPP",
                     "FBPS OPP","PITP OPP")
        x = lapply(cols_desc, get_team_rank, returnTable=T)
        x2 = lapply(cols_asc, get_team_rank, returnTable=T, descend=F)
        df = do.call(rbind.data.frame, x)
        df2 = do.call(rbind.data.frame, x2)
        return(bind_rows(df, df2))
      }, options=list(dom='t', scrollY="260px", pageLength=20), rownames=F)
    } 
    if (input$team_box11 == "Offense") {
      output$team_offense = renderPlotly({
        fig = rv$cum_current_team %>% arrange(`GAME DATE`) %>% # oldest to newest
          mutate(`FTARATE`=`FTARATE`*100) %>%
          plot_ly(x=~`GAME DATE`, y=~`AST%`, type="scatter", mode="lines+markers",
                  name="AST%") %>%
          add_trace(y=~`TOV%`, type="scatter", mode="lines+markers", name="TOV%") %>%
          add_trace(y=~`FTARATE`, type="scatter", mode="lines+markers", name="FTA Rate") %>%
          add_trace(y=~`WIN%`, name="Win %", mode="lines", yaxis="y2", 
                    line=list(shape="hv", width=3), name="WIN%") %>% 
          layout(
            hovermode = "x unified",
            xaxis = list(title="Date"),
            yaxis = list(title="% of Possessions"),
            yaxis2 = list(overlaying="y", side="right", title="Win %", showgrid=F, zeroline=F,
                          automargin=T, range = list(0, 1)),
            legend = list(x=0.15, y=1.1, orientation="h", bgcolor="#00000000"),
            margin = list(l=20, r=20, t=10, b=10)
          )
        return(fig)
      })
      output$team_table12 = DT::renderDataTable({
        cols_desc = c("AST","AST%","AST/TO","EFG%","TS%","FTARATE","PTSOFF TO",
                      "2NDPTS","FBPS","PITP")
        cols_asc = c("TOV","TOV%")
        x = lapply(cols_desc, get_team_rank, returnTable=T)
        x2 = lapply(cols_asc, get_team_rank, returnTable=T, descend=F)
        df = do.call(rbind.data.frame, x)
        df2 = do.call(rbind.data.frame, x2)
        return(bind_rows(df, df2))
      }, options=list(dom='t', scrollY="260px", pageLength=20), rownames=F)
    }
  })
  output$team_box12 = renderUI({
    box(
      id="team_box12",
      title=sprintf("%s %s Ranks", input$team, input$team_box11),
      width=NULL,
      status="success", solidHeader=T,
      DT::dataTableOutput("team_table12")
    )
  })
  output$team_box21 = renderUI({
    box(
      id="team_box21",
      width=NULL,
      solidHeader=T, status="success",
      title=sprintf("%s Keys to Success", input$team),
      plotOutput("team_tree", height="300px")
    )
  })
  output$team_tree = renderPlot({
    easy_stats = c("W/L","PTS","FGA","FG%","3PA","3P%","FTA","FT%","OREB","DREB",
                   "AST","STL","BLK","TOV","PF","OREB%","DREB%","PACE","PTSOFF TO",
                   "2NDPTS","FBPS","PITP")
    df = rv$current_team %>% select(all_of(easy_stats))
    tree = rpart(factor(`W/L`, levels=c("W","L")) ~ ., data=df, cp=0.001, control=list(maxdepth=4))
    p = prp(tree, extra=1)
    return(p)
  })
  output$team_box22 = renderUI({
    box(
      id="team_box22",
      width=NULL,
      solidHeader=T, status="success",
      title=sprintf("%s OFFRTG vs DEFRTG", input$team),
      plotlyOutput("team_plot22", height="300px")
    )
  })
  output$team_plot22 = renderPlotly({
    data = rv$boxscore %>% group_by(`TEAM`) %>%
      summarise(`ORTG`=mean(`OFFRTG`,na.rm=T), `DRTG`=mean(`DEFRTG`,na.rm=T)) %>%
      mutate(`COLOR`=rv$team_color[`TEAM`])
    data2 = data %>% filter(`TEAM` == input$team)
    nba_ortg = rv$nba_team_avg$`OFFRTG`[1]
    nba_drtg = rv$nba_team_avg$`DEFRTG`[1]
    xmin = min(data$ORTG); xmax = max(data$ORTG)
    ymin = min(data$DRTG); ymax = max(data$DRTG)
    hline = list(type="line", x0=0, x1=1, xref="paper", y0=nba_drtg, y1=nba_drtg, line=list(color="black"))
    vline = list(type="line", x0=nba_ortg, x1=nba_ortg, yref="paper", y0=0, y1=1, line=list(color="black"))
    lt_rect = list(type="rect", fillcolor="red", opacity=0.1, x0=xmin-2, y0=ymax+2, x1=nba_ortg, y1=nba_drtg, line=list(color="red"))
    rt_rect = list(type="rect", fillcolor="orange", opacity=0.1, x0=nba_ortg, y0=ymax+2, x1=xmax+2, y1=nba_drtg, line=list(color="orange"))
    lb_rect = list(type="rect", fillcolor="orange", opacity=0.1, x0=xmin-2, y0=nba_drtg, x1=nba_ortg, y1=ymin-2, line=list(color="orange"))
    rb_rect = list(type="rect", fillcolor="green", opacity=0.1, x0=nba_ortg, y0=nba_drtg, x1=xmax+2, y1=ymin-2, line=list(color="green"))
    #
    fig = plot_ly(data=data, x=~`ORTG`, y=~`DRTG`, type="scatter", mode="markers+text",
                  marker=list(size=30), color=~`TEAM`, colors=~`COLOR`, opacity=0.25,
                  hovertext=~sprintf("%s\nOFFRTG: %.1f\nDEFRTG: %.1f",`TEAM`,`ORTG`,`DRTG`),
                  hoverinfo="text", text=~`TEAM`, textposition="top") %>%
      add_trace(data=data2, x=~`ORTG`, y=~`DRTG`, type="scatter", mode="markers+text",
                marker=list(size=30), color=~`TEAM`, colors=~`COLOR`, opacity=1,
                hovertext=~sprintf("%s\nOFFRTG: %.1f\nDEFRTG: %.1f",`TEAM`,`ORTG`,`DRTG`),
                hoverinfo="text", text=~`TEAM`, textposition="top") %>%
      layout(
        shapes = list(hline, vline, lt_rect, rt_rect, lb_rect, rb_rect),
        showlegend = F,
        margin = list(l=20, r=20, t=10, b=10)
      )
    return(fig)
  })
  # Value Boxes
  output$team_rtg_box = renderInfoBox({
    avg = mean(rv$current_team$`NETRTG`, na.rm=T)
    temp = get_thumbs_icon(avg, "NETRTG", team=T)
    infoBox(
      title="OFFRTG : DEFRTG",
      value=sprintf("%.1f (%d) : %.1f (%d)", rv$cum_current_team$`OFFRTG`[1], get_team_rank("OFFRTG"),
                    rv$cum_current_team$`DEFRTG`[1], get_team_rank("DEFRTG", descend=F)),
      subtitle=sprintf("NETRTG: %.1f (%d)", avg, get_team_rank("NETRTG")),
      icon=temp$icon,
      fill=T
    )
  })
  output$team_pts_box = renderInfoBox({
    avg = rv$cum_current_team$`PTS`[1]
    temp = get_thumbs_icon(avg, "PTS", team=T)
    infoBox(
      title="Points",
      value=sprintf("%.1f (%d)", avg, get_team_rank("PTS")),
      subtitle=sprintf("NBA Average: %.1f", temp$league),
      icon=temp$icon,
      fill=T
    )
  })
  output$team_reb_box = renderInfoBox({
    avg = rv$cum_current_team$`REB`[1]
    temp = get_thumbs_icon(avg, "REB", team=T)
    infoBox(
      title="Rebounds",
      value=sprintf("%.1f (%d)", avg, get_team_rank("REB")),
      subtitle=sprintf("NBA Average: %.1f", temp$league),
      icon=temp$icon,
      fill=T
    )
  })
  output$team_ast_box = renderInfoBox({
    avg = rv$cum_current_team$`AST`[1]
    temp = get_thumbs_icon(avg, "AST", team=T)
    infoBox(
      title="Assists",
      value=sprintf("%.1f (%d)", avg, get_team_rank("AST")),
      subtitle=sprintf("NBA Average: %.1f", temp$league),
      icon=temp$icon,
      fill=T
    )
  })
  output$team_tov_box = renderInfoBox({
    avg = rv$cum_current_team$`TOV`[1]
    temp = get_thumbs_icon(avg, "TOV", team=T, high_good=F)
    infoBox(
      title="Turnovers",
      value=sprintf("%.1f (%d)", avg, get_team_rank("TOV", descend=F)),
      subtitle=sprintf("NBA Average: %.1f", temp$league),
      icon=temp$icon,
      fill=T
    )
  })
  
  
  ##### PLAYER DASHBOARD #####
  get_player_rank = function(col, descend=T) { # Note: player data already averaged
    x = rv$players %>% group_by(`PLAYER`) %>%
      summarize_at(c(col), mean, na.rm=T)
    x = x[order(x[,col], decreasing=descend),] %>% mutate("Rank"=1:n())
    return(x[x$PLAYER == input$player,"Rank"][[1]])
  }
  output$player_input = renderUI({
    selectInput(
      "player",
      label="Select a Player:",
      choices=sort(unique(rv$players$`PLAYER`)),
      selected="LeBron James"
    )
  })
  output$player_date_range = renderUI({
    dates = rv$players %>% filter(`PLAYER` == input$player) %>%
      summarise(`max`=max(`GAME DATE`), `min`=min(`GAME DATE`)) # calculate range without dynamic update of slider
    sliderInput("player_date_range",
      label="Date Range:",
      min=dates$`min`,
      max=dates$`max`,
      value=c(min(rv$players$`GAME DATE`), max(rv$players$`GAME DATE`))
    )
  })
  # Boxes for Player Plots
  output$player_box11 = renderUI({
    box(
      id="pbox11",
      width=7,
      solidHeader=T, status="success",
      title=sprintf("%s Shooting", input$player),
      plotlyOutput("plot11", height="300px")
    )
  })
  output$plot11 = renderPlotly({
    record = rv$current_player %>% arrange(`GAME DATE`) %>%
      mutate(`WON`=cumsum(`W/L`=="W"), `GP`=1:n(), `WIN%`=`WON`/`GP`) %>% 
      arrange(desc(`GAME DATE`)) %>% pull(`WIN%`)
    fig = rv$cum_current_player %>%
      plot_ly(x = ~`GAME DATE`, y = ~`FG%`, name='FG%', 
              type='scatter', mode='lines+markers') %>%
      add_trace(y = ~`3P%`, name='3P%', mode="lines+markers") %>%
      add_trace(y = ~`FT%`, name='FT%', mode="lines+markers") %>%
      add_trace(y=record, name="Win %", mode="lines", yaxis="y2", 
                line=list(dash="dot", width=3))
    fig = fig %>%
      layout(
        hovermode = "x unified",
        xaxis = list(title="Date"),
        yaxis = list(title="Shot %", range = list(0, 100)),
        yaxis2 = list(overlaying="y", side="right", title="Win %", showgrid=F, zeroline=F,
                      automargin=T, range = list(0, 1)),
        legend = list(x=0.15, y=1.1, orientation="h", bgcolor="#00000000"),
        margin = list(l=20, r=20, t=10, b=10)
      )
    return(fig)
  })
  output$player_box12 = renderUI({
    box(
      id="pbox12",
      width=5,
      solidHeader=T, status="success",
      title=sprintf("%s Scoring Makeup", input$player),
      plotlyOutput("plot12", height="195px")
    )
  })
  output$plot12 = renderPlotly({
    scoring = rv$current_player %>%
      summarise(`TOTAL PTS`=sum(`PTS`), `FT`=sum(`FTM`), `2P`=sum(`FGM`-`3PM`)*2,
                `3P`=sum(`3PM`)*3) %>%
      pivot_longer(cols=c(`FT`,`2P`,`3P`), names_to="Score Type", values_to="Value") %>%
      mutate(`Percent`=`Value`/`TOTAL PTS`)
    # Compute the cumulative percentages (top of each rectangle)
    scoring$ymax = cumsum(scoring$`Percent`)
    # Compute the bottom of each rectangle
    scoring$ymin = c(0, head(scoring$ymax, n=-1))
    # Doughnut chart using plotly
    fig = scoring %>%
      group_by(`Score Type`) %>% 
      plot_ly(text = ~`Score Type`, values = ~`Percent`) %>% 
      add_pie(hole = 0.5) %>% 
      layout(
        showlegend = F,
        margin = list(l=10, r=10, t=10, b=10),
        xaxis = list(showgrid = F, zeroline = T, showticklabels = F),
        yaxis = list(showgrid = F, zeroline = T, showticklabels = F)
      )
    return(fig)
  })
  output$player_splits = renderInfoBox({
    infoBox(
      title="Shooting Splits",
      value=sprintf("%.1f%s | %.1f%s | %.1f%s", rv$cum_current_player$`FG%`[1], "%",
                    rv$cum_current_player$`3P%`[1], "%", rv$cum_current_player$`FT%`[1], "%"),
      subtitle=sprintf("FG%s (%d) | 3P%s (%d) | FT%s (%d)", "%", get_player_rank("FG%"),
                       "%", get_player_rank("3P%"), "%", get_player_rank("FT%")),
      icon=icon("fa-solid fa-basketball", verify_fa=F),
      fill=T
    )
  })
  output$player_box21 = renderUI({
    box(
      id="pbox21",
      title=sprintf("%s Keys to Success", input$player),
      width=5,
      solidHeader=T, status="success",
      plotOutput("plot21", height="300px")
    )
  })
  output$plot21 = renderPlot({
    easy_stats = c("W/L","PTS","FGA","FG%","3PA","3P%","FTA","FT%","OREB","DREB",
                   "AST","STL","BLK","TOV","PF","OREB%","DREB%","USG%","PACE",
                   "PTS OFF TO","2ND PTS","FBPS","PITP")
    df = rv$current_player %>% select(all_of(easy_stats))
    tree = rpart(factor(`W/L`, levels=c("W","L")) ~ ., data=df, cp=0.001, control=list(maxdepth=4))
    p = prp(tree, extra=1)
    return(p)
  })
  output$player_box22 = renderUI({
    box(
      id="pbox22",
      title=sprintf("Other %s Stats", input$player),
      width=7,
      solidHeader=T, status="success",
      plotlyOutput("plot22", height="300px")
    )
  })
  output$plot22 = renderPlotly({
    nba = rv$nba_player_avg %>% select(`BLK`,`STL`,`OREB`,`DREB`) %>%
      pivot_longer(cols=c("BLK","STL","OREB","DREB"), names_to="Stat", values_to="Value")
    data = rv$current_player %>%
      summarise(`BLK`=mean(`BLK`,na.rm=T), `STL`=mean(`STL`,na.rm=T),
                `OREB`=mean(`OREB`, na.rm=T), `DREB`=mean(`DREB`,na.rm=T)) %>%
      pivot_longer(cols=c("BLK","STL","OREB","DREB"), names_to="Stat", values_to="Value")
    fig = data %>%
      plot_ly(y = ~`Stat`, x = ~`Value`, type="bar", textposition="outside",
              text=~round(`Value`,1), name=input$player) %>% 
      add_trace(data=nba, y = ~`Stat`, name="League Avg.")
    fig = fig %>%
      layout(
        yaxis = list(categoryorder = "total ascending"),
        xaxis = list(range = list(0, max(data$`Value`, nba$`Value`)+0.5)),
        barmode = "group",
        legend = list(x=1, y=0.5)
      )
    return(fig)
  })
  # Value Boxes
  output$player_rtg_box = renderInfoBox({
    avg = mean(rv$current_player$`NETRTG`, na.rm=T)
    temp = get_thumbs_icon(avg, "NETRTG")
    infoBox(
      title="OFFRTG : DEFRTG",
      value=sprintf("%.1f (%d) : %.1f (%d)", mean(rv$current_player$`OFFRTG`, na.rm=T), 
                    get_player_rank("OFFRTG"),
                    mean(rv$current_player$`DEFRTG`, na.rm=T),
                    get_player_rank("DEFRTG", descend=F)),
      subtitle=sprintf("NETRTG: %.1f (%d)", avg, get_player_rank("NETRTG")),
      icon=temp$icon,
      fill=T
    )
  })
  output$player_pts_box = renderInfoBox({
    avg = rv$cum_current_player$`PTS`[1]
    temp = get_thumbs_icon(avg, "PTS")
    infoBox(
      title="Points",
      value=sprintf("%.1f (%d)", avg, get_player_rank("PTS")),
      subtitle=sprintf("NBA Average: %.1f", temp$league),
      icon=temp$icon,
      fill=T
    )
  })
  output$player_reb_box = renderInfoBox({
    avg = rv$cum_current_player$`REB`[1]
    temp = get_thumbs_icon(avg, "REB")
    infoBox(
      title="Rebounds",
      value=sprintf("%.1f (%d)", avg, get_player_rank("REB")),
      subtitle=sprintf("NBA Average: %.1f", temp$league),
      icon=temp$icon,
      fill=T
    )
  })
  output$player_ast_box = renderInfoBox({
    avg = rv$cum_current_player$`AST`[1]
    temp = get_thumbs_icon(avg, "AST")
    infoBox(
      title="Assists",
      value=sprintf("%.1f (%d)", avg, get_player_rank("AST")),
      subtitle=sprintf("NBA Average: %.1f", temp$league),
      icon=temp$icon,
      fill=T
    )
  })
  output$player_tov_box = renderInfoBox({
    avg = rv$cum_current_player$`TOV`[1]
    temp = get_thumbs_icon(avg, "TOV", high_good=F)
    infoBox(
      title="Turnovers",
      value=sprintf("%.1f (%d)", avg, get_player_rank("TOV")),
      subtitle=sprintf("NBA Average: %.1f", temp$league),
      icon=temp$icon,
      fill=T
    )
  })
  # Player team logo
  output$player_team_logo = renderImage({
    list(src=sprintf(".\\logos\\%s.png", rv$current_player$`TEAM`[1]),
         width="35%")
  }, deleteFile=F)
  output$player_record = renderUI({
    counts = rv$current_player %>% count(`W/L`) %>% pull(`n`)
    record = sprintf("Player Record: %d - %d (%.3f)", counts[2], counts[1], counts[2]/(counts[1]+counts[2]))
    HTML(paste("<center><b>", record, "</b></center>"))
  })
  
  ##### TAB - BOXSCORE ######
  output$boxscore_filter = renderUI({
    selectInput(
      "boxscore_teams",
      label="Team(s):",
      choices=c("", sort(unique(rv$boxscore$`TEAM`))),
      multiple=T
    )
  })
  output$all_boxscores = renderDataTable({
    print(input$boxscore_teams)
    if (is.null(input$boxscore_teams)) {return(rv$boxscore)}
    return(rv$boxscore %>% filter(`TEAM` %in% input$boxscore_teams))
  }, options=list(scrollX=T))
  
  
  
  ###### STYLING ######
  output$css_style = renderUI({
    temp_color = rv$team_color[rv$current_player$`TEAM`[1]]
    if (input$sidebar_tab == "TeamTab") {
      temp_color = rv$team_color[input$team]
    }
    styles = sprintf("
      .info-box{border-radius: 10px;}
      .info-box-icon{
        border-top-left-radius: 10px;
        border-top-right-radius: 10px;
        border-bottom-right-radius: 10px;
        border-bottom-left-radius: 10px;}
      .box{border-radius: 10px;}
      .box-header{border-top-left-radius: 10px; border-top-right-radius: 10px;}
      .box.box-solid.box-success>.box-header{color: #fff; background-color: %s;}
      .box.box-solid.box-success {border: 1px solid %s;}
      .bg-aqua{background-color: %s!important;}
    ", temp_color, temp_color, temp_color)
    return(tags$style(HTML(styles)))
  })
}