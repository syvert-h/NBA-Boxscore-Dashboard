library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(plotly)
library(shinyjs)
library(rpart)
library(rpart.plot)

function(input, output, session) {
  # reactiveValues to store dataframe
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
    box_team_color = c("BOS"="green","MEM"="light-blue","DAL"="blue","ORL"="light-blue","HOU"="red","UTA"="yellow","LAC"="red","DEN"="navy","ATL"="red",
                   "SAS"="black","NOP"="navy","OKC"="aqua","PHI"="blue","CLE"="maroon","MIN"="light-blue","CHA"="teal","LAL"="yellow","CHI"="red",
                   "NYK"="blue","MIL"="olive","BKN"="black","IND"="yellow","TOR"="red","POR"="red","DET"="red","SAC"="purple","PHX"="orange",
                   "MIA"="red","GSW"="blue","WAS"="navy"),
    cum_boxscore = NULL,
    cum_all = NULL,
    players = read_csv("Regular_Season_Player_2022-23.csv") %>%
      group_by(`TEAM`)
  )
  observeEvent(input$dashboard_team, {
    # store cumulative boxscore
    rv$cum_boxscore = rv$boxscore %>%
      group_by(`TEAM`) %>%
      arrange(`TEAM`,`GAME DATE`) %>%
      mutate(across(where(is.numeric), cummean)) %>% # cumulative mean for numeric columns
      arrange(desc(`GAME DATE`)) %>% # once calculated, order by recent games
      filter(`TEAM` == input$dashboard_team)
    # change info-box color
    runjs(
      sprintf(
        "document.querySelector('#record > div').style.bg-color.background-color = '%s !important';",
        rv$team_color[input$dashboard_team]
      )
    )
    # store cumulative boxscore (no team filter - all teams)
    rv$cum_all = rv$boxscore %>%
      group_by(`TEAM`) %>%
      arrange(`TEAM`,`GAME DATE`) %>% # old to new
      mutate(across(where(is.numeric), cummean)) %>%
      mutate(`COLOR`=rv$team_color[`TEAM`]) # add team color
  })
  
  
  ### Dashboard Body ###
  get_rank = function(col, descend=T, team2=input$dashboard_team) { # gets a teams NBA rank for some column
    x = rv$boxscore %>%
      group_by(`TEAM`) %>%
      summarize_at(vars(col), list(`avg`=mean))
    return(x[order(x$avg, decreasing=descend),] %>%
      mutate(`Rank`=1:n()) %>%
      filter(`TEAM` == team2) %>%
      pull(`Rank`)
    )
  }
  ## Tab - Season Stats ##
  # Bar plot for record
  output$previous_bar = renderPlotly({
    p = rv$boxscore %>%
      filter(`TEAM` == input$dashboard_team) %>%
      mutate(`MARGIN`=`PTS`-`PTS OPP`) %>%
      arrange(`GAME DATE`) %>%
      mutate(`WON`=cumsum(`W/L`=="W"), `LOSS`=cumsum(`W/L`=="L"), 
             `RECORD`=sprintf("%d-%d",`WON`,`LOSS`)) %>%
      ggplot(aes(x=`GAME DATE`, y=`MARGIN`, fill=`W/L`,
                 text=sprintf("Opponent: %s\nRecord:%s",`TEAM OPP`,`RECORD`))) +
      geom_bar(stat="identity") +
      geom_hline(yintercept=0, size=0.2) +
      theme(
        legend.position="none", axis.ticks.y=element_blank(), axis.text.y=element_blank(),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank() #remove minor gridlines
      ) + 
      labs(y="", x="")
    ggplotly(p)
  })
  # Card for record
  output$record = renderInfoBox({
    temp = table(rv$cum_boxscore$`W/L`)
    wins = temp[2]; losses = temp[1]
    last10 = table(head(rv$cum_boxscore$`W/L`, 10)) # [1] L [2] W
    infoBox(
      title="Record (W - L):",
      subtitle=sprintf("Last 10 Games: %d - %d", last10[2], last10[1]),
      value=sprintf("%d - %d", wins, losses),
      color=rv$box_team_color[input$dashboard_team],
      width=4,
      fill=TRUE,
      icon=icon("fa-soild fa-ranking-star", verify_fa = FALSE)
    )
  })
  # Card for OFFRTG and DEFRTG (with ranks - requires recalculation)
  output$rtg = renderInfoBox({
    infoBox(
      title="OFFRTG : DEFRTG",
      value=sprintf("%.1f (%d) : %.1f (%d)", 
                    rv$cum_boxscore$`OFFRTG`[1], get_rank("OFFRTG"),
                    rv$cum_boxscore$`DEFRTG`[1], get_rank("DEFRTG", descend=F)),
      subtitle=sprintf("NETRTG: %.1f (%d)", rv$cum_boxscore$`NETRTG`[1], get_rank("NETRTG")),
      color=rv$box_team_color[input$dashboard_team],
      width=4,
      fill=TRUE,
      icon=icon("fa-regular fa-star", verify_fa = FALSE)
    )
  })
  # Card for Points
  output$avgPTS = renderInfoBox({
    league = mean(rv$boxscore$`PTS`)
    team = rv$cum_boxscore$`PTS`[1]
    temp_icon = icon("fa-solid fa-arrow-down", verify_fa = FALSE)
    if (team >= league) {temp_icon = icon("fa-solid fa-arrow-up", verify_fa = FALSE)}
    infoBox(
      title="Avg. Points",
      subtitle=sprintf("League Average: %.1f", league),
      value=sprintf("%.1f (%d)", team, get_rank("PTS")),
      color=rv$box_team_color[input$dashboard_team],
      width=4,
      fill=TRUE,
      icon=temp_icon
    )
  })
  # Tree plot
  output$dash_tree = renderPlot({
    remove_team_cols = c("MIN","PTS","FGM","FGA","3PM","3PA","FTM","FTA","REB","+/-","OFFRTG",
                         "DEFRTG","NETRTG","AST/TO","ASTRATIO","OREB%","DREB%","TOV%","EFG%",
                         "TS%","2PM","2PA","PIE","TEAM","MATCH UP")
    remove_opp_cols = c(paste0(remove_team_cols, " OPP"), "W/L OPP")
    df = rv$boxscore %>%
      filter(`TEAM` == input$dashboard_team) %>%
      select(-all_of(remove_team_cols)) %>% # remove hard to measure stats or similar
      select(-all_of(remove_opp_cols)) %>%
      select(-c("GAME DATE"))
    all_tree = rpart(factor(`W/L`)~., data=df, cp=0.0001)
    #
    preds = predict(all_tree, type="class")
    actual = rv$boxscore %>% filter(`TEAM`== input$dashboard_team) %>% pull(`W/L`)
    mat = table(preds, actual)
    #
    output$tree_win_acc = renderValueBox({
      valueBox(
        value=sprintf("%d-%d (%.1f%s)", mat[2,2], mat[2,1], mat[2,2]/sum(mat[2,])*100, "%"), # W accuracy
        subtitle="Win Accuracy",
        color=rv$box_team_color[input$dashboard_team],
        width=4,
        icon=icon("fa-solid fa-w", verify_fa=FALSE)
      )
    })
    output$tree_loss_acc = renderValueBox({
      valueBox(
        value=sprintf("%d-%d (%.1f%s)", mat[1,1], mat[1,2], mat[1,1]/sum(mat[1,])*100, "%"), # L accuracy
        subtitle="Loss Accuracy",
        color=rv$box_team_color[input$dashboard_team],
        width=4,
        icon=icon("fa-solid fa-l", verify_fa=FALSE)
      )
    })
    return(prp(all_tree))
  })
  # Plots for tabset box
  observeEvent(input$tabset1, {
    if (input$tabset1 == "Shooting") {
      # Shooting Plots
      output$shooting_dash = renderPlotly({
        p = rv$cum_boxscore %>%
          pivot_longer(cols=c(`2P%`,`3P%`), names_to="Shot Type", 
                       values_to="Cumulative %") %>% # convert to longer format to make legend easier
          ggplot(aes(x=`GAME DATE`, y=`Cumulative %`, color=`Shot Type`)) +
          geom_line() +
          geom_hline(yintercept=mean(rv$boxscore$`3P%`)) +
          geom_hline(yintercept=mean(rv$boxscore$`2P%`))
        ggplotly(p) %>%
          layout(legend=list(x=0.05, y=0.5))
      })
      output$var_card1 = renderInfoBox({
        # FG%
        infoBox(
          title="FG% | EFG% | TS%",
          subtitle=sprintf("On %.1f Attempts per Game (%d)", rv$cum_boxscore$`FGA`[1], get_rank("FGA")),
          value=sprintf("%.1f%s (%d) | %.1f%s (%d) | %.1f (%d)", 
                        rv$cum_boxscore$`FG%`[1], "%", get_rank("FG%"),
                        rv$cum_boxscore$`EFG%`[1], "%", get_rank("EFG%"),
                        rv$cum_boxscore$`TS%`[1], get_rank("TS%")),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=icon("fa-solid fa-basketball", verify_fa = FALSE)
        )
      })
      output$var_card2 = renderInfoBox({
        # Shooting Splits
        infoBox(
          title="Shooting Splits (2P% | 3P% | FT%)",
          value=sprintf("%.1f%s (%d) | %.1f%s (%d) | %.1f%s (%d)", rv$cum_boxscore$`2P%`[1], "%", get_rank("2P%"),
                        rv$cum_boxscore$`3P%`[1], "%", get_rank("3P%"),
                        rv$cum_boxscore$`FT%`[1], "%", get_rank("FT%")),
          subtitle=sprintf("%.1f (%d) | %.1f (%d) | %.1f (%d) Attempts per Game", 
                           rv$cum_boxscore$`2PA`[1], get_rank("2PA"),
                           rv$cum_boxscore$`3PA`[1], get_rank("3PA"),
                           rv$cum_boxscore$`FTA`[1], get_rank("FTA")),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=icon("fa-solid fa-basketball", verify_fa = FALSE)
        )
      })
      output$scoring_pie = renderPlotly({
        scoring = rv$boxscore %>%
          filter(`TEAM` == input$dashboard_team) %>%
          summarise(`TOTAL PTS`=sum(`PTS`), `FT`=sum(`FTM`), `2P`=sum(`2PM`)*2,
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
          plot_ly(labels = ~`Score Type`, values = ~`Percent`) %>% 
          add_pie(hole = 0.6) %>% 
          layout(
            showlegend = T,
            title = list(text=sprintf("%s Scoring Makeup", input$dashboard_team), y=0.975, x=0.5, xanchor='center', yanchor='top'),
            xaxis = list(showgrid = F, zeroline = T, showticklabels = F),
            yaxis = list(showgrid = F, zeroline = T, showticklabels = F),
            legend=list(x=1, y=0.5)
          )
        return(fig)
      })
    }
    if (input$tabset1 == "Rebounding") {
      # Rebounding Plot
      output$rebounding_dash = renderPlotly({
        p = rv$cum_boxscore %>%
          pivot_longer(cols=c(`OREB`,`DREB`), names_to="Rebound Type", 
                       values_to="Cumulative No.") %>% # convert to longer format to make legend easier
          ggplot(aes(x=`GAME DATE`, y=`Cumulative No.`, color=`Rebound Type`)) +
          geom_line() +
          geom_hline(yintercept=mean(rv$boxscore$`OREB`)) +
          geom_hline(yintercept=mean(rv$boxscore$`DREB`))
        
        ggplotly(p) %>%
          layout(legend=list(x=0.05, y=0.5))
      })
      # Variable cards
      output$var_card1 = renderInfoBox({
        # OREB%
        infoBox(
          title="Offensive Rebounding %",
          subtitle=sprintf("Grabbing %.1f Offensive Rebounds per Game (%d)", rv$cum_boxscore$`OREB`[1], get_rank("OREB")),
          value=sprintf("%.1f%s (%d)", rv$cum_boxscore$`OREB%`[1], "%", get_rank("OREB%")),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=icon("fa-solid fa-percent", verify_fa = FALSE)
        )
      })
      output$var_card2 = renderInfoBox({
        # DREB%
        infoBox(
          title="Defensive Rebounding %",
          subtitle=sprintf("Grabbing %.1f DREB's per Game (%d)", rv$cum_boxscore$`DREB`[1], get_rank("DREB")),
          value=sprintf("%.1f%s (%d)", rv$cum_boxscore$`DREB%`[1], "%", get_rank("DREB%")),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=icon("fa-solid fa-percent", verify_fa = FALSE)
        )
      })
      # Rebounding% stacked bar chart
      output$rebounding_bar = renderPlotly({
        x = rv$cum_boxscore %>%
          filter(`TEAM` == input$dashboard_team) %>%
          select(`OREB%`, `DREB%`, `OREB% OPP`, `DREB% OPP`)
        p = x[1,] %>%
          pivot_longer(cols=c(`DREB%`,`OREB% OPP`,`OREB%`,`DREB% OPP`), names_to="Rebound Type", 
                       values_to="Percent") %>%
          mutate(`Place`=c("Offensive End","Offensive End","Defensive End","Defensive End"),
                 `Team`=c(input$dashboard_team,"Opponent",input$dashboard_team,"Opponent")) %>%
          ggplot(aes(x=`Percent`, y=`Place`, fill=`Team`, label=`Rebound Type`)) +
          geom_bar(stat="identity") +
          geom_text(size = 3, position=position_stack(vjust = 0.5)) +
          theme(legend.position="none", axis.text.y=element_text(angle=90, vjust=.5, hjust=1)) +
          labs(y="")
        ggplotly(p)
      })
    }
    if (input$tabset1 == "Offense") {
      # Offensive Plots
      output$offense_dash = renderPlotly({
        p = rv$cum_boxscore %>%
          pivot_longer(cols=c(`AST`,`TOV`), names_to="Offense Type", 
                       values_to="Cumulative No.") %>% # convert to longer format to make legend easier
          ggplot(aes(x=`GAME DATE`, y=`Cumulative No.`, color=`Offense Type`)) +
          geom_line() +
          geom_hline(yintercept=mean(rv$boxscore$`AST`)) +
          geom_hline(yintercept=mean(rv$boxscore$`TOV`))
        ggplotly(p) %>%
          layout(legend=list(x=0.05, y=0.5))
      })
      # Variable cards
      output$var_card1 = renderInfoBox({
        # AST
        league = mean(rv$boxscore$`AST%`)
        team = rv$cum_boxscore$`AST%`[1]
        temp_icon = icon("fa-solid fa-thumbs-up", verify_fa = FALSE)
        if (team < league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa = FALSE)}
        infoBox(
          title="Assisted Possessions",
          subtitle=sprintf("With %.1f Assists per Game (%d)", rv$cum_boxscore$`AST`[1], get_rank("AST")),
          value=paste0(sprintf("%.1f", rv$cum_boxscore$`AST%`[1]), "%", sprintf(" (%d)", get_rank("AST%"))),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=temp_icon
        )
      })
      output$var_card2 = renderInfoBox({
        # PF (Personal Fouls)
        league = mean(rv$boxscore$`PF`)
        team = rv$cum_boxscore$`PF`[1]
        temp_icon = icon("fa-solid fa-thumbs-up", verify_fa = FALSE)
        if (team >= league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa = FALSE)}
        infoBox(
          title="Personal Fouls",
          subtitle=sprintf("League Average: %.1f", mean(rv$boxscore$`PF`)),
          value=sprintf("%.1f (%d)", rv$cum_boxscore$`PF`[1], get_rank("PF", descend=F)),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=temp_icon
        )
      })
      output$var_card3 = renderInfoBox({
        # TOV
        league = mean(rv$boxscore$`TOV%`)
        team = rv$cum_boxscore$`TOV%`[1]
        temp_icon = icon("fa-solid fa-thumbs-up", verify_fa = FALSE)
        if (team >= league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa = FALSE)}
        infoBox(
          title="Possessions Turned Over",
          subtitle=sprintf("With %.1f Turnovers per Game (%d)", rv$cum_boxscore$`TOV`[1], get_rank("TOV", descend=F)),
          value=paste0(sprintf("%.1f", rv$cum_boxscore$`TOV%`[1]), "%", sprintf(" (%d)", get_rank("TOV%", descend=F))),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=temp_icon
        )
      })
      output$var_card4 = renderInfoBox({
        # AST/TO
        league = mean(rv$boxscore$`AST/TO`)
        team = rv$cum_boxscore$`AST/TO`[1]
        temp_icon = icon("fa-solid fa-thumbs-up", verify_fa = FALSE)
        if (team < league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa = FALSE)}
        infoBox(
          title="Assists per Turnover",
          subtitle=sprintf("League Average: %.1f", mean(rv$boxscore$`AST/TO`)),
          value=sprintf("%.1f (%d)", rv$cum_boxscore$`AST/TO`[1], get_rank("AST/TO")),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=temp_icon
        )
      })
      output$var_card5 = renderInfoBox({
        # AST/TO
        infoBox(
          title="PACE",
          subtitle=sprintf("League Average: %.1f", mean(rv$boxscore$`PACE`)),
          value=sprintf("%.1f (%d)", rv$cum_boxscore$`PACE`[1], get_rank("PACE")),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=icon("fa-solid fa-gauge", verify_fa=FALSE)
        )
      })
    }
    if (input$tabset1 == "Defense") {
      output$defense_dash = renderPlotly({
        p = rv$cum_boxscore %>%
          arrange(`GAME DATE`) %>%
          pivot_longer(cols=c(`2P% OPP`,`3P% OPP`), names_to="Defense Type", 
                       values_to="Cumulative %") %>% # convert to longer format to make legend easier
          ggplot(aes(x=`GAME DATE`, y=`Cumulative %`, color=`Defense Type`)) +
          geom_line() +
          geom_hline(yintercept=mean(rv$boxscore$`2P%`)) + # league averages
          geom_hline(yintercept=mean(rv$boxscore$`3P%`))
        ggplotly(p) %>%
          layout(legend=list(x=0.05, y=0.5))
      })
      # Variable cards
      output$var_card1 = renderInfoBox({
        # OPP FG%
        league = mean(rv$boxscore$`FG% OPP`)
        team = rv$cum_boxscore$`FG% OPP`[1]
        temp_icon = icon("fa-solid fa-thumbs-up", verify_fa = FALSE)
        if (team >= league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa = FALSE)}
        infoBox(
          title="Opponent FG%",
          subtitle=sprintf("League Average: %.1f%s", mean(rv$boxscore$`FG% OPP`), "%"),
          value=sprintf("%.1f%s (%d)", rv$cum_boxscore$`FG% OPP`[1], "%", get_rank("FG% OPP", descend=F)),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=temp_icon
        )
      })
      output$var_card2 = renderInfoBox({
        # Blocks
        league = mean(rv$boxscore$`BLK`)
        team = rv$cum_boxscore$`BLK`[1]
        temp_icon = icon("fa-solid fa-thumbs-up", verify_fa = FALSE)
        if (team < league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa = FALSE)}
        infoBox(
          title="Blocks",
          subtitle=sprintf("League Average: %.1f", mean(rv$boxscore$`BLK`)),
          value=sprintf("%.1f (%d)", rv$cum_boxscore$`BLK`[1], get_rank("BLK")),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=temp_icon
        )
      })
      output$var_card3 = renderInfoBox({
        # STL
        league = mean(rv$boxscore$`STL`)
        team = rv$cum_boxscore$`STL`[1]
        temp_icon = icon("fa-solid fa-thumbs-up", verify_fa = FALSE)
        if (team < league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa = FALSE)}
        infoBox(
          title="Steals",
          subtitle=sprintf("League Average: %.1f", mean(rv$boxscore$`STL`)),
          value=sprintf("%.1f (%d)", rv$cum_boxscore$`STL`[1], get_rank("STL")),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=temp_icon
        )
      })
      output$var_card5 = renderInfoBox({
        # Opponent TOV%
        league = mean(rv$boxscore$`TOV% OPP`)
        team = rv$cum_boxscore$`TOV% OPP`[1]
        temp_icon = icon("fa-solid fa-thumbs-up", verify_fa = FALSE)
        if (team < league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa = FALSE)}
        infoBox(
          title="Opponent Turnover%",
          subtitle=sprintf("League Average: %.1f%s", mean(rv$boxscore$`TOV% OPP`), "%"),
          value=sprintf("%.1f%s (%d)", rv$cum_boxscore$`TOV% OPP`[1], "%", get_rank("TOV% OPP")),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=temp_icon
        )
      })
      output$var_card4 = renderInfoBox({
        # Opponent PITP
        league = mean(rv$boxscore$`PITP OPP`)
        team = rv$cum_boxscore$`PITP OPP`[1]
        temp_icon = icon("fa-solid fa-thumbs-up", verify_fa = FALSE)
        if (team >= league) {temp_icon = icon("fa-solid fa-thumbs-down", verify_fa = FALSE)}
        infoBox(
          title="Opponent Points In the Paint",
          subtitle=sprintf("League Average: %.1f", mean(rv$boxscore$`PITP OPP`)),
          value=sprintf("%.1f (%d)", rv$cum_boxscore$`PITP OPP`[1], get_rank("PITP OPP", descend=F)),
          color=rv$box_team_color[input$dashboard_team],
          fill=TRUE,
          icon=temp_icon
        )
      })
    }
  })
  
  ## Tab - Head-to-Head ##
  observeEvent(input$h2h_team1, {
    # Record Box Team 1
    output$h2h_record1 = renderInfoBox({
      counts = rv$boxscore %>%
        filter(`TEAM` == input$h2h_team1) %>%
        count(`W/L`) %>%
        pull(n)
      rtgs = rv$boxscore %>%
        filter(`TEAM` == input$h2h_team1) %>%
        summarise(off=mean(`OFFRTG`), def=mean(`DEFRTG`), net=mean(`NETRTG`))
      infoBox(
        title="Record (OFFRTG : DEFRTG)",
        value=sprintf("%d - %d", counts[2], counts[1]),
        subtitle=sprintf("%.1f (%d) : %.1f (%d) (NETRTG %.1f (%d))", 
                         rtgs$off[1], get_rank("OFFRTG", team2=input$h2h_team1),
                         rtgs$def[1], get_rank("DEFRTG", descend=F, team2=input$h2h_team1),
                         rtgs$net[1], get_rank("NETRTG", team2=input$h2h_team1)),
        color=rv$box_team_color[input$h2h_team1],
        fill=TRUE,
        width=4,
        icon=icon("fa-solid fa-user", verify_fa=FALSE)
      )
    })
    observeEvent(input$h2h_team2, {
      # Record Box Team 2
      output$h2h_record2 = renderInfoBox({
        counts = rv$boxscore %>%
          filter(`TEAM` == input$h2h_team2) %>%
          count(`W/L`) %>%
          pull(n)
        rtgs = rv$boxscore %>%
          filter(`TEAM` == input$h2h_team2) %>%
          summarise(off=mean(`OFFRTG`), def=mean(`DEFRTG`), net=mean(`NETRTG`))
        infoBox(
          title="Record (OFFRTG : DEFRTG)",
          value=sprintf("%d - %d", counts[2], counts[1]),
          subtitle=sprintf("%.1f (%d) : %.1f (%d) (NETRTG %.1f (%d))", 
                           rtgs$off[1], get_rank("OFFRTG", team2=input$h2h_team2),
                           rtgs$def[1], get_rank("DEFRTG", descend=F, team2=input$h2h_team2),
                           rtgs$net[1], get_rank("NETRTG", team2=input$h2h_team2)),
          color=rv$box_team_color[input$h2h_team2],
          fill=TRUE,
          width=4,
          icon=icon("fa-solid fa-user", verify_fa=FALSE)
        )
      })
      # Store H2H cumulative boxscore
      rv$h2h_cum_boxscore = rv$boxscore %>%
        group_by(`TEAM`) %>%
        arrange(`TEAM`,`GAME DATE`) %>%
        mutate(across(where(is.numeric), cummean)) %>% # cumulative mean for numeric columns
        arrange(desc(`GAME DATE`))
      # Percentage-scale Bar Plots
      perc_teams = rv$h2h_cum_boxscore %>%
        select(contains("%")) %>%
        filter(`TEAM` == input$h2h_team1 | `TEAM` == input$h2h_team2) %>% # already grouped
        slice_head(n=1)%>%
        pivot_longer(cols=contains("%"), names_to="Stat", values_to="Percent")
      # team 1 (left=offense, right=defense)
      perc_team1 = perc_teams %>%
        filter(`TEAM` == input$h2h_team1) %>%
        mutate(`Side`=c("LEFT","RIGHT")[grepl("OPP", `Stat`)+1]) %>%
        mutate(`Type`=ifelse(`Side`=="LEFT", paste0(input$h2h_team1, " Offense"), paste0(input$h2h_team1, " Defense"))) %>%
        mutate(across(`Stat`, str_replace, " OPP", ""))
      # team 2 (right=offense, left=defense)
      perc_team2 = perc_teams %>%
        filter(`TEAM` == input$h2h_team2) %>%
        mutate(`Side`=c("RIGHT","LEFT")[grepl("OPP", `Stat`)+1]) %>%
        mutate(`Type`=c(sprintf("%s Offense",input$h2h_team2),sprintf("%s Defense", input$h2h_team2))[grepl("OPP", `Stat`)+1]) %>%
        mutate(across(`Stat`, str_replace, " OPP", ""))
      perc_teams = perc_team1 %>% bind_rows(perc_team2)
      p1 = perc_teams %>%
        ggplot(aes(x=`Stat`, text=sprintf("%s\n%s\nPercent: %.1f",`Type`,`Stat`,`Percent`))) +
        geom_col(data=subset(perc_teams, `Side`=="RIGHT"),
                 aes(y=`Percent`, fill=`Type`), position=position_dodge()) +
        geom_col(data=subset(perc_teams, `Side`=="LEFT"),
                 aes(y=-`Percent`, fill=`Type`), position=position_dodge()) +
        coord_flip() +
        scale_y_continuous(breaks = seq(-100,100, by = 10),
                           labels = c(seq(100, 0, by = -10), seq(10,100,by=10))) + # creates scale
        geom_vline(xintercept=0) +
        labs(x="")
      output$h2h_bar_perc = renderPlotly({
        ggplotly(p1, tooltip=c("text")) %>%
          layout(legend=list(x=0.3, y=1.1, orientation="h"))
      })
      # Integer-scale Bar Plots
      ints = c("OREB","DREB","AST","TOV","STL","BLK","PF","PTSOFF TO","2NDPTS",
               "FBPS","PITP","2PA","3PA","FTA","FGA","PTS","OFFRTG","DEFRTG",
               "PACE")
      keep = c(ints, paste0(ints, " OPP"))
      int_teams = rv$h2h_cum_boxscore %>%
        select(all_of(keep)) %>%
        slice_head(n=1) %>% # subset first row (grouped by team)
        filter(`TEAM` == input$h2h_team1 | `TEAM` == input$h2h_team2) %>%
        pivot_longer(cols=all_of(keep), names_to="Stat", values_to="Value")
      # team 1 (left=offense, right=defense)
      int_team1 = int_teams %>%
        filter(`TEAM` == input$h2h_team1) %>%
        mutate(`Side`=c("LEFT","RIGHT")[grepl("OPP", `Stat`)+1]) %>%
        mutate(`Type`=c(sprintf("%s Offense", input$h2h_team1),sprintf("%s Defense", input$h2h_team1))[grepl("OPP", `Stat`)+1]) %>%
        mutate(across(`Stat`, str_replace, " OPP", ""))
      # team 2 (right=offense, left=defense)
      int_team2 = int_teams %>%
        filter(`TEAM` == input$h2h_team2) %>%
        mutate(`Side`=c("RIGHT","LEFT")[grepl("OPP", `Stat`)+1]) %>%
        mutate(`Type`=c(sprintf("%s Offense", input$h2h_team2),sprintf("%s Defense", input$h2h_team2))[grepl("OPP", `Stat`)+1]) %>%
        mutate(across(`Stat`, str_replace, " OPP", ""))
      int_teams = int_team1 %>% bind_rows(int_team2)
      p2 = int_teams %>%
        ggplot(aes(x=`Stat`, text=sprintf("%s\n%s\nValue: %.1f",`Type`,`Stat`,`Value`))) +
        geom_col(data=subset(int_teams, `Side`=="RIGHT"),
                 aes(y=`Value`, fill=`Type`), position=position_dodge()) +
        geom_col(data=subset(int_teams, `Side`=="LEFT"),
                 aes(y=-`Value`, fill=`Type`), position=position_dodge()) +
        coord_flip() +
        scale_y_continuous(breaks = seq(-130,130, by = 10),
                           labels = c(seq(130, 0, by = -10), seq(10,130,by=10))) + # creates scale
        geom_vline(xintercept=0) +
        labs(x="")
      output$h2h_bar_int = renderPlotly({
        ggplotly(p2, tooltip=c("text")) %>%
          layout(legend=list(x=0.3, y=1.1, orientation="h"))
      })
    })
  })
  # players table
  output$players_team = DT::renderDataTable({
    rv$players %>%
      filter(`TEAM` == input$dashboard_team) %>%
      arrange(desc(`PTS`)) %>%
      select(`PLAYER`,`TEAM`,`AGE`,`GP`,`MIN`,`PTS`,`FG%`,`3P%`,`FT%`,
             `REB`,`AST`,`TOV`,`STL`,`BLK`,`PF`,`FP`,`+/-`,`OFFRTG`,`DEFRTG`)
  }, options = list(scrollX=TRUE)
  )
  
  ## Tab - Boxscores ##
  output$boxscore = DT::renderDataTable({
    temp = rv$boxscore
    if (!is.null(input$boxscore_team)) {
      temp = temp %>%
        filter(`TEAM` %in% input$boxscore_team)
    }
    return(temp)
  }, options = list(scrollX = TRUE)
  )
  
  ## Tab -  League Stats ##
  league_barplot = function(col, descend=T) {
    x = rv$cum_all %>%
      filter(`GAME DATE` <= input$date_slider) %>%
      slice_tail(n=1) # last row (latest date)
    x = x[order(x[,col], decreasing=descend),] %>% # sort highest to lowest (default)
      head(input$league_n_slider) # top 10
    if (descend == FALSE) {
      p = x %>%
        ggplot(aes(x=reorder(`TEAM`, -get(col)), y=get(col), fill=`COLOR`)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=sprintf("%.1f", get(col))), size=5, color="white",
                  position=position_stack(vjust=0.9)) +
        scale_fill_identity() +
        coord_flip() +
        theme(legend.position="none") +
        labs(x="TEAM", y=col)
    } else {
      p = x %>%
        ggplot(aes(x=reorder(`TEAM`, get(col)), y=get(col), fill=`COLOR`)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=sprintf("%.1f", get(col))), size=5, color="white",
                  position=position_stack(vjust=0.9)) +
        scale_fill_identity() +
        coord_flip() +
        theme(legend.position="none") +
        labs(x="TEAM", y=col)
    }
    return(p)
  }
  # Date Range Slider
  output$league_date_slider = renderUI({
    sliderInput(
      inputId="date_slider",
      label="Date:",
      min=min(rv$boxscore$`GAME DATE`),
      max=max(rv$boxscore$`GAME DATE`),
      value=max(rv$boxscore$`GAME DATE`),
      dragRange=FALSE,
      width="100%"
    )
  })
  # plots
  observeEvent(input$date_slider, {
    output$league_pts = renderPlot({league_barplot("PTS")})
    output$league_reb = renderPlot({league_barplot("REB")})
    output$league_ast = renderPlot({league_barplot("AST")})
    output$`league_fg%` = renderPlot({league_barplot("FG%")})
    output$`league_3p%` = renderPlot({league_barplot("3P%")})
    output$`league_2p%` = renderPlot({league_barplot("2P%")})
    output$league_blk = renderPlot({league_barplot("BLK")})
    output$league_stl = renderPlot({league_barplot("STL")})
    output$`league_fg%opp` = renderPlot({league_barplot("FG% OPP", descend=F)})
  })
}
