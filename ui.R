library(shinydashboard)
library(DT)
library(plotly)
library(shinyjs)

dashboardPage(
  skin="black",
  dashboardHeader(
    title="NBA Team Dashboard",
    titleWidth=250,
    disable=FALSE
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team Stats", tabName="team-stats", icon=icon("dashboard", verify_fa = FALSE)),
      menuItem("League Stats", tabName="league-stats", icon=icon("dashboard", verify_fa=FALSE)),
      menuItem("Head-to-Head", tabName="h2h", icon=icon("fa-solid fa-people-arrows", verify_fa = FALSE)),
      menuItem("Boxscore", tabName="boxscore", icon=icon("fa-solid fa-table", verify_fa = FALSE))
    ),
    disable=FALSE
  ),
  
  dashboardBody(
    tabItems(
      ## First Tab - Season Stats ##
      tabItem(
        tabName="team-stats",
        fluidRow(
          column(
            width=2,
            selectInput(
              "dashboard_team",
              label="Team:",
              choices=sort(c("BOS","MEM","DAL","ORL","HOU","UTA","LAC","DEN","ATL",
                             "SAS","NOP","OKC","PHI","CLE","MIN","CHA","LAL","CHI",
                             "NYK","MIL","BKN","IND","TOR","POR","DET","SAC","PHX",
                             "MIA","GSW","WAS"))
            )
          ),
          column(
            width=10,
            plotlyOutput("previous_bar", height="100px")
          )
        ),
        fluidRow( # League-wide stats
          infoBoxOutput("record"),
          infoBoxOutput("rtg"),
          infoBoxOutput("avgPTS"),
        ),
        fluidRow(
          column(
            width=4,
            conditionalPanel(
              "input.tabset1 == 'Shooting'",
              box(
                id="win_loss_box",
                width=NULL,
                plotlyOutput("scoring_pie", height="273px")
              )
            ),
            conditionalPanel(
              "input.tabset1 == 'Rebounding'",
              box(
                width=NULL,
                plotlyOutput("rebounding_bar", height="273px") 
              )
            ),
            infoBoxOutput("var_card1", width=NULL),
            infoBoxOutput("var_card2", width=NULL),
            conditionalPanel(
              "input.tabset1 == 'Offense' || input.tabset1 == 'Defense'",
              infoBoxOutput("var_card3", width=NULL),
              infoBoxOutput("var_card4", width=NULL),
              infoBoxOutput("var_card5", width=NULL)
            )
          ),
          tabBox(
            title="",
            id="tabset1",
            height="510px",
            width=4,
            tabPanel(
              "Shooting",
              plotlyOutput("shooting_dash")
            ),
            tabPanel(
              "Rebounding",
              plotlyOutput("rebounding_dash")
            ),
            tabPanel(
              "Offense",
              plotlyOutput("offense_dash")
            ),
            tabPanel(
              "Defense",
              plotlyOutput("defense_dash")
            )
          ),
          column(
            width=4,
            box(
              id="win_loss_box",
              width=NULL,
              title="Win-Loss Formula",
              plotOutput("dash_tree", height="200px")
            ),
            valueBoxOutput("tree_win_acc", width=NULL),
            valueBoxOutput("tree_loss_acc", width=NULL)
          )
        ),
        fluidRow(
          column(
            width=12,
            DT::dataTableOutput("players_team")
          )
        )
      ),
      
      ## Second Tab - Comparison ##
      tabItem(
        tabName="h2h",
        fluidRow(
          column(
            width=6, align="center",
            selectInput(
              "h2h_team1",
              label="Team:",
              choices=sort(c("BOS","MEM","DAL","ORL","HOU","UTA","LAC","DEN","ATL",
                             "SAS","NOP","OKC","PHI","CLE","MIN","CHA","LAL","CHI",
                             "NYK","MIL","BKN","IND","TOR","POR","DET","SAC","PHX",
                             "MIA","GSW","WAS"))
            )
          ),
          column(
            width=6, align="center",
            selectInput(
              "h2h_team2",
              label="Team:",
              choices=sort(c("BOS","MEM","DAL","ORL","HOU","UTA","LAC","DEN","ATL",
                             "SAS","NOP","OKC","PHI","CLE","MIN","CHA","LAL","CHI",
                             "NYK","MIL","BKN","IND","TOR","POR","DET","SAC","PHX",
                             "MIA","GSW","WAS")),
              selected="BKN"
            )
          )
        ),
        fluidRow(
          column(width=1),
          column(
            width=4,
            infoBoxOutput("h2h_record1", width=NULL)
          ),
          column(width=1),
          column(width=1),
          column(
            width=4,
            infoBoxOutput("h2h_record2", width=NULL)
          ),
          column(width=1)
        ),
        fluidRow(
          column(
            width=12,
            plotlyOutput("h2h_bar_perc")
          )
        ),
        fluidRow(
          column(
            width=12,
            plotlyOutput("h2h_bar_int")
          )
        )
      ),
      
      ## Tab - Boxscores ##
      tabItem(
        tabName="boxscore",
        fluidRow(
          column(
            width=4,
            selectInput(
              "boxscore_team",
              label="Team:",
              choices=c("BOS","MEM","DAL","ORL","HOU","UTA","LAC","DEN","ATL",
                        "SAS","NOP","OKC","PHI","CLE","MIN","CHA","LAL","CHI",
                        "NYK","MIL","BKN","IND","TOR","POR","DET","SAC","PHX",
                        "MIA","GSW","WAS"),
              multiple=TRUE
            )
          )
        ),
        fluidRow(
          column(
            width=12,
            DT::dataTableOutput("boxscore")
          )
        )
      ),
      
      ## Tab - NBA League Stats ##
      tabItem(
        tabName="league-stats",
        fluidRow(
          column(
            width=9,
            uiOutput("league_date_slider")
          ),
          column(
            width=3,
            sliderInput("league_n_slider", "Top N Teams:", 1, 30, 5)
          )
        ),
        fluidRow(
          column(width=12,h2("Main Stats"))
        ),
        fluidRow(
          column(
            width=4,
            box(
              id="league_pts_box",
              width=NULL,
              title="Top Scoring Teams",
              plotOutput("league_pts")
            )
          ),
          column(
            width=4,
            box(
              id="league_reb_box",
              width=NULL,
              title="Top Rebounding Teams",
              plotOutput("league_reb")
            )
          ),
          column(
            width=4,
            box(
              id="league_ast_box",
              width=NULL,
              title="Top Assisting Teams",
              plotOutput("league_ast")
            )
          )
        ),
        fluidRow(
          column(width=12,h2("Shooting Stats"))
        ),
        fluidRow(
          column(
            width=4,
            box(
              id="league_fg%_box",
              width=NULL,
              title="Top FG% Teams",
              plotOutput("league_fg%")
            )
          ),
          column(
            width=4,
            box(
              id="league_3p%_box",
              width=NULL,
              title="Top 3P% Teams",
              plotOutput("league_3p%")
            )
          ),
          column(
            width=4,
            box(
              id="league_2p%_box",
              width=NULL,
              title="Top 2P% Teams",
              plotOutput("league_2p%")
            )
          )
        ),
        fluidRow(
          column(width=12,h2("Defensive Stats"))
        ),
        fluidRow(
          column(
            width=4,
            box(
              id="league_blk_box",
              width=NULL,
              title="Top Shot Blocking Teams",
              plotOutput("league_blk")
            )
          ),
          column(
            width=4,
            box(
              id="league_stl_box",
              width=NULL,
              title="Top Stealing Teams",
              plotOutput("league_stl")
            )
          ),
          column(
            width=4,
            box(
              id="league_fg%opp_box",
              width=NULL,
              title="Top Defending Teams",
              plotOutput("league_fg%opp")
            )
          )
        )
      )
    )
  )
)
