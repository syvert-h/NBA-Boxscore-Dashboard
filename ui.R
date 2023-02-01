library(shinydashboard)
library(DT)
library(plotly)

dashboardPage(
  skin="black",
  dashboardHeader(
    title="NBA Dashboard",
    disable=FALSE
  ),
  
  dashboardSidebar(
    sidebarMenu(id="sidebar_tab",
      menuItem("Player Stats", tabName="PlayerTab", icon=icon("dashboard", verify_fa = FALSE)),
      menuItem("Team Stats", tabName="TeamTab", icon=icon("dashboard", verify_fa=FALSE)),
      menuItem("Boxscore", tabName="BoxscoreTab", icon=icon("fa-solid fa-table", verify_fa = FALSE))
    ),
    disable=FALSE
  ),
  
  dashboardBody(
    ### Stylise CSS tags ###
    uiOutput("css_style"),
    ###
    tabItems(
      ### Tab - Team Stats ###
      tabItem(
        tabName = "TeamTab",
        fluidRow(
          column(
            width=2,
            selectInput(
              "team",
              label="Select a team:",
              choices=sort(c("BOS","MEM","DAL","ORL","HOU","UTA","LAC","DEN","ATL",
                             "SAS","NOP","OKC","PHI","CLE","MIN","CHA","LAL","CHI",
                             "NYK","MIL","BKN","IND","TOR","POR","DET","SAC","PHX",
                             "MIA","GSW","WAS")),
              selected="BOS",
              width=NULL
            )
          )
        ),
        fluidRow(
          column(
            width=3,
            box(id="team_record", 
                title="Team Record", 
                width=NULL, height="215px",
                solidHeader=T, status="success",
                div(
                  imageOutput("team_logo", height="100px"),
                  style="text-align: center;"
                ),
                br(),
                htmlOutput("team_record")
            ),
            infoBoxOutput("team_rtg_box", width=NULL),
            infoBoxOutput("team_pts_box", width=NULL),
            infoBoxOutput("team_reb_box", width=NULL),
            infoBoxOutput("team_ast_box", width=NULL),
            infoBoxOutput("team_tov_box", width=NULL)
          ),
          column(
            width=9,
            fluidRow(
              column(
                width=8,
                tabBox(
                  id="team_box11",
                  width=NULL,
                  tabPanel("Shooting", plotlyOutput("team_shoot", height="300px")),
                  tabPanel("Rebounding", plotlyOutput("team_rebound", height="300px")),
                  tabPanel("Defense", plotlyOutput("team_defense", height="300px")),
                  tabPanel("Offense", plotlyOutput("team_offense", height="300px"))
                )
              ),
              column(
                width=4,
                uiOutput("team_box12")
              )
            ),
            fluidRow(
              column(
                width=5,
                uiOutput("team_box21")
              ),
              column(
                width=7,
                uiOutput('team_box22')
              )
            )
          )
        )
      ),
      
      
      ### Tab - Player Stats ###
      tabItem(
        tabName = "PlayerTab",
        fluidRow(
          column(
            width=3,
            uiOutput("player_input")
          ),
          column(
            width=6,
            uiOutput("player_date_range")
          )
        ),
        fluidRow(
          column(
            width=3,
            infoBoxOutput("player_rtg_box", width=NULL),
            infoBoxOutput("player_pts_box", width=NULL),
            infoBoxOutput("player_reb_box", width=NULL),
            infoBoxOutput("player_ast_box", width=NULL),
            infoBoxOutput("player_tov_box", width=NULL),
            box(id="player_team_record", 
                title="Current Team & Player Record", 
                width=NULL, height="220px",
                solidHeader=T, status="success",
                div(
                  imageOutput("player_team_logo", height="100px"),
                  style="text-align: center;"
                ),
                br(),
                htmlOutput("player_record")
            )
          ),
          column(
            width=9,
            fluidRow(
              uiOutput("player_box11"),
              infoBoxOutput("player_splits", width=5),
              uiOutput("player_box12")
            ),
            fluidRow(
              uiOutput("player_box21"),
              uiOutput("player_box22")
            )
          )
        )
      ),
      tabItem(
        tabName="BoxscoreTab",
        fluidRow(
          column(
            width=4,
            uiOutput("boxscore_filter")
          )
        ),
        fluidRow(
          column(
            width=12,
            DT::dataTableOutput("all_boxscores")
          )
        )
      )
    )
  )
)