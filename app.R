#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(encoding = "UTF-8")
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
options(scipen=10000)
#set_config(use_proxy(url="10.3.100.207",port=8080))

source("./FPL_Functions.R")
#library(shinymanager)
#library(polished)
#library(config)
library(shinyWidgets)
library(shinydashboard)
library(bslib)
library(fresh)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(ggh4x)
library(bslib)
library(DT)
#library(ggplot2bdc)
library(httr)
library(jsonlite)
library(ggnewscale)
library(ggrepel)
library(plotly)
library(forcats)
library(zoo)
library(ggborderline)
library(shadowtext)
library(ggtext)
library(tidytext)
library(stringr)
library(extrafont)
library(ggbump)
library(readxl)
library(tibble)
library(readr)
#library(gganimate)
#library(gifski)

#loadfonts(device = "pdf")


mytheme <- create_theme(
  adminlte_color(
    # Header Colour
    light_blue = "#1D3557"
  ),
  adminlte_sidebar(
    width = "250px",
    # Sidebar Background
    dark_bg = "#457B9D",
    # Hover Background
    dark_hover_bg = "#A8DADC",
    # Text Background
    dark_color = "#F1FAEE"
  ),
  adminlte_global(
    # Whole Background
    content_bg = "#FDFFFC", #white
    # Box Background
    box_bg = "#EBFAFA", 
    # No idea what this does
    info_box_bg = "#E63946"
  )
)

c('#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C','#758D99')

c('#E9EDF0','#B7C6CF','#758D99','#3F5661')

c('#87274d','#bb5434','#347aad','#152a56','#277b8e','#f4a362')

scatter_variables <- c("Value","Goals","xG Percentile","xG Value","Assists","xA Percentile","xA Value","xG+xA Percentile","xG+xA Value","G+A Percentile","G+A","Mins Played","Bonus Points","xG Conceded","Total Points Percenitle","Total Points","Defensive Contributions","DEFCON")


######################
### Data Wrangling ###
######################

## General Info

FPL_General <- GET('https://fantasy.premierleague.com/api/bootstrap-static/')

jsonRespText<-content(FPL_General, as="text") 
JSON_Output <- fromJSON(jsonRespText)
current_players <- JSON_Output$total_players

#Teams <- JSON_Output$teams %>%
#  select(id, name, short_name, strength, strength_overall_home, strength_overall_away, strength_attack_home, strength_attack_away, strength_defence_home, strength_defence_away) %>%
#  rename(team_name = name,
#         short_team_name = short_name,
#         code = id)

#General_Info <- JSON_Output$elements %>%
#  select(id, first_name, second_name, web_name, team, element_type, photo) %>%
#  left_join(Teams, by = c('team' = 'code'))



##############################
#### Define Data Frames ######
##############################

Players_Gameweek <- read.csv('./25_26/Players_Gameweek.csv')

Players_History <- read.csv('./25_26/Players_History.csv')
#Players_History <- All_Players_Final

General_History <- read.csv('./25_26/General_Info.csv')
#General_History <- General_Info

Fixtures <- read.csv('./25_26/Fixtures.csv')

#Team_Summary <- read.csv('./25_26/Team_Summary.csv')

#team_id <- '110079'
#team_id <- '18148'
team_id <- '75141'

FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id,'/'))

jsonRespText<-content(FPL_Team, as="text") 
JSON_Output <- fromJSON(jsonRespText)

Team <- JSON_Output$name
Name <- paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name)
Country <- JSON_Output$player_region_name

prediction_vector <- c(32,33,34,35,36,37,38)
#prediction_vector <- c(1,2,3,4)

short_team_vec <- c("ARS","AVL","BHA","BOU","BRE","BUR","CHE","CRY","EVE","FUL","LIV","LUT","MCI","MUN","NEW","NFO","SHU","TOT","WHU","WOL")

#prediction_path <- './fplreview_1712224410.csv'

## Sidebar content

ui <- dashboardPage(
  #dashboardHeader(title = "FPL Dashboard <img src='./Logo4.png' width='5'>"),
  dashboardHeader(
    title = "FPL Dashboard",
    
    # Dropdown menu for messages
    dropdownMenu(type = "messages", badgeStatus = "success",
                 messageItem("Welcome!",
                             "Enjoy All Of Your FPL Data!",
                             time = format(round(Sys.time() - as.POSIXct("2024-02-09 13:59:23 GMT")), format = "%M")
                 )
    )),
  #                tags$img(src='./Logo4.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 250), style = "text-align: center;"),
  #dashboardHeader(title = tags$a(tags$img(src = "./Logo4.png", align = "left"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Comparison", tabName = "dashboard", icon = icon("zoom-out", lib = "glyphicon")),
      menuItem("Rankings", tabName = "ratings", icon = icon("equalizer", lib = "glyphicon")),
      menuItem("Bench & Captaincy", tabName = "bench", icon = icon("screenshot", lib = "glyphicon")),
      menuItem("Transfers", tabName = "transfers_tab", icon = icon("resize-small", lib = "glyphicon")),
      menuItem("Mini-League Data", tabName = "leagues", icon = icon("sort", lib = "glyphicon")),
      menuItem("Ownership", tabName = "ownership_tab", icon = icon("heart", lib = "glyphicon")),
      menuItem("Fixtures", tabName = "fixtures_tab", icon = icon("calendar", lib = "glyphicon")),
      menuItem("Team Data", tabName = "teams_tab", icon = icon("ok-circle", lib = "glyphicon"))#,
      #menuItem("Prediction Data", tabName = "predictions_tab", icon = icon("stats", lib = "glyphicon"), badgeLabel = "New!", badgeColor = "green")
      )),
  dashboardBody(use_theme(mytheme),
                includeCSS("www/custom.css"),
                #tags$head(
                #  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(tags$img(src='Logo Player Comparison.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 300), style = "text-align: center;"),
                fluidRow(align = "center",
                  box(status = "info",
                      collapsible = TRUE,
                      width = 2, 
                      selectInput("positions", label = "Positions:", choices = c("GK","DEF","MID","ST"), selected = c("ST"))),
                  box(status = "success",
                      collapsible = TRUE,
                      width = 3, 
                      sliderInput("gameweeks", label = "Gameweeks:", min = min(Players_History$round), max = max(Players_History$round), step = 1, value = c(max(Players_History$round) - 5,max(Players_History$round)))),
                  box(status = "primary",
                      collapsible = TRUE,
                      width = 2, 
                      sliderInput("starts_1", label = "Minimum Starts:", min = min(Players_History$round), max = max(Players_History$round), step = 1, value = 2)),
                  box(status = "warning",
                      collapsible = TRUE,
                      width = 3, 
                      sliderInput("value", label = "Maximum Player Value:", min = 0, max = (max(Players_History$value) / 10), step = 0.5, value = 10)),
                  box(status = "danger",
                      collapsible = TRUE,
                      inline = FALSE,
                      width = 2, 
                      selectInput("calculations", label = "Select Calculation Type:", choices = c("Total","Per 90","Per Game"))),
                  box(status = "primary",
                      collapsible = TRUE,
                      inline = FALSE,
                      width = 12, 
                      dataTableOutput('table1')),
                  box(status = "primary",
                      collapsible = TRUE,
                      width = 6, 
                      selectInput("player_metric", label = "Select Metric For Players:", choices = c("Goals","xG Value","Assists","xA Value","xG+xA Value","G+A","Mins Played","Bonus Points","Total Points","DEFCON","Defensive Contributions"))),
                  box(status = "primary",
                      collapsible = TRUE,
                      width = 6, 
                      sliderInput("top", label = "Select No. Of Top Performers:", min = 0, max = 20, step = 1, value = 10)),
                  box(status = "primary",
                      collapsible = TRUE,
                      inline = FALSE,
                      width = 12,
                      #progressBar(id = "pb9", value = 0, display_pct = T),
                      #actionButton("update9" ,"Update Player Bar Plot", icon("arrows-rotate"),
                      #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                      #             class = "btn btn-primary"),
                      plotOutput("player_metric_plot"), 
                      downloadButton("download_bar_player", label = "Download this plot")),
                  #box(status = "primary",
                  #    collapsible = TRUE,
                  #    width = 6, 
                  #    selectizeInput("teams", label = "Select Teams For Player Comparison:", choices = sort(unique(Players_History$short_team_name)), selected = sort(unique(Players_History$short_team_name))[1], options = list(placeholder = "Select Teams"), multiple = TRUE)),
                  box(status = "primary",
                      collapsible = TRUE,
                      inline = FALSE,
                      width = 12, 
                      selectizeInput("players", label = "Select Players:", choices = sort(unique(Players_History$name_club)), selected = sort(unique(Players_History$name_club))[1], options = list(placeholder = "Select Players"), multiple = TRUE)),
                  box(status = "primary",
                      collapsible = TRUE,
                      inline = FALSE,
                      width = 12,
                      #progressBar(id = "pb10", value = 0, display_pct = T),
                      #actionButton("update10" ,"Update Player Trend Graph", icon("arrows-rotate"),
                      #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                      #             class = "btn btn-primary"),
                      plotOutput("player_points_plot"), 
                      downloadButton("download_points_player", label = "Download this plot")),
                  box(status = "info",
                      collapsible = TRUE,
                      width = 6, 
                      selectInput("scatter_x", label = "X Variable:", choices = scatter_variables, selected = scatter_variables[4])),
                  box(status = "info",
                      collapsible = TRUE,
                      width = 6, 
                      selectInput("scatter_y", label = "Y Variable:", choices = scatter_variables, selected = scatter_variables[7])),
                  box(status = "primary",
                      collapsible = TRUE,
                      inline = FALSE,
                      width = 12,
                      #progressBar(id = "pb10", value = 0, display_pct = T),
                      #actionButton("update10" ,"Update Player Trend Graph", icon("arrows-rotate"),
                      #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                      #             class = "btn btn-primary"),
                      plotlyOutput("scatter_plot"),
                      downloadButton("download_scatter", label = "Download plot png"),
                      downloadButton("download_scatter_html", label = "Download plot html")),
                      #),
                )),
        
        
        # Second tab content
        tabItem(tabName = "ratings",
                fluidRow(tags$img(src='Logo Rankings.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 300), style = "text-align: center;"),
                fluidRow(align = "center",
                         box(status = "info",
                             collapsible = TRUE,
                             width = 12, 
                             numericInput("team_id", label = "Enter FPL ID:", value = "75141"),
                             #submitButton("Update Team Info"),
                             textOutput("team_text"),
                             textOutput("player_text"),
                             textOutput("country_text")),
                             #actionButton("update3" ,"Generate Plots", icon("arrows-rotate"),
                            #               style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                            #               class = "btn btn-primary")),
                         box(status = "primary",
                             collapsible = TRUE,
                             width = 12, 
                            # progressBar(id = "pb2", value = 0, display_pct = T),
                             #actionButton("update3" ,"Update GW Plot", icon("arrows-rotate"),
                            #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                            #              class = "btn btn-primary"),
                             plotOutput("plot2"), downloadButton("download_rank", label = "Download this plot")),  
                         box(status = "primary",
                             collapsible = TRUE,
                             width = 12,
                             #progressBar(id = "pb", value = 0, display_pct = T),
                             #actionButton("update4" ,"Update Rank Plot", icon("arrows-rotate"),
                            #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                            #              class = "btn btn-primary"),
                             plotOutput("plot3"), downloadButton("download_rank2", label = "Download this plot")),
                         )),
        
        # Bench tab content
        tabItem(tabName = "bench",
                fluidRow(tags$img(src='Logo Bench.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 300), style = "text-align: center;"),
                fluidRow(align = "center",
                         box(status = "info",
                             collapsible = TRUE,
                             width = 12, 
                             numericInput("bench_team_id", label = "Enter FPL ID:", value = "75141"),
                             #submitButton("Update Team Info"),
                             textOutput("bench_team_text"),
                             textOutput("bench_player_text"),
                             textOutput("bench_country_text")),
                             #actionButton("bench_update" ,"Generate Plots", icon("arrows-rotate"),
                            #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                            #              class = "btn btn-primary")),
                         box(status = "primary",
                             collapsible = TRUE,
                             width = 12, 
                             #progressBar(id = "pb5", value = 0, display_pct = T),
                             #actionButton("update5" ,"Update Bench Plot", icon("arrows-rotate"),
                             #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                             #              class = "btn btn-primary"),
                             plotOutput("bench_points"), downloadButton("download_bench", label = "Download this plot")),
                         box(status = "primary",
                             collapsible = TRUE,
                             width = 12, 
                             #progressBar(id = "pb6", value = 0, display_pct = T),
                             #actionButton("update6" ,"Update Captaincy Points", icon("arrows-rotate"),
                             #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                             #              class = "btn btn-primary"),
                             plotOutput("captaincy_points"), downloadButton("download_captaincy", label = "Download this plot")),
                )),
        
        
        # Third tab content
        tabItem(tabName = "transfers_tab",
                fluidRow(tags$img(src='Logo Transfers.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 300), style = "text-align: center;"),
                fluidRow(align = "center",
                         box(status = "info",
                             collapsible = TRUE,
                             width = 6, 
                             numericInput("team_id3", label = "Enter FPL ID:", value = "75141")),
                             #actionButton("update7" ,"Update Transfer Points", icon("arrows-rotate"),
                            #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                            #              class = "btn btn-primary")),
                             #submitButton("Update Team Info"),
                             #textOutput("team_text"),
                             #textOutput("player_text"),
                             #textOutput("country_text")),
                         box(status = "primary",
                             collapsible = TRUE,
                             width = 6,
                             selectInput("transfer_input", label = "Include Gameweeks With WC & FH:", choices = c("Yes", "No"), selected = c("Yes"))),
                         box(status = "primary",
                             collapsible = TRUE,
                             inline = FALSE,
                             width = 12, 
                             dataTableOutput('table2')),
                         box(status = "primary",
                             collapsible = TRUE,
                             width = 12, 
                            # progressBar(id = "pb7", value = 0, display_pct = T),
                            # actionButton("update7" ,"Update Transfer Points", icon("arrows-rotate"),
                            #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                            #              class = "btn btn-primary"),
                             plotOutput("transfer_plot"), downloadButton("download_transfer1", label = "Download this plot")),
                         box(status = "primary",
                             collapsible = TRUE,
                             width = 12, 
                             #progressBar(id = "pb8", value = 0, display_pct = T),
                             #actionButton("update8" ,"Update Transfer Difference Points", icon("arrows-rotate"),
                            #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                            #              class = "btn btn-primary"),
                             plotOutput("transfer_diff_plot"), downloadButton("download_transfer2", label = "Download this plot")),
                         box(status = "primary",
                             collapsible = TRUE,
                             width = 12, 
                             #progressBar(id = "pb8", value = 0, display_pct = T),
                             #actionButton("update8" ,"Update Transfer Difference Points", icon("arrows-rotate"),
                             #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                             #             class = "btn btn-primary"),
                             plotOutput("transfer_cum_plot"), downloadButton("download_transfer_cum", label = "Download this plot")),
                         box(status = "primary",
                             collapsible = TRUE,
                             width = 12, 
                             #progressBar(id = "pb8", value = 0, display_pct = T),
                             #actionButton("update8" ,"Update Transfer Difference Points", icon("arrows-rotate"),
                             #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                             #             class = "btn btn-primary"),
                             plotOutput("transfer_hits_plot"), downloadButton("download_transfer_hits", label = "Download this plot")),
                )),
# Fourth tab content
tabItem(tabName = "fixtures_tab",
        fluidRow(tags$img(src='Logo Fixtures.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 300), style = "text-align: center;"),
        fluidRow(align = "center",
                 box(status = "success",
                     collapsible = TRUE,
                     width = 4, 
                     checkboxGroupInput("gameweeks_2", label = "Gameweeks:", choices = sort(unique(Fixtures$event)), selected = sort(unique(Fixtures$event))[1:3])),
                     #sliderInput("gameweeks_2", label = "Gameweeks:", min = min(Players_History$round), max = max(Players_History$round), step = 1, value = c(max(Players_History$round) - 5,max(Players_History$round)))),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 4,
                     selectInput("fixture_input", label = "Team Strength:", choices = c("Overall", "Attack", "Defence"), selected = c("Overall"))),
                 box(status = "success",
                     collapsible = TRUE,
                     width = 4, 
                     sliderInput("fixture_slider", label = "Adjust Colour:", min = 0, max = 400, step = 20, value = 150)),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 12, 
                     plotOutput("fixture_plot"), downloadButton("download_fixtures"), label = "Download this plot"),
        )),
        
# Fifth tab content
tabItem(tabName = "leagues",
        fluidRow(tags$img(src='Logo League.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 300), style = "text-align: center;"),
        fluidRow(align = "center",
                 box(status = "info",
                     collapsible = TRUE,
                     width = 3, 
                     numericInput("team_id2", label = "Enter FPL ID (Copy & Paste ID):",  value = 75141, step = 1, min = 1, max = current_players),
                     textOutput("team_text2"),
                     textOutput("player_text2"),
                     textOutput("country_text2")),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 3,
                     selectInput("league1", label = "Select Private League:", choices = c("Candy Shop Bois V"), selected = c("Candy Shop Bois V"))),
                 box(status = "warning",
                     collapsible = TRUE,
                     width = 3, 
                     selectInput("team_league", label = "Select Rival:", choices = c("Input Team ID"), selected = c("Input Team ID"))),
                 box(status = "danger",
                     collapsible = TRUE,
                     width = 3,
                     selectInput('input_rival', label = "Rival ID", choices = c('75141')),
                     textOutput("team_text3"),
                     textOutput("player_text3"),
                     textOutput("country_text3")),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 12, 
                     #actionButton("update" ,"Update GW Plot", icon("arrows-rotate"),
                    #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                    #           class = "btn btn-primary"),
                    # progressBar(id = "pb4", value = 0, display_pct = T),
                     plotOutput("league_plot"), downloadButton("download_leaguegw"), label = "Download this plot"),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 12, 
                     #actionButton("update2" ,"Update Rank Plot", icon("arrows-rotate"),
                    #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                    #              class = "btn btn-primary"),
                    # progressBar(id = "pb3", value = 0, display_pct = T),
                     plotOutput("league_gwplot"), downloadButton("download_leaguerank"), label = "Download this plot"),
                 box(status = "warning",
                     collapsible = TRUE,
                     width = 3, 
                     selectizeInput("team_league_selectize", label = "Select Rival Teams:", choices = c("Input Rival Team"), options = list(placeholder = "Select Rival Teams"), multiple = TRUE)),
                 box(status = "success",
                     collapsible = TRUE,
                     width = 3, 
                     sliderInput("league_gweeks", label = "Gameweeks:", min = min(Players_History$round), max = max(Players_History$round), step = 1, value = c(1,max(Players_History$round)))),
                 box(status = "success",
                     collapsible = TRUE,
                     width = 3, 
                     sliderInput("bump_breaks", label = "Gameweek Breaks:", min = 1, max = 4, step = 1, value = 1)),
                 box(status = "warning",
                     collapsible = TRUE,
                     width = 3, 
                     selectizeInput("team_highlight", label = "Select Teams To Highlight (Max = 7):", choices = c("Select Highlighted Team"), options = list(placeholder = "Select Highlighted Team", maxItems = 7), multiple = TRUE)),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 12, 
                     #actionButton("update2" ,"Update Rank Plot", icon("arrows-rotate"),
                     #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                     #              class = "btn btn-primary"),
                     # progressBar(id = "pb3", value = 0, display_pct = T),
                     plotOutput("team_bumpplot"), downloadButton("download_bump1", label = "Download This Plot")),
                 #box(status = "warning",
                #     collapsible = TRUE,
                #     width = 12, 
                #     selectizeInput("player_ownership", label = "Select 1 Player To Display Ownership:", choices = sort(unique(Players_History$name_club)), options = list(placeholder = "Select Player", maxItems = 1), multiple = TRUE)),
                # box(status = "primary",
                #     collapsible = TRUE,
                #     width = 12, 
                #     #actionButton("update2" ,"Update Rank Plot", icon("arrows-rotate"),
                #     #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                #     #              class = "btn btn-primary"),
                #     # progressBar(id = "pb3", value = 0, display_pct = T),
                #     plotOutput("ownership_plot"), downloadButton("ownership_download", label = "Download This Plot")),
                 
                 
                 
                 

        )), 
# Ownership tab content
tabItem(tabName = "ownership_tab",
        fluidRow(tags$img(src='Logo Ownership.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 300), style = "text-align: center;"),
        fluidRow(align = "center",
                 box(status = "info",
                     collapsible = TRUE,
                     width = 4, 
                     numericInput("own_team_id", label = "Enter FPL ID:", value = "75141"),
                     #submitButton("Update Team Info"),
                     textOutput("own_team_text"),
                     textOutput("own_player_text"),
                     textOutput("own_country_text")),
                 box(status = "success",
                     collapsible = TRUE,
                     width = 4, 
                     sliderInput("own_gweeks", label = "Gameweeks:", min = min(Players_History$round), max = max(Players_History$round), step = 1, value = c(1,max(Players_History$round)))),
                 box(status = "success",
                     collapsible = TRUE,
                     width = 4, 
                     sliderInput("own_number", label = "Select Total Players:", min = 5, max = 20, step = 1, value = 10)),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 12,
                     plotOutput("own_plot"), downloadButton("ownership_download2", label = "Download This Plot")),
                 box(status = "info",
                     collapsible = TRUE,
                     width = 12, 
                     selectInput("own_position", label = "Positions:", choices = c("GK","DEF","MID","ST"), selected = c("ST"))),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 12,
                     plotOutput("own_position_plot"), downloadButton("ownership_download3", label = "Download This Plot")),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 4,
                     selectInput("own_league", label = "Select Private League:", choices = c("Candy Shop Bois V"), selected = c("Candy Shop Bois V"))),
                 box(status = "warning",
                     collapsible = TRUE,
                     width = 4, 
                     selectizeInput("team_own_selectize", label = "Select Rival Teams:", choices = c("Input Rival Team"), options = list(placeholder = "Select Rival Teams"), multiple = TRUE)),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 4,
                     selectInput("player_ownership", label = "Select 1 Player To Diplay Ownership:", choices = sort(unique(Players_History$name_club)), selected = sort(unique(Players_History$name_club))[1])),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 12,
                     plotOutput("ownership_plot"), downloadButton("ownership_download", label = "Download This Plot")),
                 
        )), 

## Predicted Points Tab
#tabItem(tabName = "predictions_tab",
#        #fluidRow(tags$img(src='Logo Ownership.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 300), style = "text-align: center;"),
#        fluidRow(align = "center",
#                 box(status = "info",
#                     collapsible = TRUE,
#                     width = 4, 
#                     selectInput("pred_metric", label = "Select Metric:", choices = c("Elite Ownership","Expected Points", "Expected Minutes"), selected = c("Expected Points"))),
#                 box(status = "primary",
#                     collapsible = TRUE,
#                     width = 4, 
#                     sliderInput("pred_gweeks", label = "Gameweeks:", min = min(prediction_vector), max = max(prediction_vector), step = 1, value = c(min(prediction_vector),max(prediction_vector)))),
#                box(status = "success",
#                     collapsible = TRUE,
#                     width = 4, 
#                     sliderInput("pred_value", label = "Maximum Value (£):", min = 3.5, max = 15, step = 0.5, value = c(15))),
#                 box(status = "warning",
#                     collapsible = TRUE,
#                     inline = FALSE,
#                     width = 4, 
#                     selectizeInput("pred_teams", label = "Select Teams:", choices = sort(short_team_vec), selected = sort(short_team_vec), options = list(placeholder = "Select Teams"), multiple = TRUE)),
#                 box(status = "danger",
#                     collapsible = TRUE,
#                     inline = FALSE,
#                     width = 4, 
#                     selectizeInput("pred_position", label = "Select Position:", choices = c("GK","DEF","MID","ST"), selected = c("MID"), options = list(placeholder = "Select Position"), multiple = TRUE)),
#                box(status = "info",
#                    collapsible = TRUE,
#                    width = 4, 
#                    sliderInput("pred_number", label = "No. Of Players", min = 1, max = 20, step = 1, value = c(12))),
#                box(status = "primary",
#                     collapsible = TRUE,
#                     width = 12,
#                     plotOutput("prediction_plot"), downloadButton("prediction_download", label = "Download This Plot")),
#
#        )),

# Sixth tab content
tabItem(tabName = "teams_tab",
        fluidRow(tags$img(src='Logo Team.png', alt = "Something Went Wrong", deleteFile =  FALSE, height = 300), style = "text-align: center;"),
        fluidRow(align = "center",
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 3,
                     selectInput("team_matrix", label = "Select Team:", choices = unique(Players_History$short_team_name), selected = unique(Players_History$short_team_name)[1])),
                 box(status = "success",
                     collapsible = TRUE,
                     width = 3, 
                     sliderInput("gweeks", label = "Gameweeks:", min = min(Players_History$round), max = max(Players_History$round), step = 1, value = c(1,max(Players_History$round)))),
                 box(status = "success",
                     collapsible = TRUE,
                     width = 3, 
                     sliderInput("mins_matrix", label = "Minimum Minutes:", min = 0, max = max(Players_History$round)*90, step = 50, value = 300)),
                 box(status = "warning",
                     collapsible = TRUE,
                     width = 3, 
                     selectInput("stat_matrix", label = "Select Metric:", choices = c("Minutes", "Points", "XGI"), selected = c("Minutes"))),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 12, 
                     #actionButton("update11" ,"Update Rank Plot", icon("arrows-rotate"),
                    #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                    #              class = "btn btn-primary"),
                     #progressBar(id = "pb11", value = 0, display_pct = T),
                     plotOutput("matrixplot"), 
                     downloadButton("download_matrix"),
                     label = "Download this plot"),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 6,
                     selectizeInput("team_matrix2", label = "Select Team For XG Trend (Max = 6):", choices = unique(Players_History$short_team_name), selected = unique(Players_History$short_team_name)[1], options = list(placeholder = "Select Team For XG Trend Graph", maxItems = 6), multiple = TRUE),
                     #selectizeInput("player_ownership", label = "Select 1 Player To Display Ownership:", choices = sort(unique(Players_History$name_club)), options = list(placeholder = "Select Player", maxItems = 1), multiple = TRUE)),
                     #column(width = 4, checkboxGroupInput("team_matrix2a", label = "", choices = sort(unique(Players_History$short_team_name))[1:7], selected = c("ARS"))),
                     #column(width = 4, checkboxGroupInput("team_matrix2b", label = "Select XG Trend Teams:", choices = sort(unique(Players_History$short_team_name))[8:14], selected = NA)),
                     #column(width = 4, checkboxGroupInput("team_matrix2c", label = "", choices = sort(unique(Players_History$short_team_name))[14:20], selected = NA)),
                     
                     ),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 6,
                     sliderInput("smooth_slider", label = "Adjust XG Smoothing (1 = No Smoothing):", min = 1, max = 10, step = 1, value = 5)),
                 box(status = "primary",
                     collapsible = TRUE,
                     width = 12, 
                     #actionButton("update11" ,"Update Rank Plot", icon("arrows-rotate"),
                    #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                    #              class = "btn btn-primary"),
                    # progressBar(id = "pb11", value = 0, display_pct = T),
                     plotOutput("team_xg_trend"), 
                     downloadButton("download_xgtrend",
                     label = "Download this plot")),
                 box(status = "primary",
                     collapsible = TRUE,
                     inline = FALSE,
                     width = 12, 
                     dataTableOutput('table_team')),
                 
        ))
                         
    )
  )
)


# Wrap your UI with secure_app
#ui <- secure_app(ui)
#secure_ui(ui)


server <- function(input, output, session) {
  
#  # call the server part
#  # check_credentials returns a function to authenticate users
#  res_auth <- secure_server(
#    check_credentials = check_credentials(credentials)
#  )
  
#  output$auth_output <- renderPrint({
#    reactiveValuesToList(res_auth)
#  })
  
  
#  #v <- reactiveValues(plot = NULL)
  
  
  # Combine both filters
  filters <- reactive({

    #list(input$positions, input$teams, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$value)
    list(input$positions, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$value)
  })
  
  filter2 <- reactive({
    
    #list(input$positions, input$teams, input$gameweeks[1], input$gameweeks[2], input$starts_1)
    list(input$positions, input$gameweeks[1], input$gameweeks[2], input$starts_1)
  })
  
  filter3 <- reactive({
    
    list(input$team_id2)
  })
  
  filter4 <- reactive({
    
    list(input$league1)
  })
  
  filter3_own <- reactive({
    
    list(input$own_team_id)
  })
  
  filter4_own <- reactive({
    
    list(input$own_league)
  })
  
  
  
  
  # Extract Players in that position
  observeEvent(filters(),{
    
    req(input$value)
    
    position <- input$positions
    gw_start <- input$gameweeks[1]
    gw_end <- input$gameweeks[2]
    #team <- input$teams
    starting <- input$starts_1
    v <- input$value
    
    
    player_options <- Players_History %>%
      mutate(value = as.numeric(value) / 10) %>%
      #print("hello") %>%
      filter(value <= v) %>%
      filter(Position %in% position) %>%
      filter(round >= gw_start & round <= gw_end) %>%
      #filter(short_team_name %in% team) %>%
      group_by(name_club, short_team_name) %>%
      nest() %>%
      mutate(starts = map(.x = data, ~ sum(.x$starts)),
             max_round = map(.x = data, ~ max(.x$round))) %>%
      select(-data) %>%
      unnest(cols = c(starts,max_round)) %>%
      as.data.frame() %>%
      filter(starts >= starting) %>%
      pull(name_club) %>%
      sort()
    
    updateSelectizeInput(session, inputId="players",
                             choices=unique(player_options),
                             selected = unique(player_options[1]))
    
  })
  
  # Extract Players in that position
  observeEvent(filter2(),{
    
    
    
    gw_start <- as.integer(input$gameweeks[1])
    gw_end <- as.integer(input$gameweeks[2])
    gw_diff <- (gw_end - gw_start) + 1
    
    
    updateSliderInput(session, inputId="starts_1",
                             min = 0,
                             max = gw_diff)
    
    
  })
  
  
   observeEvent(filter3(),{
     
     

   Leagues <-league_parse2(input$team_id2)
  
  updateSelectInput(session, inputId="league1",
                    choices = Leagues$name,
                    selected = Leagues$name[4])
  
})
  
  # Extract Players in that position
  observeEvent(filter4(),{
    
    

    Leagues <- league_parse2(input$team_id2) %>%
      filter(name == input$league1)
    
    AllLeague_Info <- c()
    
    count = 0
    
    for (page in seq(1,100)) {
      
      count = count + 1
      
      FPL_League <- GET(paste0('https://fantasy.premierleague.com/api/leagues-classic/', Leagues$id,'/standings/?page_standings=',page))
      
      jsonRespText<-content(FPL_League, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      Leagues_ID <- JSON_Output$standings$results
      
      if (length(Leagues_ID) == 0) {
        #print("Empty")
        break
      }    #else {
        #print("Non-Empty")
      #}
      
      AllLeague_Info <- bind_rows(AllLeague_Info,Leagues_ID)
      
    }
    
    Info <- AllLeague_Info %>%
      select(entry, entry_name) %>%
      mutate(entry = as.character(entry))
    
    updateSelectInput(session, inputId="team_league",
                      choices = AllLeague_Info$entry_name,
                      selected = AllLeague_Info$entry_name[1])
    
    updateSelectizeInput(session, inputId="team_league_selectize",
                      choices = AllLeague_Info$entry_name %>% as.list(),
                      selected = AllLeague_Info$entry_name[1:10],
                      #multiple = T,
                      options = list(placeholder = "Select Rival Teams"))
    
    teamid2 <- Info %>% filter(entry_name == input$team_league) %>% pull(entry)
    teamid_selectize_vector <- Info %>% filter(entry_name %in% input$team_league_selectize) %>% pull(entry) %>% as.list()
    
    
    
 })
  
  ###### Ownership
  
  observeEvent(filter3_own(),{
    
    
    
    Leagues <-league_parse2(input$own_team_id)
    
    updateSelectInput(session, inputId="own_league",
                      choices = Leagues$name,
                      selected = Leagues$name[4])
    
  })
  
  # Extract Players in that position
  observeEvent(filter4_own(),{
    
    
    
    Leagues <- league_parse2(input$own_team_id) %>%
      filter(name == input$own_league)
    
    AllLeague_Info <- c()
    
    count = 0
    
    for (page in seq(1,100)) {
      
      count = count + 1
      
      FPL_League <- GET(paste0('https://fantasy.premierleague.com/api/leagues-classic/', Leagues$id,'/standings/?page_standings=',page))
      
      jsonRespText<-content(FPL_League, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      Leagues_ID <- JSON_Output$standings$results
      
      if (length(Leagues_ID) == 0) {
        #print("Empty")
        break
      }    #else {
      #print("Non-Empty")
      #}
      
      AllLeague_Info <- bind_rows(AllLeague_Info,Leagues_ID)
      
    }
    
    Info <- AllLeague_Info %>%
      select(entry, entry_name) %>%
      mutate(entry = as.character(entry))
    
    #updateSelectInput(session, inputId="team_league",
    #                  choices = AllLeague_Info$entry_name,
    #                  selected = AllLeague_Info$entry_name[1])
    
    updateSelectizeInput(session, inputId="team_own_selectize",
                         choices = AllLeague_Info$entry_name %>% as.list(),
                         selected = AllLeague_Info$entry_name[1:10],
                         #multiple = T,
                         options = list(placeholder = "Select Rival Teams"))
    
    teamid2 <- Info %>% filter(entry_name == input$team_league) %>% pull(entry)
    teamid_selectize_vector <- Info %>% filter(entry_name %in% input$team_league_selectize) %>% pull(entry) %>% as.list()
    
    
    
  })
  
  Selectize_Team_Own_ID <- reactive({
    
    
    
    Leagues <- league_parse2(input$own_team_id) %>%
      filter(name == input$own_league)
    
    AllLeague_Info <- c()
    
    count = 0
    
    for (page in seq(1,100)) {
      
      count = count + 1
      
      FPL_League <- GET(paste0('https://fantasy.premierleague.com/api/leagues-classic/', Leagues$id,'/standings/?page_standings=',page))
      
      jsonRespText<-content(FPL_League, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      Leagues_ID <- JSON_Output$standings$results
      
      if (length(Leagues_ID) == 0) {
        #print("Empty")
        break
      }    #else {
      #print("Non-Empty")
      #}
      
      AllLeague_Info <- bind_rows(AllLeague_Info,Leagues_ID)
      
    }
    
    Info <- AllLeague_Info %>%
      select(entry, entry_name) %>%
      mutate(entry = as.character(entry))
    
    
    Info %>% filter(entry_name %in% input$team_own_selectize) %>% pull(entry)
    
  })
  
  Selectize_Team_ID <- reactive({
    
    
    
    Leagues <- league_parse2(input$team_id2) %>%
      filter(name == input$league1)
    
    AllLeague_Info <- c()
    
    count = 0
    
    for (page in seq(1,100)) {
      
      count = count + 1
      
      FPL_League <- GET(paste0('https://fantasy.premierleague.com/api/leagues-classic/', Leagues$id,'/standings/?page_standings=',page))
      
      jsonRespText<-content(FPL_League, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      Leagues_ID <- JSON_Output$standings$results
      
      if (length(Leagues_ID) == 0) {
        #print("Empty")
        break
      }    #else {
      #print("Non-Empty")
      #}
      
      AllLeague_Info <- bind_rows(AllLeague_Info,Leagues_ID)
      
    }
    
    Info <- AllLeague_Info %>%
      select(entry, entry_name) %>%
      mutate(entry = as.character(entry))
    
    
    Info %>% filter(entry_name %in% input$team_league_selectize) %>% pull(entry)
    
  })
  
  #output$team_selectize_text <- renderText(paste0("Team IDs: ",{Selectize_Team_ID()}))
  
  #Update Highlight Colours
  observeEvent(input$team_league_selectize,{
    
    
    
    select_from <- input$team_league_selectize
    
    updateSelectizeInput(session, inputId="team_highlight",
                         choices = select_from,
                         selected = select_from[1])
    
  })
  
  # Extract Players in that position
  observeEvent(input$team_league,{
    
    
    
    Leagues <- league_parse2(input$team_id2) %>%
      filter(name == input$league1)
    
    AllLeague_Info <- c()
    
    
    for (page in seq(1,100)) {
      
      
      FPL_League <- GET(paste0('https://fantasy.premierleague.com/api/leagues-classic/', Leagues$id,'/standings/?page_standings=',page))
      
      jsonRespText<-content(FPL_League, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      Leagues_ID <- JSON_Output$standings$results
      
      if (length(Leagues_ID) == 0) {
      #  print("Empty")
        break
      } #   else {
      #  print("Non-Empty")
      #}
      
      AllLeague_Info <- bind_rows(AllLeague_Info,Leagues_ID)
      
    }
    
    Info <- AllLeague_Info %>%
      select(entry, entry_name) %>%
      mutate(entry = as.character(entry))
    
    teamid2 <- Info %>% filter(entry_name == input$team_league) %>% pull(entry)
    
    
    updateSelectInput(session, inputId="input_rival",
                      choices = teamid2,
                      selected = teamid2)
    
  })
  


  New_Teamname <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$name %>% data.frame() %>% pull(.)
  })
  
  output$team_text <- renderText(paste0("Team Name: ",{New_Teamname()}))
  
  New_Player <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
  })
  
  output$player_text <- renderText(paste0("Player Name: ",{New_Player()}))
  
  New_Country <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$player_region_name %>% data.frame() %>% pull(.)
  })
  
  output$country_text <- renderText(paste0("Region Name: ",{New_Country()}))
  
  
  
  New_Teamname2 <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id2,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$name %>% data.frame() %>% pull(.)
  })
  
  output$team_text2 <- renderText(paste0("Team Name: ",{New_Teamname2()}))
  
  New_Player2 <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id2,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
  })
  
  output$player_text2 <- renderText(paste0("Player Name: ",{New_Player2()}))
  
  New_Country2 <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id2,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$player_region_name %>% data.frame() %>% pull(.)
  })
  
  output$country_text2 <- renderText(paste0("Region Name: ",{New_Country2()}))
  
  
  New_Teamname3 <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$input_rival,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$name %>% data.frame() %>% pull(.)
  })
  
  output$team_text3 <- renderText(paste0("Team Name: ",{New_Teamname3()}))
  
  New_Player3 <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$input_rival,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
  })
  
  output$player_text3 <- renderText(paste0("Player Name: ",{New_Player3()}))
  
  New_Country3 <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$input_rival,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$player_region_name %>% data.frame() %>% pull(.)
  })
  
  output$country_text3 <- renderText(paste0("Region Name: ",{New_Country3()}))
  
  output$team_text <- renderText(paste0("Team Name: ",{New_Teamname()}))
  
  New_Team_Bench <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$bench_team_id,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$name %>% data.frame() %>% pull(.)
  })
  
  output$bench_team_text <- renderText(paste0("Team Name: ",{New_Team_Bench()}))
  
  
  New_Player_Bench <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$bench_team_id,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
  })
  
  output$bench_player_text <- renderText(paste0("Player Name: ",{New_Player_Bench()}))
  
  New_Country_Bench <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$bench_team_id,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$player_region_name %>% data.frame() %>% pull(.)
  })
  
  output$bench_country_text <- renderText(paste0("Region Name: ",{New_Country_Bench()}))
  
  
  Own_New_Teamname <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$own_team_id,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$name %>% data.frame() %>% pull(.)
  })
  
  output$own_team_text <- renderText(paste0("Team Name: ",{Own_New_Teamname()}))
  
  
  Own_New_Player <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$own_team_id,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
  })
  
  output$own_player_text <- renderText(paste0("Player Name: ",{Own_New_Player()}))
  
  Own_New_Country <- reactive({
    
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$own_team_id,'/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    JSON_Output$player_region_name %>% data.frame() %>% pull(.)
  })
  
  output$own_country_text <- renderText(paste0("Region Name: ",{Own_New_Country()}))
  
  
  
  
  #output$plot1 <- renderPlot({
    
    
  #  Comparison <- generate_percentiles(Players_History, input$positions, input$teams, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations, input$players, input$value)
    
  #  p <- comparison_plot(Comparison, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations)
    
  #  p

    
  #    })
  
  #output$download_plot <- downloadHandler(
  #  filename = function(){paste("FPL.Player_Comparison.png",sep='')},
    
    
  #  content = function(file){
      
  #    Comparison <- generate_percentiles(Players_History, input$positions, input$teams, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations, input$players, input$value)
      
  #    #Comparison$Names <- ordered(Comparison$Names, levels = c("xGoals","Goals","xAssists","Assists","xG+xA","G+A", "Total Points", "Bonus"))
  #    #Comparison$Info <- ordered(Comparison$Info, levels = c("Goals","Assists","Contributions","Points")) 
      
  #    p <- comparison_plot(Comparison, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations)
      
  #    ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
  #  })
  
  output$player_points_plot <- renderPlot({
    
    Player_Gameweeks <- Players_History %>%
      mutate(DEFCON = case_when(Position == "DEF" & defensive_contribution >= 10 ~ 2,
                                Position == "MID" & defensive_contribution >= 12 ~ 2,
                                Position == "ST" & defensive_contribution >= 12 ~ 2,
                                T ~ 0)) %>%
      filter(name_club %in% input$players) %>%
      filter(round >= input$gameweeks[1] & round <= input$gameweeks[2]) %>%
      group_by(round, name_club) %>%
      summarise(games = n(),
                oppsition = paste(opp_short_team_name, collapse=","),
                DEFCON = paste(DEFCON, collapse=","),
                `Defensive Contributions` = paste(defensive_contribution, collapse=","),
                opp_strength = paste(opp_strength, collapse=","),
                Goals = sum(goals_scored),
                `xG Value` = sum(expected_goals),
                Assists = sum(assists),
                `xA Value` = sum(expected_assists),
                `xG+xA Value` = sum(expected_goal_involvements),
                `G+A` = sum(goals_scored) + sum(assists),
                `Mins Played` = sum(minutes),
                `Bonus Points` = sum(bonus),
                `Total Points` = sum(total_points)) %>%
      ungroup() %>%
      group_by(name_club) %>%
      mutate(line_color = lead(`Total Points`)) %>%
      ungroup() %>%
      as.data.frame() %>%
      mutate(DGW = case_when(games == 2 ~ "Yes",
                             T ~ "No")) %>%
      mutate(SGW = case_when(games == 1 & is.na(`Total Points`) == F ~ "Yes",
                             T ~ "No")) %>%
      mutate(BGW = case_when(is.na(`Total Points`) ~ "Yes",
                             T ~ "No")) %>%
      mutate(GW_Type = case_when(SGW == "Yes" ~ "SGW",
                                 DGW == "Yes" ~ "DGW",
                                 BGW == "Yes" ~ "BGW",
                                 T ~ "Other")) %>%
      mutate(GW_Type = as.factor(GW_Type)) %>%
      select(-DGW,-SGW,-BGW)
    
    
    #v10$plot <- player_points_plot1(Player_Gameweeks, input$starts_1, input$gameweeks[1], input$gameweeks[2], input$player_metric)
    p <- player_points_plot1(Player_Gameweeks, input$starts_1, input$gameweeks[1], input$gameweeks[2], input$player_metric)
    
    #maxi <- 10
    #for (i in 1:maxi) {
    #  updateProgressBar(session = session, id = "pb10", value = (i/maxi)*100)
    #  Sys.sleep(0.1)
    #}
    
    #p <- v10$plot
    
    p    
    
  })
  
  output$download_points_player <- downloadHandler(
    filename = function(){paste("FPL.Player_Points.png",sep='')},
    
    
    content = function(file){
      
      Player_Gameweeks <- Players_History %>%
        filter(name_club %in% input$players) %>%
        filter(round >= input$gameweeks[1] & round <= input$gameweeks[2]) %>%
        group_by(round, name_club) %>%
        summarise(games = n(),
                  oppsition = paste(opp_short_team_name, collapse=","),
                  opp_strength = paste(opp_strength, collapse=","),
                  Goals = sum(goals_scored),
                  `xG Value` = sum(expected_goals),
                  Assists = sum(assists),
                  `xA Value` = sum(expected_assists),
                  `xG+xA Value` = sum(expected_goal_involvements),
                  `G+A` = sum(goals_scored) + sum(assists),
                  `Mins Played` = sum(minutes),
                  `Bonus Points` = sum(bonus),
                  `Total Points` = sum(total_points)) %>%
        ungroup() %>%
        group_by(name_club) %>%
        mutate(line_color = lead(`Total Points`)) %>%
        ungroup() %>%
        as.data.frame() %>%
        mutate(DGW = case_when(games == 2 ~ "Yes",
                               T ~ "No")) %>%
        mutate(SGW = case_when(games == 1 & is.na(`Total Points`) == F ~ "Yes",
                               T ~ "No")) %>%
        mutate(BGW = case_when(is.na(`Total Points`) ~ "Yes",
                               T ~ "No")) %>%
        mutate(GW_Type = case_when(SGW == "Yes" ~ "SGW",
                                   DGW == "Yes" ~ "DGW",
                                   BGW == "Yes" ~ "BGW",
                                   T ~ "Other")) %>%
        mutate(GW_Type = as.factor(GW_Type)) %>%
        select(-DGW,-SGW,-BGW)
      
      
      #v10$plot <- player_points_plot1(Player_Gameweeks, input$starts_1, input$gameweeks[1], input$gameweeks[2], input$player_metric)
      p <- player_points_plot1(Player_Gameweeks, input$starts_1, input$gameweeks[1], input$gameweeks[2], input$player_metric)
      

      
      #maxi <- 10
      #for (i in 1:maxi) {
      #  updateProgressBar(session = session, id = "pb10", value = (i/maxi)*100)
      #  Sys.sleep(0.1)
      #}
      
      #p <- v10$plot      
      
      #ggsave_golden(file, plot = p, dpi = 600)
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  output$table1 <- DT::renderDataTable(create_table1(Players_History, input$positions, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations, input$value) %>% select(-image),
                                       rownames = F, extensions = "FixedColumns",
                                       options = list(
                                         scrollX = T,
                                         paging = TRUE, 
                                         searching = TRUE, 
                                         info = FALSE,
                                         sort = TRUE,
                                         fixedColumns = TRUE,
                                         autoWidth = TRUE))
  
  
  output$plot2 <- renderPlot({
    
    #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
    
    
    #jsonRespText<-content(FPL_Team, as="text") 
    #JSON_Output <- fromJSON(jsonRespText)
    
    #Chips <- JSON_Output$chips %>%
    #  select(-time)
    
    
    #Team_Summary <- team_picks_parse(input$team_id, max(Players_History$round)) %>%
    #  left_join(Chips, by = c('event')) %>%
    #  rename(chip = name) %>%
    #  mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
    #                                chip == 'freehit' ~ 'FH',
    #                                chip == 'bboost' ~ 'BB',
    #                                chip == '3xc' ~ 'TC',
    #                                T ~'')) %>%
    #  mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
    #  mutate(GW_Chip = str_replace(GW_Chip, " ","")) %>%
    #  mutate(rank_percentile = (rank_sort/ current_players)*100) %>%
    #  #filter(event != 7) %>%
    #  mutate(line_color = lead(rank_percentile)) %>%
    #  mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
    #                                 overall_rank < 1000 ~ as.character(overall_rank),
    #                                 T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
    # mutate(movement = case_when(rank_percentile <= 20  ~ "Elite",
    #                              rank_percentile > 20  & rank_percentile <= 40 ~ "Good",
    #                              rank_percentile > 40  & rank_percentile <= 60 ~ "Average",
    #                              T ~ "Below Average"))
    
    #small_df2 <- Team_Summary %>%
    #  filter(!is.na(chip))
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id, '/history/'))
    
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    
    #Chips <- JSON_Output$chips %>%
    #  select(-time)
    
    # Fix: Check if chips exists and handle empty/NULL case
    if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
      Chips <- JSON_Output$chips %>%
        select(-time)
    } else {
      # Create empty data frame with expected structure if no chips
      Chips <- data.frame(
        event = integer(0),
        name = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    temp_df <- team_picks_parse(input$team_id, max(Players_History$round)) %>%
      left_join(Chips, by = c('event')) %>%
      rename(chip = name) %>%
      mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                    chip == 'freehit' ~ 'FH',
                                    chip == 'bboost' ~ 'BB',
                                    chip == '3xc' ~ 'TC',
                                    chip == 'manager' ~ 'AM',
                                    T ~'')) %>%
      mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
      mutate(GW_Chip = str_replace(GW_Chip, " ",""))
    
    
    Team_Summary <- temp_df %>%
      mutate(rank_percentile = (rank_sort/ current_players)*100) %>%
      mutate(line_color = lead(rank_percentile)) %>%
      mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                     overall_rank < 1000 ~ as.character(overall_rank),
                                     T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
      mutate(movement = case_when(rank_percentile <= 20  ~ "Elite",
                                    rank_percentile > 20  & rank_percentile <= 40 ~ "Good",
                                    rank_percentile > 40  & rank_percentile <= 60 ~ "Average",
                                    T ~ "Below Average"))
    
    small_df2 <- Team_Summary %>%
      filter(!is.na(chip))
    
    #maxi <- 10
    #for (i in 1:maxi) {
    #  updateProgressBar(session = session, id = "pb2", value = (i/maxi)*100)
    #  Sys.sleep(0.1)
    #}
    
    Names1 <- Team_Summary$GW_Chip
    Breaks1 <- Team_Summary$event
    
    
    #v3$plot <- rank_percentile_plot(Team_Summary, small_df2, Names1, Breaks1)
    p <- rank_percentile_plot(Team_Summary, small_df2, Names1, Breaks1)
    
    #p <- v3$plot
    
    p
    
    
    
  })
  
  output$download_rank <- downloadHandler(
    filename = function(){paste("FPL.GW_PercentileRank.png",sep='')},
    
    
    content = function(file){
      
      #p <- v3$plot
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id, '/history/'))
      
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      Chips <- JSON_Output$chips %>%
        select(-time)
      
      temp_df <- team_picks_parse(input$team_id, max(Players_History$round)) %>%
        left_join(Chips, by = c('event')) %>%
        rename(chip = name) %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ",""))
      
      
      Team_Summary <- temp_df %>%
        mutate(rank_percentile = (rank_sort/ current_players)*100) %>%
        mutate(line_color = lead(rank_percentile)) %>%
        mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                       overall_rank < 1000 ~ as.character(overall_rank),
                                       T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
        mutate(movement = case_when(rank_percentile <= 20  ~ "Elite",
                                    rank_percentile > 20  & rank_percentile <= 40 ~ "Good",
                                    rank_percentile > 40  & rank_percentile <= 60 ~ "Average",
                                    T ~ "Below Average"))
      
      small_df2 <- Team_Summary %>%
        filter(!is.na(chip))
      
      #maxi <- 10
      #for (i in 1:maxi) {
      #  updateProgressBar(session = session, id = "pb2", value = (i/maxi)*100)
      #  Sys.sleep(0.1)
      #}
      
      Names1 <- Team_Summary$GW_Chip
      Breaks1 <- Team_Summary$event
      
      
      #v3$plot <- rank_percentile_plot(Team_Summary, small_df2, Names1, Breaks1)
      p <- rank_percentile_plot(Team_Summary, small_df2, Names1, Breaks1)

      #ggsave_golden(file, plot = p, dpi = 600)
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  output$plot3 <- renderPlot({

    #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
    
    
    #jsonRespText<-content(FPL_Team, as="text") 
    #JSON_Output <- fromJSON(jsonRespText)
    
    #Chips <- JSON_Output$chips %>%
    #  select(-time)
    
    
    #Team_Summary <- team_picks_parse(input$team_id, max(Players_History$round)) %>%
    #  left_join(Chips, by = c('event')) %>%
    #  rename(chip = name) %>%
    #  mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
    #                                chip == 'freehit' ~ 'FH',
    #                                chip == 'bboost' ~ 'BB',
    #                                chip == '3xc' ~ 'TC',
    #                                T ~'')) %>%
    #  mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
    #  mutate(GW_Chip = str_replace(GW_Chip, " ","")) %>%
    #  mutate(line_color = lead(overall_rank)) %>%
    #  mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
    #                                 overall_rank < 1000 ~ as.character(overall_rank),
    #                                 T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
    #  mutate(movement = case_when(line_color > overall_rank ~ "Red Arrow",
    #                              line_color < overall_rank ~ "Green Arrow",
    #                              T ~ "No Arrow"))
    
    #small_df2 <- Team_Summary %>%
    #  filter(!is.na(chip))
    
    #maxi <- 10
    #for (i in 1:maxi) {
    #  updateProgressBar(session = session, id = "pb", value = (i/maxi)*100)
    #  Sys.sleep(0.1)
    #}
    
    #Names1 <- Team_Summary$GW_Chip
    #Breaks1 <- Team_Summary$event
    
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id, '/history/'))
    
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    
    # Fix: Check if chips exists and handle empty/NULL case
    if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
      Chips <- JSON_Output$chips %>%
        select(-time)
    } else {
      # Create empty data frame with expected structure if no chips
      Chips <- data.frame(
        event = integer(0),
        name = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    temp_df <- team_picks_parse(input$team_id, max(Players_History$round)) %>%
      left_join(Chips, by = c('event')) %>%
      rename(chip = name) %>%
      mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                    chip == 'freehit' ~ 'FH',
                                    chip == 'bboost' ~ 'BB',
                                    chip == '3xc' ~ 'TC',
                                    chip == 'manager' ~ 'AM',
                                    T ~'')) %>%
      mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
      mutate(GW_Chip = str_replace(GW_Chip, " ",""))
    
    
    Team_Summary <- temp_df %>%
      #mutate(line_color = lead(overall_rank)) %>%
      mutate(line_color = lag(overall_rank)) %>%
      mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                     overall_rank < 1000 ~ as.character(overall_rank),
                                     T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
      mutate(movement = case_when(#line_color > overall_rank ~ "Red Arrow",
                                  #line_color < overall_rank ~ "Green Arrow",
                                  overall_rank > line_color ~ "Red Arrow",
                                  overall_rank < line_color~ "Green Arrow",
                                  T ~ "No Arrow"))
    
    small_df2 <- Team_Summary %>%
      filter(!is.na(chip))
    
    #maxi <- 10
    #for (i in 1:maxi) {
    #  updateProgressBar(session = session, id = "pb2", value = (i/maxi)*100)
    #  Sys.sleep(0.1)
    #}
    
    Names1 <- Team_Summary$GW_Chip
    Breaks1 <- Team_Summary$event
    
    
    #v4$plot <- rank_plot(Team_Summary, small_df2, Names1, Breaks1)
    p <- rank_plot(Team_Summary, small_df2, Names1, Breaks1)
    
    #p <- v4$plot
    
    p
    
  })
  
  output$download_rank2 <- downloadHandler(
    filename = function(){paste("FPL.GW_Rank.png",sep='')},
    
    
    content = function(file){
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id, '/history/'))
      
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      # Fix: Check if chips exists and handle empty/NULL case
      if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
        Chips <- JSON_Output$chips %>%
          select(-time)
      } else {
        # Create empty data frame with expected structure if no chips
        Chips <- data.frame(
          event = integer(0),
          name = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      temp_df <- team_picks_parse(input$team_id, max(Players_History$round)) %>%
        left_join(Chips, by = c('event')) %>%
        rename(chip = name) %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ",""))
      
      
      Team_Summary <- temp_df %>%
        #mutate(line_color = lead(overall_rank)) %>%
        mutate(line_color = lag(overall_rank)) %>%
        mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                       overall_rank < 1000 ~ as.character(overall_rank),
                                       T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
        mutate(movement = case_when(#line_color > overall_rank ~ "Red Arrow",
          #line_color < overall_rank ~ "Green Arrow",
          overall_rank > line_color ~ "Red Arrow",
          overall_rank < line_color~ "Green Arrow",
          T ~ "No Arrow"))
      
      small_df2 <- Team_Summary %>%
        filter(!is.na(chip))
      
      #maxi <- 10
      #for (i in 1:maxi) {
      #  updateProgressBar(session = session, id = "pb2", value = (i/maxi)*100)
      #  Sys.sleep(0.1)
      #}
      
      Names1 <- Team_Summary$GW_Chip
      Breaks1 <- Team_Summary$event
      
      
      #v4$plot <- rank_plot(Team_Summary, small_df2, Names1, Breaks1)
      p <- rank_plot(Team_Summary, small_df2, Names1, Breaks1)
      
      #ggsave_golden(file, plot = p, dpi = 600)
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  output$bench_points <- renderPlot({
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$bench_team_id, '/history/'))
    
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    
    # Fix: Check if chips exists and handle empty/NULL case
    if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
      Chips <- JSON_Output$chips %>%
        select(-time)
    } else {
      # Create empty data frame with expected structure if no chips
      Chips <- data.frame(
        event = integer(0),
        name = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    temp_df <- team_picks_parse(input$bench_team_id, max(Players_History$round)) %>%
      left_join(Chips, by = c('event')) %>%
      rename(chip = name) %>%
      mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                    chip == 'freehit' ~ 'FH',
                                    chip == 'bboost' ~ 'BB',
                                    chip == '3xc' ~ 'TC',
                                    chip == 'manager' ~ 'AM',
                                    T ~'')) %>%
      mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
      mutate(GW_Chip = str_replace(GW_Chip, " ",""))
    
    
    Team_Summary <- temp_df %>%
      mutate(line_color = lead(points_on_bench)) %>%
      mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                     overall_rank < 1000 ~ as.character(overall_rank),
                                     T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
      mutate(movement = case_when(points_on_bench >= 6 & points_on_bench <= 12 ~ "Good",
                                  points_on_bench < 6 & points_on_bench >= 0 ~ "Blank",
                                  points_on_bench < 0 ~ "Minus",
                                  T ~ "Stacked"))
    
    small_df2 <- Team_Summary %>%
      filter(!is.na(chip))
    
    #maxi <- 10
    #for (i in 1:maxi) {
    #  updateProgressBar(session = session, id = "pb5", value = (i/maxi)*100)
    #  Sys.sleep(0.1)
    #}
    
    Names1 <- Team_Summary$GW_Chip
    Breaks1 <- Team_Summary$event
    
    #v5$plot <- bench_points(Team_Summary, small_df2, Names1, Breaks1)
    p <- bench_points(Team_Summary, small_df2, Names1, Breaks1)
    
    #p <- v5$plot
    
    p
    
  })
  
  output$download_bench <- downloadHandler(
    filename = function(){paste("FPL.Bench_Points.png",sep='')},
    
    
    content = function(file){
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$bench_team_id, '/history/'))
      
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      # Fix: Check if chips exists and handle empty/NULL case
      if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
        Chips <- JSON_Output$chips %>%
          select(-time)
      } else {
        # Create empty data frame with expected structure if no chips
        Chips <- data.frame(
          event = integer(0),
          name = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      temp_df <- team_picks_parse(input$bench_team_id, max(Players_History$round)) %>%
        left_join(Chips, by = c('event')) %>%
        rename(chip = name) %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ",""))
      
      
      Team_Summary <- temp_df %>%
        mutate(line_color = lead(points_on_bench)) %>%
        mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                       overall_rank < 1000 ~ as.character(overall_rank),
                                       T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
        mutate(movement = case_when(points_on_bench >= 6 & points_on_bench <= 12 ~ "Good",
                                    points_on_bench < 6 & points_on_bench >= 0 ~ "Blank",
                                    points_on_bench < 0 ~ "Minus",
                                    T ~ "Stacked"))
      
      small_df2 <- Team_Summary %>%
        filter(!is.na(chip))
      
      #maxi <- 10
      #for (i in 1:maxi) {
      #  updateProgressBar(session = session, id = "pb5", value = (i/maxi)*100)
      #  Sys.sleep(0.1)
      #}
      
      Names1 <- Team_Summary$GW_Chip
      Breaks1 <- Team_Summary$event
      
      #v5$plot <- bench_points(Team_Summary, small_df2, Names1, Breaks1)
      p <- bench_points(Team_Summary, small_df2, Names1, Breaks1)
      
      #p <- v5$plot
      
      #ggsave_golden(file, plot = p, dpi = 600)
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  
  output$captaincy_points <- renderPlot({
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$bench_team_id, '/history/'))
    
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    
    # Fix: Check if chips exists and handle empty/NULL case
    if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
      Chips <- JSON_Output$chips %>%
        select(-time)
    } else {
      # Create empty data frame with expected structure if no chips
      Chips <- data.frame(
        event = integer(0),
        name = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    
    Team_Selection_Final <- captaincy_parse(input$bench_team_id, Players_History, max(Players_History$round)) %>%
      left_join(Chips, by = c('round' = 'event')) %>%
      rename(chip = name) %>%
      mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                    chip == 'freehit' ~ 'FH',
                                    chip == 'bboost' ~ 'BB',
                                    chip == '3xc' ~ 'TC',
                                    chip == 'manager' ~ 'AM',
                                    T ~'')) %>%
      mutate(GW_Chip = paste(round,"\n",chip_short)) %>%
      mutate(GW_Chip = str_replace(GW_Chip, " ","")) %>%
      separate(name_club, sep = " " ,c("name","club")) %>%
      mutate(name_info = paste0(name, "\n(",oppsition,")"))
    
    labels <- c()
    
    for (i in 1:length(Team_Selection_Final$player_images)){
      
      img.name <- Team_Selection_Final$label2[i]
      
      labels <- c(labels, paste0("<img src='", Team_Selection_Final$player_images[i],  "' width='20' /><br>", img.name))
      
    }
    
    small_df2 <- Team_Selection_Final %>%
      group_by(round) %>%
      summarise(chip = unique(chip)) %>%
      ungroup() %>%
      filter(!is.na(chip)) %>%
      mutate(GW_Chip = NA,
             total_points = NA)
    
    #maxi <- 10
    #for (i in 1:maxi) {
    #  updateProgressBar(session = session, id = "pb6", value = (i/maxi)*100)
    #  Sys.sleep(0.1)
    #}
    
    round1 <- Team_Selection_Final$round
    gw <- Team_Selection_Final$GW_Chip
    
    #v6$plot <- captaincy_points(Team_Selection_Final, round1)
    #v6$plot <- captaincy_points_text(Team_Selection_Final, small_df2, round1, gw)
    
    p <- captaincy_points_text(Team_Selection_Final, small_df2, round1, gw)
    
    #p <- v6$plot
    
    p
    
  })
  
  output$download_captaincy <- downloadHandler(
    filename = function(){paste("FPL.Captaincy_Points.png",sep='')},
    
    
    content = function(file){
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$bench_team_id, '/history/'))
      
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      # Fix: Check if chips exists and handle empty/NULL case
      if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
        Chips <- JSON_Output$chips %>%
          select(-time)
      } else {
        # Create empty data frame with expected structure if no chips
        Chips <- data.frame(
          event = integer(0),
          name = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      
      Team_Selection_Final <- captaincy_parse(input$bench_team_id, Players_History, max(Players_History$round)) %>%
        left_join(Chips, by = c('round' = 'event')) %>%
        rename(chip = name) %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(round,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ","")) %>%
        separate(name_club, sep = " " ,c("name","club")) %>%
        mutate(name_info = paste0(name, "\n(",oppsition,")"))
      
      labels <- c()
      
      for (i in 1:length(Team_Selection_Final$player_images)){
        
        img.name <- Team_Selection_Final$label2[i]
        
        labels <- c(labels, paste0("<img src='", Team_Selection_Final$player_images[i],  "' width='20' /><br>", img.name))
        
      }
      
      small_df2 <- Team_Selection_Final %>%
        group_by(round) %>%
        summarise(chip = unique(chip)) %>%
        ungroup() %>%
        filter(!is.na(chip))
      
      #maxi <- 10
      #for (i in 1:maxi) {
      #  updateProgressBar(session = session, id = "pb6", value = (i/maxi)*100)
      #  Sys.sleep(0.1)
      #}
      
      round1 <- Team_Selection_Final$round
      gw <- Team_Selection_Final$GW_Chip
      
      #v6$plot <- captaincy_points(Team_Selection_Final, round1)
      #v6$plot <- captaincy_points_text(Team_Selection_Final, small_df2, round1, gw)
      
      p <- captaincy_points_text(Team_Selection_Final, small_df2, round1, gw)
      
      #p <- v6$plot
      
      #ggsave_golden(file, plot = p, dpi = 600)
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  output$league_plot <- renderPlot({
    
    Combined_Team <- league_parse(input$team_id2, input$input_rival, max(Players_History$round))
    
    #maxi <- 10
    #for (i in 1:maxi) {
    #  updateProgressBar(session = session, id = "pb4", value = (i/maxi)*100)
    #  Sys.sleep(0.1)
    #}
    
    #v$plot <- league_gwscore(Combined_Team)
    p <- league_gwscore(Combined_Team)
    
    #p <- v$plot
    
    p
    
  })
  
  output$download_leaguegw <- downloadHandler(
    filename = function(){paste("FPL.League.GW.png",sep='')},
    
    
    content = function(file){
      
      Combined_Team <- league_parse(input$team_id2, input$input_rival, max(Players_History$round))
      
      #maxi <- 10
      #for (i in 1:maxi) {
      #  updateProgressBar(session = session, id = "pb4", value = (i/maxi)*100)
      #  Sys.sleep(0.1)
      #}
      
      #v$plot <- league_gwscore(Combined_Team)
      p <- league_gwscore(Combined_Team)
      
      #p <- v$plot
      
      #ggsave_golden(file, plot = p, dpi = 600)
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
      
    
  
  output$league_gwplot <- renderPlot({

    
    Combined_Team <- league_parse(input$team_id2, input$input_rival, max(Players_History$round)) %>%
      mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                     overall_rank < 1000 ~ as.character(overall_rank),
                                     T ~ paste0(round(overall_rank / 1000, digits = 0),"K")))
    
    #maxi <- 10
    #for (i in 1:maxi) {
    #  updateProgressBar(session = session, id = "pb3", value = (i/maxi)*100)
    #  Sys.sleep(0.1)
    #}
    
    #v2$plot <- league_gwrank(Combined_Team)
    p <- league_gwrank(Combined_Team)
    
    #p <- v2$plot
    
    p
    
    
  })
  
  output$download_leaguerank <- downloadHandler(
    filename = function(){paste("FPL.League.Overall_Rank.png",sep='')},
    
    
    content = function(file){
      
      Combined_Team <- league_parse(input$team_id2, input$input_rival, max(Players_History$round)) %>%
        mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                       overall_rank < 1000 ~ as.character(overall_rank),
                                       T ~ paste0(round(overall_rank / 1000, digits = 0),"K")))
      
      #maxi <- 10
      #for (i in 1:maxi) {
      #  updateProgressBar(session = session, id = "pb3", value = (i/maxi)*100)
      #  Sys.sleep(0.1)
      #}
      
      #v2$plot <- league_gwrank(Combined_Team)
      p <- league_gwrank(Combined_Team)
      
      #p <- v2$plot
      
      p
      
      #ggsave_golden(file, plot = p, dpi = 600)
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  
  output$table2 <- DT::renderDataTable(table2_parse(Players_Gameweek,input$team_id3) %>%
                                         #left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name')) %>%
                                         {if(input$transfer_input == "No") filter(., !chip %in% c('wildcard','freehit')) else mutate(., chip = chip)},
    
    #{if (input$transfer_input == "No") table2_parse(Players_Gameweek,input$team_id3) %>%
                                      #       filter(., is.na(chip))
                                      #     else table2_parse(Players_Gameweek,input$team_id3)},
                                       rownames = F, extensions = "FixedColumns",
                                       options = list(
                                         scrollX = T,
                                         paging = TRUE, 
                                         searching = TRUE, 
                                         info = FALSE,
                                         sort = TRUE,
                                         fixedColumns = TRUE,
                                         autoWidth = TRUE))
  
  output$transfer_plot <- renderPlot({
    
    # Make the API call only once
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
    
    jsonRespText <- content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    
    # More robust chips processing
    Chips <- tryCatch({
      if(!is.null(JSON_Output$chips) && length(JSON_Output$chips) > 0) {
        chips_data <- JSON_Output$chips
        # Ensure it's a data frame
        if(is.list(chips_data) && !is.data.frame(chips_data)) {
          chips_data <- as.data.frame(do.call(rbind, chips_data))
        }
        # Remove time column if it exists
        if("time" %in% names(chips_data)) {
          chips_data %>% select(-time)
        } else {
          chips_data
        }
      } else {
        data.frame(event = integer(0), name = character(0), stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      data.frame(event = integer(0), name = character(0), stringsAsFactors = FALSE)
    })
    
    calculation1 <- input$transfer_input
    
    tmp_df2 <- table2_parse(Players_Gameweek, input$team_id3)
    
    tmp_vec <- seq(1, max(Players_Gameweek$round), 1)
    
    # Fix the problematic join - don't join on chip = name
    tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, tmp_df2$Gameweek), 
                         chip = NA,
                         stringsAsFactors = FALSE)
    
    # Add chip information separately
    if(nrow(Chips) > 0) {
      # Create a lookup for chips by event/gameweek
      chip_lookup <- Chips %>%
        select(event, name) %>%
        rename(Gameweek = event, chip = name)
      
      # Update tmp_df with chip information
      tmp_df <- tmp_df %>%
        left_join(chip_lookup, by = "Gameweek") %>%
        mutate(chip = coalesce(chip.y, chip.x)) %>%
        select(-chip.x, -chip.y)
    }
    
    transfers <- bind_rows(tmp_df2, tmp_df)
    
    transfer_df <- transfers %>%
      {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit') | is.na(chip)) else mutate(., chip = chip)} %>%
      mutate(movement = case_when(`Points Difference After Hits` <= -2  ~ "Poor",
                                  `Points Difference After Hits` >= 5 ~ "Good",
                                  T ~ "Average")) %>%
      # Add chip information if not already present
      {if(nrow(Chips) > 0 && !"chip_short" %in% names(.)) {
        left_join(., Chips, by = c('Gameweek' = 'event'))
      } else {
        .
      }} %>%
      mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                    chip == 'freehit' ~ 'FH',
                                    chip == 'bboost' ~ 'BB',
                                    chip == '3xc' ~ 'TC',
                                    chip == 'manager' ~ 'AM',
                                    T ~'')) %>%
      mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
      mutate(GW_Chip = str_replace(GW_Chip, " ","")) %>%
      pivot_longer(cols = c("Points In", "Points Out"), names_to = "Points")
    
    small_df2 <- transfer_df %>%
      filter(!is.na(chip))
    
    tester_df <- transfer_df %>%
      filter(Gameweek %in% tmp_df2$Gameweek)
    
    Names1 <- transfer_df$GW_Chip
    Breaks1 <- transfer_df$Gameweek
    
    p <- transfer_plot1(transfer_df, small_df2, Names1, Breaks1, tester_df)
    
    p
    
  })
  
  output$download_transfer1 <- downloadHandler(
    filename = function(){paste("FPL.Transfer_Points.png",sep='')},
    
    
    content = function(file){
      
      # Make the API call only once
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
      
      jsonRespText <- content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      # More robust chips processing
      Chips <- tryCatch({
        if(!is.null(JSON_Output$chips) && length(JSON_Output$chips) > 0) {
          chips_data <- JSON_Output$chips
          # Ensure it's a data frame
          if(is.list(chips_data) && !is.data.frame(chips_data)) {
            chips_data <- as.data.frame(do.call(rbind, chips_data))
          }
          # Remove time column if it exists
          if("time" %in% names(chips_data)) {
            chips_data %>% select(-time)
          } else {
            chips_data
          }
        } else {
          data.frame(event = integer(0), name = character(0), stringsAsFactors = FALSE)
        }
      }, error = function(e) {
        data.frame(event = integer(0), name = character(0), stringsAsFactors = FALSE)
      })
      
      calculation1 <- input$transfer_input
      
      tmp_df2 <- table2_parse(Players_Gameweek, input$team_id3)
      
      tmp_vec <- seq(1, max(Players_Gameweek$round), 1)
      
      # Fix the problematic join - don't join on chip = name
      tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, tmp_df2$Gameweek), 
                           chip = NA,
                           stringsAsFactors = FALSE)
      
      # Add chip information separately
      if(nrow(Chips) > 0) {
        # Create a lookup for chips by event/gameweek
        chip_lookup <- Chips %>%
          select(event, name) %>%
          rename(Gameweek = event, chip = name)
        
        # Update tmp_df with chip information
        tmp_df <- tmp_df %>%
          left_join(chip_lookup, by = "Gameweek") %>%
          mutate(chip = coalesce(chip.y, chip.x)) %>%
          select(-chip.x, -chip.y)
      }
      
      transfers <- bind_rows(tmp_df2, tmp_df)
      
      transfer_df <- transfers %>%
        {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit') | is.na(chip)) else mutate(., chip = chip)} %>%
        mutate(movement = case_when(`Points Difference After Hits` <= -2  ~ "Poor",
                                    `Points Difference After Hits` >= 5 ~ "Good",
                                    T ~ "Average")) %>%
        # Add chip information if not already present
        {if(nrow(Chips) > 0 && !"chip_short" %in% names(.)) {
          left_join(., Chips, by = c('Gameweek' = 'event'))
        } else {
          .
        }} %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ","")) %>%
        pivot_longer(cols = c("Points In", "Points Out"), names_to = "Points")
      
      small_df2 <- transfer_df %>%
        filter(!is.na(chip))
      
      tester_df <- transfer_df %>%
        filter(Gameweek %in% tmp_df2$Gameweek)
      
      Names1 <- transfer_df$GW_Chip
      Breaks1 <- transfer_df$Gameweek
      
      p <- transfer_plot1(transfer_df, small_df2, Names1, Breaks1, tester_df)
      
      p
      
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  output$transfer_diff_plot <- renderPlot({
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    
    # Fix: Check if chips exists and handle empty/NULL case
    if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
      Chips <- JSON_Output$chips %>%
        select(-time)
    } else {
      # Create empty data frame with expected structure if no chips
      Chips <- data.frame(
        event = integer(0),
        name = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    
    tmp_df2 <- table2_parse(Players_Gameweek,input$team_id3)
    
    tmp_vec <- seq(1, max(Players_Gameweek$round),1)
    
    tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, tmp_df2$Gameweek), 
                         chip = NA) %>%
      left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name'))
    
    transfers <- bind_rows(tmp_df2, tmp_df)
    
    calculation1 <- input$transfer_input
    
    
    
    transfer_df <- transfers %>%
      #{if (input$transfer_input == "No") table2_parse(Players_Gameweek,input$team_id3) %>%
      #filter(., is.na(chip))
      #else table2_parse(Players_Gameweek,input$team_id3)} %>%
      {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit')) else mutate(., chip = chip)} %>%
      mutate(movement = case_when(`Points Difference After Hits` <= -2  ~ "Poor",
                                  `Points Difference After Hits` >= 5 ~ "Good",
                                  T ~ "Average")) %>%
      left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name')) %>%
      mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                    chip == 'freehit' ~ 'FH',
                                    chip == 'bboost' ~ 'BB',
                                    chip == '3xc' ~ 'TC',
                                    chip == 'manager' ~ 'AM',
                                    T ~'')) %>%
      mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
      mutate(GW_Chip = str_replace(GW_Chip, " ",""))
    
    
    small_df2 <- transfer_df %>%
      filter(!is.na(chip))
    
    
    
    #maxi <- 10
    #for (i in 1:maxi) {
    #  updateProgressBar(session = session, id = "pb8", value = (i/maxi)*100)
    #  Sys.sleep(0.1)
    #}
    
    Names1 <- transfer_df$GW_Chip
    Breaks1 <- transfer_df$Gameweek
    
    #v8$plot <- transfer_plot2(transfer_df, small_df2, input$team_id3, Names1, Breaks1)
    p <- transfer_plot2(transfer_df, small_df2, input$team_id3, Names1, Breaks1)
    
    #p <- v8$plot
    
    p
    
  })
  
 
  output$download_transfer2 <- downloadHandler(
    filename = function(){paste("FPL.Transfer_Difference.png",sep='')},
    
    
    content = function(file){
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      # Fix: Check if chips exists and handle empty/NULL case
      if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
        Chips <- JSON_Output$chips %>%
          select(-time)
      } else {
        # Create empty data frame with expected structure if no chips
        Chips <- data.frame(
          event = integer(0),
          name = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      
      tmp_df2 <- table2_parse(Players_Gameweek,input$team_id3)
      
      tmp_vec <- seq(1, max(Players_Gameweek$round),1)
      
      tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, tmp_df2$Gameweek), 
                           chip = NA) %>%
        left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name'))
      
      transfers <- bind_rows(tmp_df2, tmp_df)
      
      calculation1 <- input$transfer_input
      
      
      
      transfer_df <- transfers %>%
        #{if (input$transfer_input == "No") table2_parse(Players_Gameweek,input$team_id3) %>%
        #filter(., is.na(chip))
        #else table2_parse(Players_Gameweek,input$team_id3)} %>%
        {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit')) else mutate(., chip = chip)} %>%
        mutate(movement = case_when(`Points Difference After Hits` <= -2  ~ "Poor",
                                    `Points Difference After Hits` >= 5 ~ "Good",
                                    T ~ "Average")) %>%
        left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name')) %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ",""))
      
      
      small_df2 <- transfer_df %>%
        filter(!is.na(chip))
      
      
      
      #maxi <- 10
      #for (i in 1:maxi) {
      #  updateProgressBar(session = session, id = "pb8", value = (i/maxi)*100)
      #  Sys.sleep(0.1)
      #}
      
      Names1 <- transfer_df$GW_Chip
      Breaks1 <- transfer_df$Gameweek
      
      #v8$plot <- transfer_plot2(transfer_df, small_df2, input$team_id3, Names1, Breaks1)
      p <- transfer_plot2(transfer_df, small_df2, input$team_id3, Names1, Breaks1)
      
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  output$fixture_plot <- renderPlot({
    
    Team_Fixtures <- Fixtures %>%
      select(event,team_name,short_team_name,strength,opp_team_name,opp_team_name, opp_short_team_name, opp_strength, team_strength,team_strength_att,team_strength_def, opp_team_strength, opp_team_strength_att, opp_team_strength_def, home_team, away_team, was_home) %>%
      unique() %>%
      rename(GW = event) %>%
      filter(!is.na(GW)) %>%
      # Clean GW Strength
      mutate(opp_team_strength = max(Fixtures$opp_team_strength) - opp_team_strength,
             opp_team_strength_att = max(Fixtures$opp_team_strength_att) - opp_team_strength_att,
             opp_team_strength_def = max(Fixtures$opp_team_strength_def) - opp_team_strength_def,
             home_away = case_when(team_name == home_team ~ "H",
                                   T ~ "A")) %>%
      mutate(team_info = paste0(opp_short_team_name, " (",home_away,")")) %>%
      group_by(GW, team_name, .drop = FALSE) %>%
      summarise(#fixture_difficulty = team_strength - opp_team_strength,
        games = n(),
        opp_diffculty_short = sum(opp_strength),
        team_strength = sum(team_strength),
        team_strength_short = sum(strength),
        opp_team_strength = sum(opp_team_strength),
        opp_team_strength_short = sum(opp_strength),
        opp_team_strength_attack = sum(opp_team_strength_att),
        opp_team_strength_defence = sum(opp_team_strength_def),
        team_info = paste0(team_info, collapse = ","),
        home_away = paste0(home_away, collapse = ","),
        home_team = paste0(home_team, collapse = ","),
        opp_team = paste0(opp_short_team_name, collapse = ",")) %>%
      #complete(GW = Fixtures %>% filter(!is.na(event)) %>% pull(event) %>% unique()) %>%
      mutate(difficulty = team_strength - opp_team_strength,
             difficulty_short = team_strength_short - opp_team_strength_short) %>%
      mutate(GW_Type = case_when(games == 2 ~ "DGW",
                                 games == 1 ~ "SGW")) %>%
      ungroup() %>%
      filter(GW %in% c(input$gameweeks_2)) %>%
      group_by(team_name) %>%
      mutate(total_strength = sum(team_strength),
             total_difficulty = sum(difficulty),
             total_opp_team_strength = sum(opp_team_strength),
             total_opp_att_strength = sum(opp_team_strength_attack),
             total_opp_def_strength = sum(opp_team_strength_defence),
             total_opp_strength = sum(opp_diffculty_short)) %>%
      ungroup() %>%
      {if (input$fixture_input == "Overall") mutate(., class = fct_reorder(team_name, total_opp_team_strength, .fun='sum'))
        else if (input$fixture_input == "Attack") mutate(., class = fct_reorder(team_name, total_opp_att_strength, .fun='sum'))
        else if (input$fixture_input == "Defence") mutate(., class = fct_reorder(team_name, total_opp_def_strength, .fun='sum'))} %>%
      {if (input$fixture_input == "Overall") mutate(., fill_variable = opp_team_strength)
        else if (input$fixture_input == "Attack") mutate(., fill_variable = opp_team_strength_defence)
        else if (input$fixture_input == "Defence") mutate(., fill_variable = opp_team_strength_attack)}
    
    midpoint_number <- input$fixture_slider
    
    p <- ggplot(data = Team_Fixtures, aes(x = GW, y = class)) + 
      geom_tile(aes(fill = fill_variable), colour = "#F3E9E2", show.legend = F) +
      geom_text(data = Team_Fixtures %>% filter(GW_Type == "SGW"), aes(label = team_info), family = "Lato", size = 2.2, color = "black") +
      geom_text(data = Team_Fixtures %>% filter(GW_Type == "DGW"), aes(label = team_info), family = "Lato", size = 2.2, color = "black") +
      #geom_label(data = Team_Fixtures %>% filter(GW_Type == "SGW"), aes(label = team_info, fill = fill_variable), color = "black", face = "bold", family = "Roboto", size = 2, show.legend = F) +
      #geom_label(data = Team_Fixtures %>% filter(GW_Type == "DGW"), aes(label = team_info), fill = "green", color = "black", face = "bold", family = "Roboto", size = 2, show.legend = F) +
      scale_fill_gradient2(high = 'green', mid = 'yellow', low = 'red', midpoint = midpoint_number) +
      #scale_fill_gradient2(high = '#66C2A5', mid = '#FFBF00', low = '#F2055C', midpoint = 150) +
      #labs(title = paste0("Fixture Difficulty - ", input$fixture_input, " Strength")) +
      ylab("Teams (Ordered By Fixture Difficulty)") +
      scale_x_continuous(breaks = seq(0,38)) +
      labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
        title =  paste0("<img src='./Logo4.png' width='30'>", " Fixture Difficulty - Ordered Based On ", input$fixture_input, " Strength"),
        caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
      #Adjust Theme to suit this plot
      theme(panel.grid.major.x = element_blank(),
            #panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            #panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            panel.spacing = unit(0.5, "lines"),
            #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
            legend.position = "top",
            panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            plot.title = element_markdown(color = "black", size = 22, family = "Lato"),
            plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
            plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
            legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
            legend.title = element_text(color = "gray10", size = 12,  family = "Lato"),
            legend.key = element_blank(),
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(0,0,0,0),
            axis.text.x = element_text(color ="black", size = 9, family = "Lato"),
            axis.text.y = element_text(color ="black", size = 9, family = "Lato",hjust = 1),
            #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.length.x = unit(0.1, "cm"),
            axis.ticks.x = element_blank(),
            axis.ticks.length.y = unit(0.1, "cm"),
            axis.ticks.y = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
            strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", face = 'bold', angle = 360, vjust = 0),
            legend.background = element_blank())
    
    p
    
  })
  
  output$download_fixtures <- downloadHandler(
    filename = function(){paste("FPL.Fixture_Difficulty.png",sep='')},
    
    
    content = function(file){
      
      Team_Fixtures <- Fixtures %>%
        select(event,team_name,short_team_name,strength,opp_team_name,opp_team_name, opp_short_team_name, opp_strength, team_strength,team_strength_att,team_strength_def, opp_team_strength, opp_team_strength_att, opp_team_strength_def, home_team, away_team, was_home) %>%
        unique() %>%
        rename(GW = event) %>%
        filter(!is.na(GW)) %>%
        # Clean GW Strength
        mutate(opp_team_strength = max(Fixtures$opp_team_strength) - opp_team_strength,
               opp_team_strength_att = max(Fixtures$opp_team_strength_att) - opp_team_strength_att,
               opp_team_strength_def = max(Fixtures$opp_team_strength_def) - opp_team_strength_def,
               home_away = case_when(team_name == home_team ~ "H",
                                     T ~ "A")) %>%
        mutate(team_info = paste0(opp_short_team_name, " (",home_away,")")) %>%
        group_by(GW, team_name, .drop = FALSE) %>%
        summarise(#fixture_difficulty = team_strength - opp_team_strength,
          games = n(),
          opp_diffculty_short = sum(opp_strength),
          team_strength = sum(team_strength),
          team_strength_short = sum(strength),
          opp_team_strength = sum(opp_team_strength),
          opp_team_strength_short = sum(opp_strength),
          opp_team_strength_attack = sum(opp_team_strength_att),
          opp_team_strength_defence = sum(opp_team_strength_def),
          team_info = paste0(team_info, collapse = ","),
          home_away = paste0(home_away, collapse = ","),
          home_team = paste0(home_team, collapse = ","),
          opp_team = paste0(opp_short_team_name, collapse = ",")) %>%
        #complete(GW = Fixtures %>% filter(!is.na(event)) %>% pull(event) %>% unique()) %>%
        mutate(difficulty = team_strength - opp_team_strength,
               difficulty_short = team_strength_short - opp_team_strength_short) %>%
        mutate(GW_Type = case_when(games == 2 ~ "DGW",
                                   games == 1 ~ "SGW")) %>%
        ungroup() %>%
        filter(GW %in% c(input$gameweeks_2)) %>%
        group_by(team_name) %>%
        mutate(total_strength = sum(team_strength),
               total_difficulty = sum(difficulty),
               total_opp_team_strength = sum(opp_team_strength),
               total_opp_att_strength = sum(opp_team_strength_attack),
               total_opp_def_strength = sum(opp_team_strength_defence),
               total_opp_strength = sum(opp_diffculty_short)) %>%
        ungroup() %>%
        {if (input$fixture_input == "Overall") mutate(., class = fct_reorder(team_name, total_opp_team_strength, .fun='sum'))
          else if (input$fixture_input == "Attack") mutate(., class = fct_reorder(team_name, total_opp_att_strength, .fun='sum'))
          else if (input$fixture_input == "Defence") mutate(., class = fct_reorder(team_name, total_opp_def_strength, .fun='sum'))} %>%
        {if (input$fixture_input == "Overall") mutate(., fill_variable = opp_team_strength)
          else if (input$fixture_input == "Attack") mutate(., fill_variable = opp_team_strength_defence)
          else if (input$fixture_input == "Defence") mutate(., fill_variable = opp_team_strength_attack)}
      
      midpoint_number <- input$fixture_slider
      
      p <- ggplot(data = Team_Fixtures, aes(x = GW, y = class)) + 
        geom_tile(aes(fill = fill_variable), colour = "#F3E9E2", show.legend = F) +
        geom_text(data = Team_Fixtures %>% filter(GW_Type == "SGW"), aes(label = team_info), family = "Lato", size = 2.2, color = "black") +
        geom_text(data = Team_Fixtures %>% filter(GW_Type == "DGW"), aes(label = team_info), family = "Lato", size = 2.2, color = "black") +
        #geom_label(data = Team_Fixtures %>% filter(GW_Type == "SGW"), aes(label = team_info, fill = fill_variable), color = "black", face = "bold", family = "Roboto", size = 2, show.legend = F) +
        #geom_label(data = Team_Fixtures %>% filter(GW_Type == "DGW"), aes(label = team_info), fill = "green", color = "black", face = "bold", family = "Roboto", size = 2, show.legend = F) +
        scale_fill_gradient2(high = 'green', mid = 'yellow', low = 'red', midpoint = midpoint_number) +
        #scale_fill_gradient2(high = '#66C2A5', mid = '#FFBF00', low = '#F2055C', midpoint = 150) +
        #labs(title = paste0("Fixture Difficulty - ", input$fixture_input, " Strength")) +
        ylab("Teams (Ordered By Fixture Difficulty)") +
        scale_x_continuous(breaks = seq(0,38)) +
        labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
          title =  paste0("<img src='./Logo4.png' width='30'>", " Fixture Difficulty - Ordered Based On ", input$fixture_input, " Strength"),
          caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
        #Adjust Theme to suit this plot
        theme(panel.grid.major.x = element_blank(),
              #panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              #panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.border = element_blank(),
              panel.spacing = unit(0.5, "lines"),
              #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
              legend.position = "top",
              panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.title = element_markdown(color = "black", size = 22, family = "Lato"),
              plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
              plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
              legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
              legend.title = element_text(color = "gray10", size = 12,  family = "Lato"),
              legend.key = element_blank(),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(0,0,0,0),
              axis.text.x = element_text(color ="black", size = 9, family = "Lato"),
              axis.text.y = element_text(color ="black", size = 9, family = "Lato",hjust = 1),
              #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.length.x = unit(0.1, "cm"),
              axis.ticks.x = element_blank(),
              axis.ticks.length.y = unit(0.1, "cm"),
              axis.ticks.y = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              strip.background = element_blank(),
              strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
              strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", face = 'bold', angle = 360, vjust = 0),
              legend.background = element_blank())

  
  
        ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  output$player_metric_plot <- renderPlot({
    
      metric1 <- input$player_metric
      test_df <- create_table1(Players_History, input$positions, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations, input$value) %>%
        mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                       Team == "AVL" ~ "#670E36",
                                       Team == "BOU" ~ "#B50E12",
                                       Team == "BRE" ~ "#E30613",
                                       Team == "BHA" ~ "#0057B8",
                                       Team == "BUR" ~ "#6C1D45",
                                       Team == "CHE" ~ "#034694",
                                       Team == "CRY" ~ "#1B458F",
                                       Team == "EVE" ~ "#003399",
                                       Team == "FUL" ~ "#000000",
                                       Team == "LEE" ~ "#FFCD00",
                                       Team == "LIV" ~ "#C8102E",
                                       Team == "LUT" ~ "#F78F1E",
                                       Team == "MCI" ~ "#6CABDD",
                                       Team == "MUN" ~ "#DA291C",
                                       Team == "NEW" ~ "#241F20",
                                       Team == "NFO" ~ "#DD0000",
                                       Team == "SHU" ~ "#EE2737",
                                       Team == "SUN" ~ "#EB172B",
                                       Team == "TOT" ~ "#132257",
                                       Team == "WHU" ~ "#7A263A",
                                       Team == "WOL" ~ "#FDB913",
                                       Team == "IPS" ~ "#3a64a3",
                                       Team == "LEI" ~ "#003090",
                                       Team == "SOU" ~ "#D71920")) %>%
        mutate(Player_Space = str_replace(Player, " ","\n")) %>%
        arrange(desc(get(metric1)), Team) %>% 
        head(input$top)
    
    
    
      #test_df["labels"] <- labels
    
      test_df$Player <- factor(test_df$Player, levels = test_df$Player)
      test_df$Player_Space <- factor(test_df$Player_Space, levels = test_df$Player_Space)
    
    
    #  v9$plot <- bar_plot_player(test_df, metric1, input$top, input$player_metric, input$positions, input$gameweeks[1], input$gameweeks[2])
    p <- bar_plot_player(test_df, metric1, input$top, input$player_metric, input$positions, input$gameweeks[1], input$gameweeks[2])
    
    
    p
    
    
  })
  
  
  output$download_bar_player <- downloadHandler(
    filename = function(){paste("FPL.", input$player_metric, ".GW", input$gameweeks[1], "_", input$gameweeks[2] ,".", input$positions,".png",sep='')},
    
    
    content = function(file){
      
      metric1 <- input$player_metric
      test_df <- create_table1(Players_History, input$positions, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations, input$value) %>%
        mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                       Team == "AVL" ~ "#670E36",
                                       Team == "BOU" ~ "#B50E12",
                                       Team == "BRE" ~ "#E30613",
                                       Team == "BHA" ~ "#0057B8",
                                       Team == "BUR" ~ "#6C1D45",
                                       Team == "CHE" ~ "#034694",
                                       Team == "CRY" ~ "#1B458F",
                                       Team == "EVE" ~ "#003399",
                                       Team == "FUL" ~ "#000000",
                                       Team == "LIV" ~ "#C8102E",
                                       Team == "LUT" ~ "#F78F1E",
                                       Team == "MCI" ~ "#6CABDD",
                                       Team == "MUN" ~ "#DA291C",
                                       Team == "NEW" ~ "#241F20",
                                       Team == "NFO" ~ "#DD0000",
                                       Team == "SHU" ~ "#EE2737",
                                       Team == "TOT" ~ "#132257",
                                       Team == "WHU" ~ "#7A263A",
                                       Team == "WOL" ~ "#FDB913")) %>%
        mutate(Player_Space = str_replace(Player, " ","\n")) %>%
        arrange(desc(get(metric1)), Team) %>% 
        head(input$top)
      
      
      #labels <- c()
      
      #for (i in 1:length(test_df$image)){
      
      #  labels <- c(labels, paste0("<img src='", test_df$image[i],  "' width='20' />"))
      
      #}
      
      #test_df["labels"] <- labels
      
      test_df$Player <- factor(test_df$Player, levels = test_df$Player)
      test_df$Player_Space <- factor(test_df$Player_Space, levels = test_df$Player_Space)
      
      
      #  v9$plot <- bar_plot_player(test_df, metric1, input$top, input$player_metric, input$positions, input$gameweeks[1], input$gameweeks[2])
      p <- bar_plot_player(test_df, metric1, input$top, input$player_metric, input$positions, input$gameweeks[1], input$gameweeks[2])
      
    #  maxi <- 10
    #  for (i in 1:maxi) {
    #    updateProgressBar(session = session, id = "pb9", value = (i/maxi)*100)
    #    Sys.sleep(0.1)
    #  }
      
      #p <- v9$plot
      
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
    })
  
  #matrixplot
  
  
  output$matrixplot <- renderPlot({
    

    
    ################################
    
    fixtures_df <- Players_History %>%
      mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                    was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
      mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                              was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
      group_by(short_team_name, round, opposition, team) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      group_by(short_team_name, round) %>%
      filter(count >= 11) %>%
      ungroup() %>%
      unite("merged",opposition:team, remove = F)
    
    
    Minutes_Data <- Players_History %>%
      mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                    was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
      mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                              was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
      unite("merged",opposition:team, remove = F) %>%
      filter(round >= input$gweeks[1] & round <= input$gweeks[2]) %>%
      filter(opposition %in% fixtures_df$opposition) %>%
      filter(merged %in% fixtures_df$merged) %>%
      group_by(short_team_name, opp_short_team_name, name_club, merged) %>%
      reframe(minutes = minutes,
              Game = case_when(was_home == "TRUE" ~ "H",
                               T ~ "A"),
              opposition = unique(opposition),
              team = unique(team),
              xgi = expected_goal_involvements, 
              #all_xgi = sum(expected_goal_involvements),
              name = web_name,
              points = total_points,
              Position = Position,
              opp_team = opp_team_name,
              #points = total_points,
              #all_points = sum(total_points),
              #all_minutes = sum(minutes),
              #opp_name = paste0(opp_short_team_name, " (",Game,") - GW ", round),
              opp_name = paste0(opp_short_team_name, "\n(",Game,")\n", round),
              Gameweek = round) %>%
      unite("merged2",team:opposition, remove = F) %>%
      arrange(desc(merged)) %>%
      ungroup() %>%
      group_by(name_club) %>%
      mutate(all_minutes = sum(minutes),
             all_points = sum(points),
             all_xgi = sum(xgi)) %>%
      ungroup() %>%
      #filter(all_minutes >= 300) %>%
      filter(all_minutes >= input$mins_matrix) %>%
      #filter(Gameweek >= 18 & Gameweek <= 23) %>%
      #filter(all_minutes >= 600) %>%
      mutate(Minutes_Type = case_when(minutes == 90 ~ "90 Minutes",
                                      minutes == 0 ~ "Zero Minutes",
                                      minutes < 60 ~ "Under 60 Minutes",
                                      T ~ "60 Minutes & Over")) %>%
      mutate(Points_Type = case_when(points >= 0 & points <=3 ~ "Blank",
                                     points >=  10 ~ "Double Digit", 
                                     points < 0 ~ "Minus",
                                     T ~ "Return")) %>%
      mutate(xgi_Type = case_when(xgi >= 0.05 & xgi <= 0.3 ~ "Not Involved",
                                  xgi <  0.05 ~ "Anonymous",
                                  xgi > 0.3 & xgi <= 0.5 ~ "Involved",
                                  T ~ "Very Involved")) %>%
      filter(short_team_name == input$team_matrix) %>%
      #filter(short_team_name == "ARS") %>%
      filter(minutes > 0) %>%
      arrange(Gameweek) %>%
      mutate(opp_crest = case_when(opp_team == "Arsenal" ~ "arsenal",
                                   opp_team == "Aston Villa" ~ "aston villa",
                                   opp_team == "Bournemouth"~ "bournemouth",
                                   opp_team == "Brentford" ~ "brentford",
                                   opp_team == "Brighton" ~ "brighton",
                                   opp_team == "Burnley"  ~ "burnley",
                                   opp_team == "Chelsea"  ~ "chelsea",
                                   opp_team == "Crystal Palace" ~ "crystal palace",
                                   opp_team == "Everton" ~ "everton",
                                   opp_team == "Fulham" ~ "fulham",
                                   opp_team == "Liverpool" ~ "liverpool",
                                   opp_team == "Luton" ~ "luton town",
                                   opp_team == "Man City" ~ "man city",
                                   opp_team == "Man Utd" ~ "man utd",
                                   opp_team == "Newcastle" ~ "newcastle utd",
                                   opp_team == "Nott'm Forest" ~ "forest",
                                   opp_team == "Sheffield Utd"  ~ "sheffield utd",
                                   opp_team == "Spurs" ~ "tottenham",
                                   opp_team == "West Ham" ~ "west ham",
                                   opp_team == "Wolves" ~ "wolves" )) %>%
      ungroup()
    
    
    #labels <- c()
    
    #for (i in 1:length(Minutes_Data$opp_crest)){
      
    #  home_away <- Minutes_Data$Game[i]
    #  gw <- Minutes_Data$Gameweek[i]
      
    #  labels <- c(labels, paste0("<img src='", "./Crests_2/", Minutes_Data$opp_crest[i], "_logo.png","' height='25' /><br>",gw, " (", home_away,")"))
      
    #}  
    
    Minutes_Data$Minutes_Type <- ordered(Minutes_Data$Minutes_Type, levels = c("90 Minutes","60 Minutes & Over","Under 60 Minutes"))
    Minutes_Data$Points_Type <- ordered(Minutes_Data$Points_Type, levels = c("Minus","Blank","Return","Double Digit"))
    Minutes_Data$Position <- ordered(Minutes_Data$Position, levels = c("GK","DEF","MID","ST"))
    
    if (input$stat_matrix == "Minutes") {
      
      y_var <- "all_minutes"
      text_var <- "minutes"
      col_var <- "Minutes_Type"
      color_palette <- scale_fill_manual(breaks = c("Under 60 Minutes","60 Minutes & Over","90 Minutes"),
                                         #values = c('#76D7C4','#76BAD7','#9376D7',"#5E4FA2","#F2055C")) +
                                         values = c('#DB444B','#3EBCD2','#006BA2')) 
      #values = c('#DB444B','#006BA2','#379A8B')) 
      scale1 <- scale_y_reordered(labels = setNames(paste0(Minutes_Data$name, " (", Minutes_Data$all_minutes," Mins)"),  paste0(Minutes_Data$Position,"___",Minutes_Data$name)),
                                  position = "right")
    }
    
    if (input$stat_matrix == "Points") {
      
      y_var <- "all_points"
      text_var <- "points"
      col_var <- "Points_Type"
      color_palette <- scale_fill_manual(breaks = c("Minus","Blank","Return","Double Digit"),
                                         values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
                                         #values = c('#DB444B','#EBB434','#3EBCD2','#006BA2'))
      #c('#DB444B','#9A607F','#3EBCD2','#006BA2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C','#758D99')
      #values = c("#F2300F","#E79805","#66C2A5", "#0B775E"))
      scale1 <- scale_y_reordered(labels = setNames(paste0(Minutes_Data$name, " (", Minutes_Data$all_points," Pts)"),  paste0(Minutes_Data$Position,"___",Minutes_Data$name)),
                                  position = "right")
    }
    
    if (input$stat_matrix == "XGI") {
      
      y_var <- "all_xgi"
      text_var <- "xgi"
      col_var <- "xgi_Type"
      color_palette <- scale_fill_manual(breaks = c("Anonymous","Not Involved","Involved","Very Involved"),
                                         values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
      #values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
      #values = c("#F2300F","#E79805","#66C2A5", "#0B775E"))
      scale1 <- scale_y_reordered(labels = setNames(paste0(Minutes_Data$name, " (", Minutes_Data$all_xgi," xGI)"),  paste0(Minutes_Data$Position,"___",Minutes_Data$name)),
                                  position = "right")
    }
    
    p <- ggplot(Minutes_Data, aes(x = reorder(opp_name, Gameweek), y = reorder_within(Position, get(y_var), name), fill = get(col_var), colour = get(col_var))) +
      geom_point(shape = 22, color = "#F3E9E2", size = 8) +
      geom_text(aes(label = get(text_var)), color = "#F3E9E2", family = "Lato", size = 2.5, show.legend = F) +
      facet_grid(Position ~., scales = "free", space = "free", switch = "y") +
      color_palette +
      scale1 +
      scale_x_discrete(name = NULL, labels = setNames(as.character(Minutes_Data$opp_name), Minutes_Data$opp_team)) +
      xlab('FPL Gameweeks') +
      ylab('Player Names') +
      labs(title = paste0("<img src='./Logo4.png' width='30'> ", input$team_matrix, " | Player ", input$stat_matrix, " Matrix"),
           #subtitle = "Cumulative Points Totals For the Highest Scoring FPL Players",
           caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
           size = 'Number Of Starts',
           fill = 'Appearance Type') +
      guides(fill = guide_legend(override.aes = list(size = 10))) +
      #Adjust Theme to suit this plot
      theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
        panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
        legend.position = "top",
        panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.title = element_markdown(color = "black", size = 28, family = "Lato"),
        plot.caption = element_markdown(color = "black", size = 12, family = "Lato"),
        plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0),
        legend.text = element_text(color = "black", size = 12, family = "Lato", hjust = 0),
        legend.title = element_text(color = "black", size = 14,  family = "Lato", face = "bold"),
        legend.key = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        axis.text.x = element_text(color ="black", size = 8, family = "Lato"),
        #axis.text.x = element_markdown(color ="black", size = 10, family = "Lato"),
        axis.text.y = element_text(color ="black", size = 10, family = "Lato", hjust = 0),
        #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
        axis.title.x = element_text(color = "black", size = 12,  family = "Lato", face = 'bold'),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_line(color = "gray30", size = 0.7),
        strip.background = element_blank(),
        strip.text.x = element_markdown(color = "gray20", size = 14, family = "Lato", face = 'bold'),
        #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
        legend.background = element_blank())   
    
    #p <- v11$plot
    
    p   
    
    
  })
  
  output$download_matrix <- downloadHandler(
    filename = function(){paste("FPL.", input$stat_matrix, ".GW", input$gweeks[1], "_", input$gweeks[2] ,".", input$team_matrix,".png",sep='')},
    
    
    content = function(file){
      
      fixtures_df <- Players_History %>%
        mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                      was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
        mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                                was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
        group_by(short_team_name, round, opposition, team) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(short_team_name, round) %>%
        filter(count >= 11) %>%
        ungroup() %>%
        unite("merged",opposition:team, remove = F)
      
      
      Minutes_Data <- Players_History %>%
        mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                      was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
        mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                                was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
        unite("merged",opposition:team, remove = F) %>%
        filter(round >= input$gweeks[1] & round <= input$gweeks[2]) %>%
        filter(opposition %in% fixtures_df$opposition) %>%
        filter(merged %in% fixtures_df$merged) %>%
        group_by(short_team_name, opp_short_team_name, name_club, merged) %>%
        reframe(minutes = minutes,
                Game = case_when(was_home == "TRUE" ~ "H",
                                 T ~ "A"),
                opposition = unique(opposition),
                team = unique(team),
                xgi = expected_goal_involvements, 
                #all_xgi = sum(expected_goal_involvements),
                name = web_name,
                points = total_points,
                Position = Position,
                opp_team = opp_team_name,
                #points = total_points,
                #all_points = sum(total_points),
                #all_minutes = sum(minutes),
                #opp_name = paste0(opp_short_team_name, " (",Game,") - GW ", round),
                opp_name = paste0(opp_short_team_name, "\n(",Game,")\n", round),
                Gameweek = round) %>%
        unite("merged2",team:opposition, remove = F) %>%
        arrange(desc(merged)) %>%
        ungroup() %>%
        group_by(name_club) %>%
        mutate(all_minutes = sum(minutes),
               all_points = sum(points),
               all_xgi = sum(xgi)) %>%
        ungroup() %>%
        #filter(all_minutes >= 300) %>%
        filter(all_minutes >= input$mins_matrix) %>%
        #filter(Gameweek >= 18 & Gameweek <= 23) %>%
        #filter(all_minutes >= 600) %>%
        mutate(Minutes_Type = case_when(minutes == 90 ~ "90 Minutes",
                                        minutes == 0 ~ "Zero Minutes",
                                        minutes < 60 ~ "Under 60 Minutes",
                                        T ~ "60 Minutes & Over")) %>%
        mutate(Points_Type = case_when(points >= 0 & points <=3 ~ "Blank",
                                       points >=  10 ~ "Double Digit", 
                                       points < 0 ~ "Minus",
                                       T ~ "Return")) %>%
        mutate(xgi_Type = case_when(xgi >= 0.05 & xgi <= 0.3 ~ "Not Involved",
                                    xgi <  0.05 ~ "Anonymous",
                                    xgi > 0.3 & xgi <= 0.5 ~ "Involved",
                                    T ~ "Very Involved")) %>%
        filter(short_team_name == input$team_matrix) %>%
        #filter(short_team_name == "ARS") %>%
        filter(minutes > 0) %>%
        arrange(Gameweek) %>%
        mutate(opp_crest = case_when(opp_team == "Arsenal" ~ "arsenal",
                                     opp_team == "Aston Villa" ~ "aston villa",
                                     opp_team == "Bournemouth"~ "bournemouth",
                                     opp_team == "Brentford" ~ "brentford",
                                     opp_team == "Brighton" ~ "brighton",
                                     opp_team == "Burnley"  ~ "burnley",
                                     opp_team == "Chelsea"  ~ "chelsea",
                                     opp_team == "Crystal Palace" ~ "crystal palace",
                                     opp_team == "Everton" ~ "everton",
                                     opp_team == "Fulham" ~ "fulham",
                                     opp_team == "Liverpool" ~ "liverpool",
                                     opp_team == "Luton" ~ "luton town",
                                     opp_team == "Man City" ~ "man city",
                                     opp_team == "Man Utd" ~ "man utd",
                                     opp_team == "Newcastle" ~ "newcastle utd",
                                     opp_team == "Nott'm Forest" ~ "forest",
                                     opp_team == "Sheffield Utd"  ~ "sheffield utd",
                                     opp_team == "Spurs" ~ "tottenham",
                                     opp_team == "West Ham" ~ "west ham",
                                     opp_team == "Wolves" ~ "wolves" )) %>%
        ungroup()
      
      
      #labels <- c()
      
      #for (i in 1:length(Minutes_Data$opp_crest)){
      
      #  home_away <- Minutes_Data$Game[i]
      #  gw <- Minutes_Data$Gameweek[i]
      
      #  labels <- c(labels, paste0("<img src='", "./Crests_2/", Minutes_Data$opp_crest[i], "_logo.png","' height='25' /><br>",gw, " (", home_away,")"))
      
      #}  
      
      Minutes_Data$Minutes_Type <- ordered(Minutes_Data$Minutes_Type, levels = c("90 Minutes","60 Minutes & Over","Under 60 Minutes"))
      Minutes_Data$Points_Type <- ordered(Minutes_Data$Points_Type, levels = c("Minus","Blank","Return","Double Digit"))
      Minutes_Data$Position <- ordered(Minutes_Data$Position, levels = c("GK","DEF","MID","ST"))
      
      if (input$stat_matrix == "Minutes") {
        
        y_var <- "all_minutes"
        text_var <- "minutes"
        col_var <- "Minutes_Type"
        color_palette <- scale_fill_manual(breaks = c("Under 60 Minutes","60 Minutes & Over","90 Minutes"),
                                           #values = c('#76D7C4','#76BAD7','#9376D7',"#5E4FA2","#F2055C")) +
                                           values = c('#DB444B','#3EBCD2','#006BA2')) 
        #values = c('#DB444B','#006BA2','#379A8B')) 
        scale1 <- scale_y_reordered(labels = setNames(paste0(Minutes_Data$name, " (", Minutes_Data$all_minutes," Mins)"),  paste0(Minutes_Data$Position,"___",Minutes_Data$name)),
                                    position = "right")
      }
      
      if (input$stat_matrix == "Points") {
        
        y_var <- "all_points"
        text_var <- "points"
        col_var <- "Points_Type"
        color_palette <- scale_fill_manual(breaks = c("Minus","Blank","Return","Double Digit"),
                                           values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
        #values = c('#DB444B','#EBB434','#3EBCD2','#006BA2'))
        #c('#DB444B','#9A607F','#3EBCD2','#006BA2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C','#758D99')
        #values = c("#F2300F","#E79805","#66C2A5", "#0B775E"))
        scale1 <- scale_y_reordered(labels = setNames(paste0(Minutes_Data$name, " (", Minutes_Data$all_points," Pts)"),  paste0(Minutes_Data$Position,"___",Minutes_Data$name)),
                                    position = "right")
      }
      
      if (input$stat_matrix == "XGI") {
        
        y_var <- "all_xgi"
        text_var <- "xgi"
        col_var <- "xgi_Type"
        color_palette <- scale_fill_manual(breaks = c("Anonymous","Not Involved","Involved","Very Involved"),
                                           values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
        #values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
        #values = c("#F2300F","#E79805","#66C2A5", "#0B775E"))
        scale1 <- scale_y_reordered(labels = setNames(paste0(Minutes_Data$name, " (", Minutes_Data$all_xgi," xGI)"),  paste0(Minutes_Data$Position,"___",Minutes_Data$name)),
                                    position = "right")
      }
      
      p <- ggplot(Minutes_Data, aes(x = reorder(opp_name, Gameweek), y = reorder_within(Position, get(y_var), name), fill = get(col_var), colour = get(col_var))) +
        geom_point(shape = 22, color = "#F3E9E2", size = 8) +
        geom_text(aes(label = get(text_var)), color = "#F3E9E2", family = "Lato", size = 2.5, show.legend = F) +
        facet_grid(Position ~., scales = "free", space = "free", switch = "y") +
        color_palette +
        scale1 +
        scale_x_discrete(name = NULL, labels = setNames(as.character(Minutes_Data$opp_name), Minutes_Data$opp_team)) +
        xlab('FPL Gameweeks') +
        ylab('Player Names') +
        labs(title = paste0("<img src='./Logo4.png' width='30'> ", input$team_matrix, " | Player ", input$stat_matrix, " Matrix"),
             #subtitle = "Cumulative Points Totals For the Highest Scoring FPL Players",
             caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
             size = 'Number Of Starts',
             fill = 'Appearance Type') +
        guides(fill = guide_legend(override.aes = list(size = 10))) +
        #Adjust Theme to suit this plot
        theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
          panel.grid.major.x = element_blank(),
          #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
          panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "black", size = 28, family = "Lato"),
          plot.caption = element_markdown(color = "black", size = 12, family = "Lato"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0),
          legend.text = element_text(color = "black", size = 12, family = "Lato", hjust = 0),
          legend.title = element_text(color = "black", size = 14,  family = "Lato", face = "bold"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 8, family = "Lato"),
          #axis.text.x = element_markdown(color ="black", size = 10, family = "Lato"),
          axis.text.y = element_text(color ="black", size = 10, family = "Lato", hjust = 0),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato", face = 'bold'),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_line(color = "gray30", size = 0.7),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 14, family = "Lato", face = 'bold'),
          #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
          legend.background = element_blank())
      
      #p <- v11$plot
      
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
      
    })
  
  
  output$transfer_cum_plot <- renderPlot({
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    
    # Fix: Check if chips exists and handle empty/NULL case
    if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
      Chips <- JSON_Output$chips %>%
        select(-time)
    } else {
      # Create empty data frame with expected structure if no chips
      Chips <- data.frame(
        event = integer(0),
        name = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    transfer_df <- table2_parse(Players_Gameweek,input$team_id3)
    
    tmp_vec <- seq(1, max(Players_Gameweek$round),1)
    
    tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, transfer_df$Gameweek), 
                         chip = NA) %>%
      left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name'))
    
    transfers <- bind_rows(transfer_df, tmp_df)
    
    calculation1 <- input$transfer_input
    
    Transfers2 <- transfers %>%
      {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit')) else mutate(., chip = chip)
        
      }
    
    
    small_df <- Transfers2 %>%
      mutate(cum_transfers =  cumsum(transfers),
             lag_transfer = lead(transfers),
             cum_transfers2 = cumsum(lead(transfers))) %>%
      mutate(new_text = str_replace(transfers_in, ",","\n")) %>%
      mutate(transfer_type = case_when(hits_taken == 0 ~ "Free Transfers",
                                       hits_taken == 4 ~ "-4 Hit",
                                       hits_taken == 8 ~ "-8 Hit",
                                       hits_taken == 12 ~ "-12 Hit",
                                       T ~ "Multiple Hits")) %>%
      mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                    chip == 'freehit' ~ 'FH',
                                    chip == 'bboost' ~ 'BB',
                                    chip == '3xc' ~ 'TC',
                                    chip == 'manager' ~ 'AM',
                                    T ~'')) %>%
      mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
      mutate(GW_Chip = str_replace(GW_Chip, " ",""))
    
    small_df2 <- small_df %>%
      filter(!is.na(chip))
    
    p <- ggplot(small_df, aes(x = Gameweek, y = cum_transfers)) +
      #geom_vline(data = small_df2, aes(xintercept = Gameweek), color = "lightgray", size = 9) +
      geom_rect(data = small_df2, aes(xmin = Gameweek - 0.4, xmax = Gameweek + 0.4, ymin = -Inf, ymax = Inf), fill = "lightgray", color = NA, show.legend = F) +
      geom_step(linewidth = 1, color = '#006BA2') +
      geom_point(shape = 21, color = "#F3E9E2", fill = '#006BA2', size = 5, stroke = 1) +
      #geom_shadowtext(aes(x = 17, y = 30), label = "WC", size = 3, fontface = "bold", family = "Roboto", color = "white", bg.colour = "#F3E9E2", bg.r = 0.05) +
      geom_text(aes(label = round(cum_transfers, digits = 0)), size = 2.5, family = "Lato", color = "white") +
      coord_cartesian(clip = "off") +
      scale_x_continuous(name = NULL, labels = setNames(small_df$GW_Chip,small_df$Gameweek), breaks = small_df$Gameweek) +
      scale_y_continuous(breaks = seq(0,1000,5)) +
      labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
        title =  paste0("<img src='./Logo4.png' width='30'>", " Cumulative Transfers"),
        caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
        fill = "Points Difference After Hits",
        color = "Points Difference After Hits") +
      xlab("Gameweek") +
      ylab("No. Of Transfers") +
      #Adjust Theme to suit this plot
      theme(panel.grid.major.x = element_blank(),
            #panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
            #panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            panel.spacing = unit(0.5, "lines"),
            #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
            legend.position = "top",
            panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            plot.title = element_markdown(color = "black", size = 22, family = "Lato"),
            plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
            plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
            legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
            legend.title = element_text(color = "gray10", size = 12,  family = "Lato"),
            legend.key = element_blank(),
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(0,0,0,0),
            axis.text.x = element_text(color ="black", size = 10, family = "Lato"),
            axis.text.y = element_text(color ="black", size = 10, family = "Lato", vjust = -0.3, hjust = 1),
            #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
            axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
            axis.title.y = element_text(color = "black", size = 12,  family = "Lato"),
            axis.ticks.length.x = unit(0.1, "cm"),
            axis.ticks.x = element_line(color = "gray30", size = 1),
            axis.ticks.length.y = unit(0.1, "cm"),
            axis.ticks.y = element_line(color = "gray30", size = 1),
            axis.line.x = element_line(color = "gray30"),
            axis.line.y = element_line(color = "gray30"),
            strip.background = element_blank(),
            strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
            strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", face = 'bold', angle = 360, vjust = 0),
            legend.background = element_blank())
    
    
    p
    
    
  })
  
  output$download_transfer_cum <- downloadHandler(
    filename = function(){paste("FPL.Transfer_Cumulative.", input$team_id3,".png",sep='')},
    
    
    content = function(file){
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      # Fix: Check if chips exists and handle empty/NULL case
      if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
        Chips <- JSON_Output$chips %>%
          select(-time)
      } else {
        # Create empty data frame with expected structure if no chips
        Chips <- data.frame(
          event = integer(0),
          name = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      transfer_df <- table2_parse(Players_Gameweek,input$team_id3)
      
      tmp_vec <- seq(1, max(Players_Gameweek$round),1)
      
      tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, transfer_df$Gameweek), 
                           chip = NA) %>%
        left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name'))
      
      transfers <- bind_rows(transfer_df, tmp_df)
      
      calculation1 <- input$transfer_input
      
      Transfers2 <- transfers %>%
        {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit')) else mutate(., chip = chip)
          
        }
      
      
      small_df <- Transfers2 %>%
        mutate(cum_transfers =  cumsum(transfers),
               lag_transfer = lead(transfers),
               cum_transfers2 = cumsum(lead(transfers))) %>%
        mutate(new_text = str_replace(transfers_in, ",","\n")) %>%
        mutate(transfer_type = case_when(hits_taken == 0 ~ "Free Transfers",
                                         hits_taken == 4 ~ "-4 Hit",
                                         hits_taken == 8 ~ "-8 Hit",
                                         hits_taken == 12 ~ "-12 Hit",
                                         T ~ "Multiple Hits")) %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ",""))
      
      small_df2 <- small_df %>%
        filter(!is.na(chip))
      
      p <- ggplot(small_df, aes(x = Gameweek, y = cum_transfers)) +
        #geom_vline(data = small_df2, aes(xintercept = Gameweek), color = "lightgray", size = 9) +
        geom_rect(data = small_df2, aes(xmin = Gameweek - 0.4, xmax = Gameweek + 0.4, ymin = -Inf, ymax = Inf), fill = "lightgray", color = NA, show.legend = F) +
        geom_step(linewidth = 1, color = "#3288BD") +
        geom_point(shape = 21, color = "#F3E9E2", fill = "#3288BD", size = 5, stroke = 1) +
        #geom_shadowtext(aes(x = 17, y = 30), label = "WC", size = 3, fontface = "bold", family = "Roboto", color = "white", bg.colour = "#F3E9E2", bg.r = 0.05) +
        geom_text(aes(label = round(cum_transfers, digits = 0)), size = 2.5, family = "Lato", color = "white") +
        coord_cartesian(clip = "off") +
        scale_x_continuous(name = NULL, labels = setNames(small_df$GW_Chip,small_df$Gameweek), breaks = small_df$Gameweek) +
        scale_y_continuous(breaks = seq(0,1000,5)) +
        labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
          title =  paste0("<img src='./Logo4.png' width='30'>", " Cumulative Transfers"),
          caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
          fill = "Points Difference After Hits",
          color = "Points Difference After Hits") +
        xlab("Gameweek") +
        ylab("No. Of Transfers") +
        #Adjust Theme to suit this plot
        theme(panel.grid.major.x = element_blank(),
              #panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
              #panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.border = element_blank(),
              panel.spacing = unit(0.5, "lines"),
              #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
              legend.position = "top",
              panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.title = element_markdown(color = "black", size = 22, family = "Lato"),
              plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
              plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
              legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
              legend.title = element_text(color = "gray10", size = 12,  family = "Lato"),
              legend.key = element_blank(),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(0,0,0,0),
              axis.text.x = element_text(color ="black", size = 10, family = "Lato"),
              axis.text.y = element_text(color ="black", size = 10, family = "Lato", vjust = -0.3, hjust = 1),
              #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
              axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
              axis.title.y = element_text(color = "black", size = 12,  family = "Lato"),
              axis.ticks.length.x = unit(0.1, "cm"),
              axis.ticks.x = element_line(color = "gray30", size = 1),
              axis.ticks.length.y = unit(0.1, "cm"),
              axis.ticks.y = element_line(color = "gray30", size = 1),
              axis.line.x = element_line(color = "gray30"),
              axis.line.y = element_line(color = "gray30"),
              strip.background = element_blank(),
              strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
              strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", face = 'bold', angle = 360, vjust = 0),
              legend.background = element_blank())
      
      #p <- v11$plot
      
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
      
    })
  
  output$transfer_hits_plot <- renderPlot({
  
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    
    # Fix: Check if chips exists and handle empty/NULL case
    if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
      Chips <- JSON_Output$chips %>%
        select(-time)
    } else {
      # Create empty data frame with expected structure if no chips
      Chips <- data.frame(
        event = integer(0),
        name = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    transfer_df <- table2_parse(Players_Gameweek,input$team_id3)
    
    tmp_vec <- seq(1, max(Players_Gameweek$round),1)
    
    tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, transfer_df$Gameweek), 
                         chip = NA) %>%
      left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name'))
    
    transfers <- bind_rows(transfer_df, tmp_df)
    
    calculation1 <- input$transfer_input
    
    Transfers2 <- transfers %>%
      {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit')) else mutate(., chip = chip)
        
      }
    
    
    small_df <- Transfers2 %>%
      mutate(cum_transfers =  cumsum(transfers),
             lag_transfer = lead(transfers),
             cum_transfers2 = cumsum(lead(transfers))) %>%
      mutate(new_text = str_replace(transfers_in, ",","\n")) %>%
      mutate(transfer_type = case_when(hits_taken == 0 ~ "Free Transfers",
                                       hits_taken == 4 ~ "-4 Hit",
                                       hits_taken == 8 ~ "-8 Hit",
                                       hits_taken == 12 ~ "-12 Hit",
                                       hits_taken > 12 ~ "Multiple Hits")) %>%
      mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                    chip == 'freehit' ~ 'FH',
                                    chip == 'bboost' ~ 'BB',
                                    chip == '3xc' ~ 'TC',
                                    chip == 'manager' ~ 'AM',
                                    T ~'')) %>%
      mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
      mutate(GW_Chip = str_replace(GW_Chip, " ",""))
    
    small_df2 <- small_df %>%
      filter(!is.na(chip))
    
    if(calculation1 == "No") {
      
      scale1 <- scale_y_continuous(breaks = seq(0,50,1))
    }
    else {
      
      scale1 <- scale_y_continuous(breaks = seq(0,50,5))
      
    }
    
    p <- ggplot(small_df, aes(x = Gameweek, y = transfers, color = transfer_type, fill = transfer_type)) +
      #geom_vline(data = small_df2, aes(xintercept = Gameweek), color = "lightgray", size = 9) +
      geom_rect(data = small_df2, aes(xmin = Gameweek - 0.4, xmax = Gameweek + 0.4, ymin = -Inf, ymax = Inf), fill = "lightgray", color = NA, show.legend = F) +
      geom_segment(aes(x = Gameweek, xend = Gameweek, y = 0, yend = transfers), size = 3) +
      geom_hline(yintercept = 0, color = "gray60", size = 1.25) +
      geom_point(shape = 21, color = "#F3E9E2", size = 6, stroke = 1.5) +
      geom_text(aes(label = round(transfers, digits = 0)), size = 2.5, fontface = "bold", family = "Roboto", color = "white") +
      #geom_shadowtext(aes(x = 17, y = 11.65), label = "WC", size = 3, fontface = "bold", family = "Roboto", color = "white", bg.colour = "#F3E9E2", bg.r = 0.05) +
      #geom_shadowtext(data = small_df %>% filter(transfers == 2) ,aes(label = new_text), size = 3, fontface = "bold", family = "Roboto", color = "black", nudge_y = 1, bg.colour = "#F3E9E2", bg.r = 0.05) +
      #c("#66C2A5","#3288BD","#5E4FA2")
      scale_color_manual(breaks = c("Free Transfers","-4 Hit","-8 Hit","-12 Hit","Multiple Hits"),
                         #values = c('#66C2A5','#F2055C','red','#5E1219',"black"),
                         values = c('#379A8B','#3EBCD2','#006BA2','#EBB434','#DB444B')) +
      
      #c('#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C')
      scale_fill_manual(breaks = c("Free Transfers","-4 Hit","-8 Hit","-12 Hit","Multiple Hits"),
                        values = c('#379A8B','#3EBCD2','#006BA2','#EBB434','#DB444B')) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(name = NULL, labels = setNames(small_df$GW_Chip,small_df$Gameweek), breaks = small_df$Gameweek) +
      scale1 +
      #scale_y_continuous(breaks = seq(0,50,2)) +
      labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
        title =  paste0("<img src='./Logo4.png' width='30'>", " Transfers Per Gameweek"),
        caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
        fill = "Points Difference After Hits",
        color = "Points Difference After Hits") +
      xlab("Gameweek") +
      ylab("No. Of Transfers") +
      #Adjust Theme to suit this plot
      theme(panel.grid.major.x = element_blank(),
            #panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
            #panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            panel.spacing = unit(0.5, "lines"),
            #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
            legend.position = "top",
            panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            plot.title = element_markdown(color = "black", size = 22, family = "Lato"),
            plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
            plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
            legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
            legend.title = element_text(color = "gray10", size = 12,  family = "Lato"),
            legend.key = element_blank(),
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(0,0,0,0),
            axis.text.x = element_text(color ="black", size = 10, family = "Lato"),
            axis.text.y = element_text(color ="black", size = 10, family = "Lato", vjust = -0.3, hjust = 1),
            #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
            axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
            axis.title.y = element_text(color = "black", size = 12,  family = "Lato"),
            axis.ticks.length.x = unit(0.1, "cm"),
            axis.ticks.x = element_line(color = "gray30", size = 1),
            axis.ticks.length.y = unit(0.1, "cm"),
            axis.ticks.y = element_line(color = "gray30", size = 1),
            axis.line.x = element_line(color = "gray30"),
            axis.line.y = element_line(color = "gray30"),
            strip.background = element_blank(),
            strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
            strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", face = 'bold', angle = 360, vjust = 0),
            legend.background = element_blank())
    
    
    p
    
    
    
  })
  
  output$download_transfer_hits <- downloadHandler(
    filename = function(){paste("FPL.Transfer_Hits.", input$team_id3,".png",sep='')},
    
    
    content = function(file){
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$team_id3, '/history/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      # Fix: Check if chips exists and handle empty/NULL case
      if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
        Chips <- JSON_Output$chips %>%
          select(-time)
      } else {
        # Create empty data frame with expected structure if no chips
        Chips <- data.frame(
          event = integer(0),
          name = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      transfer_df <- table2_parse(Players_Gameweek,input$team_id3)
      
      tmp_vec <- seq(1, max(Players_Gameweek$round),1)
      
      tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, transfer_df$Gameweek), 
                           chip = NA) %>%
        left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name'))
      
      transfers <- bind_rows(transfer_df, tmp_df)
      
      calculation1 <- input$transfer_input
      
      Transfers2 <- transfers %>%
        {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit')) else mutate(., chip = chip)
          
        }
      
      
      small_df <- Transfers2 %>%
        mutate(cum_transfers =  cumsum(transfers),
               lag_transfer = lead(transfers),
               cum_transfers2 = cumsum(lead(transfers))) %>%
        mutate(new_text = str_replace(transfers_in, ",","\n")) %>%
        mutate(transfer_type = case_when(hits_taken == 0 ~ "Free Transfers",
                                         hits_taken == 4 ~ "-4 Hit",
                                         hits_taken == 8 ~ "-8 Hit",
                                         hits_taken == 12 ~ "-12 Hit",
                                         hits_taken > 12 ~ "Multiple Hits")) %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ",""))
      
      small_df2 <- small_df %>%
        filter(!is.na(chip))
      
      if(calculation1 == "No") {
        
        scale1 <- scale_y_continuous(breaks = seq(0,50,1))
      }
      else {
        
        scale1 <- scale_y_continuous(breaks = seq(0,50,5))
        
      }
      
      p <- ggplot(small_df, aes(x = Gameweek, y = transfers, color = transfer_type, fill = transfer_type)) +
        #geom_vline(data = small_df2, aes(xintercept = Gameweek), color = "lightgray", size = 9) +
        geom_rect(data = small_df2, aes(xmin = Gameweek - 0.4, xmax = Gameweek + 0.4, ymin = -Inf, ymax = Inf), fill = "lightgray", color = NA, show.legend = F) +
        geom_segment(aes(x = Gameweek, xend = Gameweek, y = 0, yend = transfers), size = 3) +
        geom_hline(yintercept = 0, color = "gray60", size = 1.25) +
        geom_point(shape = 21, color = "#F3E9E2", size = 6, stroke = 1.5) +
        geom_text(aes(label = round(transfers, digits = 0)), size = 2.5, fontface = "bold", family = "Roboto", color = "white") +
        #geom_shadowtext(aes(x = 17, y = 11.65), label = "WC", size = 3, fontface = "bold", family = "Roboto", color = "white", bg.colour = "#F3E9E2", bg.r = 0.05) +
        #geom_shadowtext(data = small_df %>% filter(transfers == 2) ,aes(label = new_text), size = 3, fontface = "bold", family = "Roboto", color = "black", nudge_y = 1, bg.colour = "#F3E9E2", bg.r = 0.05) +
        #c("#66C2A5","#3288BD","#5E4FA2")
        scale_color_manual(breaks = c("Free Transfers","-4 Hit","-8 Hit","-12 Hit","Multiple Hits"),
                           #values = c('#66C2A5','#F2055C','red','#5E1219',"black"),
                           values = c('#379A8B','#3EBCD2','#006BA2','#EBB434','#DB444B')) +
        
        #c('#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C')
        scale_fill_manual(breaks = c("Free Transfers","-4 Hit","-8 Hit","-12 Hit","Multiple Hits"),
                          values = c('#379A8B','#3EBCD2','#006BA2','#EBB434','#DB444B')) +
        coord_cartesian(clip = "off") +
        scale_x_continuous(name = NULL, labels = setNames(small_df$GW_Chip,small_df$Gameweek), breaks = small_df$Gameweek) +
        scale1 +
        #scale_y_continuous(breaks = seq(0,50,2)) +
        labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
          title =  paste0("<img src='./Logo4.png' width='30'>", " Transfers Per Gameweek"),
          caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
          fill = "Points Difference After Hits",
          color = "Points Difference After Hits") +
        xlab("Gameweek") +
        ylab("No. Of Transfers") +
        #Adjust Theme to suit this plot
        theme(panel.grid.major.x = element_blank(),
              #panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
              #panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.border = element_blank(),
              panel.spacing = unit(0.5, "lines"),
              #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
              legend.position = "top",
              panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.title = element_markdown(color = "black", size = 22, family = "Lato"),
              plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
              plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
              legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
              legend.title = element_text(color = "gray10", size = 12,  family = "Lato"),
              legend.key = element_blank(),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(0,0,0,0),
              axis.text.x = element_text(color ="black", size = 10, family = "Lato"),
              axis.text.y = element_text(color ="black", size = 10, family = "Lato", vjust = -0.3, hjust = 1),
              #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
              axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
              axis.title.y = element_text(color = "black", size = 12,  family = "Lato"),
              axis.ticks.length.x = unit(0.1, "cm"),
              axis.ticks.x = element_line(color = "gray30", size = 1),
              axis.ticks.length.y = unit(0.1, "cm"),
              axis.ticks.y = element_line(color = "gray30", size = 1),
              axis.line.x = element_line(color = "gray30"),
              axis.line.y = element_line(color = "gray30"),
              strip.background = element_blank(),
              strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
              strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", face = 'bold', angle = 360, vjust = 0),
              legend.background = element_blank())
      
      
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
      
    })
  
  output$team_xg_trend <- renderPlot({
    
    #team_matrix2 <- c(input$team_matrix2a, input$team_matrix2b, input$team_matrix2c)
    
    fixtures_df <- Players_History %>%
      mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                    was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
      mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                              was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
      group_by(short_team_name, round, opposition, team) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      group_by(short_team_name, round) %>%
      filter(count >= 11) %>%
      ungroup() %>%
      unite("merged",opposition:team, remove = F)
    
    Minutes_Data <- Players_History %>%
      mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                    was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
      mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                              was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
      unite("merged",opposition:team, remove = F) %>%
      filter(red_cards == 0) %>%
      filter(minutes == 90) %>%
      filter(merged %in% fixtures_df$merged) %>%
      group_by(short_team_name, opp_short_team_name, round, merged) %>%
      reframe(xg_conceded = max(expected_goals_conceded),
              opposition = unique(opposition),
              team = unique(team),
              opp_strength = unique(opp_strength),
              goals_conceded = max(goals_conceded)) %>% 
      unite("merged2",team:opposition, remove = F) %>%
      arrange(desc(merged))
    
    
    tmp <- Minutes_Data %>% select(-short_team_name, -opp_short_team_name, -team, -merged2) %>% rename(opp_team = opposition, mystery = merged, xg = xg_conceded, strength = opp_strength, goals = goals_conceded) #%>%
    #arrange(desc(mystery))
    
    #test <- bind_cols(Minutes_Data,tmp)
    
    final_df <- left_join(Minutes_Data,tmp, by = c('merged2' = 'mystery', 'round')) %>%
      filter(team != opposition) %>%
      select(-merged,-merged2) %>%
      arrange(round, short_team_name) %>%
      group_by(short_team_name) %>%
      mutate(smooth_xg = rollapplyr(xg, input$smooth_slider, mean, partial = TRUE),
             smooth_opp_xg = rollapplyr(xg_conceded, input$smooth_slider, mean, partial = TRUE)) %>%
      mutate(opp_crest = case_when(short_team_name == "ARS" ~ "arsenal",
                                   short_team_name == "AVL" ~ "aston villa",
                                   short_team_name == "BOU"~ "bournemouth",
                                   short_team_name == "BRE" ~ "brentford",
                                   short_team_name == "BHA" ~ "brighton",
                                   short_team_name == "BUR"  ~ "burnley",
                                   short_team_name == "CHE"  ~ "chelsea",
                                   short_team_name == "CRY" ~ "crystal palace",
                                   short_team_name == "EVE" ~ "everton",
                                   short_team_name == "FUL" ~ "fulham",
                                   short_team_name == "LIV" ~ "liverpool",
                                   short_team_name == "LUT" ~ "luton town",
                                   short_team_name == "MCI" ~ "man city",
                                   short_team_name == "MUN" ~ "man utd",
                                   short_team_name == "NEW" ~ "newcastle utd",
                                   short_team_name == "NFO" ~ "forest",
                                   short_team_name == "SHU"  ~ "sheffield utd",
                                   short_team_name == "TOT" ~ "tottenham",
                                   short_team_name == "WHU" ~ "west ham",
                                   short_team_name == "WOL" ~ "wolves" )) %>%
      ungroup() %>%
      mutate(opp_strength = as.factor(opp_strength)) %>%
      #filter(short_team_name %in% team_matrix2) %>%
      filter(short_team_name %in% input$team_matrix2) %>%
      arrange(round) %>%
      filter(round >= input$gweeks[1] & round <= input$gweeks[2]) %>%
      mutate(match = row_number()) %>%
      #separate_wider_delim(opposition, "-", names = c("gw", "fixture")) %>%
      mutate(fixture = paste0("GW", round,"\n",opp_short_team_name)) %>%
      mutate(Size = case_when(smooth_xg > smooth_opp_xg ~ "Bigger",
                              T ~ "Smaller")) %>%
      pivot_longer(cols = c('smooth_xg','smooth_opp_xg'))
    
    small_df <- final_df %>%
      filter(round == max(round))
    
    
    gw_length <- input$gweeks[2] - input$gweeks[1]
    
    if(length(unique(final_df$short_team_name)) > 1) {
      
      axis_text1 <- element_blank()
      axis_ticks1 <- element_blank()
      nudge_factor <- 0.2
      
    } else {
      
      axis_text1 <- element_text(color ="black", size = 8, family = "Lato")
      axis_ticks1 <- element_line(color = "gray30", size = 1)
      nudge_factor <- 0.8
    }
      
    
    
    p <- ggplot(final_df, aes(x = match, y = value)) +
      geom_hline(yintercept = 0, color = "gray60") +
      geom_borderline(aes(group = name, color = name), size = 1, bordercolour = "#F3E9E2", borderwidth = 0.5, show.legend = F) +
      geom_point(aes(fill = name), shape = 21, stroke = 0.75, size = 2, color = "#F3E9E2") +
      #geom_shadowtext(data = small_df %>% filter(Size == "Bigger"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = nudge_factor, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
      #geom_shadowtext(data = small_df %>% filter(Size == "Smaller"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = -1*nudge_factor, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +      scale_x_continuous(name = NULL, labels = setNames(final_df$fixture,final_df$match), breaks = final_df$match) +
      geom_shadowtext(data = small_df %>% filter(Size == "Bigger") %>% filter(name == "smooth_xg"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = 0.3, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
      geom_shadowtext(data = small_df %>% filter(Size == "Bigger") %>% filter(name == "smooth_opp_xg"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = -0.3, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
      geom_shadowtext(data = small_df %>% filter(Size == "Smaller") %>% filter(name == "smooth_opp_xg"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = 0.3, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
      geom_shadowtext(data = small_df %>% filter(Size == "Smaller") %>% filter(name == "smooth_xg"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = -0.3, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
      # "#3288BD","#5E4FA2"
      #c('#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C')
      scale_color_manual(values = c('#DB444B','#006BA2'),
                         labels = c("Opposition XG", "XG")) +
      scale_fill_manual(values = c('#DB444B','#006BA2'),
                        labels = c("Opposition XG", "XG")) +
      scale_y_continuous(limits = c(0, max(final_df$value) + 0.7)) +
      facet_wrap(~short_team_name) +
      labs(#title =  paste0("<img src='./Logo4.png' width='30'>"," Gameweek Ranks (Current Rank: ", data %>% pull(overall_rank) %>% tail(1),")"),
        title = paste0("<img src='./Logo4.png' width='30'> XG Trend Graph"),
        caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
        #subtitle = paste0("Sample includes players with at least: ", starts_1, " Starts"),
      ) +
      xlab("Match Number") +
      ylab("Expected Goals (XG)") +
      guides(fill = guide_legend(override.aes = list(size = 8))) +
      #Adjust Theme to suit this plot
      theme(panel.grid.major.x = element_blank(),
            #panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
            #panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            panel.spacing = unit(0.5, "lines"),
            #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
            legend.position = "top",
            panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
            plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
            plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
            legend.text = element_text(color = "black", size = 12, family = "Lato", hjust = 0),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(0,0,0,0),
            axis.text.x = axis_text1,
            axis.text.y = element_text(color ="black", size = 10, family = "Lato", vjust = -0.3, hjust = 1),
            #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
            axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
            axis.title.y = element_text(color = "black", size = 12,  family = "Lato"),
            axis.ticks.length.x = unit(0.1, "cm"),
            axis.ticks.x = axis_ticks1,
            axis.ticks.length.y = unit(0.1, "cm"),
            axis.ticks.y = element_line(color = "gray30", size = 1),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
            strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
            legend.background = element_blank())
    
    p
    
    
    
  })
  
  output$download_xgtrend <- downloadHandler(
    filename = function(){paste("FPL.XG_Trend.png",sep='')},
    
    
    content = function(file){
      
      fixtures_df <- Players_History %>%
        mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                      was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
        mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                                was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
        group_by(short_team_name, round, opposition, team) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(short_team_name, round) %>%
        filter(count >= 11) %>%
        ungroup() %>%
        unite("merged",opposition:team, remove = F)
      
      Minutes_Data <- Players_History %>%
        mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                      was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
        mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                                was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
        unite("merged",opposition:team, remove = F) %>%
        filter(red_cards == 0) %>%
        filter(minutes == 90) %>%
        filter(merged %in% fixtures_df$merged) %>%
        group_by(short_team_name, opp_short_team_name, round, merged) %>%
        reframe(xg_conceded = max(expected_goals_conceded),
                opposition = unique(opposition),
                team = unique(team),
                opp_strength = unique(opp_strength),
                goals_conceded = max(goals_conceded)) %>% 
        unite("merged2",team:opposition, remove = F) %>%
        arrange(desc(merged))
      
      
      tmp <- Minutes_Data %>% select(-short_team_name, -opp_short_team_name, -team, -merged2) %>% rename(opp_team = opposition, mystery = merged, xg = xg_conceded, strength = opp_strength, goals = goals_conceded) #%>%
      #arrange(desc(mystery))
      
      #test <- bind_cols(Minutes_Data,tmp)
      
      final_df <- left_join(Minutes_Data,tmp, by = c('merged2' = 'mystery', 'round')) %>%
        filter(team != opposition) %>%
        select(-merged,-merged2) %>%
        arrange(round, short_team_name) %>%
        group_by(short_team_name) %>%
        mutate(smooth_xg = rollapplyr(xg, input$smooth_slider, mean, partial = TRUE),
               smooth_opp_xg = rollapplyr(xg_conceded, input$smooth_slider, mean, partial = TRUE)) %>%
        mutate(opp_crest = case_when(short_team_name == "ARS" ~ "arsenal",
                                     short_team_name == "AVL" ~ "aston villa",
                                     short_team_name == "BOU"~ "bournemouth",
                                     short_team_name == "BRE" ~ "brentford",
                                     short_team_name == "BHA" ~ "brighton",
                                     short_team_name == "BUR"  ~ "burnley",
                                     short_team_name == "CHE"  ~ "chelsea",
                                     short_team_name == "CRY" ~ "crystal palace",
                                     short_team_name == "EVE" ~ "everton",
                                     short_team_name == "FUL" ~ "fulham",
                                     short_team_name == "LIV" ~ "liverpool",
                                     short_team_name == "LUT" ~ "luton town",
                                     short_team_name == "MCI" ~ "man city",
                                     short_team_name == "MUN" ~ "man utd",
                                     short_team_name == "NEW" ~ "newcastle utd",
                                     short_team_name == "NFO" ~ "forest",
                                     short_team_name == "SHU"  ~ "sheffield utd",
                                     short_team_name == "TOT" ~ "tottenham",
                                     short_team_name == "WHU" ~ "west ham",
                                     short_team_name == "WOL" ~ "wolves" )) %>%
        ungroup() %>%
        mutate(opp_strength = as.factor(opp_strength)) %>%
        #filter(short_team_name %in% team_matrix2) %>%
        filter(short_team_name %in% input$team_matrix2) %>%
        arrange(round) %>%
        filter(round >= input$gweeks[1] & round <= input$gweeks[2]) %>%
        mutate(match = row_number()) %>%
        #separate_wider_delim(opposition, "-", names = c("gw", "fixture")) %>%
        mutate(fixture = paste0("GW", round,"\n",opp_short_team_name)) %>%
        mutate(Size = case_when(smooth_xg > smooth_opp_xg ~ "Bigger",
                                T ~ "Smaller")) %>%
        pivot_longer(cols = c('smooth_xg','smooth_opp_xg'))
      
      small_df <- final_df %>%
        filter(round == max(round))
      
      
      gw_length <- input$gweeks[2] - input$gweeks[1]
      
      if(length(unique(final_df$short_team_name)) > 1) {
        
        axis_text1 <- element_blank()
        axis_ticks1 <- element_blank()
        nudge_factor <- 0.2
        
      } else {
        
        axis_text1 <- element_text(color ="black", size = 8, family = "Lato")
        axis_ticks1 <- element_line(color = "gray30", size = 1)
        nudge_factor <- 0.8
      }
      
      
      
      p <- ggplot(final_df, aes(x = match, y = value)) +
        geom_hline(yintercept = 0, color = "gray60") +
        geom_borderline(aes(group = name, color = name), size = 1, bordercolour = "#F3E9E2", borderwidth = 0.5, show.legend = F) +
        geom_point(aes(fill = name), shape = 21, stroke = 0.75, size = 2, color = "#F3E9E2") +
        #geom_shadowtext(data = small_df %>% filter(Size == "Bigger"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = nudge_factor, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
        #geom_shadowtext(data = small_df %>% filter(Size == "Smaller"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = -1*nudge_factor, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +      scale_x_continuous(name = NULL, labels = setNames(final_df$fixture,final_df$match), breaks = final_df$match) +
        geom_shadowtext(data = small_df %>% filter(Size == "Bigger") %>% filter(name == "smooth_xg"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = 0.3, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
        geom_shadowtext(data = small_df %>% filter(Size == "Bigger") %>% filter(name == "smooth_opp_xg"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = -0.3, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
        geom_shadowtext(data = small_df %>% filter(Size == "Smaller") %>% filter(name == "smooth_opp_xg"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = 0.3, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
        geom_shadowtext(data = small_df %>% filter(Size == "Smaller") %>% filter(name == "smooth_xg"), aes(label = round(value, digits = 2), color = name), size = 5, family = "Lato", nudge_y = -0.3, bg.colour = "#F3E9E2", bg.r = 0.05, show.legend = F) +
        # "#3288BD","#5E4FA2"
        scale_color_manual(values = c('#DB444B','#006BA2'),
                           labels = c("Opposition XG", "XG")) +
        scale_fill_manual(values = c('#DB444B','#006BA2'),
                          labels = c("Opposition XG", "XG")) +
        scale_y_continuous(limits = c(0, max(final_df$value) + 0.7)) +
        facet_wrap(~short_team_name) +
        guides(fill = guide_legend(override.aes = list(size = 8))) +
        labs(#title =  paste0("<img src='./Logo4.png' width='30'>"," Gameweek Ranks (Current Rank: ", data %>% pull(overall_rank) %>% tail(1),")"),
          title = paste0("<img src='./Logo4.png' width='30'> XG Trend Graph"),
          caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
          #subtitle = paste0("Sample includes players with at least: ", starts_1, " Starts"),
        ) +
        xlab("Match Number") +
        ylab("Expected Goals (XG)") +
        #Adjust Theme to suit this plot
        theme(panel.grid.major.x = element_blank(),
              #panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
              #panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.border = element_blank(),
              panel.spacing = unit(0.5, "lines"),
              #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
              legend.position = "top",
              panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
              plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
              plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
              legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
              legend.title = element_blank(),
              legend.key = element_blank(),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(0,0,0,0),
              axis.text.x = axis_text1,
              axis.text.y = element_text(color ="black", size = 10, family = "Lato", vjust = -0.3, hjust = 1),
              #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
              axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
              axis.title.y = element_text(color = "black", size = 12,  family = "Lato"),
              axis.ticks.length.x = unit(0.1, "cm"),
              axis.ticks.x = axis_ticks1,
              axis.ticks.length.y = unit(0.1, "cm"),
              axis.ticks.y = element_line(color = "gray30", size = 1),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              strip.background = element_blank(),
              strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
              strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
              legend.background = element_blank())
      
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
      
    })
  
  
  output$team_bumpplot <- renderPlot({
    
    Team_Summary <- league_bump(selectize_vector = Selectize_Team_ID(), 
                                league_gweeks1 = input$league_gweeks[1], 
                                league_gweeks2 = input$league_gweeks[2],
                                bump_breaks = input$bump_breaks)
    
    highlighted_managers <- input$team_highlight
      
      df <- Team_Summary %>%
        group_by(event) %>%
        mutate(manager_rank = dense_rank(overall_rank)) %>%
        ungroup() %>%
        mutate(coloured_manager = case_when(entry_name %in% highlighted_managers ~ entry_name,
                                            T ~ "Other Managers")) 
      
      #palette_1 <- c("lightgray","#66C2A5","#3288BD","#5E4FA2","#D12E7C","#F47D20","#DFCA94")
      palette_1 <- c('lightgray','#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C')
      
      all_managers <- c("Other Managers", as.list(highlighted_managers))
      
      
      p9 <- ggplot(df, aes(x = event, y = manager_rank, group = entry_name, color = coloured_manager, fill = coloured_manager)) +
        geom_bump(data = df %>% filter(coloured_manager ==  "Other Managers"),  size = 1.5, show.legend = F) +
        geom_bump(data = df %>% filter(coloured_manager !=  "Other Managers"), size = 1.5, show.legend = F) +
        geom_point(data = df %>% filter(coloured_manager ==  "Other Managers"), shape = 21, size = 2.5, stroke = 1, color = "#F3E9E2", show.legend = F) +
        geom_point(data = df %>% filter(coloured_manager !=  "Other Managers"), shape = 21, size = 2.5, stroke = 1, color = "#F3E9E2", show.legend = F) +        # Left Text
        geom_text(data = df %>% filter(event == min(event)) %>% filter(manager_rank %in% c(1)), aes(label = paste0(manager_rank, 'st')), color = 'black', family = "Lato", size = 3.5, nudge_x = -0.6) +
        geom_text(data = df %>% filter(event == min(event)) %>% filter(manager_rank %in% c(2)), aes(label = paste0(manager_rank, 'nd')), color = 'black', family = "Lato", size = 3.5, nudge_x = -0.6) +
        geom_text(data = df %>% filter(event == min(event)) %>% filter(manager_rank %in% c(3)), aes(label = paste0(manager_rank, 'rd')), color = 'black', family = "Lato", size = 3.5, nudge_x = -0.6) +
        geom_text(data = df %>% filter(event == min(event)) %>% filter(manager_rank %in% c(4,5,6,7,8,9,10)), aes(label = paste0(manager_rank, 'th')), color = 'black', family = "Lato", size = 3.5, nudge_x = -0.6) +
        
        geom_text(data = df %>% filter(event== max(event)), aes(label = entry_name), family = "Lato", fontface = "bold", size = 3.5, hjust = 0, nudge_x = 0.5, show.legend = F) +
        coord_cartesian(clip = 'off') +
        scale_x_continuous(limits = c(input$league_gweeks[1] - 0.8,input$league_gweeks[2] + 3),
                           breaks = seq(from = input$league_gweeks[1], to = input$league_gweeks[2], by = input$bump_breaks)) +
        scale_y_reverse() +
        scale_fill_manual(breaks = all_managers,
                          values = palette_1[1:length(all_managers)]) +
        scale_color_manual(breaks = all_managers,
                           values = palette_1[1:length(all_managers)]) +
        theme(legend.position = "none") +
        ylab("Relative Rank") +
        xlab("Gameweeks") +
        labs(title = paste0("<img src='./Logo4.png' width='30'> ", input$league1, " - Relative Rankings"),
             #subtitle = "Relative Ranking History of Top FPL Managers",
             caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
        #Adjust Theme to suit this plot
        theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
          panel.grid.major.x = element_blank(),
          #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
          plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
          legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
          legend.title = element_text(color = "gray20", size = 14,  family = "Lato", face = 'bold'),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
          axis.text.x = element_text(color ="black", size = 10, family = "Lato"),
          axis.text.y = element_blank(),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.x = unit(0.2, "cm"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
          #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
          legend.background = element_blank())
      
      p9
    
    
    
  })
  
  output$download_bump1 <- downloadHandler(
    filename = function(){paste("FPL.", input$league1 ,".png", sep='')},
    
    
    content = function(file){
      
      Team_Summary <- league_bump(selectize_vector = Selectize_Team_ID(), 
                                  league_gweeks1 = input$league_gweeks[1], 
                                  league_gweeks2 = input$league_gweeks[2])
      
      highlighted_managers <- input$team_highlight
      
      df <- Team_Summary %>%
        group_by(event) %>%
        mutate(manager_rank = dense_rank(overall_rank)) %>%
        ungroup() %>%
        mutate(coloured_manager = case_when(entry_name %in% highlighted_managers ~ entry_name,
                                            T ~ "Other Managers")) 
      
      #palette_1 <- c("lightgray","#66C2A5","#3288BD","#5E4FA2","#D12E7C","#F47D20","#DFCA94")
      palette_1 <- c('lightgray','#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C')
      
      all_managers <- c("Other Managers", as.list(highlighted_managers))
      
      
      p9 <- ggplot(df, aes(x = event, y = manager_rank, group = entry_name, color = coloured_manager, fill = coloured_manager)) +
        geom_bump(data = df %>% filter(coloured_manager ==  "Other Managers"),  size = 1.5, show.legend = F) +
        geom_bump(data = df %>% filter(coloured_manager !=  "Other Managers"), size = 1.5, show.legend = F) +
        geom_point(data = df %>% filter(coloured_manager ==  "Other Managers"), shape = 21, size = 2.5, stroke = 1, color = "#F3E9E2", show.legend = F) +
        geom_point(data = df %>% filter(coloured_manager !=  "Other Managers"), shape = 21, size = 2.5, stroke = 1, color = "#F3E9E2", show.legend = F) +
        # Left Text
        geom_text(data = df %>% filter(event == min(event)) %>% filter(manager_rank %in% c(1)), aes(label = paste0(manager_rank, 'st')), color = 'black', family = "Lato", size = 3.5, nudge_x = -0.6) +
        geom_text(data = df %>% filter(event == min(event)) %>% filter(manager_rank %in% c(2)), aes(label = paste0(manager_rank, 'nd')), color = 'black', family = "Lato", size = 3.5, nudge_x = -0.6) +
        geom_text(data = df %>% filter(event == min(event)) %>% filter(manager_rank %in% c(3)), aes(label = paste0(manager_rank, 'rd')), color = 'black', family = "Lato", size = 3.5, nudge_x = -0.6) +
        geom_text(data = df %>% filter(event == min(event)) %>% filter(manager_rank %in% c(4,5,6,7,8,9,10)), aes(label = paste0(manager_rank, 'th')), color = 'black', family = "Lato", size = 3.5, nudge_x = -0.6) +
        
        geom_text(data = df %>% filter(event== max(event)), aes(label = entry_name), family = "Lato", fontface = "bold", size = 3.5, hjust = 0, nudge_x = 0.3, show.legend = F) +
        coord_cartesian(clip = 'off') +
        scale_x_continuous(limits = c(input$league_gweeks[1] - 0.8,input$league_gweeks[2] + 3),
                           breaks = seq(from = input$league_gweeks[1], to = input$league_gweeks[2], by = input$bump_breaks)) +
        scale_y_reverse() +
        scale_fill_manual(breaks = all_managers,
                          values = palette_1[1:length(all_managers)]) +
        scale_color_manual(breaks = all_managers,
                           values = palette_1[1:length(all_managers)]) +
        theme(legend.position = "none") +
        ylab("Relative Rank") +
        xlab("Gameweeks") +
        labs(title = paste0("<img src='./Logo4.png' width='30'> ", input$league1 , " - Relative Rankings"),
             #subtitle = "Relative Ranking History of Top FPL Managers",
             caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
        #Adjust Theme to suit this plot
        theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
          panel.grid.major.x = element_blank(),
          #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
          plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
          legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
          legend.title = element_text(color = "gray20", size = 14,  family = "Lato", face = 'bold'),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
          axis.text.x = element_text(color ="black", size = 10, family = "Lato"),
          axis.text.y = element_blank(),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.x = unit(0.2, "cm"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
          #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
          legend.background = element_blank())

      
      ggsave(file, plot = p9, dpi = 600, height = 6, width = 10)
      
      
    })
  
  output$ownership_plot <- renderPlot({
    
    
    Team_Selection <- c()
    Team_Summary <- c()
    
    #for (ID in test_vector) {
    for (ID in Selectize_Team_Own_ID()) {
      #print(ID)
      #print(class(ID))
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      team <- JSON_Output$name
      
      # This shows your gameweek history
      #for (GW in seq(from = input$league_gweeks[1], to = input$league_gweeks[2], by = input$bump_breaks)) {
      for (GW in seq(from = input$own_gweeks[1], to = input$own_gweeks[2], by = input$bump_breaks)) {
        
        #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/event/',GW,'/picks/'))
        
        
        jsonRespText<-content(FPL_Team, as="text") 
        
        JSON_Output <- fromJSON(jsonRespText)
        Entry_History <- JSON_Output$entry_history %>%
          unlist() %>%
          t() %>%
          as.data.frame() %>%
          mutate(#player_name = player_name,
            entry_name = team)
        
        Team_1 <- JSON_Output$picks %>%
          mutate(round = GW,
                 entry_name = team)
        
        #Sub_1 <- JSON_Output$automatic_subs 
        
        Team_Summary <- bind_rows(Team_Summary, Entry_History)
        
        Team_Selection <- bind_rows(Team_Selection, Team_1)
        
        #Subs <- bind_rows(Subs, Sub_1)
        
        
      }
      
    }
    
    
    df <- Team_Summary %>%
      group_by(event) %>%
      mutate(manager_rank = dense_rank(overall_rank)) %>%
      ungroup() %>%
      filter(event == max(event)) %>%
      arrange(manager_rank,entry_name)
    
    
    df2 <- Team_Selection %>%
      left_join(Players_Gameweek, by = c("element" = "id", "round")) %>%
      #filter(name_club == "Haaland (MCI)") %>%
      mutate(input_player = input$player_ownership) %>%
      separate_wider_delim(input_player, "(", names = c("selected_player", "selected_team"), cols_remove = F) %>%
      mutate(selected_team = str_sub(selected_team, end = -2)) %>%
      mutate(player_reg = paste0("^", selected_player, "\\(", selected_team,"\\)$")) %>%
      left_join(df %>% select(entry_name, manager_rank), by = c("entry_name"))
    
    test_string <- df2 %>% pull(player_reg) %>% head(1)
    
    #df2$entry_name <- factor(df2$entry_name, levels = df$entry_name)
    
    small_df <- df2 %>%
      group_by(round, entry_name, manager_rank) %>%
      summarise(yes_in_group = any(str_detect(name_club, regex(test_string))),
                selected_player = input$player_ownership) %>%
      ungroup() %>%
      separate_wider_delim(selected_player, "(", names = c("player", "Team"), cols_remove = F) %>%
      mutate(Team = str_sub(Team, end = -2)) %>%
      mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                     Team == "AVL" ~ "#670E36",
                                     Team == "BOU" ~ "#B50E12",
                                     Team == "BRE" ~ "#E30613",
                                     Team == "BHA" ~ "#0057B8",
                                     Team == "BUR" ~ "#6C1D45",
                                     Team == "CHE" ~ "#034694",
                                     Team == "CRY" ~ "#1B458F",
                                     Team == "EVE" ~ "#003399",
                                     Team == "FUL" ~ "#000000",
                                     Team == "LIV" ~ "#C8102E",
                                     Team == "LUT" ~ "#F78F1E",
                                     Team == "MCI" ~ "#6CABDD",
                                     Team == "MUN" ~ "#DA291C",
                                     Team == "NEW" ~ "#241F20",
                                     Team == "NFO" ~ "#DD0000",
                                     Team == "SHU" ~ "#EE2737",
                                     Team == "TOT" ~ "#132257",
                                     Team == "WHU" ~ "#7A263A",
                                     Team == "WOL" ~ "#FDB913",
                                     Team == "IPS" ~ "#3a64a3",
                                     Team == "LEI" ~ "#003090",
                                     Team == "SOU" ~ "#D71920")) %>%
      mutate(team_colour = case_when(yes_in_group == F ~ "lightgray",
                                     T ~ team_colour)) %>%
      mutate(x_prev = round - 1)
    
    p9 <- ggplot(small_df, aes(x = round, y = manager_rank, group = entry_name, color = team_colour, fill = team_colour)) +
      geom_segment(aes(x = round, xend = x_prev, y = manager_rank, yend = manager_rank), size = 2, show.legend = F) +
      geom_point(shape = 21, size = 8, stroke = 1.5, color = "#F3E9E2", show.legend = F) +
      geom_text(data = small_df %>% filter(round== max(round)), aes(label = entry_name), family = "Lato", fontface = "bold", size = 3.5, hjust = 0, nudge_x = 0.5, show.legend = F) +
      geom_text(aes(label = round), family = "Lato", color = "#F3E9E2", fontface = "bold", ) +
      
      coord_cartesian(clip = 'off') +
      scale_x_continuous(limits = c(min(small_df$round)-0.5,max(small_df$round) + 3),
                         breaks = seq(min(small_df$round),max(small_df$round),1)) +
      scale_y_reverse() +
      scale_fill_manual(values = small_df$team_colour,
                        breaks = small_df$team_colour) +
      scale_color_manual(values = small_df$team_colour,
                         breaks = small_df$team_colour) +
      #scale_color_manual(breaks = all_managers,
      #                   values = palette_1[0:length(all_managers)]) +
      theme(legend.position = "none") +
      #ylab("Relative Rank") +
      xlab("Gameweeks") +
      labs(title = paste0("<img src='./Logo4.png' width='30'> Ownership - ", input$player_ownership),
           #subtitle = "Relative Ranking History of Top FPL Managers",
           caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
      #Adjust Theme to suit this plot
      theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
        legend.position = "top",
        panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
        plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
        plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
        legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
        legend.title = element_text(color = "gray20", size = 14,  family = "Lato", face = 'bold'),
        legend.key = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
        axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
        #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
        legend.background = element_blank())
    
    p9
    
    
    
  })
  
  
  output$ownership_download <- downloadHandler(
    filename = function(){paste("FPL.Ownership", input$player_ownership ,".png", sep='')},
    
    
    content = function(file){
    
    
    Team_Selection <- c()
    Team_Summary <- c()
    
    #for (ID in test_vector) {
    for (ID in Selectize_Team_ID()) {
      #print(ID)
      #print(class(ID))
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      team <- JSON_Output$name
      
      # This shows your gameweek history
      for (GW in seq(from = input$league_gweeks[1], to = input$league_gweeks[2], by = input$bump_breaks)) {
        
        #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/event/',GW,'/picks/'))
        
        
        jsonRespText<-content(FPL_Team, as="text") 
        
        JSON_Output <- fromJSON(jsonRespText)
        Entry_History <- JSON_Output$entry_history %>%
          unlist() %>%
          t() %>%
          as.data.frame() %>%
          mutate(#player_name = player_name,
            entry_name = team)
        
        Team_1 <- JSON_Output$picks %>%
          mutate(round = GW,
                 entry_name = team)
        
        #Sub_1 <- JSON_Output$automatic_subs 
        
        Team_Summary <- bind_rows(Team_Summary, Entry_History)
        
        Team_Selection <- bind_rows(Team_Selection, Team_1)
        
        #Subs <- bind_rows(Subs, Sub_1)
        
        
      }
      
    }
    
    
    df <- Team_Summary %>%
      group_by(event) %>%
      mutate(manager_rank = dense_rank(overall_rank)) %>%
      ungroup() %>%
      filter(event == max(event)) %>%
      arrange(manager_rank,entry_name)
    
    
    df2 <- Team_Selection %>%
      left_join(Players_Gameweek, by = c("element" = "id", "round")) %>%
      #filter(name_club == "Haaland (MCI)") %>%
      mutate(input_player = input$player_ownership) %>%
      separate_wider_delim(input_player, "(", names = c("selected_player", "selected_team"), cols_remove = F) %>%
      mutate(selected_team = str_sub(selected_team, end = -2)) %>%
      mutate(player_reg = paste0("^", selected_player, "\\(", selected_team,"\\)$")) %>%
      left_join(df %>% select(entry_name, manager_rank), by = c("entry_name"))
    
    test_string <- df2 %>% pull(player_reg) %>% head(1)
    
    #df2$entry_name <- factor(df2$entry_name, levels = df$entry_name)
    
    small_df <- df2 %>%
      group_by(round, entry_name, manager_rank) %>%
      summarise(yes_in_group = any(str_detect(name_club, regex(test_string))),
                selected_player = input$player_ownership) %>%
      ungroup() %>%
      separate_wider_delim(selected_player, "(", names = c("player", "Team"), cols_remove = F) %>%
      mutate(Team = str_sub(Team, end = -2)) %>%
      mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                     Team == "AVL" ~ "#670E36",
                                     Team == "BOU" ~ "#B50E12",
                                     Team == "BRE" ~ "#E30613",
                                     Team == "BHA" ~ "#0057B8",
                                     Team == "BUR" ~ "#6C1D45",
                                     Team == "CHE" ~ "#034694",
                                     Team == "CRY" ~ "#1B458F",
                                     Team == "EVE" ~ "#003399",
                                     Team == "FUL" ~ "#000000",
                                     Team == "LIV" ~ "#C8102E",
                                     Team == "LUT" ~ "#F78F1E",
                                     Team == "MCI" ~ "#6CABDD",
                                     Team == "MUN" ~ "#DA291C",
                                     Team == "NEW" ~ "#241F20",
                                     Team == "NFO" ~ "#DD0000",
                                     Team == "SHU" ~ "#EE2737",
                                     Team == "TOT" ~ "#132257",
                                     Team == "WHU" ~ "#7A263A",
                                     Team == "WOL" ~ "#FDB913",
                                     Team == "IPS" ~ "#3a64a3",
                                     Team == "LEI" ~ "#003090",
                                     Team == "SOU" ~ "#D71920")) %>%
      mutate(team_colour = case_when(yes_in_group == F ~ "lightgray",
                                     T ~ team_colour)) %>%
      mutate(x_prev = round - 1)
    
    p9 <- ggplot(small_df, aes(x = round, y = manager_rank, group = entry_name, color = team_colour, fill = team_colour)) +
      geom_segment(aes(x = round, xend = x_prev, y = manager_rank, yend = manager_rank), size = 2, show.legend = F) +
      geom_point(shape = 21, size = 8, stroke = 1.5, color = "#F3E9E2", show.legend = F) +
      geom_text(data = small_df %>% filter(round== max(round)), aes(label = entry_name), family = "Lato", fontface = "bold", size = 3.5, hjust = 0, nudge_x = 0.5, show.legend = F) +
      geom_text(aes(label = round), family = "Lato", color = "#F3E9E2", fontface = "bold") +
      
      coord_cartesian(clip = 'off') +
      scale_x_continuous(limits = c(min(small_df$round)-0.5,max(small_df$round) + 3),
                         breaks = seq(min(small_df$round),max(small_df$round),1)) +
      scale_y_reverse() +
      scale_fill_manual(values = small_df$team_colour,
                        breaks = small_df$team_colour) +
      scale_color_manual(values = small_df$team_colour,
                         breaks = small_df$team_colour) +
      #scale_color_manual(breaks = all_managers,
      #                   values = palette_1[0:length(all_managers)]) +
      theme(legend.position = "none") +
      #ylab("Relative Rank") +
      xlab("Gameweeks") +
      labs(title = paste0("<img src='./Logo4.png' width='30'> Ownership - ", input$player_ownership),
           #subtitle = "Relative Ranking History of Top FPL Managers",
           caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
      #Adjust Theme to suit this plot
      theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
        legend.position = "top",
        panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
        plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
        plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
        legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
        legend.title = element_text(color = "gray20", size = 14,  family = "Lato", face = 'bold'),
        legend.key = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
        axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
        #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
        legend.background = element_blank())
    
    
    ggsave(file, plot = p9, dpi = 600, height = 6, width = 10)
    
    
    
  })

  output$own_plot <- renderPlot({
    
    ID <- input$own_team_id
    Team_Summary <- c()
    Team_Selection <- c()
    
    # This shows your gameweek history
    for (GW in seq(from = input$own_gweeks[1], to = input$own_gweeks[2], by = 1)) {
      
      #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/event/',GW,'/picks/'))
      
      
      jsonRespText<-content(FPL_Team, as="text") 
      
      JSON_Output <- fromJSON(jsonRespText)
      Entry_History <- JSON_Output$entry_history %>%
        unlist() %>%
        t() %>%
        as.data.frame() %>%
        mutate(#player_name = player_name,
          entry_name = Own_New_Teamname())
      
      Team_1 <- JSON_Output$picks %>%
        mutate(round = GW,
               entry_name = Own_New_Teamname())
      
      #Sub_1 <- JSON_Output$automatic_subs 
      
      Team_Summary <- bind_rows(Team_Summary, Entry_History)
      
      Team_Selection <- bind_rows(Team_Selection, Team_1)
      
      #Subs <- bind_rows(Subs, Sub_1)
      
    }
    
    #}
    
    
    small_df <- Team_Selection %>%
      group_by(element) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      arrange(desc(count)) %>%
      head(input$own_number) %>%
      mutate(player_rank = row_number())
    
    
    df2 <- Team_Selection %>%
      filter(element %in% small_df$element) %>%
      left_join(Players_Gameweek, by = c("element" = "id", "round")) %>%
      separate_wider_delim(name_club, "(", names = c("player", "Team"), cols_remove = F) %>%
      mutate(Team = str_sub(Team, end = -2)) %>%
      mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                     Team == "AVL" ~ "#670E36",
                                     Team == "BOU" ~ "#B50E12",
                                     Team == "BRE" ~ "#E30613",
                                     Team == "BHA" ~ "#0057B8",
                                     Team == "BUR" ~ "#6C1D45",
                                     Team == "CHE" ~ "#034694",
                                     Team == "CRY" ~ "#1B458F",
                                     Team == "EVE" ~ "#003399",
                                     Team == "FUL" ~ "#000000",
                                     Team == "LIV" ~ "#C8102E",
                                     Team == "LUT" ~ "#F78F1E",
                                     Team == "MCI" ~ "#6CABDD",
                                     Team == "MUN" ~ "#DA291C",
                                     Team == "NEW" ~ "#241F20",
                                     Team == "NFO" ~ "#DD0000",
                                     Team == "SHU" ~ "#EE2737",
                                     Team == "TOT" ~ "#132257",
                                     Team == "WHU" ~ "#7A263A",
                                     Team == "WOL" ~ "#FDB913",
                                     Team == "IPS" ~ "#3a64a3",
                                     Team == "LEI" ~ "#003090",
                                     Team == "SOU" ~ "#D71920")) %>%
      complete(element, round) %>%
      group_by(element) %>%
      fill(name_club, .direction = "updown") %>%
      ungroup() %>%
      mutate(x_prev = round - 1,
             team_colour = case_when(is.na(team_colour) == T ~ "lightgray",
                                     T ~ team_colour)) %>%
      left_join(small_df, by = c("element"))
    
    p9 <- ggplot(df2, aes(x = round, y = player_rank, color = team_colour, fill = team_colour)) +
      geom_segment(aes(x = round, xend = x_prev, y = player_rank, yend = player_rank), size = 2, show.legend = F) +
      geom_point(shape = 21, size = 8, stroke = 1.5, color = "#F3E9E2", show.legend = F) +
      geom_text(data = df2 %>% filter(round== max(round)), aes(label = name_club), family = "Lato", fontface = "bold", size = 3.5, hjust = 0, nudge_x = 0.5, show.legend = F) +
      geom_text(aes(label = round), family = "Lato", color = "#F3E9E2", fontface = "bold") +
      
      coord_cartesian(clip = 'off') +
      scale_x_continuous(limits = c(min(df2$round) - 0.2,max(df2$round) + 3),
                         breaks = seq(min(df2$round),max(df2$round),1)) +
      scale_y_reverse() +
      scale_fill_manual(values = df2$team_colour,
                        breaks = df2$team_colour) +
      scale_color_manual(values = df2$team_colour,
                         breaks = df2$team_colour) +
      #scale_color_manual(breaks = all_managers,
      #                   values = palette_1[0:length(all_managers)]) +
      theme(legend.position = "none") +
      #ylab("Relative Rank") +
      xlab("Gameweeks") +
      labs(title = paste0("<img src='./Logo4.png' width='30'> Most Owned Players - ", Own_New_Teamname()),
           #subtitle = "Relative Ranking History of Top FPL Managers",
           caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
      #Adjust Theme to suit this plot
      theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
        legend.position = "top",
        panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
        plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
        plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
        legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
        legend.title = element_text(color = "gray20", size = 14,  family = "Lato", face = 'bold'),
        legend.key = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
        axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
        #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
        legend.background = element_blank())
    
    p9
    
    
  })
  
  
  output$ownership_download2 <- downloadHandler(
    filename = function(){paste("FPL.Ownership", Own_New_Teamname() ,".png", sep='')},
    
    
    content = function(file){
      
      ID <- input$own_team_id
      Team_Summary <- c()
      Team_Selection <- c()
      
      
      # This shows your gameweek history
      for (GW in seq(from = input$own_gweeks[1], to = input$own_gweeks[2], by = 1)) {
        
        #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/event/',GW,'/picks/'))
        
        
        jsonRespText<-content(FPL_Team, as="text") 
        
        JSON_Output <- fromJSON(jsonRespText)
        Entry_History <- JSON_Output$entry_history %>%
          unlist() %>%
          t() %>%
          as.data.frame() %>%
          mutate(#player_name = player_name,
            entry_name = Own_New_Teamname())
        
        Team_1 <- JSON_Output$picks %>%
          mutate(round = GW,
                 entry_name = Own_New_Teamname())
        
        #Sub_1 <- JSON_Output$automatic_subs 
        
        Team_Summary <- bind_rows(Team_Summary, Entry_History)
        
        Team_Selection <- bind_rows(Team_Selection, Team_1)
        
        #Subs <- bind_rows(Subs, Sub_1)
        
      }
      
      #}
      
      
      small_df <- Team_Selection %>%
        group_by(element) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        arrange(desc(count)) %>%
        head(input$own_number) %>%
        mutate(player_rank = row_number())
      
      
      df2 <- Team_Selection %>%
        filter(element %in% small_df$element) %>%
        left_join(Players_Gameweek, by = c("element" = "id", "round")) %>%
        separate_wider_delim(name_club, "(", names = c("player", "Team"), cols_remove = F) %>%
        mutate(Team = str_sub(Team, end = -2)) %>%
        mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                       Team == "AVL" ~ "#670E36",
                                       Team == "BOU" ~ "#B50E12",
                                       Team == "BRE" ~ "#E30613",
                                       Team == "BHA" ~ "#0057B8",
                                       Team == "BUR" ~ "#6C1D45",
                                       Team == "CHE" ~ "#034694",
                                       Team == "CRY" ~ "#1B458F",
                                       Team == "EVE" ~ "#003399",
                                       Team == "FUL" ~ "#000000",
                                       Team == "LIV" ~ "#C8102E",
                                       Team == "LUT" ~ "#F78F1E",
                                       Team == "MCI" ~ "#6CABDD",
                                       Team == "MUN" ~ "#DA291C",
                                       Team == "NEW" ~ "#241F20",
                                       Team == "NFO" ~ "#DD0000",
                                       Team == "SHU" ~ "#EE2737",
                                       Team == "TOT" ~ "#132257",
                                       Team == "WHU" ~ "#7A263A",
                                       Team == "WOL" ~ "#FDB913",
                                       Team == "IPS" ~ "#3a64a3",
                                       Team == "LEI" ~ "#003090",
                                       Team == "SOU" ~ "#D71920")) %>%
        complete(element, round) %>%
        group_by(element) %>%
        fill(name_club, .direction = "updown") %>%
        ungroup() %>%
        mutate(x_prev = round - 1,
               team_colour = case_when(is.na(team_colour) == T ~ "lightgray",
                                       T ~ team_colour)) %>%
        left_join(small_df, by = c("element"))
      
      p9 <- ggplot(df2, aes(x = round, y = player_rank, color = team_colour, fill = team_colour)) +
        geom_segment(aes(x = round, xend = x_prev, y = player_rank, yend = player_rank), size = 2, show.legend = F) +
        geom_point(shape = 21, size = 8, stroke = 1.5, color = "#F3E9E2", show.legend = F) +
        geom_text(data = df2 %>% filter(round== max(round)), aes(label = name_club), family = "Lato", fontface = "bold", size = 3.5, hjust = 0, nudge_x = 0.5, show.legend = F) +
        geom_text(aes(label = round), family = "Lato", color = "#F3E9E2", fontface = "bold") +
        
        coord_cartesian(clip = 'off') +
        scale_x_continuous(limits = c(min(df2$round) - 0.2,max(df2$round) + 3),
                           breaks = seq(min(df2$round),max(df2$round),1)) +
        scale_y_reverse() +
        scale_fill_manual(values = df2$team_colour,
                          breaks = df2$team_colour) +
        scale_color_manual(values = df2$team_colour,
                           breaks = df2$team_colour) +
        #scale_color_manual(breaks = all_managers,
        #                   values = palette_1[0:length(all_managers)]) +
        theme(legend.position = "none") +
        #ylab("Relative Rank") +
        xlab("Gameweeks") +
        labs(title = paste0("<img src='./Logo4.png' width='30'> Most Owned Players - ", Own_New_Teamname()),
             #subtitle = "Relative Ranking History of Top FPL Managers",
             caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
        #Adjust Theme to suit this plot
        theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
          panel.grid.major.x = element_blank(),
          #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
          plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
          legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
          legend.title = element_text(color = "gray20", size = 14,  family = "Lato", face = 'bold'),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.length.x = unit(0.2, "cm"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
          #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
          legend.background = element_blank())
      
      p9
      
      
      ggsave(file, plot = p9, dpi = 600, height = 6, width = 10)
      
      
    })
  
  
  output$own_position_plot <- renderPlot({
    
    ID <- input$own_team_id
    Team_Summary <- c()
    Team_Selection <- c()
    
    # This shows your gameweek history
    for (GW in seq(from = input$own_gweeks[1], to = input$own_gweeks[2], by = 1)) {
      
      #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/event/',GW,'/picks/'))
      
      
      jsonRespText<-content(FPL_Team, as="text") 
      
      JSON_Output <- fromJSON(jsonRespText)
      Entry_History <- JSON_Output$entry_history %>%
        unlist() %>%
        t() %>%
        as.data.frame() %>%
        mutate(#player_name = player_name,
          entry_name = Own_New_Teamname())
      
      Team_1 <- JSON_Output$picks %>%
        mutate(round = GW,
               entry_name = Own_New_Teamname())
      
      #Sub_1 <- JSON_Output$automatic_subs 
      
      Team_Summary <- bind_rows(Team_Summary, Entry_History)
      
      Team_Selection <- bind_rows(Team_Selection, Team_1)
      
      #Subs <- bind_rows(Subs, Sub_1)
      
    }
    
    #}
    
    smaller_df <- Players_History %>%
      filter(Position == input$own_position) %>%
      select(id, name_club, Position) %>%
      unique() 
    
    
    small_df <- Team_Selection %>%
      left_join(smaller_df, by = c("element" = "id")) %>%
      filter(Position == input$own_position) %>%
      group_by(element) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      arrange(desc(count)) %>%
      #head(input$own_number) %>%
      mutate(player_rank = row_number())
    
    
    df2 <- Team_Selection %>%
      #filter(element %in% small_df$element) %>%
      left_join(Players_Gameweek, by = c("element" = "id", "round")) %>%
      left_join(smaller_df, by = c("name_club")) %>%
      separate_wider_delim(name_club, "(", names = c("player", "Team"), cols_remove = F) %>%
      mutate(Team = str_sub(Team, end = -2)) %>%
      mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                     Team == "AVL" ~ "#670E36",
                                     Team == "BOU" ~ "#B50E12",
                                     Team == "BRE" ~ "#E30613",
                                     Team == "BHA" ~ "#0057B8",
                                     Team == "BUR" ~ "#6C1D45",
                                     Team == "CHE" ~ "#034694",
                                     Team == "CRY" ~ "#1B458F",
                                     Team == "EVE" ~ "#003399",
                                     Team == "FUL" ~ "#000000",
                                     Team == "LIV" ~ "#C8102E",
                                     Team == "LUT" ~ "#F78F1E",
                                     Team == "MCI" ~ "#6CABDD",
                                     Team == "MUN" ~ "#DA291C",
                                     Team == "NEW" ~ "#241F20",
                                     Team == "NFO" ~ "#DD0000",
                                     Team == "SHU" ~ "#EE2737",
                                     Team == "TOT" ~ "#132257",
                                     Team == "WHU" ~ "#7A263A",
                                     Team == "WOL" ~ "#FDB913",
                                     Team == "IPS" ~ "#3a64a3",
                                     Team == "LEI" ~ "#003090",
                                     Team == "SOU" ~ "#D71920")) %>%
      complete(element, round) %>%
      group_by(element) %>%
      fill(name_club, .direction = "updown") %>%
      ungroup() %>%
      mutate(x_prev = round - 1,
             team_colour = case_when(is.na(team_colour) == T ~ "lightgray",
                                     T ~ team_colour)) %>%
      left_join(small_df, by = c("element"))
    
    p9 <- ggplot(df2, aes(x = round, y = player_rank, color = team_colour, fill = team_colour)) +
      geom_segment(aes(x = round, xend = x_prev, y = player_rank, yend = player_rank), size = 2, show.legend = F) +
      geom_point(shape = 21, size = 8, stroke = 1.5, color = "#F3E9E2", show.legend = F) +
      geom_text(data = df2 %>% filter(round== max(round)), aes(label = name_club), family = "Lato", fontface = "bold", size = 4, hjust = 0, nudge_x = 0.5, show.legend = F) +
      geom_text(aes(label = round), family = "Lato", color = "#F3E9E2", fontface = "bold") +
      
      coord_cartesian(clip = 'off') +
      scale_x_continuous(limits = c(min(df2$round) - 0.2,max(df2$round) + 3),
                         breaks = seq(min(df2$round),max(df2$round),1)) +
      scale_y_reverse() +
      scale_fill_manual(values = df2$team_colour,
                        breaks = df2$team_colour) +
      scale_color_manual(values = df2$team_colour,
                         breaks = df2$team_colour) +
      #scale_color_manual(breaks = all_managers,
      #                   values = palette_1[0:length(all_managers)]) +
      theme(legend.position = "none") +
      #ylab("Relative Rank") +
      xlab("Gameweeks") +
      labs(title = paste0("<img src='./Logo4.png' width='30'> Ownership (", input$own_position, ") - ", Own_New_Teamname()),
           #subtitle = "Relative Ranking History of Top FPL Managers",
           caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
      #Adjust Theme to suit this plot
      theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
        legend.position = "top",
        panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
        plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
        plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
        legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
        legend.title = element_text(color = "gray20", size = 14,  family = "Lato", face = 'bold'),
        legend.key = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
        axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
        #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
        legend.background = element_blank())
    
    p9
    
    
  })
  
  
  output$ownership_download3 <- downloadHandler(
    filename = function(){paste("FPL.Ownership.", input$own_position, ".", Own_New_Teamname() ,".png", sep='')},
    
    
    content = function(file){
      
      ID <- input$own_team_id
      Team_Summary <- c()
      Team_Selection <- c()
      
      # This shows your gameweek history
      for (GW in seq(from = input$own_gweeks[1], to = input$own_gweeks[2], by = 1)) {
        
        #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/event/',GW,'/picks/'))
        
        
        jsonRespText<-content(FPL_Team, as="text") 
        
        JSON_Output <- fromJSON(jsonRespText)
        Entry_History <- JSON_Output$entry_history %>%
          unlist() %>%
          t() %>%
          as.data.frame() %>%
          mutate(#player_name = player_name,
            entry_name = Own_New_Teamname())
        
        Team_1 <- JSON_Output$picks %>%
          mutate(round = GW,
                 entry_name = Own_New_Teamname())
        
        #Sub_1 <- JSON_Output$automatic_subs 
        
        Team_Summary <- bind_rows(Team_Summary, Entry_History)
        
        Team_Selection <- bind_rows(Team_Selection, Team_1)
        
        #Subs <- bind_rows(Subs, Sub_1)
        
      }
      
      #}
      
      smaller_df <- Players_History %>%
        filter(Position == input$own_position) %>%
        select(id, name_club, Position) %>%
        unique() 
      
      
      small_df <- Team_Selection %>%
        left_join(smaller_df, by = c("element" = "id")) %>%
        filter(Position == input$own_position) %>%
        group_by(element) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        arrange(desc(count)) %>%
        #head(input$own_number) %>%
        mutate(player_rank = row_number())
      
      
      df2 <- Team_Selection %>%
        #filter(element %in% small_df$element) %>%
        left_join(Players_Gameweek, by = c("element" = "id", "round")) %>%
        left_join(smaller_df, by = c("name_club")) %>%
        separate_wider_delim(name_club, "(", names = c("player", "Team"), cols_remove = F) %>%
        mutate(Team = str_sub(Team, end = -2)) %>%
        mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                       Team == "AVL" ~ "#670E36",
                                       Team == "BOU" ~ "#B50E12",
                                       Team == "BRE" ~ "#E30613",
                                       Team == "BHA" ~ "#0057B8",
                                       Team == "BUR" ~ "#6C1D45",
                                       Team == "CHE" ~ "#034694",
                                       Team == "CRY" ~ "#1B458F",
                                       Team == "EVE" ~ "#003399",
                                       Team == "FUL" ~ "#000000",
                                       Team == "LIV" ~ "#C8102E",
                                       Team == "LUT" ~ "#F78F1E",
                                       Team == "MCI" ~ "#6CABDD",
                                       Team == "MUN" ~ "#DA291C",
                                       Team == "NEW" ~ "#241F20",
                                       Team == "NFO" ~ "#DD0000",
                                       Team == "SHU" ~ "#EE2737",
                                       Team == "TOT" ~ "#132257",
                                       Team == "WHU" ~ "#7A263A",
                                       Team == "WOL" ~ "#FDB913",
                                       Team == "IPS" ~ "#3a64a3",
                                       Team == "LEI" ~ "#003090",
                                       Team == "SOU" ~ "#D71920")) %>%
        complete(element, round) %>%
        group_by(element) %>%
        fill(name_club, .direction = "updown") %>%
        ungroup() %>%
        mutate(x_prev = round - 1,
               team_colour = case_when(is.na(team_colour) == T ~ "lightgray",
                                       T ~ team_colour)) %>%
        left_join(small_df, by = c("element"))
      
      p9 <- ggplot(df2, aes(x = round, y = player_rank, color = team_colour, fill = team_colour)) +
        geom_segment(aes(x = round, xend = x_prev, y = player_rank, yend = player_rank), size = 2, show.legend = F) +
        geom_point(shape = 21, size = 8, stroke = 1.5, color = "#F3E9E2", show.legend = F) +
        geom_text(data = df2 %>% filter(round== max(round)), aes(label = name_club), family = "Lato", fontface = "bold", size = 4, hjust = 0, nudge_x = 0.5, show.legend = F) +
        geom_text(aes(label = round), family = "Lato", color = "#F3E9E2", fontface = "bold") +
        
        coord_cartesian(clip = 'off') +
        scale_x_continuous(limits = c(min(df2$round) - 0.2,max(df2$round) + 3),
                           breaks = seq(min(df2$round),max(df2$round),1)) +
        scale_y_reverse() +
        scale_fill_manual(values = df2$team_colour,
                          breaks = df2$team_colour) +
        scale_color_manual(values = df2$team_colour,
                           breaks = df2$team_colour) +
        #scale_color_manual(breaks = all_managers,
        #                   values = palette_1[0:length(all_managers)]) +
        theme(legend.position = "none") +
        #ylab("Relative Rank") +
        xlab("Gameweeks") +
        labs(title = paste0("<img src='./Logo4.png' width='30'> Ownership (", input$own_position, ") - ", Own_New_Teamname()),
             #subtitle = "Relative Ranking History of Top FPL Managers",
             caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
        #Adjust Theme to suit this plot
        theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
          panel.grid.major.x = element_blank(),
          #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
          plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
          legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
          legend.title = element_text(color = "gray20", size = 14,  family = "Lato", face = 'bold'),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.length.x = unit(0.2, "cm"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
          #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
          legend.background = element_blank())
      
      p9
      
      
      ggsave(file, plot = p9, dpi = 600, height = 6, width = 10)
      
      
    })
  
  output$table_team <- DT::renderDataTable(make_xg_table(data = Players_History, gweeks1 = input$gweeks[1], gweeks2 = input$gweeks[2]),
                                       rownames = F, extensions = "FixedColumns",
                                       options = list(
                                         scrollX = T,
                                         paging = TRUE, 
                                         searching = TRUE, 
                                         info = FALSE,
                                         sort = TRUE,
                                         iDisplayLength = 20,
                                         fixedColumns = TRUE,
                                         autoWidth = TRUE))



  output$scatter_plot <- renderPlotly({
    
    
    player_highlight <- input$players
    
    df <- create_table1(Players_History, input$positions, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations, input$value) %>%
      mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                     Team == "AVL" ~ "#670E36",
                                     Team == "BOU" ~ "#B50E12",
                                     Team == "BRE" ~ "#E30613",
                                     Team == "BHA" ~ "#0057B8",
                                     Team == "BUR" ~ "#6C1D45",
                                     Team == "CHE" ~ "#034694",
                                     Team == "CRY" ~ "#1B458F",
                                     Team == "EVE" ~ "#003399",
                                     Team == "FUL" ~ "#000000",
                                     Team == "LEE" ~ "#FFCD00",
                                     Team == "LIV" ~ "#C8102E",
                                     Team == "LUT" ~ "#F78F1E",
                                     Team == "MCI" ~ "#6CABDD",
                                     Team == "MUN" ~ "#DA291C",
                                     Team == "NEW" ~ "#241F20",
                                     Team == "NFO" ~ "#DD0000",
                                     Team == "SHU" ~ "#EE2737",
                                     Team == "SUN" ~ "#EB172B",
                                     Team == "TOT" ~ "#132257",
                                     Team == "WHU" ~ "#7A263A",
                                     Team == "WOL" ~ "#FDB913",
                                     Team == "IPS" ~ "#3a64a3",
                                     Team == "LEI" ~ "#003090",
                                     Team == "SOU" ~ "#D71920")) %>%
      mutate(final_colour = case_when(Player %in% player_highlight ~ team_colour,
                                      T ~ "gray")) %>%
      mutate(Tooltip = paste0("\n\nPlayer: ", Player, "\n",input$scatter_x, ": ", get(input$scatter_x), "\n",input$scatter_y, ": ", get(input$scatter_y)))
    
    small_df <- df %>%
      filter(final_colour != "gray")
    
    
    p <- ggplot(df, aes(x = get(input$scatter_x), y = get(input$scatter_y))) +
      geom_point(data = df %>% filter(final_colour == "gray"), aes(label = Tooltip), shape = 21, stroke = 1, size = 3, color = "#F3E9E2", fill = "gray", alpha = 0.5) +
      geom_text_repel(data = df %>% filter(final_colour != "gray"), aes(label = Player, colour = Player), size = 3, bg.color = "#F3E9E2",bg.r = .15, family = "Lato", min.segment.length = unit(0, 'lines'), segment.curvature = -0.1, segment.ncp = 3, segment.angle = 20, nudge_x =  0.15, nudge_y = 0.35, show.legend = F) +
      geom_point(data = df %>% filter(final_colour != "gray"), size = 5.5, color = "white", alpha = 0.5, show.legend = F) +
      geom_point(data = df %>% filter(final_colour != "gray"), aes(fill = Player, label = Tooltip), shape = 21, stroke = 0.5, size = 4, color = "white", alpha = 0.8, show.legend = F) +
      geom_hline(yintercept = 0, color = "gray30", size = 0.75) +
      scale_fill_manual(values = small_df$team_colour,
                        breaks = small_df$Player) +#
      scale_colour_manual(values = small_df$team_colour,
                          breaks = small_df$Player) +
      coord_cartesian(clip = "off") +
      xlab(input$scatter_x) +
      ylab(input$scatter_y) +
      #labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      #  title =  paste0(input$scatter_x, " Vs. ", input$scatter_y, ": GW", input$gameweeks[1], " - ", input$gameweeks[2]),
      #  caption = "Data Taken From FPL Website | Follow @Data_Vizard_") +
      #Adjust Theme to suit this plot
      theme(panel.grid.major.x = element_line(color = "white", linewidth = 0.50, linetype = "dashed"),
            #panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "white", linewidth = 0.50),
            #panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            panel.spacing = unit(0.5, "lines"),
            #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
            legend.position = "top",
            panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
            #plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
            plot.title = element_text(color = "black", size = 24, family = "Lato"),
            plot.caption = element_text(color = "black", size = 10, family = "Lato"),
            plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
            legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,-10,-10),
            axis.text.x = element_text(color ="black", size = 12, family = "Lato"),
            axis.text.y = element_text(color ="black", size = 12, family = "Lato", vjust = -0.3, hjust = 1),
            #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
            axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
            axis.title.y = element_text(color = "black", size = 12,  family = "Lato"),
            #axis.ticks.length.x = unit(0.1, "cm"),
            axis.ticks.x = element_blank(),
            #axis.ticks.length.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_line(color = "gray30", size = 0.75),
            strip.background = element_blank(),
            strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
            strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
            legend.background = element_blank())
    
    #p
    ggplotly(p, tooltip = "label") #%>%
    #  plot_ly(hovertemplate = paste(
    #    "<b>%{text}</b><br><br>",
    #    "%{yaxis.title.text}: %{y:.2f}<br>",
    #    "%{xaxis.title.text}: %{x:.2f}<br>",
    #    "Player: %{Player}",
    #    "<extra></extra>"
    #  ))
    
    
  })
  
  
  output$download_scatter <- downloadHandler(
    filename = function(){paste("FPL.Scatter.", input$positions, ".GW", input$gameweeks[1] , "_GW", input$gameweeks[2],".png", sep='')},
    
    
    content = function(file){
      
      player_highlight <- input$players
      
      df <- create_table1(Players_History, input$positions, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations, input$value) %>%
        mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                       Team == "AVL" ~ "#670E36",
                                       Team == "BOU" ~ "#B50E12",
                                       Team == "BRE" ~ "#E30613",
                                       Team == "BHA" ~ "#0057B8",
                                       Team == "BUR" ~ "#6C1D45",
                                       Team == "CHE" ~ "#034694",
                                       Team == "CRY" ~ "#1B458F",
                                       Team == "EVE" ~ "#003399",
                                       Team == "FUL" ~ "#000000",
                                       Team == "LIV" ~ "#C8102E",
                                       Team == "LUT" ~ "#F78F1E",
                                       Team == "MCI" ~ "#6CABDD",
                                       Team == "MUN" ~ "#DA291C",
                                       Team == "NEW" ~ "#241F20",
                                       Team == "NFO" ~ "#DD0000",
                                       Team == "SHU" ~ "#EE2737",
                                       Team == "TOT" ~ "#132257",
                                       Team == "WHU" ~ "#7A263A",
                                       Team == "WOL" ~ "#FDB913",
                                       Team == "IPS" ~ "#3a64a3",
                                       Team == "LEI" ~ "#003090",
                                       Team == "SOU" ~ "#D71920")) %>%
        mutate(final_colour = case_when(Player %in% player_highlight ~ team_colour,
                                        T ~ "gray")) %>%
        mutate(Tooltip = paste0("\n\nPlayer: ", Player, "\n",input$scatter_x, ": ", get(input$scatter_x), "\n",input$scatter_y, ": ", get(input$scatter_y)))
      
      small_df <- df %>%
        filter(final_colour != "gray")
      
      
      p <- ggplot(df, aes(x = get(input$scatter_x), y = get(input$scatter_y))) +
        geom_point(data = df %>% filter(final_colour == "gray"), aes(label = Tooltip), shape = 21, stroke = 1, size = 3, color = "#F3E9E2", fill = "gray", alpha = 0.5) +
        geom_text_repel(data = df %>% filter(final_colour != "gray"), aes(label = Player, colour = Player), size = 3, bg.color = "#F3E9E2",bg.r = .15, family = "Lato", min.segment.length = unit(0, 'lines'), segment.curvature = -0.1, segment.ncp = 3, segment.angle = 20, nudge_x =  0.15, nudge_y = 0.35, show.legend = F) +
        geom_point(data = df %>% filter(final_colour != "gray"), size = 5.5, color = "white", alpha = 0.5, show.legend = F) +
        geom_point(data = df %>% filter(final_colour != "gray"), aes(fill = Player, label = Tooltip), shape = 21, stroke = 0.5, size = 4, color = "white", alpha = 0.8, show.legend = F) +
        geom_hline(yintercept = 0, color = "gray30", size = 0.75) +
        scale_fill_manual(values = small_df$team_colour,
                          breaks = small_df$Player) +#
        scale_colour_manual(values = small_df$team_colour,
                            breaks = small_df$Player) +
        coord_cartesian(clip = "off") +
        xlab(input$scatter_x) +
        ylab(input$scatter_y) +
        labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
          title =  paste0("<img src='./Logo4.png' width='30'> ", input$scatter_x, " Vs. ", input$scatter_y, ": GW", input$gameweeks[1], " - ", input$gameweeks[2]),
          caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
        #Adjust Theme to suit this plot
        theme(panel.grid.major.x = element_line(color = "white", linewidth = 0.50, linetype = "dashed"),
              #panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = "white", linewidth = 0.50),
              #panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.border = element_blank(),
              panel.spacing = unit(0.5, "lines"),
              #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
              legend.position = "top",
              panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              #plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
              plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
              plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
              plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
              legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
              legend.title = element_blank(),
              legend.key = element_blank(),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(-10,-10,-10,-10),
              axis.text.x = element_text(color ="black", size = 12, family = "Lato"),
              axis.text.y = element_text(color ="black", size = 12, family = "Lato", vjust = -0.3, hjust = 1),
              #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
              axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
              axis.title.y = element_text(color = "black", size = 12,  family = "Lato"),
              #axis.ticks.length.x = unit(0.1, "cm"),
              axis.ticks.x = element_blank(),
              #axis.ticks.length.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_line(color = "gray30", size = 0.75),
              strip.background = element_blank(),
              strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
              strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
              legend.background = element_blank())
      
      #p
      #ggplot(p, tooltip = "label") #%>%
      
      
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
      
    })
  
  output$download_scatter_html <- downloadHandler(
    filename = function(){paste("FPL.Scatter.", input$positions, ".GW", input$gameweeks[1] , "_GW", input$gameweeks[2],".html", sep='')},
    
    
    content = function(file){
      
      player_highlight <- input$players
      
      df <- create_table1(Players_History, input$positions, input$gameweeks[1], input$gameweeks[2], input$starts_1, input$calculations, input$value) %>%
        mutate(team_colour = case_when(Team == "ARS" ~ "#EF0107",
                                       Team == "AVL" ~ "#670E36",
                                       Team == "BOU" ~ "#B50E12",
                                       Team == "BRE" ~ "#E30613",
                                       Team == "BHA" ~ "#0057B8",
                                       Team == "BUR" ~ "#6C1D45",
                                       Team == "CHE" ~ "#034694",
                                       Team == "CRY" ~ "#1B458F",
                                       Team == "EVE" ~ "#003399",
                                       Team == "FUL" ~ "#000000",
                                       Team == "LIV" ~ "#C8102E",
                                       Team == "LUT" ~ "#F78F1E",
                                       Team == "MCI" ~ "#6CABDD",
                                       Team == "MUN" ~ "#DA291C",
                                       Team == "NEW" ~ "#241F20",
                                       Team == "NFO" ~ "#DD0000",
                                       Team == "SHU" ~ "#EE2737",
                                       Team == "TOT" ~ "#132257",
                                       Team == "WHU" ~ "#7A263A",
                                       Team == "WOL" ~ "#FDB913",
                                       Team == "IPS" ~ "#3a64a3",
                                       Team == "LEI" ~ "#003090",
                                       Team == "SOU" ~ "#D71920")) %>%
        mutate(final_colour = case_when(Player %in% player_highlight ~ team_colour,
                                        T ~ "gray")) %>%
        mutate(Tooltip = paste0("\n\nPlayer: ", Player, "\n",input$scatter_x, ": ", get(input$scatter_x), "\n",input$scatter_y, ": ", get(input$scatter_y)))
      
      small_df <- df %>%
        filter(final_colour != "gray")
      
      
      p <- ggplot(df, aes(x = get(input$scatter_x), y = get(input$scatter_y))) +
        geom_point(data = df %>% filter(final_colour == "gray"), aes(label = Tooltip), shape = 21, stroke = 1, size = 3, color = "#F3E9E2", fill = "gray", alpha = 0.5) +
        geom_text_repel(data = df %>% filter(final_colour != "gray"), aes(label = Player, colour = Player), size = 3, bg.color = "#F3E9E2",bg.r = .15, family = "Lato", min.segment.length = unit(0, 'lines'), segment.curvature = -0.1, segment.ncp = 3, segment.angle = 20, nudge_x =  0.15, nudge_y = 0.35, show.legend = F) +
        geom_point(data = df %>% filter(final_colour != "gray"), size = 5.5, color = "white", alpha = 0.5, show.legend = F) +
        geom_point(data = df %>% filter(final_colour != "gray"), aes(fill = Player, label = Tooltip), shape = 21, stroke = 0.5, size = 4, color = "white", alpha = 0.8, show.legend = F) +
        geom_hline(yintercept = 0, color = "gray30", size = 0.75) +
        scale_fill_manual(values = small_df$team_colour,
                          breaks = small_df$Player) +#
        scale_colour_manual(values = small_df$team_colour,
                            breaks = small_df$Player) +
        coord_cartesian(clip = "off") +
        xlab(input$scatter_x) +
        ylab(input$scatter_y) +
        labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
          title =  paste0(input$scatter_x, " Vs. ", input$scatter_y, ": GW", input$gameweeks[1], " - ", input$gameweeks[2]),
          caption = "Data Taken From FPL Website | Follow @Data_Vizard_") +
        #Adjust Theme to suit this plot
        theme(panel.grid.major.x = element_line(color = "white", linewidth = 0.50, linetype = "dashed"),
              #panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = "white", linewidth = 0.50),
              #panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.border = element_blank(),
              panel.spacing = unit(0.5, "lines"),
              #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
              legend.position = "top",
              panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
              #plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
              plot.title = element_text(color = "black", size = 24, family = "Lato"),
              plot.caption = element_text(color = "black", size = 10, family = "Lato"),
              plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Lato", hjust = 0.19),
              legend.text = element_text(color = "gray20", size = 12, family = "Lato", hjust = 0),
              legend.title = element_blank(),
              legend.key = element_blank(),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(-10,-10,-10,-10),
              axis.text.x = element_text(color ="black", size = 12, family = "Lato"),
              axis.text.y = element_text(color ="black", size = 12, family = "Lato", vjust = -0.3, hjust = 1),
              #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
              axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
              axis.title.y = element_text(color = "black", size = 12,  family = "Lato"),
              #axis.ticks.length.x = unit(0.1, "cm"),
              axis.ticks.x = element_blank(),
              #axis.ticks.length.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_line(color = "gray30", size = 0.75),
              strip.background = element_blank(),
              strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
              strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
              legend.background = element_blank())
      
      #p
      #ggplot(p, tooltip = "label") #%>%
      
      fig <- ggplotly(p, tooltip = "label")
      
      htmlwidgets::saveWidget(
        widget = fig, #the plotly object
        file = file, #the path & file name
        selfcontained = TRUE #creates a single html file
      )
      
      #ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
      
      
    })

output$prediction_plot <- renderPlot({
  
  
  GW_vector <- seq(from = input$pred_gweeks[1], to = input$pred_gweeks[2], by = 1)
  
  xMins <- c()
  Pts <- c()
  
  for (GW in GW_vector){
    
    xMins <- c(xMins, paste0(GW, "_xMins"))
    Pts <- c(Pts, paste0(GW, "_Pts"))
  }
  
  x_var <- input$pred_metric
  
  tmp_df <- read_csv(prediction_path) %>%
    #read_excel(prediction_path) %>%
    mutate(team_colour = case_when(Team == "Arsenal" ~ "#EF0107",
                                   Team == "Aston Villa" ~ "#670E36",
                                   Team == "Bournemouth" ~ "#B50E12",
                                   Team == "Brentford" ~ "#E30613",
                                   Team == "Brighton" ~ "#0057B8",
                                   Team == "Burnley" ~ "#6C1D45",
                                   Team == "Chelsea" ~ "#034694",
                                   Team == "Crystal Palace" ~ "#1B458F",
                                   Team == "Everton" ~ "#003399",
                                   Team == "Fulham" ~ "#000000",
                                   Team == "Liverpool" ~ "#C8102E",
                                   Team == "Luton" ~ "#F78F1E",
                                   Team == "Man City" ~ "#6CABDD",
                                   Team == "Man Utd" ~ "#DA291C",
                                   Team == "Newcastle" ~ "#241F20",
                                   Team == "Nott'm Forest" ~ "#DD0000",
                                   Team == "Sheffield Utd" ~ "#EE2737",
                                   Team == "Spurs" ~ "#132257",
                                   Team == "West Ham" ~ "#7A263A",
                                   Team == "Wolves" ~ "#FDB913")) %>%
    mutate(team_short = case_when(Team == "Arsenal" ~ "ARS",
                                  Team == "Aston Villa" ~ "AVL",
                                  Team == "Bournemouth" ~ "BOU",
                                  Team == "Brentford" ~ "BRE",
                                  Team == "Brighton" ~ "BHA",
                                  Team == "Burnley" ~ "BUR",
                                  Team == "Chelsea" ~ "CHE",
                                  Team == "Crystal Palace" ~ "CRY",
                                  Team == "Everton" ~ "EVE",
                                  Team == "Fulham" ~ "FUL",
                                  Team == "Liverpool" ~ "LIV",
                                  Team == "Luton" ~ "LUT",
                                  Team == "Man City" ~ "MCI",
                                  Team == "Man Utd" ~ "MUN",
                                  Team == "Newcastle" ~ "NEW",
                                  Team == "Nott'm Forest" ~ "NFO",
                                  Team == "Sheffield Utd" ~ "SHU",
                                  Team == "Spurs" ~ "TOT",
                                  Team == "West Ham" ~ "WHU",
                                  Team == "Wolves" ~ "WOL")) %>%
    #head(12) %>%
    pivot_longer(cols = xMins, names_to = "GW_xMins", values_to = "Expected Minutes") %>%
    pivot_longer(cols = Pts, names_to = "GW_xPts", values_to = "Expected Points") %>%
    separate_wider_delim(GW_xMins, "_", names = c("Gameweek_xMins", "Type_1"), cols_remove = T) %>%
    separate_wider_delim(GW_xPts, "_", names = c("Gameweek_xPts", "Type_2"), cols_remove = T) %>%
    filter(Gameweek_xMins == Gameweek_xPts) %>%
    mutate(`Elite%` = `Elite%`*100) %>%
    rename(Gameweek = Gameweek_xMins,
           `Elite Ownership` = `Elite%`) %>%
    select(-Gameweek_xPts) %>%
    filter(Gameweek %in% GW_vector) %>%
    filter(team_short %in% input$pred_teams) %>%
    filter(BV <= input$pred_value) %>%
    mutate(Position = case_when(Pos == "G" ~ "GK",
                                Pos == "D" ~ "DEF",
                                Pos == "M" ~ "MID",
                                Pos == "F" ~ "ST")) %>%
    filter(Position %in% input$pred_position) %>%
    group_by(ID, Name) %>%
    {if (input$pred_metric %in% c("Expected Points"))
      reframe(., x_var = sum(get(input$pred_metric)),
              Team = unique(team_short),
              team_colour= unique(team_colour)) 
      else reframe(., x_var = mean(get(input$pred_metric)),
                   Team = unique(team_short),
                   team_colour= unique(team_colour)) } %>%
    #reframe(x_var = sum(get(input$pred_metric)),
    #        Team = unique(team_short),
    #        team_colour= unique(team_colour)) %>%
    arrange(desc(x_var)) %>%
    head(input$pred_number)
  
  if(input$pred_metric %in% c("Elite Ownership")) {
    
    title_text <- paste0("<img src='./Logo4.png' width='30'> ", input$pred_metric, ": GW", max(Players_History$round))
  }
  else if(input$pred_metric %in% c("Expected Minutes")) {
    
    title_text <- paste0("<img src='./Logo4.png' width='30'> ", input$pred_metric, " (per90): GW", input$pred_gweeks[1], " - GW", input$pred_gweeks[2])
  }
   else{
    
    title_text <- paste0("<img src='./Logo4.png' width='30'> ", input$pred_metric, ": GW", input$pred_gweeks[1], " - GW", input$pred_gweeks[2])
}
  
  p <- ggplot(tmp_df, aes(x = x_var, y = reorder(ID, x_var), color = Team)) +
    
    geom_segment(aes(x = 0, xend = x_var, y = reorder(ID, x_var), yend = reorder(ID, x_var)), size = 5, show.legend = F) +
    geom_point(aes(fill = Team), shape = 21, size = 9, stroke = 1.5, color = "#F3E9E2", show.legend = F) +
    geom_text(aes(label = round(x_var, digits = 1)), size = 3, color = "white", family = "Lato") +
    geom_vline(xintercept = 0, color = "gray30") +
    scale_color_manual(breaks = tmp_df$Team,
                       values = tmp_df$team_colour) +
    scale_fill_manual(breaks = tmp_df$Team,
                      values = tmp_df$team_colour) +
    
    scale_y_discrete(name = NULL, labels = setNames(paste0(tmp_df$Name," (",tmp_df$Team,")"), tmp_df$ID)) +
    
    labs(title = title_text,
         #subtitle = "Cumulative Points Totals For the Highest Scoring FPL Players",
         caption = "Data Taken From @fplreview | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
    xlab(input$pred_metric) +
    theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
      panel.grid.major.x = element_line(color = "gray", linewidth = 0.5),
      #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
      legend.position = "top",
      panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
      plot.caption = element_markdown(color = "black", size = 8, family = "Lato"),
      plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
      legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(0,0,0,0),
      #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
      axis.text.x = element_text(color ="black", size = 10, family = "Lato"),
      axis.text.y = element_text(color ="black", size = 10, family = "Lato", hjust = 1),
      #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
      axis.title.x = element_text(color ="black", size = 12, family = "Lato"),
      axis.title.y = element_blank(),
      axis.ticks.x = element_line(color = "gray30"),
      axis.ticks.y = element_blank(),
      axis.ticks.length.x = unit(0.2, "cm"),
      axis.line.x = element_line(color = "gray30"),
      axis.line.y = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_markdown(color = "gray20", size = 8, family = "Lato"),
      #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", , angle = 360, vjust = 0),
      legend.background = element_blank()) #+
  #enter_appear()
  #transition_reveal(get(metric1)) +
  #view_follow(fixed_y = T)
  
  p
  
  
})

    
output$prediction_download <- downloadHandler(
  filename = function(){paste("FPL_Bar.", input$pred_metric, ".GW", input$pred_gweeks[1] , "_GW", input$pred_gweeks[2],".png", sep='')},
  
  
  content = function(file){
    
    GW_vector <- seq(from = input$pred_gweeks[1], to = input$pred_gweeks[2], by = 1)
    
    xMins <- c()
    Pts <- c()
    
    for (GW in GW_vector){
      
      xMins <- c(xMins, paste0(GW, "_xMins"))
      Pts <- c(Pts, paste0(GW, "_Pts"))
    }
    
    x_var <- input$pred_metric
    
    tmp_df <- read_csv(prediction_path) %>%
      #read_excel(prediction_path) %>%
      mutate(team_colour = case_when(Team == "Arsenal" ~ "#EF0107",
                                     Team == "Aston Villa" ~ "#670E36",
                                     Team == "Bournemouth" ~ "#B50E12",
                                     Team == "Brentford" ~ "#E30613",
                                     Team == "Brighton" ~ "#0057B8",
                                     Team == "Burnley" ~ "#6C1D45",
                                     Team == "Chelsea" ~ "#034694",
                                     Team == "Crystal Palace" ~ "#1B458F",
                                     Team == "Everton" ~ "#003399",
                                     Team == "Fulham" ~ "#000000",
                                     Team == "Liverpool" ~ "#C8102E",
                                     Team == "Luton" ~ "#F78F1E",
                                     Team == "Man City" ~ "#6CABDD",
                                     Team == "Man Utd" ~ "#DA291C",
                                     Team == "Newcastle" ~ "#241F20",
                                     Team == "Nott'm Forest" ~ "#DD0000",
                                     Team == "Sheffield Utd" ~ "#EE2737",
                                     Team == "Spurs" ~ "#132257",
                                     Team == "West Ham" ~ "#7A263A",
                                     Team == "Wolves" ~ "#FDB913")) %>%
      mutate(team_short = case_when(Team == "Arsenal" ~ "ARS",
                                    Team == "Aston Villa" ~ "AVL",
                                    Team == "Bournemouth" ~ "BOU",
                                    Team == "Brentford" ~ "BRE",
                                    Team == "Brighton" ~ "BHA",
                                    Team == "Burnley" ~ "BUR",
                                    Team == "Chelsea" ~ "CHE",
                                    Team == "Crystal Palace" ~ "CRY",
                                    Team == "Everton" ~ "EVE",
                                    Team == "Fulham" ~ "FUL",
                                    Team == "Liverpool" ~ "LIV",
                                    Team == "Luton" ~ "LUT",
                                    Team == "Man City" ~ "MCI",
                                    Team == "Man Utd" ~ "MUN",
                                    Team == "Newcastle" ~ "NEW",
                                    Team == "Nott'm Forest" ~ "NFO",
                                    Team == "Sheffield Utd" ~ "SHU",
                                    Team == "Spurs" ~ "TOT",
                                    Team == "West Ham" ~ "WHU",
                                    Team == "Wolves" ~ "WOL")) %>%
      #head(12) %>%
      pivot_longer(cols = xMins, names_to = "GW_xMins", values_to = "Expected Minutes") %>%
      pivot_longer(cols = Pts, names_to = "GW_xPts", values_to = "Expected Points") %>%
      separate_wider_delim(GW_xMins, "_", names = c("Gameweek_xMins", "Type_1"), cols_remove = T) %>%
      separate_wider_delim(GW_xPts, "_", names = c("Gameweek_xPts", "Type_2"), cols_remove = T) %>%
      filter(Gameweek_xMins == Gameweek_xPts) %>%
      mutate(`Elite%` = `Elite%`*100) %>%
      rename(Gameweek = Gameweek_xMins,
             `Elite Ownership` = `Elite%`) %>%
      select(-Gameweek_xPts) %>%
      filter(Gameweek %in% GW_vector) %>%
      filter(team_short %in% input$pred_teams) %>%
      filter(BV <= input$pred_value) %>%
      mutate(Position = case_when(Pos == "G" ~ "GK",
                                  Pos == "D" ~ "DEF",
                                  Pos == "M" ~ "MID",
                                  Pos == "F" ~ "ST")) %>%
      filter(Position %in% input$pred_position) %>%
      group_by(ID, Name) %>%
      {if (input$pred_metric %in% c("Expected Points"))
        reframe(., x_var = sum(get(input$pred_metric)),
                Team = unique(team_short),
                team_colour= unique(team_colour)) 
        else reframe(., x_var = mean(get(input$pred_metric)),
                     Team = unique(team_short),
                     team_colour= unique(team_colour)) } %>%
      #reframe(x_var = sum(get(input$pred_metric)),
      #        Team = unique(team_short),
      #        team_colour= unique(team_colour)) %>%
      arrange(desc(x_var)) %>%
      head(input$pred_number)
    
    if(input$pred_metric %in% c("Elite Ownership")) {
      
      title_text <- paste0("<img src='./Logo4.png' width='30'> ", input$pred_metric, ": GW", max(Players_History$round))
    }
    else if(input$pred_metric %in% c("Expected Minutes")) {
      
      title_text <- paste0("<img src='./Logo4.png' width='30'> ", input$pred_metric, " (per90): GW", input$pred_gweeks[1], " - GW", input$pred_gweeks[2])
    }
    else{
      
      title_text <- paste0("<img src='./Logo4.png' width='30'> ", input$pred_metric, ": GW", input$pred_gweeks[1], " - GW", input$pred_gweeks[2])
    }
    
    p <- ggplot(tmp_df, aes(x = x_var, y = reorder(ID, x_var), color = Team)) +
      
      geom_segment(aes(x = 0, xend = x_var, y = reorder(ID, x_var), yend = reorder(ID, x_var)), size = 5, show.legend = F) +
      geom_point(aes(fill = Team), shape = 21, size = 9, stroke = 1.5, color = "#F3E9E2", show.legend = F) +
      geom_text(aes(label = round(x_var, digits = 1)), size = 3, color = "white", family = "Lato") +
      geom_vline(xintercept = 0, color = "gray30") +
      scale_color_manual(breaks = tmp_df$Team,
                         values = tmp_df$team_colour) +
      scale_fill_manual(breaks = tmp_df$Team,
                        values = tmp_df$team_colour) +
      
      scale_y_discrete(name = NULL, labels = setNames(paste0(tmp_df$Name," (",tmp_df$Team,")"), tmp_df$ID)) +
      
      labs(title = title_text,
           #subtitle = "Cumulative Points Totals For the Highest Scoring FPL Players",
           caption = "Data Taken From @fplreview | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
      xlab(input$pred_metric) +
      theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5),
        #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
        legend.position = "top",
        panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
        plot.title = element_markdown(color = "black", size = 24, family = "Lato"),
        plot.caption = element_markdown(color = "black", size = 8, family = "Lato"),
        plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
        legend.text = element_text(color = "gray30", size = 12, family = "Lato", hjust = 0),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        #axis.text.x = element_text(color ="black", size = 10, family = "Lato", angle = 90),
        axis.text.x = element_text(color ="black", size = 10, family = "Lato"),
        axis.text.y = element_text(color ="black", size = 10, family = "Lato", hjust = 1),
        #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
        axis.title.x = element_text(color ="black", size = 12, family = "Lato"),
        axis.title.y = element_blank(),
        axis.ticks.x = element_line(color = "gray30"),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.line.x = element_line(color = "gray30"),
        axis.line.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_markdown(color = "gray20", size = 8, family = "Lato"),
        #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", , angle = 360, vjust = 0),
        legend.background = element_blank()) 
    
    ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
    
    
  })


}

#secure_server(server)

# Run the application 
shinyApp(ui = ui, server = server)
