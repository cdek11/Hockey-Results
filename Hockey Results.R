# clear history
rm(list = ls())

# load packages
library(shiny)
library(leaflet)
library(rvest)
library(dplyr)
library(ggmap)
library(maptools)
library(maps)
library(lubridate)
library(stringr)
library(qdap)
library(date)
library(DT)
library(RColorBrewer)
library(ggplot2)
library(scales)

# get websites that have the schedule information
espn_url = "http://www.espn.com"
espn_site <- read_html(paste0(espn_url, "/nhl/teams"))

hockey_schedule_urls <- espn_site %>% html_nodes("a:nth-child(2)") %>% html_attr("href")
hockey_schedule_urls <- hockey_schedule_urls[seq(3, length(hockey_schedule_urls), 3)] %>%
  paste0(espn_url, .)

team_names <- espn_site %>% html_nodes("a:nth-child(2) , h5") %>% html_text()
team_names <- team_names[seq(1, length(team_names), 4)]

# merges team names and schedule into data frame
nhl_schedule_urls <- as.data.frame(cbind(team_names, hockey_schedule_urls))

# get all nhl teams, their respective cities, and the latitude and longitude values of cities
wiki_url <- read_html("https://en.wikipedia.org/wiki/National_Hockey_League")
wiki_team_list <- wiki_url %>%
  html_nodes("h4+ .wikitable td:nth-child(2) , th~ td:nth-child(3) , h4+ .wikitable td:nth-child(1)") %>%
  html_text()

# create dataframe with team locations. remove vegas golden knights since they are a new team without data.
teams <- wiki_team_list[seq(1, length(wiki_team_list), 2)]
cities <- as.character(wiki_team_list[seq(2, length(wiki_team_list), 2)])
team_locations <- as.data.frame(cbind(teams,cities)) %>% 
  filter(teams != "Vegas Golden Knights")

# scrape team logo
wiki_team_url <- "https://en.wikipedia.org/wiki/"
wiki_team_urls <- paste0(wiki_team_url, gsub(" ", "_", as.character(team_locations$teams)))
logo_urls <- character(length = length(team_locations$teams))
for(i in 1:length(team_locations$teams)) {
  logo_urls[i] <- read_html(wiki_team_urls[i]) %>% html_nodes(".infobox td > .image img") %>% html_attr("src")
}
logo_urls <- paste0("https:", logo_urls)
team_logos <- data.frame(teams = team_locations$teams, urls = as.character(logo_urls))

# four pieces of information in dataset: hockey team, city/state, longitude and latitude
team_locations <- cbind(team_locations,geocode(as.character(team_locations$cities)))


#### function to scrape team data
scrape_team_data <- function(team_url) {
  
  # scrape team schedule data from the espn website
  team_data_list <- read_html(team_url) %>% html_nodes('table') %>% html_table(fill = TRUE)
  team_data_tmp <- (as.data.frame(team_data_list[[1]][1:9]))
  names(team_data_tmp) <- c("Date", "Opponent", "Result", "W-L-OL", "GOALIE", 
                            "TOP_PERFORMER", "SF-SA", "PP", "PK")
  
  # get plot data, which includes game date, and num shots scored and attempted for team and opponent
  # need to filter to observations that have already been played
  plot_team_data <- as.data.frame(team_data_tmp) %>%
    filter(row_number() > 1 & Opponent != "2017 Preseason Schedule" & Opponent != "OPPONENT" & !Date %in% c("SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER", "JANUARY", "FEBRUARY", "MARCH")) %>%
    mutate(new_date = ifelse(substr(gsub(".*, ", "", Date), 1, 3) %in% c("Sep", "Oct", "Nov", "Dec"),            
                             as.POSIXct(strptime(paste0(gsub(".*, ", "", Date), " 2016"), format = "%b %d %Y")),
                             as.POSIXct(strptime(paste0(gsub(".*, ", "", Date), " 2017"), format = "%b %d %Y"))),
           `Game Date` = as.factor(as.POSIXct(new_date, origin = "1970-01-01"))) %>%
    filter(as.character(`Game Date`) < (Sys.Date()) & `SF-SA` != "" & Result != "Postponed") %>%
    mutate(game_result = substr(Result, 1, 1),
           num_goals = as.numeric(ifelse(game_result == "W" , 
                                         gsub("-.*$", "", substr(word(Result, start = 1L), 2, 5)),
                                         gsub(".*-", "", substr(word(Result, start = 1L), 2, 5)))),
           other_team_goals = as.numeric(ifelse(game_result == "W" , 
                                                gsub(".*-", "", substr(word(Result, start = 1L), 2, 5)),
                                                gsub("-.*$", "", substr(word(Result, start = 1L), 2, 5)))),
           num_shots = as.numeric(gsub("-.*$", "", `SF-SA`)),
           other_team_shots = as.numeric(gsub(".*-", "", `SF-SA`))) %>%
    arrange(`Game Date`) %>%
    select(c(`Game Date`, num_goals, other_team_goals, num_shots, other_team_shots))
  
  # get table data, which only includes game date, opponent, and result of the game 
  table_team_data <- as.data.frame(team_data_tmp) %>%
    filter(row_number() > 1 & Opponent != "2017 Preseason Schedule" & Opponent != "OPPONENT" & !Date %in% c("SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER", "JANUARY", "FEBRUARY", "MARCH")) %>%
    mutate(new_date = ifelse(substr(gsub(".*, ", "", Date), 1, 3) %in% c("Sep", "Oct", "Nov", "Dec"),            
                             as.POSIXct(strptime(paste0(gsub(".*, ", "", Date), " 2016"), format = "%b %d %Y")),
                             as.POSIXct(strptime(paste0(gsub(".*, ", "", Date), " 2017"), format = "%b %d %Y"))),
           `Game Date` = as.factor(as.POSIXct(new_date, origin = "1970-01-01")),
           Opponent = gsub("vs", "", gsub("@", "", Opponent))) %>%
    select(c(`Game Date`, Opponent, Result))
  
  return(list(plot_team_data, table_team_data))
} #end function to scrape team data


shinyApp(
  ui = fluidPage(
  
    # Add title
    titlePanel("Hockey Schedule and Results"),
    
    # one input: nhl team interested in
    # three outputs: map of all nhl teams, schedule table for team (date, opponent, and result),
    # and plot of num shots on goal and num shots scored across games.
    fluidRow(
      column(width = 8, leafletOutput("map", height = 150)),
      column(width = 3, uiOutput("logo"))
    ),
    
    fluidRow(
      column(3, 
      selectInput("nhl_teams", "NHL Team", team_locations$teams, selected = "Carolina Hurricanes"),
      div(DT::dataTableOutput("table"), width = "50%", style = "font-size:80%")),
      column(4, br(), plotOutput("distPlot"))
    )
  ),

  server = function(input, output, session) { 
  
    # Code to use both map and dropdown menu.
    
    observeEvent(input$nhl_teams, {
      v$team <- input$nhl_teams
    })
    
    observeEvent(input$map_marker_click, {
      p <- input$map_marker_click
      updateSelectInput(session, "nhl_teams", selected = p$id)
    })
    
    v <- reactiveValues(team = NULL)
    
    # based on team user is interested in, look up latitude and longitude for the team's schedule 
    # in the team_locations data. 
    ##### currently this isn't used
    get_team_location <- reactive( {
      selected_city_data <- team_locations %>% 
        filter(team_locations$teams == v$team) %>%
        select(lon, lat)
      return(selected_city_data)
    } )
    
    # based on team user is interested in, get url for schedule info and use that as input
    # for the scrape_team_data function. this output is used for table/plot inputs.
    get_team_data <- reactive( {
      url <- nhl_schedule_urls %>% 
        filter(nhl_schedule_urls$team_names == v$team)
      
      url1 <- paste("",url$hockey_schedule_urls,"", sep = "")
      team_data <- scrape_team_data(url1)
      return(team_data)
    } )
    
    # output table that shows the team schedule, including date, opponent, and game score
    output$table <- DT::renderDataTable({
      DT::datatable(get_team_data()[[2]], options = list(pageLength = 5),
                    rownames = FALSE)
    })
    
    # map that contains markers for each team. popup shows team name for that marker.
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(~as.numeric(team_locations$lon), 
                         ~as.numeric(team_locations$lat),
                         data = team_locations,
                         popup = paste0(team_locations$teams),
                         radius = 5,
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         layerId=team_locations$teams)
    })
    
    # plot number of shots scored and number of shots taken across games
    output$distPlot = renderPlot( {
      color_values <- c("deepskyblue", "red", "dodgerblue4", "firebrick4")
      label_values <- c(paste0(v$team, " Goals"), "Opponent Goals", paste0(v$team, " Shots Attempted"), "Opponent Shots Attempted")
      
      ggplot(get_team_data()[[1]], aes(as.Date(get_team_data()[[1]]$`Game Date`, "%Y-%m-%d"), group = 1)) + 
        geom_line(aes(y = num_goals, color = "deepskyblue"), size = .75) + 
        geom_point(aes(y = num_goals, color = "deepskyblue"), size = .5) +
        geom_line(aes(y = other_team_goals, color = "deepyellow"), size = .75) + 
        geom_point(aes(y = other_team_goals, color = "deepyellow"), size = .5) +
        geom_line(aes(y = num_shots, color = "dodgerblue4"), size = .75) + 
        geom_point(aes(y = num_shots, color = "dodgerblue4"), size = .5) +
        geom_line(aes(y = other_team_shots, color = "firebrick4"), size = .75) + 
        geom_point(aes(y = other_team_shots, color = "firebrick4"), size = .5) +
        labs(x = "Date", y = "Number of Shots", title = "Successful and Unsuccessful Shots on Goal") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        scale_colour_manual(name = "Shot Type", values = color_values,
                            labels = label_values)  +
        scale_x_date(breaks = date_breaks("2 weeks"), labels = date_format("%b %m %Y"))
    }, height = 400, width = 700)
    
    output$logo <- renderUI({
      team <- v$team
      url <- team_logos$urls[team_logos$teams==team]
      img(
        src = as.character(url),
        width = 150,
        height = 150,
        alt=paste0(team, " Logo")
      )
    })
  
  }
)