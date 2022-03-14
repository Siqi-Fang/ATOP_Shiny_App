#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)
library(ggplot2)
library(visNetwork)
library(shinydashboard)

source("data_cleaning.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # --------- Side Nav Bar ------------
    dashboardHeader(
      title= "ATOP"
    ),
    
    dashboardSidebar(
      sidebarMenu(
        #menuItem("Home", tabName = "tab_dashboard", icon = icon("dashboard")),
        menuItem("Alliance Count", tabName = "alliance_count"),
        menuItem("Networks", tabName = "networks"),
        menuItem("About", tabName = "tab_about", icon = icon("info"))
      )
    ),
    
  #---------- Nav Bar Content -----------------
    dashboardBody(
      tabItems(
        # Homepage 
        #tabItem("tab_dashboard",tags$head())
        # Alliance Count ------------------------
        tabItem("alliance_count",
                h2("ATOP Alliance Count Over Time"),
                fluidRow(
                  # Selection 
                  column(4,hr(),
                         selectInput('name1', # selection result stored in 
                                     'Type or select country1 to plot', #label
                                     cowid$StateNme,  # default = 1st item
                                     selectize=TRUE)),
                  column(4,hr(),
                         selectInput('name2', # selection result stored in 
                                     'Type or select country2 to plot', #label
                                     cowid$StateNme,  # default = 1st item
                                     selected = "Canada",
                                     selectize=TRUE))),

                plotlyOutput("AC")      
                ), # alliance end <<<<<<<
        
        # Networks Graph -----------------
        tabItem("networks",
                visNetworkOutput("Network")
                ), #  networks end <<<<<<<<<<<<
        
        # About Page --------------
        tabItem("tab_about",
                fluidRow(span("See the code:  "),
                  # need to wrap raw html statements with HTML("")
                         HTML("<a href='https://github.com/Siqi-Fang/ATOP_Shiny_App'>
                    <img src='https://img.shields.io/badge/GitHub-black??
                    style=plastic&logo=github'></a>")),
                fluidRow(span("Data Source:  "),
                         HTML("<a href='http://www.atopdata.org/'>
                  <img src='https://img.shields.io/badge/ATOP-Database-black
                  ??style=plastic'></a>"))
                )# about end <<<<<<<<<<<<<<<<<<
      ), # tabitems end
    )#dash body end
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$AC <- 
      renderPlotly({
        # match by abbriviation 
        country1 <- cowid[cowid$StateNme==input$name1,]$StateAbb
        country2 <- cowid[cowid$StateNme==input$name2,]$StateAbb
        
        # set x axis lim (# drop rows w/ both na's & get min&max)
        state <- data.frame(year = states_count_by_year$year)
        # name column with the country abbr
        state[,country1] <-get(country1,states_count_by_year)
        state[,country2] <-get(country2,states_count_by_year)
        state <- filter(state, rowSums(is.na(state)) != 2) 
        xmin <- min(state$year)
        xmax <- max(state$year)
        
        # transform data
        state <- melt(state,id.vars = 'year', variable.name = 'country')
        
        pal <- c("cadetblue", "indianred")
        plot_ly(data = state, x = ~year, y=~value, 
                color = ~country,
                colors = pal)
      })
    
    output$Network <- 
      renderVisNetwork({
        visNetwork(nodes, links, width="100%",
                   main="US's defensive military alliance partners in 2018", 
                   submain="Created by Ahra Wu (Source: ATOP)") 
      })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
