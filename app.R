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

source("data_cleaning.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ATOP Alliance Over Time by Country"),
    fluidRow(
      column(4,
             tags$a(href="https://github.com/Siqi-Fang/ATOP_Shiny_App", 
                    "Github")),
      column(4,
             tags$a(href="http://www.atopdata.org/", "Database"))
      ),
    fluidRow(
    # Text Input with selection
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
                       selectize=TRUE))
    ),
        # Show a plot of the generated distribution
    mainPanel( # plotOutput take a variable from output 
          plotlyOutput("AC"))
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
        state[,country1] <-get(country1,states_count_by_year)
        state[,country2] <-get(country2,states_count_by_year)
        state <- filter(state, rowSums(is.na(state)) != 2) 
        xmin <- min(state$year)
        xmax <- max(state$year)
        
        # transform data
        state <- melt(state,id.vars = 'year', variable.name = 'country')
        
        pal <- c("cadetblue", "indianred")
        #pal <- setNames(pal, c(country1,country2))
        plot_ly(data = state, x = ~year, y=~value, 
                color = ~country,
                colors = pal)
      })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
