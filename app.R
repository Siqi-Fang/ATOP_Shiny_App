#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(haven)
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

#---------------LOAD & TRANSFORM DATA --------------
# country-year level data (e.g., one row for US-2020)
atop5_0sy <- read_dta("ATOP 5_0 (dta)/atop5_0sy.dta")

# Correlates of War country code, country names, and abbreviations
cowid <- read_csv("ATOP 5_0 (dta)/COW country codes.csv") %>%
  distinct(StateAbb, CCode, StateNme) # unique names

states_count_by_year <-data.frame(year = c(1815:2018))
# iterate thru all countries 
for(i in 1:nrow(cowid)) { 
  code = as.character(cowid[i,]$CCode)      # local vars
  name = cowid[i,]$StateAbb
  # each time count the allies for 1 country 
  state_data <- atop5_0sy %>% filter(state==code)
  state_data$count = rowSums(!is.na(state_data %>% select(starts_with("atopid"))))
  state_data = subset(state_data, select=c(year,count)) 
  state_data = rename(state_data, !!name := count)
  # result is in single file with each country = 1 col, named by StateAbb
  states_count_by_year = merge(x = states_count_by_year, y = state_data, by = 
                                 "year", all.x = TRUE)
}


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
          plotOutput("distPlot"))
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- 
      renderPlot({                      
        # match by abbriviation 
        country1 <- cowid[cowid$StateNme==input$name1,]$StateAbb
        country2 <- cowid[cowid$StateNme==input$name2,]$StateAbb
        # set x axis lim (# drop rows w/ both na's & get min&max)
        state <- data.frame(country1 = get(country1,states_count_by_year), 
                            country2 = get(country2,states_count_by_year),
                            year = states_count_by_year$year)
        state <- filter(state, rowSums(is.na(state)) != 2) 
        xmin <- min(state$year)
        xmax <- max(state$year)
        # plot !
        state <- melt(state,id.vars = 'year', variable.name = 'country')
        
        ggplot(state, aes(year,value,colour=country)) +
          geom_point()+
          ggtitle("Alliances Count Over Time")+
          labs(y="alliance count")+
          scale_color_discrete(name="country",
                             labels=c(country1,country2))+
          xlim(xmin,xmax)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
