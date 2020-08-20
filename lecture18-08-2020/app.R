#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pti <- c("shiny","tidyverse","ggplot2movies")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}

##########
### Shiny starter code
##########
library(shiny)
library(tidyverse)
library(ggplot2movies)

# Set randomness seed
set.seed(61)
# Prepare data
shiny_movie_set <- 
    movies %>% 
    filter(year >= 2000) %>%
    select(title,year,length,rating,votes,Action:Short) %>% 
    gather(genre,value,Action:Short) %>% 
    filter(value == 1) %>% 
    select(-value)

# Get genre list
genres <- 
    shiny_movie_set %>% 
    distinct(genre) %>% 
    unlist(.)

names(genres) <- NULL

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Movie Length and IMDB Scores"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Years",
                        min = 2000,
                        max = 2005,
                        value = c(2002,2003),
                        sep=""),
            selectInput(inputId="genre",label="Genre",choices=c("All",genres),selected="All",multiple=TRUE),
            sliderInput("vote","At least X vote",
                        min=min(shiny_movie_set$votes),
                        max=max(shiny_movie_set$votes),
                        value=0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterplot <- renderPlot({
        
        print(input$year)
        print(input$genre)
        print(input$vote)
        if(!("All" %in% input$genre)){
        plot_df<-shiny_movie_set    %>%
         filter(genre %in% input$genre) %>%    
         filter(votes>input$vote)   %>%
         filter(year>=input$year[1] & year<=input$year[2])
        }
        else{
            plot_df<-shiny_movie_set     %>%
                filter(votes>input$vote) %>%
                filter(year>=input$year[1] & year<=input$year[2])
        }
     
        ggplot(plot_df,aes(x=length,y=rating,color=genre))+geom_point()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
