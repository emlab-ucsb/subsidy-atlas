#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

location = c("Hobbiton", "Hobbiton", "Rivendell", "Rivendell", "Minas Tirith", "Minas Tirith") 
last = c("A", "B", "C", "D", "E", "F") 
first = c("G", "H", "I", "J", "K", "L") 
locations = data.frame(location, last, first)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Select Dates of interest and location"),
    
    fluidRow(
        column(3, wellPanel(
        )
        ),
        column(3, wellPanel(
        )
        ),
        column(
            width = 6
        )
    ),
    
    fluidRow(
        tabsetPanel(
            type = "tabs",
            # summary tab
            tabPanel(
                "  Select Dates and Location",
                uiOutput("loc"),
                uiOutput("dt"),
                shiny::dataTableOutput("merged")
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$loc<-renderUI({
        selectInput("loc", label = h4("Choose location"),
                    choices = locations$location ,selected = 1
        )
    })
    
    rt<-reactive({
        #dataframe creation
        locations %>%
            filter(location == input$loc)
    })
    
    output$dt<-renderUI({
        selectInput("dt", label = h4("Choose Dates"), 
                    choices = as.vector(rbind(as.character(rt()$first),as.character(rt()$last))),
                    selected = 1,
                    multiple = T)
    })
    
    output$merged <- shiny::renderDataTable({
        locations %>%
            filter(location == input$loc,
                   first %in% input$dt | last %in% input$dt)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)