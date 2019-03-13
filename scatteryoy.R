library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tools)
library(DT)
library(plotly)
library(shinydashboard)
library(reshape2)
library(shinyjs)
library(psych)
library(gmodels)

edited <- read.csv("finaldata.csv")

#UI
ui <- navbarPage("Poverty Tracker Data", windowTitle = "Poverty Tracker Data", theme = shinytheme ("cosmo"), 
                 tabPanel("Explore Data", tabsetPanel(type = "tabs",
                                                      id = "tabsetpanel",
                                                      tabPanel(title = "Scatter", useShinyjs(), sidebarPanel(width = 4,
                                                                                                          h2("Plotting"),
                                                                                                          
                                                                                                          #Type of plot
                                                                                                          selectInput("x", "X Variable", c("Age" = "age",
                                                                                                                                           "Number of Children" = "number_of_children", 
                                                                                                                                           "Number of Household Members" = "number_of_household_members",
                                                                                                                                           "SPM Household Resources Baseline" = "SPM_household_resources",
                                                                                                                                           "OPM Household Resources Baseline" = "OPM_household_resources",
                                                                                                                                           "SPM Household Resources Year 1" = "SPM_household_resources_y1",
                                                                                                                                           "OPM Household Resources Year 1" = "OPM_household_resources_y1",
                                                                                                                                           "SPM Household Resources Year 2" = "SPM_household_resources_y2",
                                                                                                                                           "OPM Household Resources Year 2" = "OPM_household_resources_y2",
                                                                                                                                           "SPM Income to Needs Ratio Year 2" = "SPM_income_to_needs_y2", 
                                                                                                                                           "OPM Income to Needs Ratio Year 2" = "OPM_income_to_needs_y2"), "SPM_household_resources"),
                                                                                                          selectInput("y", "Y Variable", c("Age" = "age",
                                                                                                                                           "Number of Children" = "number_of_children", 
                                                                                                                                           "Number of Household Members" = "number_of_household_members",
                                                                                                                                           "SPM Household Resources Baseline" = "SPM_household_resources",
                                                                                                                                           "OPM Household Resources Baseline" = "OPM_household_resources",
                                                                                                                                           "SPM Household Resources Year 1" = "SPM_household_resources_y1",
                                                                                                                                           "OPM Household Resources Year 1" = "OPM_household_resources_y1",
                                                                                                                                           "SPM Household Resources Year 2" = "SPM_household_resources_y2",
                                                                                                                                           "OPM Household Resources Year 2" = "OPM_household_resources_y2",
                                                                                                                                           "SPM Income to Needs Ratio Year 2" = "SPM_income_to_needs_y2", 
                                                                                                                                           "OPM Income to Needs Ratio Year 2" = "OPM_income_to_needs_y2"), "age"),
                                                                                                          textInput(inputId = "plot_title", 
                                                                                                                    label = "Plot title", 
                                                                                                                    placeholder = "Enter text to be used as plot title"),
                                                                                                          
                                                                                                          #Size of points
                                                                                                          sliderInput(inputId = "size", label = "Point Size", value = 1, min = 1, max = 6),
                                                                                                          
                                                                                                          #Color of points
                                                                                                          selectInput(inputId = "color", label = "Point Color", 
                                                                                                                      choices = c("Black", "Red", " Dark Green", "Blue", "Orange"), 
                                                                                                                      selected = "Black"),
                                                                                                          
                                                                                                          #Line of best fit
                                                                                                          checkboxInput(inputId = "fit", label = "Add line of best fit", value = FALSE)),
                                                                                                          
                                                               
                                                               mainPanel(width = 8,
                                                                         br(), br(),
                                                                         plotlyOutput(outputId = "plot"),
                                                                         br(), br(), 
                                                                         h5(textOutput("description")),
                                                                         br(), 
                                                                         HTML ("Description of X Variable"), 
                                                                         verbatimTextOutput(outputId = "xtable"), 
                                                                         br(), br(),
                                                                         HTML ("Description of Y Variable"), 
                                                                         verbatimTextOutput(outputId = "ytable"))
                                                                         
                                                      )
                                                      )
                 )
                 )

# Server
server <- function(input, output, session) {
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  

  completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }
  
  edited_scatter <- reactive ({
      completeFun(edited, c(input$x, input$y)) 
  })
  

  # Create scatterplot object the plotOutput function is expecting 
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(data = edited_scatter(), aes_string(x = input$x, y = input$y)) +
        geom_point(size = input$size, col = input$color) +
        labs(x = x(),
             y = y(),
             color = toTitleCase(str_replace_all(input$z, "_", " ")),
             title = toTitleCase(input$plot_title))
      
      # Create line of best fit
      if (input$fit == TRUE) {
        p <- p + geom_smooth(method = "lm")
      }
      p
    })
  })
  
  # Create description of plot
  output$description <- renderText({
    paste("The plot above shows the relationship between",
          x(),
          "and",
          y(),
          "for",
          nrow(edited_scatter()),
          "participants.")
  })
  
  output$xtable <- renderPrint ({
    xtab <- describe(edited[input$x])
    xtab
  })
  
  output$ytable <- renderPrint ({
    ytab <- describe(edited[input$y])
    ytab
  }) 
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)