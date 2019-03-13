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
f1 <- read.csv("test.csv")

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
                                                                         
                                                      ),
                                                      tabPanel(title = "Year-On-Year Comparisons", sidebarPanel(width = 4,
                                                                                                                h2("Plotting"),
                                                                                                                selectInput("v", "Variable of Interest", c("SPM Poverty" = "in_poverty_SPM",
                                                                                                                                                           "OPM Poverty" = "in_poverty_OPM",
                                                                                                                                                           "Material Hardship" = "material_hardship", 
                                                                                                                                                           "Health Problem" = "health_problem",
                                                                                                                                                           "SPM Household Resources" = "SPM_household_resources",
                                                                                                                                                           "OPM Household Resources" = "OPM_household_resources",
                                                                                                                                                           "SPM Income to Needs Ratio" = "SPM_income_to_needs", 
                                                                                                                                                           "OPM Income to Needs Ratio" = "OPM_income_to_needs"), "in_poverty_SPM")
                                                      ),
                                                      mainPanel(width = 8,
                                                                br(), br(),
                                                                plotlyOutput(outputId = "plot2"),
                                                                br(), br(), 
                                                                h5(textOutput("description2")))
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
      completeFun(edited, c(input$x, input$y)) %>% 
      group_by_(input$x, input$y) %>%
      summarize(Percentage = n()) %>%
      group_by_(input$x) %>%
      mutate(Percentage = Percentage / sum(Percentage) * 100) %>%
      arrange_(input$x) %>%
      mutate(label_pos = cumsum(Percentage) - Percentage / 2,
             perc_text = paste0(round(Percentage), "%"))
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
  
  f1$in_poverty_SPM <- factor(parse_number(f1$in_poverty_SPM), 
                                 levels = c(0:1), 
                                 labels = c("Not in Poverty", "Poverty"))
  
  f1$in_poverty_SPM_y1 <- factor(parse_number(f1$in_poverty_SPM_y1), 
                                     levels = c(0:1), 
                                     labels = c("Not in Poverty", "Poverty"))
  
  f1$in_poverty_SPM_y2 <- factor(parse_number(f1$in_poverty_SPM_y2), 
                                 levels = c(0:1), 
                                 labels = c("Not in Poverty", "Poverty"))
  
  data_source <- reactive ({
    if(input$v == "in_poverty_SPM") {
      data <- f1
    } 
    return(data)
  })

  edited_stackbar <- reactive ({
    completeFun(data_source(), c("in_poverty_SPM", "in_poverty_SPM_y1", "in_poverty_SPM_y2")) %>%
      group_by_("in_poverty_SPM", "in_poverty_SPM_y1", "in_poverty_SPM_y2") %>%
      summarize(Percentage = n()) %>%
      mutate(Percentage = Percentage / sum(Percentage) * 100) %>%
      mutate(label_pos = cumsum(Percentage) - Percentage / 2,
             perc_text = paste0(round(Percentage), "%"))
  })
  
  # Create plot
  output$plot2 <- renderPlotly({
    ggplotly({
      sbc <- ggplot(data = edited_stackbar(), aes(x = c("y1", "y2", "y3"), y = "Percentage", fill = "Percentage")) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_text(aes(label = perc_text), position = position_stack(vjust = 0.5)) +
        scale_fill_brewer(name = y(), palette = "Spectral", direction = 1)  +
        labs(x = x(),
             y = y(),
             title = toTitleCase(input$plot_title))
      sbc
    })
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)