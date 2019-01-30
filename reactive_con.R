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

edited <- read.csv("for_Derek.csv")

#UI
ui <- navbarPage("Poverty Tracker Data", windowTitle = "Poverty Tracker Data", theme = shinytheme ("cosmo"), 
                 tabPanel("Explore Data", tabsetPanel(type = "tabs",
                                                      id = "tabsetpanel",
                                                      tabPanel(title = "Plot", useShinyjs(), sidebarPanel(width = 4,
                                                                                                          h2("Plotting"),
                                                                                                          
                                                                                                          #Type of plot
                                                                                                          selectInput("x", "X Variable", c("SPM Poverty" = "spmpov", 
                                                                                                                                           "Material Hardship" = "sevhard", 
                                                                                                                                           "Health Problem" = "sevhealthd", 
                                                                                                                                           "Gender" = "imp_female",
                                                                                                                                           "SPM Resources" = "spmres",
                                                                                                                                           "Age" = "imp_age",
                                                                                                                                           "Race" = "imp_race",
                                                                                                                                           "Education Level" = "imp_educat"), "spmpov"),
                                                                                                          uiOutput("y1"),
                                                                                                          uiOutput("title")),
                                                               
                                                      mainPanel(width = 8,
                                                                br(), br(), 
                                                                plotOutput(outputId = "plot"),
                                                                br(), br(), 
                                                                HTML ("Description of X Variable"), 
                                                                verbatimTextOutput(outputId = "xtable"), 
                                                                br(), br(),
                                                                HTML ("Description of Y Variable"), 
                                                                verbatimTextOutput(outputId = "ytable"),
                                                                br(), br(),
                                                                HTML ("Cross-Table of X and Y Variables"), 
                                                                verbatimTextOutput(outputId = "crosstable"))
                                                      )
                                                      )
                          ), 
                                                      
                 tabPanel("About", sidebarPanel(width = 4, tags$img(src = "graphic.png", width = "85%", height = "85%", style = "display:block; margin-left:auto; margin-right: auto;")
                 ), 
                 mainPanel(width = 8, tags$b("ROBIN HOOD POVERTY TRACKER"), br(), br(),"With funding from Robin Hood, the Poverty Tracker documents the dynamics of poverty and disadvantage in New York City. 
                           The Poverty Tracker is based on the New York City Longitudinal Survey of Wellbeing, which follows a representative panel of approximately 2,300 New York City households. 
                           The initial survey, fielded between December 2012 and March 2013, collected detailed information on income, material hardships, and family health and well-being. 
                           Following the initial survey, respondents were enrolled in a panel to be followed over time, with periodic survey modules at 3-month intervals covering topics like assets and debt, neighborhoods and program service utilization, and adult and child health. 
                           Every 3-month follow-up contains basic questions on various experiences that families may have experienced in between waves, including moves into and out of the household, gains and losses of jobs, unexpected major expenses, and large gains or losses in income. 
                           With this rich information in hand, the study seeks to understand how New Yorkers are faring over time. 
                           A second panel is now underway, which began in 2015. The second panel follows a representative panel of approximately 4,000 New York City households.")
                 
                 )
                 )

# Server
server <- function(input, output, session) {
  
  #Reactive Y Variable
  selectiony <- reactive({
    if (input$x == "imp_age") {
      selectInput("y", "Y Variable", c("SPM Resources" = "spmres"), "spmres")
    } else if (input$x == "spmres") {
      selectInput("y", "Y Variable", c("Age" = "imp_age"), "imp_age")
    } else {
      selectInput("y", "Y Variable", c("SPM Poverty" = "spmpov", 
                                       "Material Hardship" = "sevhard", 
                                       "Health Problem" = "sevhealthd", 
                                       "Gender" = "imp_female",
                                       "SPM Resources" = "spmres",
                                       "Age" = "imp_age",
                                       "Education Level" = "imp_educat"), "imp_female")
    }
  })
  
  output$y1 <- renderUI ({
    selectiony()
  })
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Enter text for plot title
  output$title <- renderUI ({
    textInput(inputId = "plot_title", 
              label = "Plot title", 
              placeholder = "Enter text to be used as plot title")
  })
  
  #Data Cleaning for Pie Chart
  edited_pie <- reactive({
    edited %>% 
      group_by_(input$y, input$x) %>% 
      summarize(Percentage = n()) %>%
      group_by_(input$x) %>%
      mutate(Percentage = Percentage / sum(Percentage) * 100) %>%
      arrange_(input$x) %>% 
      mutate(label_pos = cumsum(Percentage) - Percentage / 2,
             perc_text = paste0(round(Percentage), "%"))
  })

  # Data Cleaning for Bar Chart
  edited_bar <- reactive ({ 
    edited %>%
      group_by_(input$x) %>%
      summarize(count = n(), 
                totalY = sum(get(input$y), na.rm = TRUE), 
                meanY = totalY / count)
  })
  
  # Create plot
  output$plot <- renderPlot({
    if ((input$x == "spmres" & input$y == "imp_age") | (input$x == "imp_age" & input$y == "spmres")) {
      ggplot(edited, aes_string(x = input$x, y = input$y)) +
        geom_point() + 
        labs(x = x(),
             y = y(),
             title = toTitleCase(input$plot_title))
    } else if ((input$x == "spmpov" & input$y == "sevhard") | (input$x == "spmpov" & input$y == "sevhealthd")
               | (input$x == "spmpov" & input$y == "imp_female") | (input$x == "sevhard" & input$y == "spmpov") 
               | (input$x == "sevhard" & input$y == "sevhealthd") | (input$x == "sevhard" & input$y == "imp_female")
               | (input$x == "sevhealthd" & input$y == "spmpov") | (input$x == "sevhealthd" & input$y == "sevhard")
               | (input$x == "sevhealthd" & input$y == "imp_female") | (input$x == "imp_female" & input$y == "spmpov")
               | (input$x == "imp_female" & input$y == "sevhard") | (input$x == "imp_female" & input$y == "sevhealthd")) {
      ggplot(edited_pie(), aes_string(x = factor(1),  y = "Percentage", fill = input$y)) + 
        geom_bar(stat = "identity", width = 1) +
        geom_text(aes(x = factor(1), y = label_pos, label = perc_text), size = 4) + 
        scale_fill_distiller(palette = "Oranges") +
        coord_polar("y") + 
        facet_wrap( ~get(input$x)) + 
        theme_void()
    } else {
      ggplot(data = edited_bar(), aes_string(x = input$x, y = "meanY")) +
        geom_bar(stat = "identity", fill = "cornflowerblue", width = 0.5) +
        geom_text(aes_string(label = "meanY"), vjust = -0.5) +
        labs(x = x(),
             y = y(),
             title = toTitleCase(input$plot_title))
    }
    })
  
  output$xtable <- renderPrint ({
    xtab <- describe(edited[input$x])
    xtab
  })
  
  output$ytable <- renderPrint ({
    ytab <- describe(edited[input$y])
    ytab
  }) 
  
  output$crosstable <- renderPrint ({
    crosstab <- CrossTable(edited[, input$x], edited[, input$y], 
                           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, 
                           dnn = c("X Variable", "Y Variable"))
    crosstab
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)