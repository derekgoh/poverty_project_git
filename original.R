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
baseline <- read.csv("baseline.publicuse.oct17.csv")
f3 <- read.csv("followup3.csv")
f6 <- read.csv("followup6.csv")
f9 <- read.csv("followup9.csv")
f12 <- read.csv("followup12.csv")
f15 <- read.csv("followup15.csv")
f18 <- read.csv("followup18.csv")
f21 <- read.csv("followup21.csv")
f24 <- read.csv("followup24.csv")

#UI
ui <- navbarPage("Poverty Tracker Data", windowTitle = "Poverty Tracker Data", theme = shinytheme ("cosmo"), 
                 tabPanel("Explore Data", tabsetPanel(type = "tabs",
                                                      id = "tabsetpanel",
                                                      tabPanel(title = "Plot", useShinyjs(), sidebarPanel(width = 4,
                                                                                                          h2("Plotting"),
                                                                                                          
                                                                                                          #Type of plot
                                                                                                          selectInput("x", "X Variable", c("SPM Poverty" = "in_poverty", 
                                                                                                                                           "Material Hardship" = "material_hardship", 
                                                                                                                                           "Health Problem" = "health_problem", 
                                                                                                                                           "Gender" = "gender",
                                                                                                                                           "Age" = "age",
                                                                                                                                           "Race" = "race", 
                                                                                                                                           "Education Level" = "education_level"), "in_poverty"),
                                                                                                          selectInput("y", "Y Variable", c("SPM Poverty" = "in_poverty", 
                                                                                                                                           "Material Hardship" = "material_hardship", 
                                                                                                                                           "Health Problem" = "health_problem", 
                                                                                                                                           "Gender" = "gender",
                                                                                                                                           "SPM Resources" = "household_resources", 
                                                                                                                                           "Age" = "age",
                                                                                                                                           "Race" = "race", 
                                                                                                                                           "Education Level" = "education_level"), "gender"),
                                                                                                          uiOutput("title"),
                                                                                                          uiOutput("line")),
                                                               
                                                      mainPanel(width = 8,
                                                                br(), br(),
                                                                plotlyOutput(outputId = "plot"),
                                                                br(), br(), 
                                                                HTML ("Description of X Variable"), 
                                                                verbatimTextOutput(outputId = "xtable"), 
                                                                br(), br(),
                                                                HTML ("Description of Y Variable"), 
                                                                verbatimTextOutput(outputId = "ytable"),
                                                                br(), br(),
                                                                actionButton("crosstable", "Click Here to View Cross-Table of X and Y Variable"),
                                                                actionButton("reset", "Clear", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                br(), br(), 
                                                                verbatimTextOutput(outputId = "table"))
                                                      ),
                                                      tabPanel(title = "Datasets", sidebarPanel(width = 4,
                                                                                                h2("Download Data"),
                                                                                                HTML("Select filetype, dataset and variables, then hit 'Download Data'"), 
                                                                                                br(), br(),
                                                                                                radioButtons(inputId = "filetype",
                                                                                                             label = "Filetype:",
                                                                                                             choices = "csv",
                                                                                                             selected = "csv"),
                                                                                                
                                                                                                radioButtons(inputId = "source", 
                                                                                                             label = "Dataset:", 
                                                                                                             choices = c(
                                                                                                               "Baseline" = "baseline", 
                                                                                                               "Adult and Child Health and Wellbeing" = "f3", 
                                                                                                               "Neighborhoods and Service Utilization" = "f6", 
                                                                                                               "Assets and Debts" = "f9", 
                                                                                                               "First Annual Core Follow-Up" = "f12", 
                                                                                                               "Consumer Expenditures" = "f15", 
                                                                                                               "Employment and Job Search" = "f18", 
                                                                                                               "Adult and Child Health and Wellbeing, Immigration History" = "f21", 
                                                                                                               "Second Annual Core Follow-up" = "f24"), 
                                                                                                             selected = "baseline"), 
                                                                                                
                                                                                                uiOutput("colnames"),
                                                                                                downloadButton(outputId = "download_data", label = "Download Data")
                                                      ),
                                                      
                                                      mainPanel(width = 8,
                                                                br(), br(),
                                                                HTML ("Select the relevant variables on the left"),
                                                                br(), br(), 
                                                                DT::dataTableOutput(outputId = "povertytable"), 
                                                                br(), br(), 
                                                                HTML ("For a more detailed explanation of each variable, please refer to the codebook below:"),
                                                                br(), br(), 
                                                                downloadButton(outputId = "codebook", label = "Download codebook"))
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
  
  #Data Cleaning for Labels
  edited$race <- factor(parse_number(edited$race), 
                        levels = c(1:5), 
                        labels = c("White Non-Hispanic", "Black Non-Hispanic", "Asian Non-Hispanic", "Other / MultiRacial", "Hispanic"))
  edited$education_level <- ordered(parse_number(edited$education_level), 
                                    levels = c(1:4), 
                                    labels = c("Less than HS", "HS Graduate or GED", "Some College or Associate's Degree", "Bachelor's Degree or More"))
  edited$gender <- factor(parse_number(edited$gender), 
                          levels = c(0:1), 
                          labels = c("Male", "Female"))
  edited$material_hardship <- factor(parse_number(edited$material_hardship), 
                                     levels = c(0:1), 
                                     labels = c("No", "Yes"))
  edited$health_problem <- factor(parse_number(edited$health_problem), 
                                  levels = c(0:1), 
                                  labels = c("No", "Yes"))
  edited$in_poverty <- factor(parse_number(edited$in_poverty), 
                              levels = c(0:1), 
                              labels = c("No", "Yes"))
    
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  #Reactive dataset variable 
  data_source <- reactive ({
    if(input$source == "baseline") {
      data <- baseline 
    } else if (input$source == "f3") {
      data <- f3
    } else if (input$source == "f6") {
      data <- f6
    } else if (input$source == "f9") {
      data <- f9
    } else if (input$source == "f12") {
      data <- f12
    } else if (input$source == "f15") {
      data <- f15
    } else if (input$source == "f18") {
      data <- f18
    } else if (input$source == "f21") {
      data <- f21
    } else if (input$source == "f24") {
      data <- f24
    }
    return(data)
  })
  
  #Col Names 
  output$colnames <- renderUI ({
    names <- colnames(data_source())
    checkboxGroupInput("selected_var", "Variables:", names, "subject_id")
  })
  
  # Enter text for plot title
  output$title <- renderUI ({
    textInput(inputId = "plot_title", 
              label = "Plot title", 
              placeholder = "Enter text to be used as plot title")
  })
  
  lbf <- reactive ({
    if(input$x == "age" & input$y == "household_resources") {
      checkboxInput(inputId = "fit", label = "Add line of best fit", value = FALSE)
    }
  })
  
  output$line <- renderUI ({
    lbf()
  })
  
  # Data Cleaning for Bar Chart
  edited_bar <- reactive ({ 
    edited %>%
      group_by_(input$x) %>%
      summarize(count = n(), 
                totalY = sum(get(input$y), na.rm = TRUE), 
                meanY = totalY / count) %>%
      mutate(b = format(round(meanY, digits = 2), nsmall = 2))
  })
  
  #Data Cleaning for Stacked Bar Chart
  edited_stackbar <- reactive ({
    edited %>%
      group_by_(input$x, input$y) %>% 
      summarize(Percentage = n()) %>%
      group_by_(input$x) %>%
      mutate(Percentage = Percentage / sum(Percentage) * 100) %>%
      arrange_(input$x) %>%
      mutate(label_pos = cumsum(Percentage) - Percentage / 2,
             perc_text = paste0(round(Percentage), "%"))
  })
  
  # Create plot
  output$plot <- renderPlotly({
    if ((input$x == "household_resources" & input$y == "age") | (input$x == "age" & input$y == "household_resources")) {
      ggplotly({
        sp <- ggplot(edited, aes_string(x = input$x, y = input$y)) +
          geom_point() +
          labs(x = x(),
               y = y(),
               title = toTitleCase(input$plot_title)) 
        
        # Create line of best fit
        
        if (input$fit == TRUE) {
          sp <- sp + geom_smooth(method = "lm")
        }
        sp
      })
    } else if ((input$x == "in_poverty" & input$y == "household_resources") | (input$x == "in_poverty" & input$y == "age")
               | (input$x == "material_hardship" & input$y == "household_resources") | (input$x == "material_hardship" & input$y == "age")
               | (input$x == "health_problem" & input$y == "household_resources") | (input$x == "health_problem" & input$y == "age")
               | (input$x == "gender" & input$y == "household_resources") | (input$x == "gender" & input$y == "age")
               | (input$x == "race" & input$y == "household_resources") | (input$x == "race" & input$y == "age") 
               | (input$x == "education_level" & input$y == "household_resources") | (input$x == "education_level" & input$y == "age")
               | (input$x == "age" & input$y == "in_poverty") | (input$x == "age" & input$y == "material_hardship") 
               | (input$x == "age" & input$y == "health_problem") | (input$x == "age" & input$y == "gender") 
               | (input$x == "age" & input$y == "race") | (input$x == "age" & input$y == "education_level")) {
      ggplotly({
        bc <- ggplot(data = edited_bar(), aes_string(x = input$x, y = "meanY")) +
          geom_bar(stat = "identity", fill = "lightsalmon2", width = 0.5) +
          geom_text(aes(label = b, vjust = 1)) +
          labs(x = x(),
               y = y(),
               title = toTitleCase(input$plot_title))
        bc
      })
    } else {
      ggplotly({
      sbc <- ggplot(data = edited_stackbar(), aes_string(x = input$x, y = "Percentage", fill = input$y)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_text(aes(label = perc_text), position = position_stack(vjust = 0.5)) +
        scale_fill_brewer(name = y(), palette = "Spectral", direction = 1)  +
        labs(x = x(),
             y = y(),
             title = toTitleCase(input$plot_title))
      sbc
      })
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
  
  observeEvent(input$crosstable, {
    output$table <- renderPrint ({
      crosstab <- CrossTable(edited[, input$x], edited[, input$y], 
                           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, 
                           dnn = c("X Variable", "Y Variable"))
      crosstab
  })
  })
  
  observeEvent(input$reset, {
    output$table <- NULL
    })
  
  # Print data table
  output$povertytable <- DT::renderDataTable(
    DT::datatable(data = data_source()[input$selected_var],
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )
  
  # Download codebook
  output$codebook <- downloadHandler(
    filename = function () {
      paste0("All_Codebooks", ".zip", sep = "")
    },
    content = function(file) {
      file.copy("codebooks.zip", file)
    }, 
    contentType = "application/zip"
  )
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$source, ".csv", sep = "")
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(data_source() %>% select(input$selected_var), path = file) 
      }
    }
  )
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)