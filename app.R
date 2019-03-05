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
                                                                                                          selectInput("x", "X Variable", c("Race" = "race",
                                                                                                                                           "Age" = "age",
                                                                                                                                           "Gender" = "gender",
                                                                                                                                           "Education Level" = "education_level",
                                                                                                                                           "Number of Children" = "number_of_children", 
                                                                                                                                           "Number of Household Members" = "number_of_household_members",
                                                                                                                                           "SPM Poverty Baseline" = "in_poverty_SPM",
                                                                                                                                           "OPM Poverty Baseline" = "in_poverty_OPM",
                                                                                                                                           "Material Hardship Baseline" = "material_hardship", 
                                                                                                                                           "Health Problem Baseline" = "health_problem",
                                                                                                                                           "SPM Household Resources Baseline" = "SPM_top_coded_resources",
                                                                                                                                           "OPM Household Resources Baseline" = "OPM_top_coded_resources",
                                                                                                                                           "SPM Income to Needs Ratio" = "SPM_income_to_needs", 
                                                                                                                                           "OPM Income to Needs Ratio" = "OPM_income_to_needs", 
                                                                                                                                           "SPM Poverty Year 1" = "in_poverty_SPM_y1",
                                                                                                                                           "OPM Poverty Year 1" = "in_poverty_OPM_y1",
                                                                                                                                           "Material Hardship Year 1" = "material_hardship_y1", 
                                                                                                                                           "Health Problem Year 1" = "health_problem_y1",
                                                                                                                                           "SPM Household Resources Year 1" = "SPM_top_coded_resources_y1",
                                                                                                                                           "OPM Household Resources Year 1" = "OPM_top_coded_resources_y1",
                                                                                                                                           "SPM Income to Needs Ratio Year 1" = "SPM_income_to_needs_y1", 
                                                                                                                                           "OPM Income to Needs Ratio Year 1" = "OPM_income_to_needs_y1",
                                                                                                                                           "SPM Poverty Year 2" = "in_poverty_SPM_y2",
                                                                                                                                           "OPM Poverty Year 2" = "in_poverty_OPM_y2",
                                                                                                                                           "Material Hardship Year 2" = "material_hardship_y2", 
                                                                                                                                           "Health Problem Year 2" = "health_problem_y2",
                                                                                                                                           "SPM Household Resources Year 2" = "SPM_top_coded_resources_y2",
                                                                                                                                           "OPM Household Resources Year 2" = "OPM_top_coded_resources_y2",
                                                                                                                                           "SPM Income to Needs Ratio Year 2" = "SPM_income_to_needs_y2", 
                                                                                                                                           "OPM Income to Needs Ratio Year 2" = "OPM_income_to_needs_y2"), "race"),
                                                                                                          selectInput("y", "Y Variable", c("Race" = "race",
                                                                                                                                           "Age" = "age",
                                                                                                                                           "Gender" = "gender",
                                                                                                                                           "Education Level" = "education_level",
                                                                                                                                           "Number of Children" = "number_of_children", 
                                                                                                                                           "Number of Household Members" = "number_of_household_members",
                                                                                                                                           "SPM Poverty Baseline" = "in_poverty_SPM",
                                                                                                                                           "OPM Poverty Baseline" = "in_poverty_OPM",
                                                                                                                                           "Material Hardship Baseline" = "material_hardship", 
                                                                                                                                           "Health Problem Baseline" = "health_problem",
                                                                                                                                           "SPM Household Resources Baseline" = "SPM_top_coded_resources",
                                                                                                                                           "OPM Household Resources Baseline" = "OPM_top_coded_resources",
                                                                                                                                           "SPM Income to Needs Ratio" = "SPM_income_to_needs", 
                                                                                                                                           "OPM Income to Needs Ratio" = "OPM_income_to_needs", 
                                                                                                                                           "SPM Poverty Year 1" = "in_poverty_SPM_y1",
                                                                                                                                           "OPM Poverty Year 1" = "in_poverty_OPM_y1",
                                                                                                                                           "Material Hardship Year 1" = "material_hardship_y1", 
                                                                                                                                           "Health Problem Year 1" = "health_problem_y1",
                                                                                                                                           "SPM Household Resources Year 1" = "SPM_top_coded_resources_y1",
                                                                                                                                           "OPM Household Resources Year 1" = "OPM_top_coded_resources_y1",
                                                                                                                                           "SPM Income to Needs Ratio Year 1" = "SPM_income_to_needs_y1", 
                                                                                                                                           "OPM Income to Needs Ratio Year 1" = "OPM_income_to_needs_y1",
                                                                                                                                           "SPM Poverty Year 2" = "in_poverty_SPM_y2",
                                                                                                                                           "OPM Poverty Year 2" = "in_poverty_OPM_y2",
                                                                                                                                           "Material Hardship Year 2" = "material_hardship_y2", 
                                                                                                                                           "Health Problem Year 2" = "health_problem_y2",
                                                                                                                                           "SPM Household Resources Year 2" = "SPM_top_coded_resources_y2",
                                                                                                                                           "OPM Household Resources Year 2" = "OPM_top_coded_resources_y2",
                                                                                                                                           "SPM Income to Needs Ratio Year 2" = "SPM_income_to_needs_y2", 
                                                                                                                                           "OPM Income to Needs Ratio Year 2" = "OPM_income_to_needs_y2"), "age"),
                                                                                                          uiOutput("title")),
                                                                                                          
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
                                     labels = c("No Hardship", "Hardship"))
  edited$health_problem <- factor(parse_number(edited$health_problem), 
                                  levels = c(0:1), 
                                  labels = c("No Problem", "Problem"))
  edited$in_poverty_SPM <- factor(parse_number(edited$in_poverty_SPM), 
                              levels = c(0:1), 
                              labels = c("Not in Poverty", "Poverty"))
  edited$age <- factor(cut(edited$age, 
                           breaks = c(-1, 34, 44, 54, 64, 120), 
                           label = c("0 to 34", "35 to 44", "45 to 54", "55 to 64", "65 and Above"), 
                           ordered = TRUE))
  edited$number_of_children <- factor(cut(edited$number_of_children,
                                           breaks = c(-1, 0, 2, 4, 8), 
                                           label = c("0 Children", "1-2 Children", "3-4 Children", "5 or More Children"), 
                                           ordered = TRUE))
  edited$number_of_household_members <- factor(cut(edited$number_of_household_members,
                                          breaks = c(-1, 1, 3, 6, 10), 
                                          label = c("1 Member", "2-3 Members", "4-6 Members", "7 or More Members"), 
                                          ordered = TRUE))
  edited$in_poverty_OPM <- factor(parse_number(edited$in_poverty_OPM), 
                                  levels = c(0:1), 
                                  labels = c("Not in Poverty", "Poverty"))
  edited$SPM_income_to_needs <- ordered(parse_number(edited$SPM_income_to_needs), 
                                    levels = c(1:4), 
                                    labels = c("Ratio: Less than 100%", "Ratio: 100% - 200%", "Ratio: 200% - 300%", "Ratio: More than 300%"))
  edited$OPM_income_to_needs <- ordered(parse_number(edited$OPM_income_to_needs), 
                                        levels = c(1:4), 
                                        labels = c("Ratio: Less than 100%", "Ratio: 100% - 200%", "Ratio: 200% - 300%", "Ratio: More than 300%"))
  edited$in_poverty_SPM_y1 <- factor(parse_number(edited$in_poverty_SPM_y1), 
                                  levels = c(0:1), 
                                  labels = c("Not in Poverty", "Poverty"))
  edited$in_poverty_OPM_y1 <- factor(parse_number(edited$in_poverty_OPM_y1), 
                                  levels = c(0:1), 
                                  labels = c("Not in Poverty", "Poverty"))
  edited$material_hardship_y1 <- factor(parse_number(edited$material_hardship_y1), 
                                     levels = c(0:1), 
                                     labels = c("No Hardship", "Hardship"))
  edited$health_problem_y1 <- factor(parse_number(edited$health_problem_y1), 
                                  levels = c(0:1), 
                                  labels = c("No Problem", "Problem"))
  edited$SPM_income_to_needs_y1 <- ordered(parse_number(edited$SPM_income_to_needs_y1), 
                                        levels = c(1:4), 
                                        labels = c("Ratio: Less than 100%", "Ratio: 100% - 200%", "Ratio: 200% - 300%", "Ratio: More than 300%"))
  edited$OPM_income_to_needs_y1 <- ordered(parse_number(edited$OPM_income_to_needs_y1), 
                                        levels = c(1:4), 
                                        labels = c("Ratio: Less than 100%", "Ratio: 100% - 200%", "Ratio: 200% - 300%", "Ratio: More than 300%"))
  edited$in_poverty_SPM_y2 <- factor(parse_number(edited$in_poverty_SPM_y2), 
                                     levels = c(0:1), 
                                     labels = c("Not in Poverty", "Poverty"))
  edited$in_poverty_OPM_y2 <- factor(parse_number(edited$in_poverty_OPM_y2), 
                                     levels = c(0:1), 
                                     labels = c("Not in Poverty", "Poverty"))
  edited$material_hardship_y2 <- factor(parse_number(edited$material_hardship_y2), 
                                        levels = c(0:1), 
                                        labels = c("No Hardship", "Hardship"))
  edited$health_problem_y2 <- factor(parse_number(edited$health_problem_y2), 
                                     levels = c(0:1), 
                                     labels = c("No Problem", "Problem"))
  edited$SPM_income_to_needs_y2 <- ordered(parse_number(edited$SPM_income_to_needs_y2), 
                                           levels = c(1:4), 
                                           labels = c("Ratio: Less than 100%", "Ratio: 100% - 200%", "Ratio: 200% - 300%", "Ratio: More than 300%"))
  edited$OPM_income_to_needs_y2 <- ordered(parse_number(edited$OPM_income_to_needs_y2), 
                                           levels = c(1:4), 
                                           labels = c("Ratio: Less than 100%", "Ratio: 100% - 200%", "Ratio: 200% - 300%", "Ratio: More than 300%"))
  edited$SPM_top_coded_resources <- factor(cut(edited$SPM_top_coded_resources,
                                           breaks = c(-1, 23759, 47266, 89429, 424000), 
                                           label = c("First Quantile", "Second Quantile", "Third Quantile", "Fourth Quantile"), 
                                           ordered = TRUE))
  edited$OPM_top_coded_resources <- factor(cut(edited$OPM_top_coded_resources,
                                               breaks = c(-1, 19902, 51995, 108196, 600001), 
                                               label = c("First Quantile", "Second Quantile", "Third Quantile", "Fourth Quantile"), 
                                               ordered = TRUE))
  edited$SPM_top_coded_resources_y1 <- factor(cut(edited$SPM_top_coded_resources_y1,
                                               breaks = c(-1, 24018, 49563, 93568, 400000), 
                                               label = c("First Quantile", "Second Quantile", "Third Quantile", "Fourth Quantile"), 
                                               ordered = TRUE))
  edited$OPM_top_coded_resources_y1 <- factor(cut(edited$OPM_top_coded_resources_y1,
                                               breaks = c(-1, 18000, 55000, 116928, 600001), 
                                               label = c("First Quantile", "Second Quantile", "Third Quantile", "Fourth Quantile"), 
                                               ordered = TRUE))
  edited$SPM_top_coded_resources_y2 <- factor(cut(edited$SPM_top_coded_resources_y2,
                                               breaks = c(-1, 22484, 46199, 91723, 400000), 
                                               label = c("First Quantile", "Second Quantile", "Third Quantile", "Fourth Quantile"), 
                                               ordered = TRUE))
  edited$OPM_top_coded_resources_y2 <- factor(cut(edited$OPM_top_coded_resources_y2,
                                               breaks = c(-1, 17000, 52034, 117877, 490000), 
                                               label = c("First Quantile", "Second Quantile", "Third Quantile", "Fourth Quantile"), 
                                               ordered = TRUE))
  
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
  
  #Data Cleaning for Stacked Bar Chart
  edited_stackbar <- reactive ({
    edited %>%
      group_by_(input$x, input$y) %>%
      na.omit() %>%
      summarize(Percentage = n()) %>%
      group_by_(input$x) %>%
      mutate(Percentage = Percentage / sum(Percentage) * 100) %>%
      arrange_(input$x) %>%
      mutate(label_pos = cumsum(Percentage) - Percentage / 2,
             perc_text = paste0(round(Percentage), "%"))
  })
  
  # Create plot
  output$plot <- renderPlotly({
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