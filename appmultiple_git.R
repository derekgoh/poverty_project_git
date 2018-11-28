library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tools)
library(DT)
library(plotly)
library(shinydashboard)

shortlist <- read.csv("shortlist.csv")
shortlistpie <- read.csv("shortlistpie.csv")
shortlistscatter <- read.csv("shortlistscatter.csv")
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
                                                      tabPanel(title = "Plot", sidebarPanel(width = 4,
                                                                                            h2("Plotting"),
                                                                                            
                                                                                            #Type of plot
                                                                                            radioButtons(inputId = "type", 
                                                                                                         label = "Type of Plot:", 
                                                                                                         choices = c("Scatter Plot", "Pie Chart", "Bar Chart"), 
                                                                                                         selected = "Scatter Plot"),
                                                                                            
                                                                                            uiOutput("x1"),
                                                                                            uiOutput("y1"),
                                                                                            uiOutput("title"), 
                                                                                            uiOutput("sizep"),
                                                                                            uiOutput("pointc"), 
                                                                                            uiOutput("best")
                                                      ),
                                                                                            
                                                               mainPanel(width = 8,
                                                                         br(), br(), 
                                                                         plotlyOutput(outputId = "plot"),
                                                                         br(),
                                                                         h5(textOutput("description"))
                                                                         )
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
  
  #Reactive Variables
  variable_source <- reactive ({
    if(input$type == "Scatter Plot") {
      variable <- shortlistscatter
    } else if (input$type == "Pie Chart") {
      variable <- shortlistpie
    } else if (input$type == "Bar Chart") {
      variable <- shortlistpie
    }
    return(variable)
  })
  
  selectiony <- reactive({
    if (input$type == "Scatter Plot"){
      vnames <- colnames(variable_source())
      radioButtons("y", "Y Variable", vnames, "Household.Resources")
    } else if (input$type == "Pie Chart") {
      vnames <- colnames(variable_source())
      radioButtons("y", "Y Variable", vnames, "Education")
    } else if (input$type == "Bar Chart") {
      vnames <- colnames(variable_source())
      radioButtons("y", "Y Variable", vnames, "Education")
    }
  })
  
  selectionx <- reactive({
    if (input$type == "Scatter Plot"){
      vnames <- colnames(variable_source())
      radioButtons("x", "X Variable", vnames, "Age")
    } else if (input$type == "Pie Chart") {
      vnames <- colnames(variable_source())
      radioButtons("x", "X Variable", vnames, "Gender")
    } else if (input$type == "Bar Chart") {
      vnames <- colnames(variable_source())
      radioButtons("x", "X Variable", vnames, "Gender")
    }
  })
  

  #Variables
  output$y1 <- renderUI ({
    selectiony()
  })
  
  output$x1 <- renderUI ({
    selectionx()
  })
  
  # Enter text for plot title
  output$title <- renderUI ({
  textInput(inputId = "plot_title", 
            label = "Plot title", 
            placeholder = "Enter text to be used as plot title")
  })
  
  psize <- reactive ({
    if(input$type == "Scatter Plot") {
      sliderInput(inputId = "size", label = "Point Size", value = 1, min = 1, max = 6)
  }
  })
  
  #Size of points
  output$sizep <- renderUI ({
    psize()
  })
  
  pcolor <- reactive({
    if(input$type == "Scatter Plot") {
      selectInput(inputId = "color", label = "Point Color", 
                  choices = c("Black", "Red", " Dark Green", "Blue", "Orange"), 
                  selected = "Black")
    }
  })
  
  #Color of points
  output$pointc <- renderUI ({
    pcolor()
  })
  
  lbest <- reactive ({
    if(input$type == "Scatter Plot") {
      checkboxInput(inputId = "fit", label = "Add line of best fit", value = FALSE)
    }
  })
  
  #Line of best fit
  output$best <- renderUI ({
    lbest()
  })
  
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
  
  # Create plot 
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(data = variable_source(), aes_string(x = input$x, y = input$y)) +
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
          nrow(shortlist),
          "participants.")
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