# created by: Elizabeth Brooks
# date: 10 October 2023

#### Setup ####

# load packages
library(shiny)
library(shinythemes)
library(topGO)
library(edgeR)
library(ggplot2)
library(Rgraphviz)
library(tidyr)

#### UI ####

# Define UI 
ui <- fluidPage(
  # view available themes
  #shinythemes::themeSelector(),
  
  # use a theme
  theme = shinytheme("yeti"),
  #theme = shinytheme("superhero"),
  
  # add application title
  titlePanel("Gene Ontology (GO) Term Enrichment Analysis in topGO"),
  
  # setup sidebar layout
  sidebarLayout(
    
    # setup sidebar panel
    sidebarPanel(
      
      # file uploads
      tags$p(
        "Upload analysis data table (*.csv):"
      ),
      fileInput(
        "analysisTable", 
        label = NULL,
        multiple = FALSE,
        accept = ".csv"
      ),
      tags$p(
        "Upload GO mapping table (*.csv):"
      ),
      fileInput(
        "mappingTable", 
        label = NULL,
        multiple = FALSE,
        accept = ".csv"
      )
    ),
    
    # Output: Show plots
    mainPanel(
      
      # getting started text
      conditionalPanel(
        condition = "!output.dataUploaded",
        tags$h1("Getting Started", align = "center"),
        tags$br(),
        tags$p(
          HTML("<b>Hello!</b>"),
          "Start by uploading CSV files with the data table in the left-hand sidebar."
        )
      ),
      
      # results text and plots
      conditionalPanel(
        condition = "output.dataUploaded",
        # set of tab panels
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tips",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Helpful Tips</b>")
            )
          ),
          
          # data cleaning tab
          tabPanel(
            "Analysis Setup",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Analysis Setup</b>")
            )
          )
        )
      )
    )
  )
)

#### Server ####

# Define server 
server <- function(input, output, session) {
  
  ##
  # Data Setup
  ##
  
  # retrieve input data
  inputData <- reactive({
    # require input data
    req(input$analysisTable)
    # check the input table is not null
    if(is.null(input$analysisTable)){
      return(NULL)
    }
    # read the file
    dataTableInput <- read.csv(file = input$analysisTable$datapath, row.names=1)
    # return data
    dataTableInput
  })
  
  # check if file has been uploaded
  output$dataUploaded <- function(){
    return(!is.null(inputData()))
  }
  outputOptions(output, 'dataUploaded', suspendWhenHidden=FALSE)
  
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)
          
          