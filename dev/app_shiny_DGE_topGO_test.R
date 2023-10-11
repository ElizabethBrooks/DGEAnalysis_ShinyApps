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
  inputAnalysisTable <- reactive({
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
  
  # retrieve input data
  inputMappingTable <- reactive({
    # require input data
    req(input$mappingTable)
    # check the input table is not null
    if(is.null(input$mappingTable)){
      return(NULL)
    }
    # read the file
    GOmaps <- readMappings(file = input$mappingTable$datapath)
    # return data
    GOmaps
  })
  
  # check if file has been uploaded
  output$dataUploaded <- function(){
    # check the input tables are not null
    if(is.null(inputAnalysisTable())){
      return(FALSE)
    }else if(is.null(inputMappingTable())){
      return(FALSE)
    }
    return(TRUE)
  }
  outputOptions(output, 'dataUploaded', suspendWhenHidden=FALSE)
  
  
  ## 
  # GO Enrichment - DGE
  ##
  
  # function to create gene universe
  createUniverseDGE <- function(){
    # check for inputs
    if(is.null(inputAnalysisTable())){
      return(NULL)
    }
    # create named list of all genes (gene universe) and values
    # the gene universe is set to be the list of all genes contained in the gene2GO list of annotated genes
    list_genes <- as.numeric(DGE_results_table$FDR)
    list_genes <- setNames(list_genes, DGE_results_table$gene)
    list_genes_filtered <- list_genes[names(list_genes) %in% names(GOmaps)]
    
  }
  
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)
          
          