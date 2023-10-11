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

# the following setting is important, do not omit.
options(stringsAsFactors = FALSE)

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
            ),
            imageOutput(outputId = "BPPHistDGE", height="100%", width="100%"),
            imageOutput(outputId = "MFPHistDGE", height="100%", width="100%"),
            imageOutput(outputId = "CCPHistDGE", height="100%", width="100%"),
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
  
  ## TO-DO: add check of a DGE FDR or network module number column 
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
    # retrieve results for analysis
    DGE_results_table <- inputAnalysisTable()
    # create named list of all genes (gene universe) and values
    # the gene universe is set to be the list of all genes contained in the gene2GO list of annotated genes
    list_genes <- as.numeric(DGE_results_table$FDR)
    list_genes <- setNames(list_genes, DGE_results_table$gene)
    list_genes_filtered <- list_genes[names(list_genes) %in% names(GOmaps)]
    # return list
    list_genes_filtered
  }
  
  # function to retrieve interesting genes
  # returns list of interesting DE genes (0 == not significant, 1 == significant)
  retrieveInterestingDGE <- function(){
    # retrieve gene universe
    geneUniverse <- createUniverseDGE()
    interesting_DE_genes <- rep(0, length(geneUniverse))
    for(i in 1:length(geneUniverse)){
      if(geneUniverse[i] < 0.05){
        interesting_DE_genes[i] = 1
      }
    }
    interesting_DE_genes <- setNames(interesting_DE_genes, names(geneUniverse))
    # return the interesting genes
    interesting_DE_genes
  }
  
  # function to create BP topGOdata objects
  createBPDGE <- function(){
    # retrieve gene universe
    list_genes_filtered <- createUniverseDGE()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    BP_GO_data <- new('topGOdata', ontology = 'BP', allGenes = list_genes_filtered, 
                    geneSel = retrieveInterestingDGE, nodeSize = 10, annot = annFUN.gene2GO, 
                    gene2GO = GOmaps)
  }
  
  # function to create MF topGOdata objects
  createMFDGE <- function(){
    # retrieve gene universe
    list_genes_filtered <- createUniverseDGE()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    MF_GO_data <- new('topGOdata', ontology = 'MF', allGenes = list_genes_filtered, 
                      geneSel = retrieveInterestingDGE, nodeSize = 10, annot = annFUN.gene2GO, 
                      gene2GO = GOmaps)
  }
  
  # function to create CC topGOdata objects
  createCCDGE <- function(){
    # retrieve gene universe
    list_genes_filtered <- createUniverseDGE()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    CC_GO_data <- new('topGOdata', ontology = 'CC', allGenes = list_genes_filtered, 
                      geneSel = retrieveInterestingDGE, nodeSize = 10, annot = annFUN.gene2GO, 
                      gene2GO = GOmaps)
  }
  
  # function to perform BP GO enrichment 
  performGODGE <- function(){
    # retrieve topGOdata object
    BP_GO_data <- createBPDGE()
    # perform GO enrichment using the topGOdata objects
    BP_GO_results <- runTest(BP_GO_data, statistic = 'Fisher')
  }
  
  # function to perform MF GO enrichment 
  performGODGE <- function(){
    # retrieve topGOdata object
    MF_GO_data <- createMFDGE()
    # perform GO enrichment using the topGOdata objects
    MF_GO_results <- runTest(MF_GO_data, statistic = 'Fisher')
  }
  
  # function to perform CC GO enrichment 
  performGODGE <- function(){
    # retrieve topGOdata object
    CC_GO_data <- createCCDGE()
    # perform GO enrichment using the topGOdata objects
    CC_GO_results <- runTest(CC_GO_data, statistic = 'Fisher')
  }
  
  # function to create BP p-value histogram
  createBPPHistDGE <- function(){
    # store p-values as named list...
    pval_BP_GO <- score(BP_GO_results)
    # plot histogram to see range of p-values
    par(mfrow=c(3, 1),mar=c(1,1,1,1))
    hist(pval_BP_GO, 35, xlab = "p-values", main = "Range of BP GO term p-values")
  }
 
  # render BP p-value histogram
  output$BPPHistDGE <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_BP_DGE.pdf"
    png(exportFile)
    createBPPHistDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  # function to create MF p-value histogram
  createMFPHistDGE <- function(){
    # store p-values as named list...
    pval_MF_GO <- score(MF_GO_results)
    # plot histogram to see range of p-values
    par(mfrow=c(3, 1),mar=c(1,1,1,1))
    hist(pval_MF_GO, 35, xlab = "p-values", main = "Range of MF GO term p-values")
  }
  
  # render MF p-value histogram
  output$MFPHistDGE <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_MF_DGE.pdf"
    png(exportFile)
    createMFPHistDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  # function to create CC p-value histogram
  createMFPHistDGE <- function(){
    # store p-values as named list...
    pval_CC_GO <- score(CC_GO_results)
    # plot histogram to see range of p-values
    par(mfrow=c(3, 1),mar=c(1,1,1,1))
    hist(pval_CC_GO, 35, xlab = "p-values", main = "Range of CC GO term p-values")
  }
  
  # render BP p-value histogram
  output$CCPHistDGE <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_CC_DGE.pdf"
    png(exportFile)
    createCCPHistDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "This is alternate text")
  }, deleteFile = TRUE)
  
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)
          
          