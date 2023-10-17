# created by: Elizabeth Brooks
# date: 13 October 2023

#### Setup ####

# load packages
library(shiny)
library(shinythemes)
library(topGO)
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
  #theme = shinytheme("yeti"),
  theme = shinytheme("superhero"),
  
  # add application title
  titlePanel("Gene Ontology (GO) Term Enrichment Analysis with topGO"),
  
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
        "Upload GO mapping table (*.txt):"
      ),
      fileInput(
        "mappings", 
        label = NULL,
        multiple = FALSE,
        accept = "text"
      )
    ),
    
    # Output: Show plots
    mainPanel(
      
      # getting started text
      conditionalPanel(
        condition = "!output.dataUploaded",
        tags$h1(
          align = "center",
          "Getting Started"
        ),
        tags$br(),
        tags$p(
          HTML("<b>Hello!</b>"),
          HTML("Start by uploading <i>.csv</i> files with the data table in the left-hand sidebar.")
        )
      ),
      
      # processing text
      conditionalPanel(
        condition = "output.dataUploaded && !output.resultsCompleted",
        tags$h1(
          "Processing", 
          align="center"
        ),
        tags$br(),
        tags$p(
          "The GO term enrichment analysis results and plots may take several moments to process depending on the size of the input normalized gene tables."
        )
      ),
      
      # results text and plots
      conditionalPanel(
        condition = "output.dataUploaded && output.resultsCompleted",
        # set of tab panels
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tips",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Helpful Tips</b>")
            ),
            tags$p(
              HTML("<b>Tip 1:</b> The results may take several moments to appear depending on the size of the input data tables.")
            )
          ),
          
          # Enrichment tab
          tabPanel(
            "Enrichment",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Enrichment</b>")
            ),
            imageOutput(outputId = "BPPHist", height="100%", width="100%"),
            imageOutput(outputId = "MFPHist", height="100%", width="100%"),
            imageOutput(outputId = "CCPHist", height="100%", width="100%"),
            plotOutput(outputId = "BPDensity"),
            plotOutput(outputId = "MFDensity"),
            plotOutput(outputId = "CCDensity")#,
            #tags$iframe(src = "sigGO_subgraphs_BP_weight01_5_all.pdf", style="height:600px; width:100%"),
            #tags$iframe(src = "sigGO_subgraphs_MF_weight01_5_all.pdf", style="height:600px; width:100%"),
            #tags$iframe(src = "sigGO_subgraphs_CC_weight01_5_all.pdf", style="height:600px; width:100%")
            
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
    # TO-DO: add column header check for gene header
    # read the file
    dataTableInput <- read.csv(file = input$analysisTable$datapath, row.names=1)
    # return data
    dataTableInput
  })
  
  # retrieve input data
  inputMappings <- reactive({
    # require input data
    req(input$mappings)
    # check the input table is not null
    if(is.null(input$mappings)){
      return(NULL)
    }
    # read the file
    GOmaps <- readMappings(file = input$mappings$datapath)
    # return data
    GOmaps
  })
  
  # check if file has been uploaded
  output$dataUploaded <- function(){
    # check the input tables are not null
    if(is.null(inputAnalysisTable())){
      return(FALSE)
    }else if(is.null(inputMappings())){
      return(FALSE)
    }
    return(TRUE)
  }
  outputOptions(output, 'dataUploaded', suspendWhenHidden=FALSE)
  
  
  ## 
  # GO Enrichment
  ##
  
  # function to create gene universe
  createUniverse <- function(){
    # check for inputs
    if(is.null(inputAnalysisTable())){
      return(NULL)
    }
    # retrieve results for analysis
    resultsTable <- inputAnalysisTable()
    # retrieve go mappings
    GOmaps <- inputMappings()
    # create named list of all genes (gene universe) and values
    # the gene universe is set to be the list of all genes contained in the gene2GO list of annotated genes
    list_genes <- as.numeric(resultsTable$FDR)
    list_genes <- setNames(list_genes, rownames(resultsTable))
    list_genes_filtered <- list_genes[names(list_genes) %in% names(GOmaps)]
    # return list
    list_genes_filtered
  }
  
  # TO-DO: add check for DGE vs network data
  # function to retrieve interesting genes
  retrieveInteresting <- function(){
    # function that returns list of interesting DE genes (0 == not significant, 1 == significant)
    get_interesting_DE_genes <- function(geneUniverse){
      interesting_DE_genes <- rep(0, length(geneUniverse))
      for(i in 1:length(geneUniverse)){
       if(geneUniverse[i] < 0.05){
         interesting_DE_genes[i] = 1
        }
     }
      interesting_DE_genes <- setNames(interesting_DE_genes, names(geneUniverse))
      return(interesting_DE_genes)
    }
  }
  
  # function to create BP, MF, or CC topGOdata objects
  createOntology <- function(ontologyID){
    # retrieve gene universe
    list_genes_filtered <- createUniverse()
    # retrieve go mappings
    GOmaps <- inputMappings()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    BP_GO_data <- new('topGOdata', ontology = ontologyID, allGenes = list_genes_filtered, 
                    geneSel = retrieveInteresting(), nodeSize = 10, annot = annFUN.gene2GO, 
                    gene2GO = GOmaps)
  }
  
  # function to perform BP, MF, or CC GO enrichment 
  performGO <- function(ontologyID){
    # retrieve topGOdata object
    GO_data <- createOntology(ontologyID)
    # perform GO enrichment using the topGOdata objects
    GO_results <- runTest(GO_data, statistic = 'Fisher')
  }
  
  # TO-DO: this causes additional function calls
  # check if results are complete
  output$resultsCompleted <- function(){
    if(!is.null(performGO("BP")) || !is.null(performGO("MF")) || !is.null(performGO("CC"))){
      return(TRUE)
    }
    return(FALSE)
  }
  outputOptions(output, 'resultsCompleted', suspendWhenHidden=FALSE, priority=0)
  
  # function to create BP, MF, or CC p-value histogram
  createPHist <- function(ontologyID){
    # retrieve results
    GO_results <- performGO(ontologyID)
    # store p-values as named list...
    pval_GO <- score(GO_results)
    # plot histogram to see range of p-values
    hist(pval_GO, 35, xlab = "p-values", main = "Range of GO Term P-Values")
  }
  
  # render BP p-value histogram
  output$BPPHist <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_BP_"
    png(exportFile, width = 12, height = 9, units="in", res=150)
    createPHist("BP")
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results", height = "500px")
  }, deleteFile = TRUE)
  
  # render MF p-value histogram
  output$MFPHist <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_MF_"
    png(exportFile, width = 12, height = 9, units="in", res=150)
    createPHist("MF")
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results", height = "500px")
  }, deleteFile = TRUE)
  
  # render CC p-value histogram
  output$CCPHist <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_CC_"
    png(exportFile, width = 12, height = 9, units="in", res=150)
    createPHist("CC")
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results", height = "500px")
  }, deleteFile = TRUE)
  
  # TO-DO: allow input number of sig nodes
  # function to plot BP, MF, or CC subgraphs
  createSubgraphs <- function(ontologyID){
    # retrieve topGOdata object
    GO_data <- createOntology(ontologyID)
    # retrieve results
    GO_results <- performGO(ontologyID)
    # plot subgraphs induced by the most significant GO terms
    printGraph(GO_data, GO_results, firstSigNodes = 5, 
             fn.prefix = paste("sigGO_subgraphs", ontologyID, sep="_"), useInfo = "all", pdfSW = TRUE)
  }
  
  # function to get statistics on BP, MF, or CC GO terms
  getStats <- function(ontologyID){
    # TO-DO: fix render in UI
    # create subgraph PDFs for rendering in the UI
    createSubgraphs(ontologyID)
    # retrieve topGOdata object
    GO_data <- createOntology(ontologyID)
    # retrieve results
    GO_results <- performGO(ontologyID)
    # retrieve statistics
    list_GO_terms <- usedGO(GO_data)
    # retrieve results table
    BP_GO_results_table <- GenTable(GO_data, weightFisher = GO_results, orderBy = 'weightFisher', 
                                    topNodes = length(list_GO_terms))
  }
  
  # TO-DO: allow user input weightFisher
  # function to get significant BP, MF, or CC GO terms
  getSig <- function(ontologyID){
    # retrieve stats
    GO_results_table <- getStats(ontologyID)
    # create table of significant GO terms
    sigGO_results_table <- GO_results_table[GO_results_table$weightFisher <= 0.05, ]
  }
  
  # function to get most significant BP, MF, or CC GO terms
  getMostSig <- function(ontologyID){
    # retrieve stats
    GO_results_table <- getStats(ontologyID)
    # retrieve most significant GO term
    topSigGO_ID <- GO_results_table[1, 'GO.ID']
  }
  
  # TO-DO: allow input GO ID from results
  # function to create BP, MF, or CC density plots
  createDensity <- function(ontologyID){
    # retrieve topGOdata object
    GO_data <- createOntology(ontologyID)
    # retrieve GO ID
    topSigGO_ID <- getMostSig(ontologyID)
    # create density plot
    showGroupDensity(GO_data, whichGO = topSigGO_ID, ranks = TRUE)
  }
  
  # render BP density plot
  output$BPDensity <- renderPlot({
    # create plot
    createDensity("BP")
  })
  
  # render MF density plot
  output$MFDensity <- renderPlot({
    # create plot
    createDensity("MF")
  })
  
  # render CC density plot
  output$CCDensity <- renderPlot({
    # create plot
    createDensity("CC")
  })
  
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)
          
          