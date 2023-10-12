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
        "Upload GO mapping table (*.txt):"
      ),
      fileInput(
        "mappings", 
        label = NULL,
        multiple = FALSE,
        accept = ".txt"
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
            ),
            tags$p(
              HTML("<b>Tip 1:</b> The results may take several moments to appear depending on the size of the input data tables.")
            ),
          ),
          
          # DGE Universe tab
          tabPanel(
            "DGE Universe",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>DGE Universe</b>")
            ),
            imageOutput(outputId = "BPPHistDGE", height="100%", width="100%"),
            imageOutput(outputId = "MFPHistDGE", height="100%", width="100%"),
            imageOutput(outputId = "CCPHistDGE", height="100%", width="100%"),
            imageOutput(outputId = "BPDensityDGE", height="100%", width="100%"),
            imageOutput(outputId = "MFDensityDGE", height="100%", width="100%"),
            imageOutput(outputId = "CCDensityDGE", height="100%", width="100%"),
            imageOutput(outputId = "BPSubgraphsDGE", height="100%", width="100%"),
            imageOutput(outputId = "MFSubgraphsDGE", height="100%", width="100%"),
            imageOutput(outputId = "CCSubgraphsDGE", height="100%", width="100%"),
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
    # retrieve go mappings
    GOmaps <- inputMappings()
    # create named list of all genes (gene universe) and values
    # the gene universe is set to be the list of all genes contained in the gene2GO list of annotated genes
    list_genes <- as.numeric(DGE_results_table$FDR)
    list_genes <- setNames(list_genes, rownames(DGE_results_table))
    list_genes_filtered <- list_genes[names(list_genes) %in% names(GOmaps)]
    # return list
    list_genes_filtered
  }
  
  # function to retrieve interesting genes
  retrieveInterestingDGE <- function(){
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
  
  # function to create BP topGOdata objects
  createBPDGE <- function(){
    # retrieve gene universe
    list_genes_filtered <- createUniverseDGE()
    # retrieve go mappings
    GOmaps <- inputMappings()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    BP_GO_data <- new('topGOdata', ontology = 'BP', allGenes = list_genes_filtered, 
                    geneSel = retrieveInterestingDGE(), nodeSize = 10, annot = annFUN.gene2GO, 
                    gene2GO = GOmaps)
  }
  
  # function to create MF topGOdata objects
  createMFDGE <- function(){
    # retrieve gene universe
    list_genes_filtered <- createUniverseDGE()
    # retrieve go mappings
    GOmaps <- inputMappings()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    MF_GO_data <- new('topGOdata', ontology = 'MF', allGenes = list_genes_filtered, 
                      geneSel = retrieveInterestingDGE(), nodeSize = 10, annot = annFUN.gene2GO, 
                      gene2GO = GOmaps)
  }
  
  # function to create CC topGOdata objects
  createCCDGE <- function(){
    # retrieve gene universe
    list_genes_filtered <- createUniverseDGE()
    # retrieve go mappings
    GOmaps <- inputMappings()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    CC_GO_data <- new('topGOdata', ontology = 'CC', allGenes = list_genes_filtered, 
                      geneSel = retrieveInterestingDGE(), nodeSize = 10, annot = annFUN.gene2GO, 
                      gene2GO = GOmaps)
  }
  
  # function to perform BP GO enrichment 
  performBPGODGE <- function(){
    # retrieve topGOdata object
    BP_GO_data <- createBPDGE()
    # perform GO enrichment using the topGOdata objects
    BP_GO_results <- runTest(BP_GO_data, statistic = 'Fisher')
  }
  
  # function to perform MF GO enrichment 
  performMFGODGE <- function(){
    # retrieve topGOdata object
    MF_GO_data <- createMFDGE()
    # perform GO enrichment using the topGOdata objects
    MF_GO_results <- runTest(MF_GO_data, statistic = 'Fisher')
  }
  
  # function to perform CC GO enrichment 
  performCCGODGE <- function(){
    # retrieve topGOdata object
    CC_GO_data <- createCCDGE()
    # perform GO enrichment using the topGOdata objects
    CC_GO_results <- runTest(CC_GO_data, statistic = 'Fisher')
  }
  
  # function to create BP p-value histogram
  createBPPHistDGE <- function(){
    # retrieve results
    BP_GO_results <- performBPGODGE()
    # store p-values as named list...
    pval_BP_GO <- score(BP_GO_results)
    # plot histogram to see range of p-values
    hist(pval_BP_GO, 35, xlab = "p-values", main = "Range of BP GO term p-values")
  }
  
  # function to create MF p-value histogram
  createMFPHistDGE <- function(){
    # retrieve results
    MF_GO_results <- performMFGODGE()
    # store p-values as named list...
    pval_MF_GO <- score(MF_GO_results)
    # plot histogram to see range of p-values
    hist(pval_MF_GO, 35, xlab = "p-values", main = "Range of MF GO term p-values")
  }
  
  # function to create CC p-value histogram
  createCCPHistDGE <- function(){
    # retrieve results
    CC_GO_results <- performCCGODGE()
    # store p-values as named list...
    pval_CC_GO <- score(CC_GO_results)
    # plot histogram to see range of p-values
    hist(pval_CC_GO, 35, xlab = "p-values", main = "Range of CC GO term p-values")
  }
  
  # render BP p-value histogram
  output$BPPHistDGE <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_BP_DGE"
    png(exportFile)
    createBPPHistDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # render MF p-value histogram
  output$MFPHistDGE <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_MF_DGE"
    png(exportFile)
    createMFPHistDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # render CC p-value histogram
  output$CCPHistDGE <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_CC_DGE"
    png(exportFile)
    createCCPHistDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # function to get statistics on BP GO terms
  getBPStatsDGE <- function(){
    # retrieve topGOdata object
    BP_GO_data <- createBPDGE()
    # retrieve results
    BP_GO_results <- performBPGODGE()
    # retrieve statistics
    list_BP_GO_terms <- usedGO(BP_GO_data)
    # retrieve results table
    BP_GO_results_table <- GenTable(BP_GO_data, weightFisher = BP_GO_results, orderBy = 'weightFisher', 
                                    topNodes = length(list_BP_GO_terms))
  }
  
  # function to get statistics on MF GO terms
  getMFStatsDGE <- function(){
    # retrieve topGOdata object
    MF_GO_data <- createMFDGE()
    # retrieve results
    MF_GO_results <- performMFGODGE()
    # retrieve statistics
    list_MF_GO_terms <- usedGO(MF_GO_data)
    # retrieve results table
    MF_GO_results_table <- GenTable(MF_GO_data, weightFisher = MF_GO_results, orderBy = 'weightFisher', 
                                    topNodes = length(list_MF_GO_terms))
  }
  
  # function to get statistics on CC GO terms
  getCCStatsDGE <- function(){
    # retrieve topGOdata object
    CC_GO_data <- createCCDGE()
    # retrieve results
    CC_GO_results <- performCCGODGE()
    # retrieve statistics
    list_CC_GO_terms <- usedGO(CC_GO_data)
    # retrieve results table
    CC_GO_results_table <- GenTable(CC_GO_data, weightFisher = CC_GO_results, orderBy = 'weightFisher', 
                                    topNodes = length(list_CC_GO_terms))
  }
  
  # function to get significant BP GO terms
  getBPSigDGE <- function(){
    # retrieve stats
    BP_GO_results_table <- getBPStatsDGE()
    # create table of significant GO terms
    BP_sigGO_results_table <- BP_GO_results_table[BP_GO_results_table$weightFisher <= 0.05, ]
  }
  
  # function to get significant MF GO terms
  getMFSigDGE <- function(){
    # retrieve stats
    MF_GO_results_table <- getMFStatsDGE()
    # create table of significant GO terms
    MF_sigGO_results_table <- MF_GO_results_table[MF_GO_results_table$weightFisher <= 0.05, ]
  }
  
  # function to get significant CC GO terms
  getCCSigDGE <- function(){
    # retrieve stats
    CC_GO_results_table <- getCCStatsDGE()
    # create table of significant GO terms
    CC_sigGO_results_table <- CC_GO_results_table[CC_GO_results_table$weightFisher <= 0.05, ]
  }
  
  # function to get most significant BP GO term
  getBPMostSigDGE <- function(){
    # retrieve stats
    BP_GO_results_table <- getBPStatsDGE()
    # retrieve most significant GO term
    BP_topSigGO_ID <- BP_GO_results_table[1, 'GO.ID']
  }
  
  # function to get most significant MF GO term
  getMFMostSigDGE <- function(){
    # retrieve stats
    MF_GO_results_table <- getMFStatsDGE()
    # retrieve most significant GO term
    MF_topSigGO_ID <- MF_GO_results_table[1, 'GO.ID']
  }
  
  # function to get most significant CC GO term
  getCCMostSigDGE <- function(){
    # retrieve stats
    CC_GO_results_table <- getCCStatsDGE()
    # retrieve most significant GO term
    CC_topSigGO_ID <- CC_GO_results_table[1, 'GO.ID']
  }
  
  # function to create BP density plot
  createBPDensityDGE <- function(){
    # retrieve topGOdata object
    BP_GO_data <- createBPDGE()
    ## TO-DO: allow input GO ID from results
    # retrieve GO ID
    BP_topSigGO_ID <- getBPMostSigDGE()
    # create density plot
    showGroupDensity(BP_GO_data, whichGO = BP_topSigGO_ID, ranks = TRUE)
  }
  
  # function to create MF density plot
  createMFDensityDGE <- function(){
    # retrieve topGOdata object
    MF_GO_data <- createMFDGE()
    ## TO-DO: allow input GO ID from results
    # retrieve GO ID
    MF_topSigGO_ID <- getMFMostSigDGE()
    # create density plot
    showGroupDensity(MF_GO_data, whichGO = MF_topSigGO_ID, ranks = TRUE)
  }
  
  # function to create CC density plot
  createCCDensityDGE <- function(){
    # retrieve topGOdata object
    CC_GO_data <- createCCDGE()
    ## TO-DO: allow input GO ID from results
    # retrieve GO ID
    CC_topSigGO_ID <- getCCMostSigDGE()
    # create density plot
    showGroupDensity(CC_GO_data, whichGO = CC_topSigGO_ID, ranks = TRUE)
  }
  
  # render BP density plot
  output$BPDensityDGE <- renderImage({
    # save the plot
    exportFile <- "density_BP_DGE"
    png(exportFile)
    createBPDensityDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # render MF density plot
  output$MFDensityDGE <- renderImage({
    # save the plot
    exportFile <- "density_MF_DGE"
    png(exportFile)
    createMFDensityDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # render CC density plot
  output$CCDensityDGE <- renderImage({
    # save the plot
    exportFile <- "density_CC_DGE"
    png(exportFile)
    createCCDensityDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # function to plot BP subgraphs
  createBPSubgraphsDGE <- function(){
    # retrieve topGOdata object
    BP_GO_data <- createBPDGE()
    # retrieve results
    BP_GO_results <- performBPGODGE()
    # plot subgraphs induced by the most significant GO terms
    printGraph(BP_GO_data, BP_GO_results, firstSigNodes = 5, 
             fn.prefix = "sigGO_subgraphs_BP_DGE", useInfo = "all", pdfSW = TRUE)
  }
  
  # function to plot MF subgraphs
  createMFSubgraphsDGE <- function(){
    # retrieve topGOdata object
    MF_GO_data <- createMFDGE()
    # retrieve results
    MF_GO_results <- performMFGODGE()
    # plot subgraphs induced by the most significant GO terms
    printGraph(MF_GO_data, MF_GO_results, firstSigNodes = 5, 
               fn.prefix = "sigGO_subgraphs_MF_DGE", useInfo = "all", pdfSW = TRUE)
  }
  
  # function to plot CC subgraphs
  createCCSubgraphsDGE <- function(){
    # retrieve topGOdata object
    CC_GO_data <- createCCDGE()
    # retrieve results
    CC_GO_results <- performCCGODGE()
    # plot subgraphs induced by the most significant GO terms
    printGraph(CC_GO_data, CC_GO_results, firstSigNodes = 5, 
               fn.prefix = "sigGO_subgraphs_CC_DGE", useInfo = "all", pdfSW = TRUE)
  }
  
  # render BP subgraphs
  output$BPSubgraphsDGE <- renderImage({
    # save the plot
    exportFile <- "sigGO_subgraphs_BP_DGE"
    png(exportFile)
    createBPSubgraphsDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # render MF subgraphs
  output$MFSubgraphsDGE <- renderImage({
    # save the plot
    exportFile <- "sigGO_subgraphs_MF_DGE"
    png(exportFile)
    createMFSubgraphsDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # render CC subgraphs
  output$CCSubgraphsDGE <- renderImage({
    # save the plot
    exportFile <- "sigGO_subgraphs_CC_DGE"
    png(exportFile)
    createCCSubgraphsDGE()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)
          
          