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
  theme = shinytheme("yeti"),
  #theme = shinytheme("superhero"),
  
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
        tags$h1("Getting Started", align = "center"),
        tags$br(),
        tags$p(
          HTML("<b>Hello!</b>"),
          "Start by uploading <i>.csv</i> files with the data table in the left-hand sidebar."
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
          
          # Enrichment tab
          tabPanel(
            "Enrichment",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Enrichment</b>")
            ),
            imageOutput(outputId = "BPPHistDGE", height="100%", width="100%"),
            imageOutput(outputId = "MFPHistDGE", height="100%", width="100%"),
            imageOutput(outputId = "CCPHistDGE", height="100%", width="100%"),
            plotOutput(outputId = "BPDensityDGE"),
            plotOutput(outputId = "MFDensityDGE"),
            plotOutput(outputId = "CCDensityDGE"),
            plotOutput(outputId = "BPSubgraphsDGE"),#, height="100%", width="100%"),
            plotOutput(outputId = "MFSubgraphsDGE"),#, height="100%", width="100%"),
            plotOutput(outputId = "CCSubgraphsDGE"),#, height="100%", width="100%"),
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
  
  # function to create BP topGOdata objects
  createBP <- function(){
    # retrieve gene universe
    list_genes_filtered <- createUniverse()
    # retrieve go mappings
    GOmaps <- inputMappings()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    BP_GO_data <- new('topGOdata', ontology = 'BP', allGenes = list_genes_filtered, 
                    geneSel = retrieveInteresting(), nodeSize = 10, annot = annFUN.gene2GO, 
                    gene2GO = GOmaps)
  }
  
  # function to create MF topGOdata objects
  createMF <- function(){
    # retrieve gene universe
    list_genes_filtered <- createUniverse()
    # retrieve go mappings
    GOmaps <- inputMappings()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    MF_GO_data <- new('topGOdata', ontology = 'MF', allGenes = list_genes_filtered, 
                      geneSel = retrieveInteresting(), nodeSize = 10, annot = annFUN.gene2GO, 
                      gene2GO = GOmaps)
  }
  
  # function to create CC topGOdata objects
  createCC <- function(){
    # retrieve gene universe
    list_genes_filtered <- createUniverse()
    # retrieve go mappings
    GOmaps <- inputMappings()
    # create topGOdata objects for enrichment analysis (1 for each ontology)
    CC_GO_data <- new('topGOdata', ontology = 'CC', allGenes = list_genes_filtered, 
                      geneSel = retrieveInteresting(), nodeSize = 10, annot = annFUN.gene2GO, 
                      gene2GO = GOmaps)
  }
  
  # function to perform BP GO enrichment 
  performBPGO <- function(){
    # retrieve topGOdata object
    BP_GO_data <- createBP()
    # perform GO enrichment using the topGOdata objects
    BP_GO_results <- runTest(BP_GO_data, statistic = 'Fisher')
  }
  
  # function to perform MF GO enrichment 
  performMFGO <- function(){
    # retrieve topGOdata object
    MF_GO_data <- createMF()
    # perform GO enrichment using the topGOdata objects
    MF_GO_results <- runTest(MF_GO_data, statistic = 'Fisher')
  }
  
  # function to perform CC GO enrichment 
  performCCGO <- function(){
    # retrieve topGOdata object
    CC_GO_data <- createCC()
    # perform GO enrichment using the topGOdata objects
    CC_GO_results <- runTest(CC_GO_data, statistic = 'Fisher')
  }
  
  # function to create BP p-value histogram
  createBPPHist <- function(){
    # retrieve results
    BP_GO_results <- performBPGO()
    # store p-values as named list...
    pval_BP_GO <- score(BP_GO_results)
    # plot histogram to see range of p-values
    hist(pval_BP_GO, 35, xlab = "p-values", main = "Range of BP GO term p-values")
  }
  
  # function to create MF p-value histogram
  createMFPHist <- function(){
    # retrieve results
    MF_GO_results <- performMFGO()
    # store p-values as named list...
    pval_MF_GO <- score(MF_GO_results)
    # plot histogram to see range of p-values
    hist(pval_MF_GO, 35, xlab = "p-values", main = "Range of MF GO term p-values")
  }
  
  # function to create CC p-value histogram
  createCCPHist <- function(){
    # retrieve results
    CC_GO_results <- performCCGO()
    # store p-values as named list...
    pval_CC_GO <- score(CC_GO_results)
    # plot histogram to see range of p-values
    hist(pval_CC_GO, 35, xlab = "p-values", main = "Range of CC GO term p-values")
  }
  
  # render BP p-value histogram
  output$BPPHist <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_BP_"
    png(exportFile)
    createBPPHist()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # render MF p-value histogram
  output$MFPHist <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_MF_"
    png(exportFile)
    createMFPHist()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # render CC p-value histogram
  output$CCPHist <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_CC_"
    png(exportFile)
    createCCPHist()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # function to get statistics on BP GO terms
  getBPStats <- function(){
    # retrieve topGOdata object
    BP_GO_data <- createBP()
    # retrieve results
    BP_GO_results <- performBPGO()
    # retrieve statistics
    list_BP_GO_terms <- usedGO(BP_GO_data)
    # retrieve results table
    BP_GO_results_table <- GenTable(BP_GO_data, weightFisher = BP_GO_results, orderBy = 'weightFisher', 
                                    topNodes = length(list_BP_GO_terms))
  }
  
  # function to get statistics on MF GO terms
  getMFStats <- function(){
    # retrieve topGOdata object
    MF_GO_data <- createMF()
    # retrieve results
    MF_GO_results <- performMFGO()
    # retrieve statistics
    list_MF_GO_terms <- usedGO(MF_GO_data)
    # retrieve results table
    MF_GO_results_table <- GenTable(MF_GO_data, weightFisher = MF_GO_results, orderBy = 'weightFisher', 
                                    topNodes = length(list_MF_GO_terms))
  }
  
  # function to get statistics on CC GO terms
  getCCStats <- function(){
    # retrieve topGOdata object
    CC_GO_data <- createCC()
    # retrieve results
    CC_GO_results <- performCCGO()
    # retrieve statistics
    list_CC_GO_terms <- usedGO(CC_GO_data)
    # retrieve results table
    CC_GO_results_table <- GenTable(CC_GO_data, weightFisher = CC_GO_results, orderBy = 'weightFisher', 
                                    topNodes = length(list_CC_GO_terms))
  }
  
  # function to get significant BP GO terms
  getBPSig <- function(){
    # retrieve stats
    BP_GO_results_table <- getBPStats()
    # create table of significant GO terms
    BP_sigGO_results_table <- BP_GO_results_table[BP_GO_results_table$weightFisher <= 0.05, ]
  }
  
  # function to get significant MF GO terms
  getMFSig <- function(){
    # retrieve stats
    MF_GO_results_table <- getMFStats()
    # create table of significant GO terms
    MF_sigGO_results_table <- MF_GO_results_table[MF_GO_results_table$weightFisher <= 0.05, ]
  }
  
  # function to get significant CC GO terms
  getCCSig <- function(){
    # retrieve stats
    CC_GO_results_table <- getCCStats()
    # create table of significant GO terms
    CC_sigGO_results_table <- CC_GO_results_table[CC_GO_results_table$weightFisher <= 0.05, ]
  }
  
  # function to get most significant BP GO term
  getBPMostSig <- function(){
    # retrieve stats
    BP_GO_results_table <- getBPStats()
    # retrieve most significant GO term
    BP_topSigGO_ID <- BP_GO_results_table[1, 'GO.ID']
  }
  
  # function to get most significant MF GO term
  getMFMostSig <- function(){
    # retrieve stats
    MF_GO_results_table <- getMFStats()
    # retrieve most significant GO term
    MF_topSigGO_ID <- MF_GO_results_table[1, 'GO.ID']
  }
  
  # function to get most significant CC GO term
  getCCMostSig <- function(){
    # retrieve stats
    CC_GO_results_table <- getCCStats()
    # retrieve most significant GO term
    CC_topSigGO_ID <- CC_GO_results_table[1, 'GO.ID']
  }
  
  # function to create BP density plot
  createBPDensity <- function(){
    # retrieve topGOdata object
    BP_GO_data <- createBP()
    ## TO-DO: allow input GO ID from results
    # retrieve GO ID
    BP_topSigGO_ID <- getBPMostSig()
    # create density plot
    showGroupDensity(BP_GO_data, whichGO = BP_topSigGO_ID, ranks = TRUE)
  }
  
  # function to create MF density plot
  createMFDensity <- function(){
    # retrieve topGOdata object
    MF_GO_data <- createMF()
    ## TO-DO: allow input GO ID from results
    # retrieve GO ID
    MF_topSigGO_ID <- getMFMostSig()
    # create density plot
    showGroupDensity(MF_GO_data, whichGO = MF_topSigGO_ID, ranks = TRUE)
  }
  
  # function to create CC density plot
  createCCDensity <- function(){
    # retrieve topGOdata object
    CC_GO_data <- createCC()
    ## TO-DO: allow input GO ID from results
    # retrieve GO ID
    CC_topSigGO_ID <- getCCMostSig()
    # create density plot
    showGroupDensity(CC_GO_data, whichGO = CC_topSigGO_ID, ranks = TRUE)
  }
  
  # render BP density plot
  output$BPDensity <- renderPlot({
    # create plot
    createBPDensity()
  })
  
  # render MF density plot
  output$MFDensity <- renderPlot({
    # create plot
    createMFDensity()
  })
  
  # render CC density plot
  output$CCDensity <- renderPlot({
    # create plot
    createCCDensity()
  })
  
  # function to plot BP subgraphs
  createBPSubgraphs <- function(){
    # retrieve topGOdata object
    BP_GO_data <- createBP()
    # retrieve results
    BP_GO_results <- performBPGO()
    # plot subgraphs induced by the most significant GO terms
    printGraph(BP_GO_data, BP_GO_results, firstSigNodes = 5, 
             fn.prefix = "sigGO_subgraphs_BP_", useInfo = "all", pdfSW = TRUE)
  }
  
  # function to plot MF subgraphs
  createMFSubgraphs <- function(){
    # retrieve topGOdata object
    MF_GO_data <- createMF()
    # retrieve results
    MF_GO_results <- performMFGO()
    # plot subgraphs induced by the most significant GO terms
    printGraph(MF_GO_data, MF_GO_results, firstSigNodes = 5, 
               fn.prefix = "sigGO_subgraphs_MF_", useInfo = "all", pdfSW = TRUE)
  }
  
  # function to plot CC subgraphs
  createCCSubgraphs <- function(){
    # retrieve topGOdata object
    CC_GO_data <- createCC()
    # retrieve results
    CC_GO_results <- performCCGO()
    # plot subgraphs induced by the most significant GO terms
    printGraph(CC_GO_data, CC_GO_results, firstSigNodes = 5, 
               fn.prefix = "sigGO_subgraphs_CC_", useInfo = "all", pdfSW = TRUE)
  }
  
  # render BP subgraphs
  output$BPSubgraphs <- renderPlot({
    # save the plot
    #exportFile <- "sigGO_subgraphs_BP_"
    #png(exportFile)
    createBPSubgraphs()
    #dev.off()
    # Return a list
    #list(src = exportFile, alt = "Invalid Results")
  })#, deleteFile = TRUE)
  
  # render MF subgraphs
  output$MFSubgraphs <- renderPlot({
    # save the plot
    #exportFile <- "sigGO_subgraphs_MF_"
    #png(exportFile)
    createMFSubgraphs()
    #dev.off()
    # Return a list
    #list(src = exportFile, alt = "Invalid Results")
  })#, deleteFile = TRUE)
  
  # render CC subgraphs
  output$CCSubgraphs <- renderPlot({
    # save the plot
    #exportFile <- "sigGO_subgraphs_CC_"
    #png(exportFile)
    createCCSubgraphs()
    #dev.off()
    # Return a list
    #list(src = exportFile, alt = "Invalid Results")
  })#, deleteFile = TRUE)
  
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)
          
          