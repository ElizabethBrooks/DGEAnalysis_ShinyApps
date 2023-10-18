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
library(rcartocolor)

# the following setting is important, do not omit.
options(stringsAsFactors = FALSE)

# plotting palette
plotColors <- carto_pal(12, "Safe")
plotColorSubset <- c(plotColors[5], plotColors[6])

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
            imageOutput(outputId = "BPHist", height="100%", width="100%"),
            downloadButton(outputId = "downloadBPHist", label = "Download Plot"),
            imageOutput(outputId = "MFHist", height="100%", width="100%"),
            downloadButton(outputId = "downloadMFHist", label = "Download Plot"),
            imageOutput(outputId = "CCHist", height="100%", width="100%"),
            downloadButton(outputId = "downloadCCHist", label = "Download Plot"),
            plotOutput(outputId = "dotPlot"),
            downloadButton(outputId = "downloadDotPlot", label = "Download Plot"),
            plotOutput(outputId = "BPDensity"),
            downloadButton(outputId = "downloadBPDensity", label = "Download Plot"),
            plotOutput(outputId = "MFDensity"),
            downloadButton(outputId = "downloadMFDensity", label = "Download Plot"),
            plotOutput(outputId = "CCDensity"),
            downloadButton(outputId = "downloadCCDensity", label = "Download Plot"),
            downloadButton(outputId = "downloadBPSubgraphs", label = "Download Plot"),
            downloadButton(outputId = "downloadMFSubgraphs", label = "Download Plot"),
            downloadButton(outputId = "downloadCCSubgraphs", label = "Download Plot")
            # TO-DO: fix rendering of PDFs
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
    GOResults <- runTest(GO_data, statistic = 'Fisher')
  }
  
  # TO-DO: this causes additional function calls
  # check if results are complete
  output$resultsCompleted <- function(){
    if(!is.null(performGO("BP"))){
      return(TRUE)
    }
    return(FALSE)
  }
  outputOptions(output, 'resultsCompleted', suspendWhenHidden=FALSE, priority=0)
  
  # function to create BP, MF, or CC p-value histogram
  createPHist <- function(ontologyID){
    # retrieve results
    GOResults <- performGO(ontologyID)
    # store p-values as named list...
    pvalGO <- score(GOResults)
    # plot histogram to see range of p-values
    hist(pvalGO, 35, xlab = "p-values", main = "Range of GO Term P-Values")
  }
  
  # render BP p-value histogram
  output$BPHist <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_BP.png"
    png(exportFile, width = 12, height = 9, units="in", res=150)
    createPHist("BP")
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results", height = "500px")
  }, deleteFile = TRUE)
  
  # download handler for the BP histogram
  output$downloadBPHist <- downloadHandler(
    filename = function() {
      "pValueRanges_BP.png"
    },
    content = function(file) {
      # save the plot
      png(file, width = 12, height = 9, units="in", res=150)
      createPHist("BP")
      dev.off()
    }
  )
  
  # render MF p-value histogram
  output$MFHist <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_MF.png"
    png(exportFile, width = 12, height = 9, units="in", res=150)
    createPHist("MF")
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results", height = "500px")
  }, deleteFile = TRUE)
  
  # download handler for the MF histogram
  output$downloadMFHist <- downloadHandler(
    filename = function() {
      "pValueRanges_MF.png"
    },
    content = function(file) {
      # save the plot
      png(file, width = 12, height = 9, units="in", res=150)
      createPHist("MF")
      dev.off()
    }
  )
  
  # render CC p-value histogram
  output$CCHist <- renderImage({
    # save the plot
    exportFile <- "pValueRanges_CC.png"
    png(exportFile, width = 12, height = 9, units="in", res=150)
    createPHist("CC")
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results", height = "500px")
  }, deleteFile = TRUE)
  
  # download handler for the CC histogram
  output$downloadCCHist <- downloadHandler(
    filename = function() {
      "pValueRanges_CC.png"
    },
    content = function(file) {
      # save the plot
      png(file, width = 12, height = 9, units="in", res=150)
      createPHist("CC")
      dev.off()
    }
  )
  
  # function to get statistics on BP, MF, or CC GO terms
  getStats <- function(ontologyID){
    # retrieve topGOdata object
    GO_data <- createOntology(ontologyID)
    # retrieve results
    GOResults <- performGO(ontologyID)
    # retrieve statistics
    list_GO_terms <- usedGO(GO_data)
    # retrieve results table
    BP_GOResults_table <- GenTable(GO_data, weightFisher = GOResults, orderBy = 'weightFisher', 
                                    topNodes = length(list_GO_terms))
  }
  
  # TO-DO: allow user input weightFisher
  # function to get significant BP, MF, or CC GO terms
  getSig <- function(ontologyID){
    # retrieve stats
    GOResults_table <- getStats(ontologyID)
    # create table of significant GO terms
    sigGOResults_table <- GOResults_table[GOResults_table$weightFisher <= 0.05, ]
  }
  
  # function to get most significant BP, MF, or CC GO terms
  getMostSig <- function(ontologyID){
    # retrieve stats
    GOResults_table <- getStats(ontologyID)
    # retrieve most significant GO term
    topSigID <- GOResults_table[1, 'GO.ID']
  }
  
  # TO-DO: allow users to select the number of sig GO terms (to a limit)
  # function to format data for use with dot plots
  # default it top 5 most significant GO terms
  dotPlotSigData <- function(){
    # retrieve ontology result tables
    BPTable <- getSig("BP")
    MFTable <- getSig("MF")
    CCTable <- getSig("CC")
    # subset the tables
    BPTable <- BPTable[1:5, ]
    MFTable <- MFTable[1:5, ]
    CCTable <- CCTable[1:5, ]
    # add column with ontology ID
    BPPlotTable <- cbind('ID' = 'BP', BPTable)
    MFPlotTable <- cbind('ID' = 'MF', MFTable)
    CCPlotTable <- cbind('ID' = 'CC', CCTable)
    # combine all tables
    allPlotTable <- rbind(BPPlotTable, MFPlotTable, CCPlotTable)
    # remove NAs
    plotTable <- na.omit(allPlotTable)
  }
  
  # TO-DO: update x-axis title based on data or user input
  # function to create dot plots
  createDotPlot <- function(){
    # check for valid plotting data
    #if(!is.null(dotPlotSigData())){
    #return(NULL)
    #}
    # retrieve formatted data
    plotTable <- dotPlotSigData()
    # create faceting by ontology
    facet <- factor(plotTable$ID, levels = c('BP', 'CC', 'MF'))
    # create dot plot
    dotplot <- ggplot(data = plotTable, aes(x = "Enrichment", y = Term, size = Significant, color = as.numeric(weightFisher))) + 
      facet_grid(rows = facet, space = 'free_y', scales = 'free') +
      geom_point() +
      #scale_color_gradientn(colors = heat.colors(10), limits=c(0, 0.05)) + 
      scale_color_gradientn(colors = plotColorSubset) +
      #scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme_bw() +
      xlab('Effect') +
      ylab('GO Term') + 
      #scale_x_discrete(labels=c("Interaction"=expression(italic("Interaction")), parse=TRUE)) +
      labs(color = 'P-Value', size = 'Gene Rank')
    # view plot
    dotplot
  }
  
  # render dot plot
  output$dotPlot <- renderPlot({
    # create plot
    createDotPlot()
  })
  
  # TO-DO: update file names based on data subset
  # download handler for the dot plot
  output$downloadDotPlot <- downloadHandler(
    filename = function() {
      "dotPlot.png"
    },
    content = function(file) {
      # create plot
      dotPlotResults <- createDotPlot()
      # save plot
      ggsave(file, plot = dotPlotResults, device = "png")
    }
  )
  
  # TO-DO: allow input GO ID from results
  # function to create BP, MF, or CC density plots
  # default is most sig GO term
  createDensity <- function(ontologyID){
    # retrieve topGOdata object
    GO_data <- createOntology(ontologyID)
    # retrieve GO ID
    topSigID <- getMostSig(ontologyID)
    # create density plot
    showGroupDensity(GO_data, whichGO = topSigID, ranks = TRUE)
  }
  
  # render BP density plot
  output$BPDensity <- renderPlot({
    # create plot
    createDensity("BP")
  })
  
  # download handler for the BP density plot
  output$downloadBPDensity <- downloadHandler(
    filename = function() {
      "density_BP.png"
    },
    content = function(file) {
      # save the plot
      png(file, width = 12, height = 9, units="in", res=150)
      createDensity("BP")
      dev.off()
    }
  )
  
  # render MF density plot
  output$MFDensity <- renderPlot({
    # create plot
    createDensity("MF")
  })
  
  # download handler for the MF density plot
  output$downloadMFDensity <- downloadHandler(
    filename = function() {
      "density_MF.png"
    },
    content = function(file) {
      # save the plot
      png(file, width = 12, height = 9, units="in", res=150)
      createDensity("MF")
      dev.off()
    }
  )
  
  # render CC density plot
  output$CCDensity <- renderPlot({
    # create plot
    createDensity("CC")
  })
  
  # download handler for the CC density plot
  output$downloadCCDensity <- downloadHandler(
    filename = function() {
      "density_CC.png"
    },
    content = function(file) {
      # save the plot
      png(file, width = 12, height = 9, units="in", res=150)
      createDensity("CC")
      dev.off()
    }
  )
  
  # TO-DO: allow input number of sig nodes
  # function to plot BP, MF, or CC subgraphs
  createSubgraphs <- function(ontologyID){
    # retrieve topGOdata object
    GO_data <- createOntology(ontologyID)
    # retrieve results
    GOResults <- performGO(ontologyID)
    # plot subgraphs induced by the most significant GO terms
    printGraph(GO_data, GOResults, firstSigNodes = 5, 
               fn.prefix = paste(ontologyID, "sigGO_subgraphs", sep="_"), useInfo = "all", pdfSW = TRUE)
  }
  
  # download button for PDFs of BP subgraphs
  output$downloadBPSubgraphs <- downloadHandler(
    filename = function() {
      "sigGO_subgraphs_BP_weight01_5_all.pdf"
    },
    content = function(file) {
      # create subgraph PDFs
      createSubgraphs("BP")
    }
  )
  
  # download button for PDFs of MF subgraphs
  output$downloadMFSubgraphs <- downloadHandler(
    filename = function() {
      "sigGO_subgraphs_MF_weight01_5_all.pdf"
    },
    content = function(file) {
      # create subgraph PDFs
      createSubgraphs("MF")
    }
  )
  
  # download button for PDFs of CC subgraphs
  output$downloadCCSubgraphs <- downloadHandler(
    filename = function() {
      "sigGO_subgraphs_CC_weight01_5_all.pdf"
    },
    content = function(file) {
      # create subgraph PDFs
      createSubgraphs("CC")
    }
  )
  
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)
          
          