# load packages 
library(shiny)
library(DT)
library(shinythemes)
library(rcartocolor)
library(WGCNA)

# the following setting is important, do not omit.
options(stringsAsFactors = FALSE)



# color blind safe plotting palettes
# https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
# https://github.com/Nowosad/rcartocolor
plotColors <- carto_pal(12, "Safe")
plotColorSubset <- c(plotColors[4], plotColors[5], plotColors[6])


# Define UI 
ui <- fluidPage(
  # view available themes
  #shinythemes::themeSelector(),
  
  # use a theme
  theme = shinytheme("yeti"),
  
  # add application title
  titlePanel("Weighted Gene Co-Expression Network Analysis (WGCNA)"),
  
  # setup sidebar layout
  sidebarLayout(
    
    # setup sidebar panel
    sidebarPanel(
      
      # header for file uploads
      tags$p(
        "Upload table of gene counts (*.csv):"
      ),
      # select a file
      fileInput(
        "geneCountsTable", 
        label = NULL,
        multiple = FALSE,
        accept = ".csv"
      ),
      # header for comparison selection
      tags$p(
        "Upload table with the experimental design (*.csv):"),
      # select a file
      fileInput(
        "expDesignTable", 
        label = NULL,
        multiple = FALSE,
        accept = ".csv"
      ),
      # show panel depending on input files
      conditionalPanel(
        condition = "output.inputCheck && (output.countsUploaded && output.designUploaded) && (output.pairwiseResultsCompleted || output.glmResultsCompleted)",
        tags$p(
          "Select Analysis Type:"
        ),
        selectInput(
          inputId = "analysisType",
          label = "Analysis Type",
          choices = list("pairwise", "glm")
        ),
        # horizontal line
        tags$hr(),
        fluidRow(
          # organize tables with columns
          column(
            width = 2,
            # display table of sample IDs in order of header from input table
            tableOutput(outputId = "sampleIDs")
            #DTOutput("sampleTable")
          ),
          column(
            width = 2,
            # display input design table
            tableOutput(outputId = "designTable")
          )
        )
      )
    ),
    
    # Output: Show plots
    mainPanel(
      
    )
  )
)

# Define server 
server <- function(input, output, session) {
  ##
  # Data Setup
  ##
  
  #Set working directory
  #workingDir = args[1];
  #workingDir="/Users/bamflappy/PfrenderLab/OLYM_dMelUV/KAP4/ensembl/GCA_021134715.1/biostatistics/NetworkAnalysis/WGCN_tolerance_WGCNA"
  #setwd(workingDir)
  
  #Import normalized gene count data
  #inputTable <- read.csv(file=args[2], row.names="gene")
  #inputTable <- read.csv(file="/Users/bamflappy/PfrenderLab/OLYM_dMelUV/KAP4/ensembl/GCA_021134715.1/biostatistics/DEAnalysis/Genotypes/glmQLF_normalizedCounts.csv", row.names="gene", header=TRUE)
  
  #Subset input counts by genotype or factor
  #inputTable_subset <- inputTable[,args[3]:args[4]]
  #inputTable_subset <- inputTable[,1:24]
  
  # retrieve subsetTag tag
  #tag <- args[5]
  #tag <- "OLYM"
  
  # load in the trait data
  #allTraits = read.csv(args[6])
  #allTraits = read.csv("/Users/bamflappy/Repos/TranscriptomeAnalysisPipeline_DaphniaUVTolerance/InputData/expDesign_treatment_WGCNA_Olympics.csv")
  
  # retrieve input data
  inputGeneCounts <- reactive({
    # require input data
    req(input$geneCountsTable)
    # check the input table is not null
    if(is.null(input$geneCountsTable)){
      return(NULL)
    }
    # read the file
    geneCounts <- read.csv(file = input$geneCountsTable$datapath, row.names=1)
  })
  
  # check if file has been uploaded
  output$countsUploaded <- reactive({
    return(!is.null(inputGeneCounts()))
  })
  outputOptions(output, 'countsUploaded', suspendWhenHidden=FALSE)
  
  
  # check input counts type
  countsType <- reactive({
    # retrieve input gene counts
    geneCounts <- inputGeneCounts()    
    # loop over each data frame column
    for(i in 1:ncol(geneCounts)) { 
      # check data type
      if(!is.integer(geneCounts[,i])){
        return(NULL)
      }
    }
    # return the counts table
    geneCounts
  })
  
  # retrieve input data
  inputDesign <- reactive({
    # require input data
    req(input$expDesignTable)
    # check the input table is not null
    if(is.null(input$expDesignTable)){
      return(NULL)
    }
    # import grouping factor
    targets <- read.csv(input$expDesignTable$datapath, row.names=1)
  })
  
  # check if file has been uploaded
  output$designUploaded <- reactive({
    return(!is.null(inputDesign()))
  })
  outputOptions(output, 'designUploaded', suspendWhenHidden=FALSE)
  
  # check input design type
  designFactors <- reactive({
    # check if the input files are valid
    if(is.null(inputGeneCounts())) {
      return(NULL)
    }else if(is.null(inputDesign())) {
      return(NULL)
    }else if(is.null(compareSamples())) {
      return(NULL)
    }
    # retrieve input design
    targets <- inputDesign()
    # check data type of the first column of sample names
    if(!is.character(targets[,1])){
      return(NULL)
    }
    # setup a design matrix
    #factor(paste(targets[,1],targets[,2],sep="."))
    factor(targets[,1])
  })
  
  # compare input design and counts samples
  compareSamples <- reactive({
    # retrieve input design samples
    targets <- inputDesign()
    designSamples <- data.frame(ID1 = targets[,1])
    # retrieve input gene counts samples
    geneCounts <- inputGeneCounts() 
    countsSamples <- data.frame(ID2 = colnames(geneCounts))
    # first simply check if the number of samples matches
    if(nrow(designSamples) != nrow(countsSamples)) return(NULL)
    # find samples in counts, but not in design
    mismatch_counts <- designSamples %>% 
      filter(!designSamples$ID1 %in% countsSamples$ID2)
    # find samples in design, but not in counts
    mismatch_design <- countsSamples %>% 
      filter(!countsSamples$ID2 %in% designSamples$ID1)
    # check total non matches
    totalMismatches <- nrow(mismatch_counts) + nrow(mismatch_design)
    # check if all matched
    if(totalMismatches == 0){
      return(NULL) # all matched
    }else{
      return(TRUE) # there were mismatches
    }
  })
  
  # check if file has been uploaded
  output$inputCheck <- reactive({
    if(is.null(inputGeneCounts())) {
      return(NULL)
    }else if(is.null(inputDesign())) {
      return(NULL)
    }else if(is.null(compareSamples())) {
      return(NULL)
    }
    return(TRUE)
  })
  outputOptions(output, 'inputCheck', suspendWhenHidden=FALSE)
  
  ##
  # Data Prep
  ##
  
  # reactive function to prepare data
  prepareData <- reactive({
    # transpose each subset
    datExpr0 = data = as.data.frame(t(inputTable_subset))
    names(datExpr0) = rownames(inputTable_subset)
    rownames(datExpr0) = names(inputTable_subset)
    
    #Check the genes across all samples
    gsg = goodSamplesGenes(datExpr0, verbose = 3)
    gsg$allOK
    
    # remove the offending genes and samples from the data
    if (!gsg$allOK){
      # Optionally, print the gene and sample names that were removed:
      if (sum(!gsg$goodGenes)>0)
        printFlush(paste("Removing genes:", paste(names(datExpr0)[!gsg$goodGenes], collapse = ", ")));
      if (sum(!gsg$goodSamples)>0)
        printFlush(paste("Removing samples:", paste(rownames(datExpr0)[!gsg$goodSamples], collapse = ", ")));
      # Remove the offending genes and samples from the data:
      datExpr0 = datExpr0[gsg$goodSamples, gsg$goodGenes]
    }
  })
  
  # function to render clustering plot
  clusterSamples <- renderPlot({
    # retrieve prepared data
    datExpr0 <- prepareData()
    # cluster the samples to see if there are any obvious outliers
    sampleTree = hclust(dist(datExpr0), method = "average")
    # Plot the sample tree: Open a graphic output window of size 12 by 9 inches
    # The user should change the dimensions if the window is too large or too small.
    exportFile <- paste(tag, "sampleClustering.png", sep="_")
    png(file = exportFile, width = 12, height = 9, units="in", res=150)
    sizeGrWindow(12,9)
    par(cex = 0.6)
    par(mar = c(0,4,2,0))
    plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
         cex.axis = 1.5, cex.main = 2)
    # Plot a line to show the cut
    #abline(h = 15, col = "red")
    dev.off()
  })
  
  ## TO-DO: allow users to specify cutoff
  # Determine cluster under the line
  #clust = cutreeStatic(sampleTree, cutHeight = 15, minSize = 10)
  #table(clust)
  # clust 1 contains the samples we want to keep.
  #keepSamples = (clust==1)
  
  # reactive function to prepare trait data
  traitData <- reactive({
    # retrieve prepared data
    datExpr0 <- prepareData()
    # filter out samples
    datExpr <- datExpr0
    #datExpr = datExpr0[keepSamples, ]
    nGenes = ncol(datExpr)
    nSamples = nrow(datExpr)
    
    # Form a data frame analogous to expression data that will hold the traits
    samples = rownames(datExpr)
    traitRows = match(samples, allTraits$sample)
    datTraits = allTraits[traitRows, -1]
    rownames(datTraits) = allTraits[traitRows, 1]
    
    # clean up memory
    collectGarbage()
  })
  
  # function to render updated clustering plot
  clusterSamples <- renderPlot({
    # Re-cluster samples
    exportFile <- paste(tag, "sampleDendrogram_traitHeatmap.png", sep="_")
    png(file = exportFile, width = 10, height = 7, units="in", res=150)
    sizeGrWindow(10,7)
    sampleTree2 = hclust(dist(datExpr), method = "average")
    # Convert traits to a color representation: white means low, red means high, grey means missing entry
    traitColors = numbers2colors(datTraits, signed = FALSE)
    # Plot the sample dendrogram and the colors underneath.
    plotDendroAndColors(sampleTree2, traitColors,
                        groupLabels = names(datTraits),
                        main = "Sample dendrogram and trait heatmap")
    dev.off()
  })
  
  
}

# create the Shiny app object 
shinyApp(ui = ui, server = server)