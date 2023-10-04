# load packages 
library(shiny)
#library(DT)
library(shinythemes)
library(rcartocolor)
library(WGCNA)
require(dplyr)
library(ggplot2)

# the following setting is important, do not omit.
options(stringsAsFactors = FALSE)

# Allow multi-threading within WGCNA. At present this call is necessary.
# Any error here may be ignored but you may want to update WGCNA if you see one.
# Caution: skip this line if you run RStudio or other third-party R environments.
# See note above.
#enableWGCNAThreads()

# color blind safe plotting palettes
plotColors <- carto_pal(12, "Safe")
plotColorSubset <- c(plotColors[4], plotColors[5], plotColors[6])


# Define UI 
ui <- fluidPage(
  # view available themes
  #shinythemes::themeSelector(),
  
  # use a theme
  #theme = shinytheme("yeti"),
  theme = shinytheme("superhero"),
  
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
        #condition = "output.inputCheck && (output.countsUploaded && output.designUploaded)",
        condition = "(output.countsUploaded && output.designUploaded)",
        tags$hr(),
        fluidRow(
          align = "center",
          # display input design table
          tableOutput(outputId = "designTable")
        )
      )
    ),
    
    # Output: Show plots
    mainPanel(
      
      textOutput(outputId = "testSamples"),
      textOutput(outputId = "testGenes"),
      imageOutput(outputId = "samplesOutliers", height="50%", width="50%"),
      imageOutput(outputId = "clusterSamples", height="50%", width="50%"),
      imageOutput(outputId = "plotThreshold", height="50%", width="50%")
    )
  )
)

# Define server 
server <- function(input, output, session) {
  ##
  # Example Data Setup
  ##
  
  # render example gene counts table
  output$exampleCountsOne <- renderTable({
    # create example counts table
    exCountsTable <- data.frame(
      Gene = c("gene-1", "gene-2", "gene-3", "gene-4", "gene-5"),
      SampleOne = c(0, 0, 0, 0, 0),
      SampleTwo = c(10, 20, 30, 40, 50),
      SampleThree = c(111, 222, 333, 444, 555),
      SampleFour = c(1, 2, 3, 4, 5),
      SampleFive = c(0, 0, 0, 0, 0),
      SampleSix = c(1000, 2000, 3000, 4000, 5000),
      SampleSeven = c(11, 12, 13, 14, 15),
      SampleEight = c(0, 0, 0, 0, 0)
    )
  })
  
  # render example gene counts table
  output$exampleCountsTwo <- renderTable({
    # create example counts table
    exCountsTable <- data.frame(
      Gene = c("geneA", "geneB", "geneC"),
      sample_1 = c(0, 0, 0),
      sample_2 = c(10, 20, 30),
      sample_3 = c(111, 222, 333),
      sample_4 = c(1, 2, 3),
      sample_5 = c(3, 3, 3),
      sample_6 = c(1000, 2000, 3000),
      sample_7 = c(11, 12, 13),
      sample_8 = c(1, 1, 1),
      sample_9 = c(123, 12, 1),
      sample_10 = c(3, 32, 321),
      sample_11 = c(33, 333, 33),
      sample_12 = c(2, 2, 2)
    )
  })
  
  # render first example gene counts table
  output$exampleDesignOne <- renderTable({
    # create example counts table
    exDesignTable <- data.frame(
      Sample = c("SampleOne", "SampleTwo", "SampleThree", "SampleFour", "SampleFive", "SampleSix"),
      Group = c("1", "1", "1", "2", "2", "2")
    )
  })
  
  # render second example gene counts table
  output$exampleDesignTwo <- renderTable({
    # create example counts table
    exDesignTable <- data.frame(
      Individual = c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10", "sample_11", "sample_12"),
      Treatment = c("1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2"),
      Genotype = c("1", "1", "1", "2", "2", "2", "1", "1", "1", "2", "2", "2")
    )
  })
  
  ##
  # Data Setup
  ##

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
  
  # render experimental design table
  output$designTable <- renderTable({
    # check if the input files are valid
    if(is.null(inputGeneCounts())) {
      return(NULL)
    }else if(is.null(inputDesign())) {
      return(NULL)
    }else if(is.null(compareSamples())) {
      return(NULL)
    }
    # retrieve input gene counts table
    geneCounts <- inputGeneCounts()
    # retrieve column names
    sampleNames <- colnames(geneCounts)
    # retrieve input design table
    allTraits <- inputDesign()
    # create data frame
    design <- data.frame(
      Sample = sampleNames,
      Factors = allTraits
    )
  })
  
  ##
  # Data Prep
  ##
  
  # render text with gene test results
  output$testGenes <- renderText({
    # begin to construct the DGE list object
    geneCounts <- inputGeneCounts()
    # transpose each subset
    datExpr0 = data = as.data.frame(t(geneCounts))
    names(datExpr0) = rownames(geneCounts)
    rownames(datExpr0) = names(geneCounts)
    #Check the genes across all samples
    gsg = goodSamplesGenes(datExpr0, verbose = 3)
    # remove the offending genes and samples from the data
    if (!gsg$allOK){
      # Optionally, print the gene and sample names that were removed:
      if (sum(!gsg$goodGenes)>0){
        print(paste("Removing genes:", paste(names(datExpr0)[!gsg$goodGenes], collapse = ", ")))
      }else{
        print("Input normalized counts for the genes are good.")
      }
    }else{
      print("Input normalized counts for the genes are good.")
    }
  })
  
  # reactive function to setup the data
  setupData <- reactive({
    # begin to construct the DGE list object
    geneCounts <- inputGeneCounts()
    # transpose each subset
    datExpr0 = data = as.data.frame(t(geneCounts))
    names(datExpr0) = rownames(geneCounts)
    rownames(datExpr0) = names(geneCounts)
    # return the expression data
    datExpr0
  })
  
  # reactive function to check the data
  checkData <- reactive({
    # retrieve setup data
    datExpr0 <- setupData()
    #Check the genes across all samples
    gsg = goodSamplesGenes(datExpr0, verbose = 3)
  })
  
  # reactive function to prepare the data
  prepareData <- reactive({
    # retrieve checked data
    datExpr0 <- setupData()
    # retrieve checked data results
    gsg <- checkData()
    # remove the offending genes and samples from the data
    if (!gsg$allOK){
      # Remove the offending genes and samples from the data:
      datExpr0 = datExpr0[gsg$goodSamples, gsg$goodGenes]
    }
    # return the expression data
    datExpr0
  })
  
  # render text with sample test results
  output$testSamples <- renderText({
    # retrieve prepared data
    datExpr0 <- prepareData()
    # retrieve checked data results
    gsg <- checkData()
    # remove the offending genes and samples from the data
    if (!gsg$allOK){
      if (sum(!gsg$goodSamples)>0){
        print(paste("Removing samples:", paste(rownames(datExpr0)[!gsg$goodSamples], collapse = ", ")))
      }else{
        print("Input normalized counts for the samples are good.")
      }
    }else{
      print("Input normalized counts for the samples are good.")
    }
  })
  
  # function to render clustering plot
  output$samplesOutliers <- renderImage({
    # retrieve prepared data
    datExpr0 <- prepareData()
    # cluster the samples to see if there are any obvious outliers
    sampleTree = hclust(dist(datExpr0), method = "average")
    # Plot the sample tree: Open a graphic output window of size 12 by 9 inches
    # The user should change the dimensions if the window is too large or too small.
    exportFile <- "sampleClustering.png"
    png(file = exportFile, width = 12, height = 9, units = "in", res = 150)
    sizeGrWindow(12,9)
    par(cex = 0.6)
    par(mar = c(0,4,2,0))
    plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
         cex.axis = 1.5, cex.main = 2)
    # Plot a line to show the cut
    #abline(h = 15, col = "red")
    dev.off()
    # Return a list
    list(src = exportFile, alt = "This is alternate text", height = "900px")
  }, deleteFile = TRUE)
  
  # reactive function to prepare trait data
  traitData <- reactive({
    # retrieve input design table
    allTraits <- inputDesign()
    # retrieve prepared data
    datExpr0 <- prepareData()
    ## TO-DO: allow users to specify cutoff
    # Determine cluster under the line
    #clust = cutreeStatic(sampleTree, cutHeight = 15, minSize = 10)
    #table(clust)
    # clust 1 contains the samples we want to keep.
    #keepSamples = (clust==1)
    # filter out samples
    #datExpr <- datExpr0
    #datExpr = datExpr0[keepSamples, ]
    nGenes = ncol(datExpr0)
    nSamples = nrow(datExpr0)
    # Form a data frame analogous to expression data that will hold the traits
    samples = rownames(datExpr0)
    traitRows = match(samples, rownames(allTraits))
    datTraits = allTraits[traitRows,]
    # clean up memory
    #collectGarbage()
    # return the trait data
    datTraits
  })
  
  # function to render updated clustering plot
  output$clusterSamples <- renderImage({
    # retrieve prepared data
    datExpr <- prepareData()
    # retrieve the trait data
    datTraits <- traitData()
    # Re-cluster samples
    exportFile <- "sampleDendrogram_traitHeatmap.png"
    png(file = exportFile, width = 10, height = 7, units = "in", res = 150)
    sizeGrWindow(10,7)
    sampleTree2 = hclust(dist(datExpr), method = "average")
    # Convert traits to a color representation: white means low, red means high, grey means missing entry
    traitColors = numbers2colors(datTraits, signed = FALSE)
    # Plot the sample dendrogram and the colors underneath.
    plotDendroAndColors(sampleTree2, traitColors,
                        groupLabels = names(datTraits),
                        main = "Sample dendrogram and trait heatmap")
    dev.off()
    # Return a list
    list(src = exportFile, alt = "This is alternate text", height = "700px")
  }, deleteFile = TRUE)
  
  
  ##
  # Pick Soft Thresholding Powers
  ##
  
  # reactive function to select soft powers
  selectPowers <- reactive({
    # Choose a set of soft-thresholding powers
    powers = c(c(1:10), seq(from = 12, to=36, by=2))
  })
  
  # reactive function to pick powers
  pickPowers <- reactive({
    # retrieve prepared data
    datExpr <- prepareData()
    # retrieve selected powers
    powers <- selectPowers()
    # Call the network topology analysis function
    sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)
  })
  
  # render plot with scale independence and mean connectivity
  output$plotThreshold <- renderImage({
    # retrieve soft thresholding powers
    sft <- pickPowers()
    # retrieve selected powers
    powers <- selectPowers()
    # Plot the results
    cex1 = 0.9
    exportFile <- "SoftPowers.png"
    png(file = exportFile, wi = 9, he = 5, units = "in", res = 150)
    sizeGrWindow(9, 5)
    par(mfrow = c(1,2))
    # Scale-free topology fit index as a function of the soft-thresholding power
    plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
         xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
         main = paste("Scale independence"));
    text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
         labels=powers,cex=cex1,col="red");
    # this line corresponds to using an R^2 cut-off of h
    abline(h=0.80,col="red")
    abline(h=0.90,col="blue")
    # Mean connectivity as a function of the soft-thresholding power
    plot(sft$fitIndices[,1], sft$fitIndices[,5],
         xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
         main = paste("Mean connectivity"))
    text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
    dev.off()
    # Return a list
    list(src = exportFile, alt = "This is alternate text", height = "500px")
  }, deleteFile = TRUE)
  
  
}

# create the Shiny app object 
shinyApp(ui = ui, server = server)