# load packages 
library(shiny)
library(DT)
library(shinythemes)
library(ggplot2)
library(ghibli)
library(ggVennDiagram)
library(edgeR)
library(grid)
library(ggplotify)
library(ggpubr)


# Define UI 
ui <- fluidPage(
  # view available themes
  #shinythemes::themeSelector(),
  
  # use the superhero theme
  theme = shinytheme("superhero"),
  
  # add application title
  titlePanel("Differential Gene Expression (DGE) Analysis"),
  
  # setup sidebar layout
  sidebarLayout(
    
    # setup sidebar panel
    sidebarPanel(
      
      # header for file uploads
      tags$p(
        "Upload table of gene counts (*.csv):"),
      # select a file
      fileInput("geneCountsTable", label = NULL,
                multiple = FALSE),
      # header for comparison selection
      tags$p(
        "Upload table with the experimental design (*.csv):"),
      # select a file
      fileInput("expDesignTable", label = NULL,
                multiple = FALSE),
      # show panel depending on input file
      conditionalPanel(
        condition = "output.countsUploaded && output.designUploaded",
        # request strings for factors and levels associated with samples
        # header for comparison selection
        tags$p(
          "Choose factor levels for comparison:"),
        # select variable for the first level
        selectInput(
        inputId = "levelOne",
        label = "First Level",
        choices = c("")
        ),
        # select variable for the second level
        selectInput(
        inputId = "levelTwo",
        label = "Second Level",
        choices = c("")
        )
        # horizontal line
        #tags$hr(),
        # add section header
        #tags$p(
          #"Samples & Factors.Levels:"),
        # display design table
        #tableOutput(outputId = "designTable")
        # display table of sample IDs in order of header from input table
        #tableOutput(outputId = "sampleIDs")
        #DTOutput("sampleTable")
      )
    ),

    # Output: Show plots
    mainPanel(
      conditionalPanel(
        condition = "!output.countsUploaded && !output.designUploaded",
        # placeholder text
        tags$p(
          align="center",
          HTML("<b>Getting Started</b>")),
        tags$p(
          HTML("<b>Hello!</b>"),
          HTML("Start by uploading CSV files with the gene counts and experimental design in the left-hand sidebar.")),
        tags$hr(),
        tags$p(
          "Example Gene Counts Table:"),
        tableOutput(outputId = "exampleCounts"),
        tags$hr(),
        tags$p(
          "Example Experimental Design Table:"),
        tableOutput(outputId = "exampleDesign")),
      # show panel depending on output results
      conditionalPanel(
        condition = "!output.resultsCompleted",
        tags$p(
          align="center",
          HTML("<b>Processing...</b>")
        )
      ),
      # show panel depending on output results
      conditionalPanel(
        condition = "output.resultsCompleted",
        # set of tab panels
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Data Normalization & Exploration",
            tags$p(
              align="center",
              HTML("<b>Data Normalization</b>")),
            plotOutput(outputId = "librarySizes"),
            tags$p(
              "Number of Genes with Sufficiently Large Counts"),
            tableOutput(outputId = "numNorm"),
            tags$p(
              "Normalized Gene Counts"),
            downloadButton("cpmNorm", "Download"),
            tags$hr(),
            tags$p(
              align="center",
              HTML("<b>Data Exploration</b>")),
            plotOutput(outputId = "MDS"),
            plotOutput(outputId = "heatmap"),
            plotOutput(outputId = "BCV")),
          tabPanel(
            "Pairwise Analysis", 
            tags$p(
              align="center",
              HTML("<b>Pairwise Comparison</b>")),
            span(textOutput(outputId = "pairwise"), align="center"),
            tags$p(
              "Number of Differentially Expressed Genes"),
            tableOutput(outputId = "pairwiseSummary"),
            tags$p(
              "Differentially Expressed Genes"),
            downloadButton("pairwiseResults", "Download"),
            tags$hr(),
            tags$p(
              align="center",
              HTML("<b>Results Exploration</b>")),
            plotOutput(outputId = "MD"),
            plotOutput(outputId = "volcano")),
          tabPanel(
            "Reference",
            tags$p(
              "The RNA-seq data and background information was obtained from",
              tags$a("ScienceDirect", href = "https://www.sciencedirect.com/science/article/pii/S0147651319302684"), "and",
              tags$a("NCBI", href = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA504739/"), "."
            ),
            tags$p(
              "Gene tables were created by processing the RNA-seq data as described in the", 
              tags$a("Bioinformatics Analysis of Omics Data with the Shell & R", href = "https://morphoscape.wordpress.com/2022/07/28/bioinformatics-analysis-of-omics-data-with-the-shell-r/"), "."
            ),
            tags$p(
              "A tutorial of the analysis performed in this application is provided in the", 
              tags$a("Downstream Bioinformatics Analysis of Omics Data with edgeR", href = "https://morphoscape.wordpress.com/2022/08/09/downstream-bioinformatics-analysis-of-omics-data-with-edger/"), "."
            )
          )
        )
      )
    )
  )
)

# Define server 
server <- function(input, output, session) {
  ##
  # Data Setup
  ##
  # retrieve the vector of colors associated with PonyoMedium
  ghibli_colors <- ghibli_palette("PonyoMedium", type = "discrete")
  
  # render example gene counts table
  output$exampleCounts <- renderTable({
    # create example counts table
    exCountsTable <- data.frame(
      Gene = c("gene-1", "gene-2", "gene-3"),
      SampleOne = c(0, 0, 0),
      SampleTwo = c(10, 20, 30),
      SampleThree = c(111, 222, 333),
      SampleFour = c(1, 2, 3),
      SampleFive = c(0, 0, 0),
      SampleSix = c(1000, 2000, 3000),
      SampleSeven = c(11, 12, 13),
      SampleEight = c(0, 0, 0)
    )
  })
  
  # render example gene counts table
  output$exampleDesign <- renderTable({
    # create example counts table
    exDesignTable <- data.frame(
      Sample = c("SampleOne", "SampleTwo", "SampleThree", "SampleFour", "SampleFive", "SampleSix", "SampleSeven", "SampleEight"),
      Group = c("cntrl.high", "cntrl.high", "cntrl.low", "cntrl.low", "treat.high", "treat.high", "treat.low", "treat.low")
    )
  })
  
  # retrieve input data
  inputGeneCounts <- reactive({
    # check the input table
    if(is.null(input$geneCountsTable)){
      return(NULL)
    }
    # check data type
    #if(is.integer(input$geneCountsTable)){
      # read the file
      #read.csv(file = input$geneCountsTable$datapath, row.names=1)
      # test with subset of data
      head(read.csv(file = input$geneCountsTable$datapath, row.names=1), n = 1000)
    #} else {
      #return(NULL)
    #}
  })
  
  # check if file has been uploaded
  output$countsUploaded <- reactive({
    return(!is.null(inputGeneCounts()))
  })
  outputOptions(output, 'countsUploaded', suspendWhenHidden=FALSE)
  
  # retrieve input data
  inputExpDesign <- reactive({
    # check the input table
    if(is.null(input$expDesignTable)){
      return(NULL)
    } 
    # check data type
    #if(is.character(input$geneCountsTable)){
      # import grouping factor
      targets <- read.csv(input$expDesignTable$datapath, row.names=1)
      # setup a design matrix
      #factor(paste(targets[,1],targets[,2],sep="."))
      factor(targets[,1])
    #} else {
      #return(NULL)
    #}
  })
  
  # check if file has been uploaded
  output$designUploaded <- reactive({
    return(!is.null(inputExpDesign()))
  })
  outputOptions(output, 'designUploaded', suspendWhenHidden=FALSE)
  
  # update select inputs for comparisons
  observe({
    # retrieve input design table
    group <- levels(inputExpDesign())
  
    # update and set the first select items
    updateSelectInput(session, "levelOne",
                      choices = group,
    )
    
    # update and set the second select items
    updateSelectInput(session, "levelTwo",
                      choices = group,
                      selected = tail(group, 1)
    )
  })
  
  # render experimental design table
  output$designTable <- renderTable({
    # retrieve input design table
    group <- inputExpDesign()
    # retrieve input gene counts table
    geneCounts <- inputGeneCounts()
    # retrieve column names
    sampleNames <- colnames(geneCounts)
    # create data frame
    design <- data.frame(
      Sample = sampleNames,
      Factor.Level = group
    )
  })
  
  # render table of sample IDs
  #output$sampleIDs <- renderTable({
    # retrieve input gene counts table
    #geneCounts <- inputGeneCounts()
    # retrieve column names
    #sampleNames <- colnames(geneCounts)
    # add header
    #c("Samples", sampleNames)
  #}, colnames = FALSE)
  
  # update table of sample IDs and factors
  #sampleValues <- reactiveValues(data = {
    # read the column names of the input file
    #samples <- colnames(inputGeneCounts())
    #samples <- colnames(read.csv(file = input$geneCountsTable$datapath, row.names=1))
    #samples <- reactive({
      #colnames(read.csv(file = input$geneCountsTable$datapath, row.names=1))
    #})
    # create a data frame for storing the factors and levels for each sample
    #input_data <- data.frame(
      #Sample = samples(),
      #Factor = rep("", length(samples()))
    #)
    # return data frame
    #input_data
  #})
  
  # output the data table based on the data frame (and make it editable)
  #output$sampleTable <- renderDT({
    # create data table
    #DT::datatable(sampleValues$data, editable = TRUE)
  #})
  
  # when there is any edit to a cell, write that edit to the initial data frame
  #observeEvent(input$sampleTable_cell_edit, {
    # get values
    #info = input$sampleTable_cell_edit
    #rowNum = as.numeric(info$row)
    #colNum = as.numeric(info$col)
    #cellVal = as.numeric(info$value)
    # write values to reactive
    #sampleValues$data[rowNum,colNum] <- cellVal
  #})
  
  # render text with pairwise comparison
  output$pairwise <- renderText({
    # create string with factor levels
    paste(input$levelTwo, input$levelOne, sep = " vs ")
  })

  ##
  # Data Normalization & Exploration
  ##
  
  # download table with number of filtered genes
  output$cpmNorm <- downloadHandler(
    # retrieve file name
    filename = function() {
      # setup output file name
      paste("normalizedCounts", "csv", sep = ".")
    },
    # read in data
    content = function(file) {
      # retrieve input design table
      group <- inputExpDesign()
      # begin to construct the DGE list object
      geneCounts <- inputGeneCounts()
      list <- DGEList(counts=geneCounts,group=group)
      # compute counts per million (CPM) using normalized library sizes
      normList <- cpm(list, normalized.lib.sizes=TRUE)
      # output table
      write.table(normList, file, sep=",", row.names=TRUE)
    }
  )
  
  # render plot of library sizes before normalization
  output$librarySizes <- renderPlot({
    # retrieve input design table
    group <- inputExpDesign()
    # begin to construct the DGE list object
    geneCounts <- inputGeneCounts()
    list <- DGEList(counts=geneCounts,group=group)
    # create barplot of library sizes before normalization
    barplot(list$samples$lib.size*1e-6, names=1:12, ylab="Library size (millions)", main = "Library Sizes Before Normalization")
  })
  
  # render table with number of filtered genes
  output$numNorm <- renderTable({
    # retrieve input design table
    group <- inputExpDesign()
    # begin to construct the DGE list object
    geneCounts <- inputGeneCounts()
    list <- DGEList(counts=geneCounts,group=group)
    # filter the list of gene counts based on expression levels
    keep <- filterByExpr(list)
    # view the number of filtered genes
    table(keep)
  }, colnames = FALSE)
  
  # render MDS plot
  output$MDS <- renderPlot({
    # retrieve input design table
    group <- inputExpDesign()
    # begin to construct the DGE list object
    geneCounts <- inputGeneCounts()
    list <- DGEList(counts=geneCounts,group=group)
    # filter the list of gene counts based on expression levels
    keep <- filterByExpr(list)
    # remove genes that are not expressed in either experimental condition
    list <- list[keep, , keep.lib.sizes=FALSE]
    # calculate scaling factors
    list <- calcNormFactors(list)
    # vector of shape numbers for the MDS plot
    points <- c(0,1,15,16)
    # vector of colors for the MDS plot
    colors <- rep(c(ghibli_colors[3], ghibli_colors[6]), 2)
    # add extra space to right of plot area and change clipping to figure
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    # MDS plot with distances approximating log2 fold changes
    plotMDS(list, col=colors[group], pch=points[group], main = "Multi-Dimensional Scaling (MDS) Plot")
    # place the legend outside the right side of the plot
    legend("topright", inset=c(-0.4,0), legend=levels(group), pch=points, col=colors)
  })
  
  # render heatmap of individual RNA-seq samples using moderated log CPM
  output$heatmap <- renderPlot({
    # retrieve input design table
    group <- inputExpDesign()
    # begin to construct the DGE list object
    geneCounts <- inputGeneCounts()
    list <- DGEList(counts=geneCounts,group=group)
    # filter the list of gene counts based on expression levels
    keep <- filterByExpr(list)
    # remove genes that are not expressed in either experimental condition
    list <- list[keep, , keep.lib.sizes=FALSE]
    # calculate scaling factors
    list <- calcNormFactors(list)
    # calculate the log CPM of the gene count data
    logcpm <- cpm(list, log=TRUE)
    # create heatmap of individual RNA-seq samples using moderated log CPM
    heatmap(logcpm, main = "Heatmap of RNA-seq Samples Using Moderated Log CPM")
  })
  
  # render plot of dispersion estimates and biological coefficient of variation
  output$BCV <- renderPlot({
    # retrieve input design table
    group <- inputExpDesign()
    # begin to construct the DGE list object
    geneCounts <- inputGeneCounts()
    list <- DGEList(counts=geneCounts,group=group)
    # filter the list of gene counts based on expression levels
    keep <- filterByExpr(list)
    # remove genes that are not expressed in either experimental condition
    list <- list[keep, , keep.lib.sizes=FALSE]
    # calculate scaling factors
    list <- calcNormFactors(list)
    # estimate common dispersion and tagwise dispersions to produce a matrix of pseudo-counts
    list <- estimateDisp(list)
    # create BCV plot
    plotBCV(list, main = "Biological Coefficient of Variation (BCV) Plot")
  })
  
  ##
  # Pairwise Comparisons (Contrasts)
  ##
  
  # render table of DE genes
  pairwiseTest <- reactive({
    # retrieve input design table
    group <- inputExpDesign()
    # begin to construct the DGE list object
    geneCounts <- inputGeneCounts()
    list <- DGEList(counts=geneCounts,group=group)
    # filter the list of gene counts based on expression levels
    keep <- filterByExpr(list)
    # remove genes that are not expressed in either experimental condition
    list <- list[keep, , keep.lib.sizes=FALSE]
    # calculate scaling factors
    list <- calcNormFactors(list)
    # estimate common dispersion and tagwise dispersions to produce a matrix of pseudo-counts
    list <- estimateDisp(list)
    # perform exact test
    exactTest(list, pair=c(input$levelOne, input$levelTwo))
  })  
  
  # check if results output
  output$resultsCompleted <- reactive({
    return(!is.null(pairwiseTest()))
  })
  outputOptions(output, 'resultsCompleted', suspendWhenHidden=FALSE)
  
  # render table of DE genes
  output$pairwiseSummary <- renderTable({
    # perform exact test
    tested <- pairwiseTest()
    # view the total number of differentially expressed genes at a p-value of 0.05
    summary(decideTests(tested))
  }, colnames = FALSE)
  
  # render plot of log-fold change against log-counts per million with DE genes highlighted
  output$MD <- renderPlot({
    # perform exact test
    tested <- pairwiseTest()
    # create MD plot
    plotMD(tested, main = "Mean-Difference (MD) Plot")
    # add blue lines to indicate 2-fold changes
    abline(h=c(-1, 1), col="blue")
  })
  
  # render volcano plot
  output$volcano <- renderPlot({
    # perform exact test
    tested <- pairwiseTest()
    # create a results table of DE genes
    resultsTbl <- topTags(tested, n=nrow(tested$table), adjust.method="fdr")$table
    # add column for identifying direction of DE gene expression
    resultsTbl$topDE <- "NA"
    # identify significantly up DE genes
    resultsTbl$topDE[resultsTbl$logFC > 1 & resultsTbl$FDR < 0.05] <- "Up"
    # identify significantly down DE genes
    resultsTbl$topDE[resultsTbl$logFC < -1 & resultsTbl$FDR < 0.05] <- "Down"
    # vector with a subset of colors associated with PonyoMedium
    ghibli_subset <- c(ghibli_colors[3], ghibli_colors[6], ghibli_colors[4])
    # create volcano plot
    ggplot(data=resultsTbl, aes(x=logFC, y=-log10(FDR), color = topDE)) + 
      geom_point() +
      theme_minimal() +
      scale_colour_discrete(type = ghibli_subset, breaks = c("Up", "Down")) +
      ggtitle("Volcano Plot") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face="bold"))
  })
  
  # download table with number of filtered genes
  output$pairwiseResults <- downloadHandler(
    filename = function() {
      # setup output file name
      paste(
        paste(input$levelTwo, input$levelOne, sep = "_"),
        "topDEGs.csv", 
        sep = "."
      )
    },
    content = function(file) {
      # perform exact test
      tested <- pairwiseTest()
      # view results table of top 10 DE genes
      resultsTbl <- topTags(tested, n=nrow(tested$table), adjust.method="fdr")$table
      # output table
      write.table(resultsTbl, file, sep=",", row.names=TRUE, quote=FALSE)
    }
  )
}

# create the Shiny app object 
shinyApp(ui = ui, server = server)