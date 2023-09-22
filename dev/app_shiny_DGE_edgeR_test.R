# load packages 
library(shiny)
library(DT)
library(shinythemes)
library(ggplot2)
library(rcartocolor)
library(ggVennDiagram)
library(edgeR)
library(grid)
library(ggplotify)
library(ggpubr)
require(dplyr)

# color blind safe plotting palettes
plotColors <- carto_pal(12, "Safe")
plotColorSubset <- c(plotColors[4], plotColors[5], plotColors[6])

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
        "Upload table of gene counts (*.csv):"
      ),
      # select a file
      fileInput(
        "geneCountsTable", 
        label = NULL,
        multiple = FALSE
      ),
      # header for comparison selection
      tags$p(
        "Upload table with the experimental design (*.csv):"),
      # select a file
      fileInput(
        "expDesignTable", 
        label = NULL,
        multiple = FALSE
      ),
      # show panel depending on input files
      conditionalPanel(
        condition = "output.inputCheck",
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
      
      # getting started text
      conditionalPanel(
        condition = "!(output.countsUploaded && output.designUploaded)",
        tags$h1("Getting Started", align = "center"),
        tags$br(),
        tags$p(
          HTML("<b>Hello!</b>"),
          "Start by uploading CSV files with the gene counts and experimental design in the left-hand sidebar."
        ),
        tags$br(),
        tags$p(
          "Note that the DGE analysis results and plots may take several moments to process depending on the size of the input gene counts table."
        ),
        tags$br(),
        tags$p(
          "Example gene counts and experimental design tables are displayed below."
        ),
        tags$hr(),
        HTML("<b>Example</b> gene counts table of six samples and five genes:"),
        tableOutput(outputId = "exampleCountsOne"),
        HTML("<b>Example</b> gene counts table of twelve samples and three genes:"),
        tableOutput(outputId = "exampleCountsTwo"),
        tags$hr(),
        fluidRow(
          column(
            width = 6,
            HTML("<b>Example</b> experimental design table of six samples and one factor with two levels:"),
            tableOutput(outputId = "exampleDesignOne"), 
          ),
          column(
            width = 6,
            HTML("<b>Example</b> experimental design table of twelve samples and two factors each with two levels:"),
            tableOutput(outputId = "exampleDesignTwo") 
          ),
        )
      ),
      
      # error text
      conditionalPanel(
        condition = "!output.inputCheck && (output.countsUploaded && output.designUploaded)",
        tags$h1(
          "Error", 
          align="center"
        ),
        tags$br(),
        tags$p(
          "The data in the uploaded file(s) are not of the correct type or the sample names do not match.",
        ),
        tags$br(),
        tags$p(
          HTML("<b>Tip 1:</b> The input gene counts table is expected to contain <b>numeric</b> values."),
        ),
        tags$p(
          HTML("<b>Tip 2:</b> Sample names contained in the first column of the experimental design table are expected to be <b>character</b> values.")
        ),
        tags$p(
          HTML("<b>Tip 3:</b> Sample names in the first line of the gene counts table <b>must match</b> the sample names contained in the first column of the experimental design table.")
        ),
        tags$br(),
        tags$p(
          "Please check that each of the input files were uploaded correctly in the left-hand side bar."
        ),
        tags$p(
          HTML("<b>Allow a moment for processing</b> after uploading new input file(s).")
        ),
      ),
      
      # processing text
      conditionalPanel(
        condition = "output.inputCheck && (output.countsUploaded && output.designUploaded) && !(output.pairwiseResultsCompleted || output.glmResultsCompleted)",
        #condition = "(output.compareCheck && output.countsCheck && output.designCheck) && !(output.pairwiseResultsCompleted || output.glmResultsCompleted)",
        tags$h1(
          "Processing", 
          align="center"
        ),
        tags$br(),
        "The DGE analysis results and plots may take several moments to process depending on the size of the input gene counts or experimental design tables."
      ),
      
      # results text and plots
      conditionalPanel(
        condition = "output.inputCheck && (output.countsUploaded && output.designUploaded) && (output.pairwiseResultsCompleted || output.glmResultsCompleted)",
        #condition = "(output.compareCheck && output.countsCheck && output.designCheck) && (output.pairwiseResultsCompleted || output.glmResultsCompleted)",
        # set of tab panels
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tips",
            tags$p(
              align="center",
              HTML("<b>Helpful Tips</b>")
            ),
            tags$p(
              HTML("<b>Tip 1:</b> The results may take several moments to appear depending on the size of the input gene counts table.")
            ),
            tags$p(
              HTML("<b>Tip 2:</b> Navigate to the data exploration or analysis results by clicking the tabs above.")
            ),
            tags$p(
              HTML("<b>Tip 3:</b> It is possible to change the type of analysis in the left-hand sidebar.")
            ),
            tags$p(
              HTML("<b>Tip 4:</b> It is possible to change the comparison for an analysis on the <b>Analysis Results</b> tab above.")
            ),
            tags$p(
              HTML("<b>Tip 5:</b> It is possible to change the input gene counts or experimental design tables in the left-hand sidebar.")
            )
          ),
          
          # data normalization and exploration tab
          tabPanel(
            "Data Normalization & Exploration",
            tags$p(
              align="center",
              HTML("<b>Data Normalization</b>")
            ),
            plotOutput(outputId = "librarySizes"),
            downloadButton(outputId = "downloadLibrarySizes", label = "Download Plot"),
            tags$p(
              "The plot of library sizes shows the sequencing library size for each sample before TMM normalization."
            ),
            tags$br(),
            tags$p(
              HTML("<b>Number of Genes with Sufficiently Large Counts:</b>")
            ),
            tableOutput(outputId = "numNorm"),
            tags$p(
              "Filtering is performed to remove genes that were identified as not sufficiently expressed under the experimental conditions."
            ),
            tags$br(),
            tags$p(
              HTML("<b>Normalized Gene Counts Table:</b>")
            ),
            downloadButton(outputId = "cpmNorm", label = "Download Table"),
            tags$p(
              "Normalized values were calcuated in counts per million (CPM) using the normalized library sizes.",
              "The normalization method used with edgeR was the Trimmed Mean of M-values (TMM).",
              "Note that TMM normalization factors do not take into account library sizes."
            ),
            tags$hr(),
            tags$p(
              align="center",
              HTML("<b>Data Exploration</b>")
            ),
            plotOutput(outputId = "MDS"),
            downloadButton(outputId = "downloadMDS", label = "Download Plot"),
            tags$p(
              "In a multidimensional scaling (MDS) plot the distances between samples approximate the expression differences.",
              "The expression differences were calculated as the the average of the largest (leading) absolute log-fold changes between each pair of samples.",
              "Note that the points are replaced by the sample name and colored by the associated factor level (e.g., cntrl or treat)."
            ),
            tags$br(),
            plotOutput(outputId = "heatmap"),
            downloadButton(outputId = "downloadHeatmap", label = "Download Plot"),
            tags$p(
              "The heatmap uses hierarchical clustering of the individual samples by the log2 CPM expression values.",
              "Furthermore, the log2 CPM that has undefined values avoided and poorly defined log fold changes (logFC) for low counts shrunk towards zero"
            ),
            tags$br(),
            plotOutput(outputId = "BCV"),
            downloadButton(outputId = "downloadBCV", label = "Download Plot"),
            tags$p(
              "The biological coefficient of variation (BCV) plot is the square root of the dispersion parameter under the negative binomial model and is equivalent to estimating the dispersions of the negative binomial model."
            )
          ),
          
          # analysis results tab
          tabPanel(
            "Analysis Results", 
            # show pairwise results
            conditionalPanel(
              condition = "input.analysisType == 'pairwise'",
              tags$p(
                align="center",
                HTML("<b>Pairwise Comparison</b>")
              ),
              span(
                textOutput(outputId = "pairwiseComparison"), 
                align="center"
              ),
              tags$br(),
              # request strings for factors and levels associated with samples
              # header for comparison selection
              tags$p(
                "Choose factor levels for comparison:"
              ),
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
              ),
              # radio button to toggle showing analysis results
              #radioButtons(
                #"runPairwise", 
                #"Show results?",
                #c("Yes" = "show", "No" = "noshow")
              #),
              tags$hr(),
              # show glm results
              conditionalPanel(
                condition = "output.pairwiseResultsCompleted",
                #condition = "(input.runPairwise == 'show') && output.pairwiseResultsCompleted",
                #tags$hr(),
                tags$p(
                  HTML("<b>Number of Differentially Expressed Genes:</b>")
                ),
                tableOutput(outputId = "pairwiseSummary"),
                tags$br(),
                tags$p(
                  HTML("<b>Differentially Expressed Genes Table:</b>")
                ),
                downloadButton(outputId = "pairwiseResults", label = "Download Table"),
                tags$p(
                  "A comparison or contrast is a linear combination of means for a group of samples.",
                  "It is common to consider genes with FDR adjusted p-values < 0.05 to be significantly DE."
                ),
                tags$hr(),
                tags$p(
                  align="center",
                  HTML("<b>Results Exploration</b>")
                ),
                plotOutput(outputId = "pairwiseMD"),
                downloadButton(outputId = "downloadpairwiseMD", label = "Download Plot"),
                tags$p(
                  "The mean-difference (MD) plot shows the log fold changes expression differences versus average log CPM values."
                ),
                tags$br(),
                #imageOutput(outputId = "pairwiseVolcano"),
                #uiOutput(outputId = "plotDone"),
                plotOutput(outputId = "pairwiseVolcano"),
                           #click = "pairwiseVolcano_click",
                           #dblclick = "pairwiseVolcano_dblclick",
                           #hover = "pairwiseVolcano_hover",
                           #brush = "pairwiseVolcano_brush"),
                #verbatimTextOutput(outputId = "pairwiseVolcanoInfo")
                downloadButton(outputId = "downloadpairwiseVolcano", label = "Download Plot"),
                tags$p(
                  "The volcano plot is a scatterplot that displays the association between statistical significance (e.g., p-value) and magnitude of gene expression (fold change)."
                )
              )
            ),
            # show glm comparison
            conditionalPanel(
              condition = "input.analysisType == 'glm'",
              tags$p(
                align="center",
                HTML("<b>GLM Comparison</b>")
              ),
              span(
                textOutput(outputId = "glmComparison"), 
                align="center"
              ),
              tags$br(),
              # request strings for factors and levels associated with samples
              # header for comparison selection
              tags$p(
                "Enter expression for comparison:"
              ),
              # request text input
              textInput("compareExpression", "Expression"),
              # radio button to toggle showing analysis results
              #radioButtons(
                #"runGLM", 
                #"Show results?",
                #c("Yes" = "show", "No" = "noshow")
              #),
              tags$p(
                "Valid expressions must consist of the factors contained in the input experimental design file, which is displayed in the left-hand sidebar.",
                "Examples and a description of expressions for ANOVA-like tests is availble in the",
                tags$a("edgeR manual", href = "https://www.bioconductor.org/packages/release/bioc/vignettes/edgeR/inst/doc/edgeRUsersGuide.pdf"),
                " (e.g., sections 3.2.6 & 4.4.9)."
              ),
              tags$p(
                HTML("<b>Tip!</b> Make sure that the factors used in the expression are spelled the same is in the experimental design file and shown in the left-hand sidebar.")
              ),
              tags$hr(),
              # show glm results
              conditionalPanel(
                condition = "output.glmResultsCompleted",
                #condition = "(input.runGLM == 'show') && output.glmResultsCompleted",
                tags$p(
                  HTML("<b>Number of Differentially Expressed Genes:</b>")
                ),
                tableOutput(outputId = "glmSummary"),
                tags$br(),
                tags$p(
                  HTML("<b>Differentially Expressed Genes Table:</b>")
                ),
                downloadButton(outputId = "glmResults", label = "Download Table"),
                tags$p(
                  "The GLM was used to perform ANOVA-like analysis to identify any significant main effect associated with an explanatory variable.",
                  "An explanatory variable may be a categorical factor with two or more levels, such as treat and cntrl.",
                  "It is common to consider genes with FDR adjusted p-values < 0.05 to be significantly DE."
                ),
                tags$hr(),
                tags$p(
                  align="center",
                  HTML("<b>Model Exploration</b>")
                ),
                tags$br(),
                plotOutput(outputId = "glmDispersions"),
                downloadButton(outputId = "downloadGLMDispersions", label = "Download Plot"),
                tags$p(
                  "Above is a plot of the genewise quasi-likelihood (QL) dispersion against the log2 CPM gene expression levels.",
                  "Dispersion estimates are obtained after fitting negative binomial models and calculating dispersion estimates."
                ),
                tags$hr(),
                tags$p(
                  align="center",
                  HTML("<b>Results Exploration</b>")
                ),
                tags$br(),
                plotOutput(outputId = "glmMD"),
                downloadButton(outputId = "downloadGLMMD", label = "Download Plot"),
                tags$p(
                  "The mean-difference (MD) plot shows the log fold changes expression differences versus average log CPM values."
                ),
                tags$br(),
                plotOutput(outputId = "glmVolcano"),
                downloadButton(outputId = "downloadGLMVolcano", label = "Download Plot"),
                tags$p(
                  "The volcano plot is a scatterplot that displays the association between statistical significance (e.g., p-value) and magnitude of gene expression (fold change)."
                )
              )
            )
          ),
            
          # information tab
          tabPanel(
            "Information",
            tags$p(
              align="center",
              HTML("<b>Helpful Information</b>")
            ),
            tags$p(
              "This application for DGE analysis was created by",
              tags$a("Elizabeth Brooks",href = "https://www.linkedin.com/in/elizabethmbrooks/"),
              "."
            ),
            tags$p(
              "The latest version of this application may be downloaded from",
              tags$a("GitHub",href = "https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps"),
              "."
            ),
            tags$p(
              "Example gene counts and experimental design tables are also provided on",
              tags$a("GitHub", href = "https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps/tree/main/data"),
              "."
            ),
            tags$p(
              "An example RNA-seq data set may be obtained from",
              tags$a("ScienceDirect", href = "https://www.sciencedirect.com/science/article/pii/S0147651319302684"), "and",
              tags$a("NCBI", href = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA504739/"), 
              "."
            ),
            tags$p(
              "Gene tables were may be created from RNA-seq data as described in ", 
              tags$a("Bioinformatics Analysis of Omics Data with the Shell & R", href = "https://morphoscape.wordpress.com/2022/07/28/bioinformatics-analysis-of-omics-data-with-the-shell-r/"), 
              "."
            ),
            tags$p(
              "A tutorial of the biostatistical analysis performed in this application is provided in ", 
              tags$a("Downstream Bioinformatics Analysis of Omics Data with edgeR", href = "https://morphoscape.wordpress.com/2022/08/09/downstream-bioinformatics-analysis-of-omics-data-with-edger/"), 
              "."
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
      Group = c("cntrl", "cntrl", "cntrl", "treat", "treat", "treat")
    )
  })
  
  # render second example gene counts table
  output$exampleDesignTwo <- renderTable({
    # create example counts table
    exDesignTable <- data.frame(
      Individual = c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10", "sample_11", "sample_12"),
      Factors = c("cntrl.high", "cntrl.high", "cntrl.high", "cntrl.low", "cntrl.low", "cntrl.low", "treat.high", "treat.high", "treat.high", "treat.low", "treat.low", "treat.low")
    )
  })
  
  # retrieve input data
  inputGeneCounts <- reactive({
    # require input data
    req(input$geneCountsTable)
    # check the input table is not null
    ## TO-DO: and is a csv
    if(is.null(input$geneCountsTable)){
      return(NULL)
    }
    # read the file
    geneCounts <- read.csv(file = input$geneCountsTable$datapath, row.names=1)
    # test with subset of data
    #geneCounts <- head(read.csv(file = input$geneCountsTable$datapath, row.names=1), n = 4000)
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
  
  # check if file has been uploaded
  #output$countsCheck <- reactive({
    #return(!is.null(countsType()))
  #})
  #outputOptions(output, 'countsCheck', suspendWhenHidden=FALSE)
  
  # retrieve input data
  inputDesign <- reactive({
    # require input data
    req(input$expDesignTable)
    # check the input table is not null
    ## TO-DO: and is a csv
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
  
  # check if file has been uploaded
  #output$designCheck <- reactive({
    #return(!is.null(designFactors()))
  #})
  #outputOptions(output, 'designCheck', suspendWhenHidden=FALSE)
  
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
  #output$compareCheck <- reactive({
    #return(!is.null(compareSamples())) # if not null, all samples matched
  #})
  #outputOptions(output, 'compareCheck', suspendWhenHidden=FALSE)
  
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

  # update inputs for comparisons
  observe({
    # retrieve input design table
    group <- levels(designFactors())
    # update and set the first select items
    updateSelectInput(
      session, 
      "levelOne",
      choices = group,
    )
    # update and set the second select items
    updateSelectInput(
      session, 
      "levelTwo",
      choices = group,
      selected = tail(group, 1)
    )
    # create temporary expression
    tmpExpression <- paste(head(group, 1), tail(group, 1), sep = "-")
    # update and set the glm comparison expression
    updateTextInput(
      session,
      "compareExpression",
      value = tmpExpression
    )
  })
  
  # render experimental design table
  output$designTable <- renderTable({
    # retrieve input design table
    group <- designFactors()
    # retrieve input gene counts table
    geneCounts <- inputGeneCounts()
    # retrieve column names
    sampleNames <- colnames(geneCounts)
    # create data frame
    design <- data.frame(
      Sample = sampleNames,
      Factors = group
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

  ##
  # Data Normalization & Exploration
  ##
  
  # reactive function for data normalization
  normalizeData <- reactive({
    # check if the input files are valid
    if(is.null(inputGeneCounts())) {
      return(NULL)
    }else if(is.null(inputDesign())) {
      return(NULL)
    }else if(is.null(compareSamples())) {
      return(NULL)
    }
    # retrieve input design table
    group <- designFactors()
    # begin to construct the DGE list object
    geneCounts <- inputGeneCounts()
    list <- DGEList(counts=geneCounts,group=group)
  })
  
  # reactive function for normalized data filtering
  filterNorm <- reactive({
    # begin to construct the DGE list object
    list <- normalizeData()
    # filter the list of gene counts based on expression levels
    keep <- filterByExpr(list)
    # remove genes that are not expressed in either experimental condition
    list <- list[keep, , keep.lib.sizes=FALSE]
    # calculate scaling factors
    list <- calcNormFactors(list)
  })
    
  # download table with number of filtered genes
  output$cpmNorm <- downloadHandler(
    # retrieve file name
    filename = function() {
      # setup output file name
      paste("normalizedCounts", "csv", sep = ".")
    },
    # read in data
    content = function(file) {
      # begin to construct the DGE list object
      list <- normalizeData()
      # compute counts per million (CPM) using normalized library sizes
      normList <- cpm(list, normalized.lib.sizes=TRUE)
      # output table
      write.table(normList, file, sep=",", row.names=TRUE)
    }
  )
  
  # render plot of library sizes before normalization
  output$librarySizes <- renderPlot({
    # begin to construct the DGE list object
    list <- normalizeData()
    # retrieve the number of samples
    numSamples <- ncol(list)
    # save as png
    #png("librarySizesPlot.png")
    # create barplot of library sizes before normalization
    #barplot(list$samples$lib.size*1e-6, names=1:12, ylab="Library size (millions)", main = "Library Sizes Before Normalization")
    # turn off the device
    #dev.off()
    # create barplot of library sizes before normalization
    barplot(list$samples$lib.size*1e-6, names=1:numSamples, ylab="Library size (millions)", main = "Library Sizes Before Normalization")
  })
  
  # download handler for the volcano plot
  output$downloadLibrarySizes <- downloadHandler(
    filename = function() {
      "librarySizesPlot.png"
    },
    content = function(file) {
      file.copy("librarySizesPlot.png", file, overwrite=TRUE)
    }
  )
  
  # render table with number of filtered genes
  output$numNorm <- renderTable({
    # begin to construct the DGE list object
    list <- normalizeData()
    # filter the list of gene counts based on expression levels
    keep <- filterByExpr(list)
    # view the number of filtered genes
    table(keep)[2]
  }, colnames = FALSE)
  
  # render MDS plot
  output$MDS <- renderPlot({
    # retrieve input design table
    group <- designFactors()
    # calculate scaling factors
    list <- filterNorm()
    ## TO-DO: allow users to input shapes and colors or other features
    ## retrieve the number of grouping levels
    ##numLevels <- length(levels(group))
    ## vector of shape numbers for the MDS plot
    ##points <- c(0:numLevels,0:numLevels+15)
    ## vector of colors for the MDS plot
    ##colors <- rep(c(plotColorSubset[4], plotColorSubset[5]), 2)
    ## add extra space to right of plot area and change clipping to figure
    ##par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    ## MDS plot with distances approximating log2 fold changes
    ##plotMDS(list, col=colors[group], pch=points[group], main = "Multi-Dimensional Scaling (MDS) Plot")
    ## place the legend outside the right side of the plot
    ##legend("topright", inset=c(-0.1,0), legend=levels(group), pch=points, col=colors)
    # retrieve the number of grouping levels
    #numLevels <- length(levels(group))
    # setup chapes
    #points <- c(1:numLevels-1)
    # setup colors
    colors <- as.numeric(group)
    # add extra space to right of plot area and change clipping to figure
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    # MDS plot with distances approximating log2 fold changes
    plotMDS(list, col=colors, main = "Multi-Dimensional Scaling (MDS) Plot")
    # place the legend outside the right side of the plot
    #legend("topright", inset=c(-0.1,0), legend=levels(group), fill=colors)
  })
  
  # download handler for the volcano plot
  output$downloadMDS <- downloadHandler(
    filename = function() {
      "MDSPlot.png"
    },
    content = function(file) {
      file.copy("MDSPlot.png", file, overwrite=TRUE)
    }
  )
  
  # render heatmap of individual RNA-seq samples using moderated log CPM
  output$heatmap <- renderPlot({
    # calculate scaling factors
    list <- filterNorm()
    # calculate the log CPM of the gene count data
    logcpm <- cpm(list, log=TRUE)
    # save the plot
    #png("heatmapPlot.png")
    # create heatmap of individual RNA-seq samples using moderated log CPM
    #heatmap(logcpm, main = "Heatmap of RNA-seq Samples Using Moderated Log CPM")
    # turn off the device
    #dev.off()
    # create heatmap of individual RNA-seq samples using moderated log CPM
    heatmap(logcpm, main = "Heatmap of RNA-seq Samples Using Moderated Log CPM")
  })
  
  # download handler for the volcano plot
  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      "heatmapPlot.png"
    },
    content = function(file) {
      file.copy("heatmapPlot.png", file, overwrite=TRUE)
    }
  )
  
  # render plot of dispersion estimates and biological coefficient of variation
  output$BCV <- renderPlot({
    # calculate scaling factors
    list <- filterNorm()
    # estimate common dispersion and tagwise dispersions to produce a matrix of pseudo-counts
    list <- estimateDisp(list)
    # save the plot
    #png("BCVPlot.png")
    # create BCV plot
    #plotBCV(list, main = "Biological Coefficient of Variation (BCV) Plot")
    # trun off the device
    #dev.off()
    # create BCV plot
    plotBCV(list, main = "Biological Coefficient of Variation (BCV) Plot")
  })
  
  # download handler for the volcano plot
  output$downloadBCV <- downloadHandler(
    filename = function() {
      "BCVPlot.png"
    },
    content = function(file) {
      file.copy("BCVPlot.png", file, overwrite=TRUE)
    }
  )
  
  ##
  # Pairwise Comparisons (Contrasts)
  ##
  
  # render text with pairwise comparison
  output$pairwiseComparison <- renderText({
    # check the analysis type
    #if(analysisType$data != "pairwise") return()
    # require input data
    req(input$levelOne)
    req(input$levelTwo)
    # create string with factor levels
    paste(input$levelTwo, input$levelOne, sep = " vs ")
  })
  
  # function to calculate table of DE genes
  pairwiseTest <- reactive({
    # check the analysis type
    #if(is.null(analysisType$data != "pairwise") return()
    # require input data
    req(input$levelOne)
    req(input$levelTwo)
    # begin to construct the DGE list object
    # calculate scaling factors
    list <- filterNorm()
    # estimate common dispersion and tagwise dispersions to produce a matrix of pseudo-counts
    list <- estimateDisp(list)
    # perform exact test
    exactTest(list, pair=c(input$levelOne, input$levelTwo))
  })  
    
  # check if file has been uploaded
  output$pairwiseResultsCompleted <- reactive({
    if(is.null(pairwiseTest())){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  outputOptions(output, 'pairwiseResultsCompleted', suspendWhenHidden=FALSE, priority=0)
  
  # render results summary
  output$pairwiseSummary <- renderTable({
    # perform exact test
    tested <- pairwiseTest()
    # view the total number of differentially expressed genes at a p-value of 0.05
    resultsSummary <- summary(decideTests(tested))
    # create the results summary
    resultsTable <- data.frame(
      Direction = c("Down", "NotSig", "Up"),
      Number = resultsSummary[,1]
    )
    # return the formatted results summary
    resultsTable
  })
  
  # render plot of log-fold change against log-counts per million with DE genes highlighted
  output$pairwiseMD <- renderPlot({
    # perform exact test
    tested <- pairwiseTest()
    # save as png
    #png("pairwiseMDPlot.png")
    # display the plot
    #plotMD(tested, main = "Mean-Difference (MD) Plot")
    # add blue lines to indicate 2-fold changes
    #abline(h=c(-1, 1), col="blue")
    # close the device
    #dev.off()
    # return MD plot
    plotMD(tested, main = "Mean-Difference (MD) Plot")
    # add blue lines to indicate 2-fold changes
    abline(h=c(-1, 1), col="blue")
  })
  
  # download handler for the volcano plot
  output$downloadPairwiseMD <- downloadHandler(
    filename = function() {
      "pairwiseMDPlot.png"
    },
    content = function(file) {
      file.copy("pairwiseMDPlot.png", file, overwrite=TRUE)
    }
  )
  
  # render volcano plot
  output$pairwiseVolcano <- renderPlot({
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
    # add column with -log10(FDR) values
    resultsTbl$negLog10FDR <- -log10(resultsTbl$FDR)
    # create volcano plot
    volcanoPlotPairwise <- ggplot(data=resultsTbl, aes(x=logFC, y=negLog10FDR, color = topDE)) + 
      geom_point() +
      theme_minimal() +
      scale_colour_discrete(type = plotColorSubset, breaks = c("Up", "Down")) +
      ggtitle("Volcano Plot") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face="bold"))
    # save the plot
    #file = "pairwiseVolcanoPlot.png"
    #ggsave(file, plot = volcanoPlotPairwise, device = "png")
    # display the plot
    volcanoPlotPairwise
  })
  
  # download handler for the volcano plot
  output$downloadPairwiseVolcano <- downloadHandler(
    filename = function() {
      "pairwiseVolcanoPlot.png"
    },
    content = function(file) {
      file.copy("pairwiseVolcanoPlot.png", file, overwrite=TRUE)
    }
  )
  
  # render text from brushed plot points
  #output$volcanoInfo <- renderText({
    # perform exact test
    #tested <- pairwiseTest()
    # create a results table of DE genes
    #resultsTbl <- topTags(tested, n=nrow(tested$table), adjust.method="fdr")$table
    # add column with -log10(FDR) values
    #resultsTbl$negLog10FDR <- -log10(resultsTbl$FDR)
    # With base graphics, need to tell it what the x and y variables are.
    #brushedPoints(resultsTbl, input$volcano_brush, xvar = "logFC", yvar = "negLog10FDR")
  #})
    
  # download table with number of filtered genes
  output$pairwiseResults <- downloadHandler(
    filename = function() {
      # setup output file name
      paste(
        paste(input$levelTwo, input$levelOne, sep = "_"),
        "pairwiseTopDEGs.csv", 
        sep = "_"
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
  
  ##
  # GLM Fitting
  ##
  
  # reactive function to create the glm design
  glmDesign <- reactive({
    # require input data
    req(input$compareExpression)
    # retrieve input design table
    group <- designFactors()
    # parametrize the experimental design with a one-way layout 
    design <- model.matrix(~ 0 + group)
    # add group names
    colnames(design) <- levels(group)
    # return design layout
    design
  })
    
  # reactive function for fitting the glm
  glmFitting <- reactive({
    # calculate scaling factors
    list <- filterNorm()
    # retrieve the experimental design 
    design <- glmDesign()
    # estimate common dispersion and tagwise dispersions to produce a matrix of pseudo-counts
    list <- estimateDisp(list, design, robust=TRUE)
    # estimate the QL dispersions
    glmQLFit(list, design, robust=TRUE)
  })
    
  # render plot of QL dispersions
  output$glmDispersions <- renderPlot({
    # retrieve the fitted glm
    fit <- glmFitting()
    # save the plot
    #png("glmDispersionsPlot.png")
    #plotQLDisp(fit)
    #dev.off()
    # return the plot
    plotQLDisp(fit)
  })
    
  # download handler for the volcano plot
  output$downloadGLMDispersions <- downloadHandler(
    filename = function() {
      "glmDispersionsPlot.png"
    },
    content = function(file) {
      file.copy("glmDispersionsPlot.png", file, overwrite=TRUE)
    }
  )
    
  ##
  # GLM Contrasts
  ##
  
  # render text with glm comparison
  output$glmComparison <- renderText({
    # set the current expression as a global value
    glmExpression <<- input$compareExpression
    # require input data
    req(glmExpression)
    # output string with comparison
    glmExpression
  })
  
  # reactive function to perform glm contrasts
  glmContrast <- reactive({
    # set the current expression as a global value
    glmExpression <<- input$compareExpression
    # require input data
    req(glmExpression)
    # retrieve the fitted glm
    fit <- glmFitting()
    # retrieve the experimental design 
    design <- glmDesign()
    # examine the overall effect of treatment
    glmContrast <- makeContrasts(glmSet = glmExpression,
                                   levels=design)
    # look at genes with significant expression across all UV groups
    glmTreat(fit, contrast=glmContrast)
  })
  
  # check if file has been uploaded
  output$glmResultsCompleted <- reactive({
    if(is.null(glmContrast())){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  outputOptions(output, 'glmResultsCompleted', suspendWhenHidden=FALSE, priority=0)
  
  # render table with the summary of results
  output$glmSummary <- renderTable({
    # perform glm test
    tested <- glmContrast()
    # view the total number of differentially expressed genes at a p-value of 0.05
    resultsSummary <- summary(decideTests(tested))
    # create the results summary
    resultsTable <- data.frame(
      Direction = c("Down", "NotSig", "Up"),
      Number = resultsSummary[,1]
    )
    # return the formatted results summary
    resultsTable
  })
  
  # render plot of log-fold change against log-counts per million with DE genes highlighted
  output$glmMD <- renderPlot({
    # perform glm test
    tested <- glmContrast()
    # save as png
    #png("glmMDPlot.png")
    # display the plot
    #plotMD(tested, main = "Mean-Difference (MD) Plot")
    # add blue lines to indicate 2-fold changes
    #abline(h=c(-1, 1), col="blue")
    # close the device
    #dev.off()
    # return MD plot
    plotMD(tested, main = "Mean-Difference (MD) Plot")
    # add blue lines to indicate 2-fold changes
    abline(h=c(-1, 1), col="blue")
  })
  
  # download handler for the volcano plot
  output$downloadGLMMD <- downloadHandler(
    filename = function() {
      "glmMDPlot.png"
    },
    content = function(file) {
      file.copy("glmMDPlot.png", file, overwrite=TRUE)
    }
  )
  
  # render volcano plot
  output$glmVolcano <- renderPlot({
    # perform glm test
    tested <- glmContrast()
    # create a results table of DE genes
    resultsTbl <- topTags(tested, n=nrow(tested$table), adjust.method="fdr")$table
    # add column for identifying direction of DE gene expression
    resultsTbl$topDE <- "NA"
    # identify significantly up DE genes
    resultsTbl$topDE[resultsTbl$logFC > 1 & resultsTbl$FDR < 0.05] <- "Up"
    # identify significantly down DE genes
    resultsTbl$topDE[resultsTbl$logFC < -1 & resultsTbl$FDR < 0.05] <- "Down"
    # add column with -log10(FDR) values
    resultsTbl$negLog10FDR <- -log10(resultsTbl$FDR)
    # create volcano plot
    volcanoPlotGLM <- ggplot(data=resultsTbl, aes(x=logFC, y=negLog10FDR, color = topDE)) + 
      geom_point() +
      theme_minimal() +
      scale_colour_discrete(type = plotColorSubset, breaks = c("Up", "Down")) +
      ggtitle("Volcano Plot") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face="bold"))
    # save the plot
    #file = "glmVolcanoPlot.png"
    #ggsave(file, plot = volcanoPlotGLM, device = "png")
    # display the plot
    volcanoPlotGLM
  })
  
  # download handler for the volcano plot
  output$downloadGLMVolcano <- downloadHandler(
    filename = function() {
      "glmVolcanoPlot.png"
    },
    content = function(file) {
      file.copy("glmVolcanoPlot.png", file, overwrite=TRUE)
    }
  )
  
  # download table with number of filtered genes
  output$glmResults <- downloadHandler(
    filename = function() {
      # setup output file name
      paste(input$compareExpression, "glmTopDEGs.csv", sep = "_")
    },
    content = function(file) {
      # perform glm test
      tested <- glmContrast()
      # view results table of top 10 DE genes
      resultsTbl <- topTags(tested, n=nrow(tested$table), adjust.method="fdr")$table
      # output table
      write.table(resultsTbl, file, sep=",", row.names=TRUE, quote=FALSE)
    }
  )
  
  # don't suspend hidden output processes
  #outputOptions(output, suspendWhenHidden=FALSE)
}

# create the Shiny app object 
shinyApp(ui = ui, server = server)