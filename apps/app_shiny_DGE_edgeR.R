# created by: Elizabeth Brooks
# date: 18 October 2023

#### Setup ####

# load packages 
suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(ggplot2)
  library(rcartocolor)
  library(edgeR)
  library(dplyr)
})

# color blind safe plotting palettes
plotColors <- carto_pal(12, "Safe")
plotColorSubset <- c(plotColors[4], plotColors[5], plotColors[6])

#### UI ####

# Define UI 
ui <- fluidPage(
  # view available themes
  #shinythemes::themeSelector(),
  
  # use a theme
  theme = shinytheme("yeti"),
  
  # add application title
  titlePanel("Differential Gene Expression (DGE) Analysis with edgeR"),
  
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
      # show panel depending on inputs check
      conditionalPanel(
        #condition = "output.inputsUploaded && !input.runAnalysis",
        condition = "output.inputsUploaded && !input.runUpload",
        tags$hr(),
        tags$p(
          "Click to Upload Data:"
        ),  
        actionButton("runUpload", "Upload")
      ),
      # show panel depending on upload check
      conditionalPanel(
        condition = "input.runUpload && output.inputCheck && !input.runAnalysis",
        tags$hr(),
        tags$p(
          "Click to Run Analysis:"
        ),  
        actionButton("runAnalysis", "Run Analysis"),
      ),
      # show panel depending on input files
      conditionalPanel(
        condition = "input.runAnalysis && (output.pairwiseResultsCompleted || output.glmResultsCompleted)",
        tags$hr(),
        # To-DO: connect
        tags$p(
          "Click to Update Analysis:"
        ),  
        actionButton("updateAnalysis", "Update Analysis"),
        tags$hr(),
        tags$p(
          "Select Analysis Type:"
        ),
        selectInput(
          inputId = "analysisType",
          label = NULL,
          choices = list("pairwise", "glm")
        ),
        tags$hr(),
        tags$p(
          "Set Cut Offs:"
        ),
        sliderInput(
          "LFCcut", 
          tags$p("Fold Change Cut Off"), 
          min=0, 
          max=10, 
          step=0.1,
          value=1.2
        ),
        sliderInput(
          "FDRcut",
          tags$p("FDR Cut Off"),
          min = 0, 
          max = 0.1, 
          value=0.05 
        ),
        tags$hr(),
        tags$p(
          "Design Table:"
        ),
        fluidRow(
          align = "center",
          # display input design table
          tableOutput(outputId = "designTable")
        )
      )
    ),
    
    # Output: Show plots
    mainPanel(
      
      # getting started text
      conditionalPanel(
        condition = "!input.runAnalysis",
        tags$h1("Getting Started", align = "center"),
        tags$br(),
        # TO-DO: add input step numbers
        tags$p(
          HTML("<b>Hello!</b>"),
          HTML("Start in the left-hand sidebar by:")
        ),
        tags$p(
          HTML("<b>1.</b> uploading a <i>.csv</i> file with the gene counts")
        ),
        tags$p(
          HTML("<b>2.</b> uploading a <i>.csv</i> files with the experimental design")
        ),
        tags$p(
          HTML("<b>3.</b> clicking the <i>Upload</i> button to check that the inputs are valid, which appears after the format of the inputs are checked")
        ),
        tags$p(
          HTML("<b>4.</b> clicking the <i>Run Analysis</i> button, which appears after the input files are verified as valid for analysis")
        ),
        tags$br(),
        tags$p(
          "Note that the DGE analysis results and plots may take several moments to process depending on the size of the input gene counts table."
        ),
        tags$hr(),
        tags$p(
          align = "center",
          HTML("<b>Helpful Tip</b>")
        ),
        tags$p(
          HTML("<b>Tip:</b> If the tables of gene counts were created using <i>HTSeq</i>, you may need to delete the last five lines of gene count statistics (e.g., __not_aligned) before uploading for DGE analysis.")
        ),
        tags$hr(),
        tags$p(
          align="center",
          HTML("<b>Data Formatting</b>")
        ),
        tags$p(
          "Example gene counts and experimental design tables are displayed below."
        ),
        tags$br(),
        HTML("<b>Example</b> gene counts table of six samples and five genes:"),
        tableOutput(outputId = "exampleCountsOne"),
        HTML("<b>Example</b> gene counts table of twelve samples and three genes:"),
        tableOutput(outputId = "exampleCountsTwo"),
        tags$br(),
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
      
      # warning text
      conditionalPanel(
        condition = "output.inputsUploaded && !output.inputCheck",
        tags$h1(
          "Warning", 
          align="center"
        ),
        tags$br(),
        tags$p(
          "The data in the uploaded file(s) are not of the correct type or the sample names do not match.",
        ),
        tags$br(),
        tags$p(
          "Please check that each of the input files were uploaded correctly in the left-hand side bar."
        ),
        tags$p(
          HTML("Please <b>allow a moment for processing</b> after uploading new input file(s).")
        ),
        tags$hr(),
        tags$p(
          align = "center",
          HTML("<b>Helpful Tips</b>")
        ),
        tags$p(
          HTML("<b>Tip 1:</b> The input gene counts table is expected to contain <i>numeric</i> integer values."),
        ),
        tags$p(
          HTML("<b>Tip 2:</b> Sample names contained in the first column of the gene counts and experimental design tables are expected to be <i>character</i> values.")
        ),
        tags$p(
          HTML("<b>Tip 3:</b> Sample names in the first line of the gene counts table <i>must match</i> the sample names contained in the first column of the experimental design table.")
        ),
        tags$p(
          HTML("<b>Tip 4:</b> The input gene counts and experimental design tables must end in the <i>.csv</i> file extension.")
        )
      ),
      
      # processing text
      conditionalPanel(
        condition = "output.inputCheck && !(output.pairwiseResultsCompleted || output.glmResultsCompleted)",
        tags$h1(
          "Processing", 
          align="center"
        ),
        tags$br(),
        "The DGE analysis results and plots may take several moments to process depending on the size of the input gene counts or experimental design tables."
      ),
      
      # results text and plots
      conditionalPanel(
        condition = "(input.runAnalysis && output.inputCheck) && (output.pairwiseResultsCompleted || output.glmResultsCompleted)",
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
            tags$br(),
            tags$p(
              HTML("<b>Tip 1:</b> The results may take several moments to appear depending on the size of the input gene counts table.")
            ),
            tags$p(
              HTML("<b>Tip 2:</b> Navigate to the <i>Data Normalization</i>, <i>Data Exploration</i>, or <i>Analysis & Results</i> steps by clicking the tabs above.")
            ),
            tags$p(
              HTML("<b>Tip 3:</b> It is possible to change the type of analysis in the left-hand sidebar.")
            ),
            tags$p(
              HTML("<b>Tip 4:</b> It is possible to change the comparison for an analysis in the left-hand sidebar")
            ),
            tags$p(
              HTML("<b>Tip 6:</b> If the normalizaion plot or other results look strange, double check the input raw gene counts (<i>not normalized</i>).")
            )
          ),
          
          # data normalization tab
          tabPanel(
            "Data Normalization",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Data Normalization</b>")
            ),
            imageOutput(outputId = "librarySizes", height="100%", width="100%"),
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
            )
          ),
          
          # data exploration tab
          tabPanel(
            "Data Exploration",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Data Exploration</b>")
            ),
            imageOutput(outputId = "PCA", height="100%", width="100%"),
            downloadButton(outputId = "downloadPCA", label = "Download Plot"),
            tags$p(
              "In a principal component analysis (PCA) plot the distances between samples approximate the expression differences.",
              "The expression differences were calculated as the the average of the largest (leading) absolute log-fold changes between each pair of samples and the same genes were selected for all comparisons.",
              "Note that the points are replaced by the sample name and colored by the associated factor level (e.g., cntrl or treat)."
            ),
            tags$br(),
            imageOutput(outputId = "MDS", height="100%", width="100%"),
            downloadButton(outputId = "downloadMDS", label = "Download Plot"),
            tags$p(
              "In a multidimensional scaling (MDS) plot the distances between samples approximate the expression differences.",
              "The expression differences were calculated as the the average of the largest (leading) absolute log-fold changes between each pair of samples and the top genes were selected separately for each pairwise comparison.",
              "Note that the points are replaced by the sample name and colored by the associated factor level (e.g., cntrl or treat)."
            ),
            tags$br(),
            imageOutput(outputId = "heatmap", height="100%", width="100%"),
            downloadButton(outputId = "downloadHeatmap", label = "Download Plot"),
            tags$p(
              "The heatmap uses hierarchical clustering of the individual samples by the log2 CPM expression values.",
              "Furthermore, the log2 CPM that has undefined values avoided and poorly defined log fold changes (logFC) for low counts shrunk towards zero"
            ),
            tags$br(),
            imageOutput(outputId = "BCV", height="100%", width="100%"),
            downloadButton(outputId = "downloadBCV", label = "Download Plot"),
            tags$p(
              "The biological coefficient of variation (BCV) plot is the square root of the dispersion parameter under the negative binomial model and is equivalent to estimating the dispersions of the negative binomial model."
            )
          ),
          
          # analysis & results tab
          tabPanel(
            "Analysis & Results", 
            # show pairwise results
            conditionalPanel(
              condition = "input.analysisType == 'pairwise'",
              tags$br(),
              tags$p(
                align="center",
                HTML("<b>Pairwise Comparison</b>")
              ),
              span(
                textOutput(outputId = "pairwiseComparison"), 
                align="center"
              ),
              tags$br(),
              tags$p(
                HTML("<b>Choose factor levels for comparison:</b>")
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
              # show pairwise results
              conditionalPanel(
                condition = "output.pairwiseResultsCompleted",
                tags$hr(),
                tags$p(
                  align="center",
                  HTML("<b>Pairwise Results</b>")
                ),
                tags$br(),
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
                tags$br(),
                tags$p(
                  HTML("<b>Differentially Expressed Genes IDs:</b>")
                ),
                downloadButton(outputId = "pairwiseResultsIDs", label = "Download Table"),
                tags$p(
                  "A list of the DE gene IDs from the pairwise analysis may be downloaded by clicking the above button.",
                ),
                tags$hr(),
                tags$p(
                  align="center",
                  HTML("<b>Results Exploration</b>")
                ),
                imageOutput(outputId = "pairwiseMD", height="100%", width="100%"),
                downloadButton(outputId = "downloadPairwiseMD", label = "Download Plot"),
                tags$p(
                  "The mean-difference (MD) plot shows the log fold changes expression differences versus average log CPM values."
                ),
                tags$br(),
                plotOutput(outputId = "pairwiseVolcano"),
                #click = "pairwiseVolcano_click",
                #dblclick = "pairwiseVolcano_dblclick",
                #hover = "pairwiseVolcano_hover",
                #brush = "pairwiseVolcano_brush"),
                #verbatimTextOutput(outputId = "pairwiseVolcanoInfo")
                downloadButton(outputId = "downloadPairwiseVolcano", label = "Download Plot"),
                tags$p(
                  "The volcano plot is a scatterplot that displays the association between statistical significance (e.g., p-value) and magnitude of gene expression (fold change)."
                )
              )
            ),
            # show glm comparison
            conditionalPanel(
              condition = "input.analysisType == 'glm'",
              tags$br(),
              tags$p(
                align="center",
                HTML("<b>GLM Comparison</b>")
              ),
              span(
                textOutput(outputId = "glmComparison"), 
                align="center"
              ),
              tags$br(),
              tags$p(
                HTML("<b>Enter expression for comparison:</b>")
              ),
              textInput("compareExpression", "Expression"),
              tags$p(
                "Valid expressions must consist of the factors contained in the input experimental design file, which is displayed in the left-hand sidebar."
              ),
              tags$p(
                "Examples of designing model expressions for ANOVA-like tests are availble in the",
                tags$a("edgeR manual", href = "https://www.bioconductor.org/packages/release/bioc/vignettes/edgeR/inst/doc/edgeRUsersGuide.pdf"),
                " (e.g., sections 3.2.6 & 4.4.9)."
              ),
              #tags$p(
                #"A detailed description of designing model expressions are provided in the paper",
                #tags$a("\"A guide to creating design matrices for gene expression experiments\"", href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7873980/"),
                #" (e.g., studies with multiple factors)."
              #),
              tags$p(
                HTML("<b>Tip!</b> Make sure that the factors used in the expression are spelled the same as in the experimental design file and shown in the left-hand sidebar.")
              ),
              # show glm results
              conditionalPanel(
                condition = "output.glmResultsCompleted",
                tags$hr(),
                tags$p(
                  align="center",
                  HTML("<b>GLM Results</b>")
                ),
                tags$br(),
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
                tags$br(),
                tags$p(
                  HTML("<b>Differentially Expressed Genes IDs:</b>")
                ),
                downloadButton(outputId = "glmResultsIDs", label = "Download Table"),
                tags$p(
                  "A list of the DE gene IDs from the ANOVA-like analysis may be downloaded by clicking the above button.",
                ),
                tags$hr(),
                tags$p(
                  align="center",
                  HTML("<b>Results Exploration</b>")
                ),
                tags$br(),
                imageOutput(outputId = "glmMD", height="100%", width="100%"),
                downloadButton(outputId = "downloadGLMMD", label = "Download Plot"),
                tags$p(
                  "The mean-difference (MD) plot shows the log fold changes expression differences versus average log CPM values."
                ),
                tags$br(),
                plotOutput(outputId = "glmVolcano"),
                downloadButton(outputId = "downloadGLMVolcano", label = "Download Plot"),
                tags$p(
                  "The volcano plot is a scatterplot that displays the association between statistical significance (e.g., p-value) and magnitude of gene expression (fold change)."
                ),
                tags$hr(),
                tags$p(
                  align="center",
                  HTML("<b>Model Exploration</b>")
                ),
                tags$br(),
                imageOutput(outputId = "glmDispersions", height="100%", width="100%"),
                downloadButton(outputId = "downloadGLMDispersions", label = "Download Plot"),
                tags$p(
                  "Above is a plot of the genewise quasi-likelihood (QL) dispersion against the log2 CPM gene expression levels.",
                  "Dispersion estimates are obtained after fitting negative binomial models and calculating dispersion estimates."
                )
              )
            )
          ),
          
          # information tab
          tabPanel(
            "Information",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Helpful Information</b>")
            ),
            tags$br(),
            tags$p(
              "This application for DGE analysis was created by ",
              tags$a("Elizabeth Brooks",href = "https://www.linkedin.com/in/elizabethmbrooks/"),
              "."
            ),
            tags$p(
              "The latest version of this application may be downloaded from ",
              tags$a("GitHub",href = "https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps"),
              "."
            ),
            tags$p(
              "Example gene counts and experimental design tables are also provided on ",
              tags$a("GitHub", href = "https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps/tree/main/data/edgeR"),
              "."
            ),
            tags$p(
              "Gene tables may be created from RNA-seq data as described in ", 
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

#### Server ####

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
  
  ##
  # Data Setup
  ##
  
  # reactive function to retrieve input data
  inputGeneCounts <- reactive({
    # require input data
    req(input$geneCountsTable)
    # check the input table is not null
    if(is.null(input$geneCountsTable)){
      return(NULL)
    }
    # read the file
    geneCounts <- read.csv(file = input$geneCountsTable$datapath, row.names=1)
    # TO-DO: consider adding check for trimming lines of non-count data
    # trim the data table to remove lines with counting statistics (htseq)
    #countsTable <- head(geneCounts, - 5)
  })
  
  # check input counts type
  countsType <- function(){
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
  }
  
  # reactive function to retrieve input data
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
  
  # check if input files have been uploaded
  output$inputsUploaded <- function(){
    # check if the input files are valid
    if(is.null(inputGeneCounts())) {
      return(FALSE)
    }else if(is.null(inputDesign())) {
      return(FALSE)
    }
    return(TRUE)
  }
  outputOptions(output, 'inputsUploaded', suspendWhenHidden=FALSE)
  
  # check input design type
  designFactors <- reactive({
    # require input data
    req(input$expDesignTable)
    # retrieve input design
    targets <- inputDesign()
    # check data type of the sample names
    if(!is.character(rownames(targets))){
      return(NULL)
    }
    # setup a design matrix
    factor(targets[,1])
  })
  
  # compare input design and counts samples
  compareSamples <- function(){
    # check the inputs
    if(is.null(countsType())) {
      return(NULL)
    }else if(is.null(inputDesign())) {
      return(NULL)
    } 
    # retrieve input design samples
    targets <- inputDesign()
    designSamples <- data.frame(ID1 = targets[,1])
    # retrieve input gene counts samples
    geneCounts <- countsType() 
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
    if(totalMismatches != 0){
      # all matched
      return(TRUE)
    }
    # there were mismatches
    return(NULL)
  }
  
  # check if inputs are good
  output$inputCheck <- function(){
    if(is.null(compareSamples())) {
      return(FALSE)
    }
    return(TRUE)
  }
  outputOptions(output, 'inputCheck', suspendWhenHidden=FALSE)
  
  # update inputs for comparisons
  observeEvent(input$runUpload, {
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
    geneCounts <- countsType()
    # retrieve column names
    sampleNames <- colnames(geneCounts)
    # create data frame
    design <- data.frame(
      Sample = sampleNames,
      Factors = group
    )
  })
  
  ##
  # Data Normalization & Exploration
  ##
  
  # function for data normalization
  normalizeData <- function(){
    # retrieve input design table
    group <- designFactors()
    # begin to construct the DGE list object
    geneCounts <- countsType()
    list <- DGEList(counts=geneCounts,group=group)
  }
  
  # function for normalized data filtering
  filterNorm <- function(){
    # begin to construct the DGE list object
    list <- normalizeData()
    # filter the list of gene counts based on expression levels
    keep <- filterByExpr(list)
    # remove genes that are not expressed in either experimental condition
    list <- list[keep, , keep.lib.sizes=FALSE]
    # calculate scaling factors
    list <- calcNormFactors(list)
  }
  
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
      # add gene row name tag
      normList <- as_tibble(normList, rownames = "gene")
      # output table
      write.table(normList, file, sep=",", row.names=FALSE, quote=FALSE)
    }
  )
  
  # plot of library sizes before normalization
  createLibrarySizes <- function(){
    # begin to construct the DGE list object
    list <- normalizeData()
    # retrieve the number of samples
    numSamples <- ncol(list)
    # create barplot of library sizes before normalization
    barplot(list$samples$lib.size*1e-6, names=1:numSamples, ylab="Library size (millions)", main = "Library Sizes Before Normalization")
  }
    
  # render plot of library sizes before normalization
  output$librarySizes <- renderImage({
    # save the plot
    exportFile <- "librarySizesPlot.png"
    png(exportFile)
    createLibrarySizes()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # download handler for the bar plot
  output$downloadLibrarySizes <- downloadHandler(
    filename = function() {
      "librarySizesPlot.png"
    },
    content = function(file) {
      # save the plot
      png(file)
      createLibrarySizes()
      dev.off()
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
  
  # PCA plot
  createPCA <- function(){
    # retrieve input design table
    group <- designFactors()
    # calculate scaling factors
    list <- filterNorm()
    ## TO-DO: consider allowing users to input shapes and colors or other features
    # retrieve the number of grouping levels
    stringLevels <- gsub("\\..*","", (levels(group)))
    # setup colors and points
    colors <- plotColors[1:length(stringLevels)]
    #points <- c(0:length(unique(stringLevels)))
    # add extra space to right of plot area and change clipping to figure
    par(mar=c(6.5, 5.5, 5.5, 9.5), xpd=TRUE)
    # PCA plot with distances approximating log2 fold changes
    #plotMDS(list, col=colors[group], pch=points[group], gene.selection="common", main = "Principal Component Analysis (PCA) Plot")
    plotMDS(list, col=colors[group], gene.selection="common", main = "Principal Component Analysis (PCA) Plot")
    # place the legend outside the right side of the plot
    #legend("topright", inset=c(-0.5,0), legend=levels(group), pch=points, col=colors)
    legend("topright", inset=c(-0.5,0), legend=levels(group), fill=colors)
  }
  
  # render PCA plot
  output$PCA <- renderImage({
    # save the plot
    exportFile <- "PCAPlot.png"
    png(exportFile)
    createPCA()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # download handler for the PCA plot
  output$downloadPCA <- downloadHandler(
    filename = function() {
      "PCAPlot.png"
    },
    content = function(file) {
      # save the plot
      png(file)
      createPCA()
      dev.off()
    }
  )
  
  # MDS plot
  createMDS <- function(){
    # retrieve input design table
    group <- designFactors()
    # calculate scaling factors
    list <- filterNorm()
    ## TO-DO: consider allowing users to input shapes and colors or other features
    # retrieve the number of grouping levels
    stringLevels <- gsub("\\..*","", (levels(group)))
    # setup colors and points
    colors <- plotColors[1:length(stringLevels)]
    #points <- c(0:length(unique(stringLevels)))
    # add extra space to right of plot area and change clipping to figure
    par(mar=c(6.5, 5.5, 5.5, 9.5), xpd=TRUE)
    # PCA plot with distances approximating log2 fold changes
    #plotMDS(list, col=colors[group], pch=points[group], main = "Multi-Dimensional Scaling (MDS) Plot")
    plotMDS(list, col=colors[group], main = "Multi-Dimensional Scaling (MDS) Plot")
    # place the legend outside the right side of the plot
    #legend("topright", inset=c(-0.5,0), legend=levels(group), pch=points, col=colors)
    legend("topright", inset=c(-0.5,0), legend=levels(group), fill=colors)
  }
  
  # render MDS plot
  output$MDS <- renderImage({
    # save the plot
    exportFile <- "MDSPlot.png"
    png(exportFile)
    createMDS()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # download handler for the MDS plot
  output$downloadMDS <- downloadHandler(
    filename = function() {
      "MDSPlot.png"
    },
    content = function(file) {
      # save the plot
      png(file)
      createMDS()
      dev.off()
    }
  )
  
  # heatmap of individual RNA-seq samples using moderated log CPM
  createHeatmap <- function(){
    # calculate scaling factors
    list <- filterNorm()
    # calculate the log CPM of the gene count data
    logcpm <- cpm(list, log=TRUE)
    # create heatmap of individual RNA-seq samples using moderated log CPM
    heatmap(logcpm, main = "Heatmap of RNA-seq Samples")
  }
  
  # render heatmap of individual RNA-seq samples using moderated log CPM
  output$heatmap <- renderImage({
    # save the plot
    exportFile <- "heatmapPlot.png"
    png(exportFile)
    createHeatmap()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # download handler for the heatmap plot
  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      "heatmapPlot.png"
    },
    content = function(file) {
      # save the plot
      png(file)
      createHeatmap()
      dev.off()
    }
  )
  
  # plot of dispersion estimates and biological coefficient of variation
  createBCV <- function(){
    # calculate scaling factors
    list <- filterNorm()
    # estimate common dispersion and tagwise dispersions to produce a matrix of pseudo-counts
    list <- estimateDisp(list)
    # create BCV plot
    plotBCV(list, main = "Biological Coefficient of Variation (BCV) Plot")
  }
  
  # render plot of dispersion estimates and biological coefficient of variation
  output$BCV <- renderImage({
    # save the plot
    exportFile <- "BCVPlot.png"
    png(exportFile)
    createBCV()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # download handler for the BCV plot
  output$downloadBCV <- downloadHandler(
    filename = function() {
      "BCVPlot.png"
    },
    content = function(file) {
      # save the plot
      png(file)
      createBCV()
      dev.off()
    }
  )
  
  ##
  # Pairwise Comparisons (Contrasts)
  ##
  
  # render text with pairwise comparison
  output$pairwiseComparison <- renderText({
    # require input data
    req(input$levelOne, input$levelTwo)
    # create string with factor levels
    paste(input$levelTwo, input$levelOne, sep = " vs ")
  })
  
  # function to calculate table of DE genes
  pairwiseTest <- function(){
    # require valid inputs
    if(is.null(compareSamples())){
      return(NULL)
    }
    # require input data
    req(input$levelOne, input$levelTwo)
    # calculate scaling factors
    list <- filterNorm()
    # estimate common dispersion and tagwise dispersions to produce a matrix of pseudo-counts
    list <- estimateDisp(list)
    # perform exact test
    exactTest(list, pair=c(input$levelOne, input$levelTwo))
  }
  
  # TO-DO: this causes additional function calls
  # check if results have completed
  output$pairwiseResultsCompleted <- function(){
    if(is.null(pairwiseTest())){
      return(FALSE)
    }
    return(TRUE)
  }
  outputOptions(output, 'pairwiseResultsCompleted', suspendWhenHidden=FALSE, priority=0)
  
  # render results summary
  output$pairwiseSummary <- renderTable({
    # perform exact test
    tested <- pairwiseTest()
    # view the total number of differentially expressed genes at a p-value of 0.05
    DGEgenes = decideTests(tested,p.value=input$FDRcut, lfc=input$LFCcut)
    resultsSummary <- summary(DGEgenes)
    # create the results summary
    resultsTable <- data.frame(
      Direction = c("Down", "NotSig", "Up"),
      Number = resultsSummary[,1]
    )
    # return the formatted results summary
    resultsTable
  })
  
  # plot of log-fold change against log-counts per million with DE genes highlighted
  createMD <- function(tested, inputLFC){
    # return MD plot
    plotMD(tested, main = "Mean-Difference (MD) Plot")
    # add blue lines to indicate 2-fold changes
    abline(h=c((-1*inputLFC), inputLFC), col="blue") 
  }
  
  # render plot of log-fold change against log-counts per million with DE genes highlighted
  output$pairwiseMD <- renderImage({
    # perform exact test
    tested <- pairwiseTest()
    # retrieve input LFC cut
    inputLFC <- input$LFCcut
    # save the plot
    exportFile <- "pairwiseMDPlot.png"
    png(exportFile)
    createMD(tested, inputLFC)
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  
  # download handler for the MD plot
  output$downloadPairwiseMD <- downloadHandler(
    filename = function() {
      "pairwiseMDPlot.png"
    },
    content = function(file) {
      # perform exact test
      tested <- pairwiseTest()
      # retrieve input LFC cut
      inputLFC <- input$LFCcut
      # save the plot
      png(file)
      createMD(tested, inputLFC)
      dev.off()
    }
  )
  
  # create volcano plot
  plotVolcano <- function(tested){
    # create a results table of DE genes
    resultsTbl <- topTags(tested, n=nrow(tested$table), adjust.method="fdr")$table
    # add column for identifying direction of DE gene expression
    resultsTbl$topDE <- "NA"
    # identify significantly up DE genes
    resultsTbl$topDE[resultsTbl$logFC > input$LFCcut & resultsTbl$FDR < input$FDRcut] <- "Up"
    # identify significantly down DE genes
    resultsTbl$topDE[resultsTbl$logFC < (-1*input$LFCcut) & resultsTbl$FDR < input$FDRcut] <- "Down"
    # add column with -log10(FDR) values
    resultsTbl$negLog10FDR <- -log10(resultsTbl$FDR)
    # create volcano plot
    ggplot(data=resultsTbl, aes(x=logFC, y=negLog10FDR, color = topDE)) + 
      geom_point() +
      theme_minimal() +
      scale_colour_discrete(type = plotColorSubset, breaks = c("Up", "Down")) +
      ggtitle("Volcano Plot") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face="bold"))
  }
  
  # render volcano plot
  output$pairwiseVolcano <- renderPlot({
    # perform exact test
    tested <- pairwiseTest()
    # create plot
    plotVolcano(tested)
  })
  
  
  # download handler for the volcano plot
  output$downloadPairwiseVolcano <- downloadHandler(
    filename = function() {
      "pairwiseVolcanoPlot.png"
    },
    content = function(file) {
      # perform exact test
      tested <- pairwiseTest()
      # create plot
      volcanoPlotPairwise <- plotVolcano(tested)
      # save plot
      ggsave(file, plot = volcanoPlotPairwise, device = "png")
    }
  )
  
  ## TO-DO: allow users to select plot points
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
  
  # TO-DO: add FDR cut off filter
  # download table with number of filtered genes
  output$pairwiseResults <- downloadHandler(
    filename = function() {
      # setup output file name
      paste(input$levelTwo, input$levelOne, "pairwiseTopDEGs.csv", sep = "_")
    },
    content = function(file) {
      # perform exact test
      tested <- pairwiseTest()
      # view results table of top 10 DE genes
      resultsTbl <- topTags(tested, n=nrow(tested$table), adjust.method="fdr")$table
      # add gene row name tag
      resultsTbl <- as_tibble(resultsTbl, rownames = "gene")
      # output table
      write.table(resultsTbl, file, sep=",", row.names=FALSE, quote=FALSE)
    }
  )
  
  # TO-DO: add FDR cut off filter
  # function to retrieve gene IDs from results tables
  retrieveGeneIDs <- function(tested){
    # view results table of top 10 DE genes
    resultsTbl <- topTags(tested, n=nrow(tested$table), adjust.method="fdr")$table
    # retrieve gene IDS
    resultsTblNames <- rownames(resultsTbl)
    # add commas
    resultsTblNames <- paste(resultsTblNames, ",", sep="")
    # retrieve last entry
    lastEntry <- resultsTblNames[length(resultsTblNames)]
    # remove extra trailing comma
    resultsTblNames[length(resultsTblNames)] <- gsub(",", "\n", lastEntry)
    # return list of gene IDs
    resultsTblNames
  }
  
  # download table with number of filtered genes IDs
  output$pairwiseResultsIDs <- downloadHandler(
    filename = function() {
      # setup output file name
      paste(input$levelTwo, input$levelOne, "pairwiseTopDEGs_geneIDs.csv", sep = "_")
    },
    content = function(file) {
      # perform glm test
      tested <- pairwiseTest()
      # add commas
      resultsTblNames <- retrieveGeneIDs(tested)
      # output table
      writeLines(resultsTblNames, con = file, sep = "")
    }
  )
  
  ##
  # GLM Fitting
  ##
  
  # function to create the glm design
  glmDesign <- function(){
    # require valid inputs
    if(is.null(compareSamples())){
      return(NULL)
    }
    # retrieve input design table
    group <- designFactors()
    # parametrize the experimental design with a one-way layout 
    design <- model.matrix(~ 0 + group)
    # add group names
    colnames(design) <- levels(group)
    # return design layout
    design
  }
  
  # function for fitting the glm
  glmFitting <- function(){
    # calculate scaling factors
    list <- filterNorm()
    # retrieve the experimental design 
    design <- glmDesign()
    # estimate common dispersion and tagwise dispersions to produce a matrix of pseudo-counts
    list <- estimateDisp(list, design, robust=TRUE)
    # estimate the QL dispersions
    glmQLFit(list, design, robust=TRUE)
  }
  
  # plot of QL dispersions
  createGLMDispersions <- function(){
    # retrieve the fitted glm
    fit <- glmFitting()
    # return the plot
    plotQLDisp(fit)
  }
  
  # render plot of QL dispersions
  output$glmDispersions <- renderImage({
    # save the plot
    exportFile <- "glmDispersionsPlot.png"
    png(exportFile)
    createGLMDispersions()
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # download handler for the GLM dispersions plot
  output$downloadGLMDispersions <- downloadHandler(
    filename = function() {
      "glmDispersionsPlot.png"
    },
    content = function(file) {
      # save the plot
      png(file)
      createGLMDispersions()
      dev.off()
    }
  )
  
  ##
  # GLM Contrasts
  ##
  
  # render text with glm comparison
  output$glmComparison <- renderText({
    # require the expression
    req(input$compareExpression)
    # return the expression
    input$compareExpression
  })
  
  # function to perform glm contrasts
  glmContrast <- function(){
    # require the expression
    req(input$compareExpression)
    # set the input expression as global
    inputExpression <<- input$compareExpression
    # retrieve the fitted glm
    fit <- glmFitting()
    # retrieve the experimental design 
    design <- glmDesign()
    # examine the overall effect of treatment
    glmContrast <- makeContrasts(glmSet = inputExpression,
                                 levels=design)
    # look at genes with significant expression across all UV groups
    glmTreat(fit, contrast=glmContrast)
  }
  
  # check if results have completed
  output$glmResultsCompleted <- function(){
    if(is.null(glmContrast())){
      return(FALSE)
    }
    return(TRUE)
  }
  outputOptions(output, 'glmResultsCompleted', suspendWhenHidden=FALSE, priority=0)
  
  # render table with the summary of results
  output$glmSummary <- renderTable({
    # perform glm test
    tested <- glmContrast()
    # view the total number of differentially expressed genes at a p-value of 0.05
    resultsSummary <- summary(decideTests(tested), p.value=input$FDRcut, lfc=input$LFCcut)
    # create the results summary
    resultsTable <- data.frame(
      Direction = c("Down", "NotSig", "Up"),
      Number = resultsSummary[,1]
    )
    # return the formatted results summary
    resultsTable
  })
  
  # render plot of log-fold change against log-counts per million with DE genes highlighted
  output$glmMD <- renderImage({
    # perform glm test
    tested <- glmContrast()
    # retrieve input LFC cut
    inputLFC <- input$LFCcut
    # save the plot
    exportFile <- "glmMDPlot.png"
    png(exportFile)
    createMD(tested, inputLFC)
    dev.off()
    # Return a list
    list(src = exportFile, alt = "Invalid Results")
  }, deleteFile = TRUE)
  
  # download handler for the GLM MD plot
  output$downloadGLMMD <- downloadHandler(
    filename = function() {
      "glmMDPlot.png"
    },
    content = function(file) {
      # perform glm test
      tested <- glmContrast()
      # retrieve input LFC cut
      inputLFC <- input$LFCcut
      # save the plot
      png(file)
      createMD(tested, inputLFC)
      dev.off()
    }
  )
  
  # render GLM volcano plot
  output$glmVolcano <- renderPlot({
    # perform glm test
    tested <- glmContrast()
    # create plot
    plotVolcano(tested)
  })
  
  # download handler for the volcano plot
  output$downloadGLMVolcano <- downloadHandler(
    filename = function() {
      "glmVolcanoPlot.png"
    },
    content = function(file) {
      # perform glm test
      tested <- glmContrast()
      # create plot
      volcanoPlotGLM <- plotVolcano(tested)
      # save plot
      ggsave(file, plot = volcanoPlotGLM, device = "png")
    }
  )
  
  # TO-DO: add FDR cut off filter
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
      # add gene row name tag
      resultsTbl <- as_tibble(resultsTbl, rownames = "gene")
      # output table
      write.table(resultsTbl, file, sep=",", row.names=FALSE, quote=FALSE)
    }
  )
  
  # TO-DO: add FDR cut off filter
  # download table with number of filtered genes IDs
  output$glmResultsIDs <- downloadHandler(
    filename = function() {
      # setup output file name
      paste(input$compareExpression, "glmTopDEGs_geneIDs.csv", sep = "_")
    },
    content = function(file) {
      # perform glm test
      tested <- glmContrast()
      # retrieve gene IDS
      resultsTblNames <- retrieveGeneIDs(tested)
      # output table
      writeLines(resultsTblNames, con = file, sep = "")
    }
  )
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)