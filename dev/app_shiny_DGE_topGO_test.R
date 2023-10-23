# created by: Elizabeth Brooks
# date: 18 October 2023

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
          HTML("Start by uploading in the left-hand sidebar:")
        ),
        tags$p(
          HTML("<b>1.</b> a gene score <i>.csv</i> file with the results table from DGE or WGCNA")
        ),
        tags$p(
          HTML("<b>2.</b> a mappings <i>.txt</i> file with the gene to GO term mappings")
        ),
        tags$br(),
        #tags$p(
          #"After uploading the gene score table and mappings file, the application will facilitate:"
        #),
        #tags$p(
          #HTML("<b>1.</b> enrichment analysis of GO terms")
        #),
        #tags$p(
          #HTML("<b>2.</b> interpretation and visualisation of the results")
        #),
        tags$br(),
        tags$p(
          "Note that the GO term enrichment analysis results and plots may take several moments to process depending on the size of the input tables."
        ),
        tags$hr(),
        tags$p(
          "Example gene score and mappings tables are displayed below."
        ),
        tags$br(),
        fluidRow(
          column(
            width = 4,
            HTML("<b>Example</b> gene score table for six genes scored by DGE analysis:"),
            tableOutput(outputId = "exampleDGEScore"), 
          ),
          column(
            width = 4,
            HTML("<b>Example</b> gene score table for six genes scored by WGCNA:"),
            tableOutput(outputId = "exampleWGCNAScore"), 
          ),
          column(
            width = 4,
            HTML("<b>Example</b> gene to GO term mappings for six genes:"),
            tableOutput(outputId = "exampleMappings") 
          )
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
              align = "center",
              HTML("<b>GO Term Enrichment</b>")
            ),
            tags$br(),
            # TO-DO: add note about selecting a scoring statistic
            # FDR
            # module number
            tags$p(
              "Enter statistic for gene scoring:"
            ),
            textInput(
              inputId = "scoreStat",
              label = NULL,
              value = "FDR"
            ),
            # TO-DO: add note about not pre-filtering the gene list (e.g., on FDR)
            # < 0.05
            # = 1
            tags$p(
              "Enter expression for gene scoring:"
            ),
            textInput(
              inputId = "universeCut",
              label = NULL,
              value = "< 0.05"
            ),
            tags$p(
              "Select an algorithm:"
            ),
            radioButtons(
              inputId = "testAlg",
              label = NULL,
              choices = c("Default" = "weight01",
                          "Classic" = "classic",
                          "Elim" = "elim"),
              selected = "Default"
            ),
            tags$p(
              "Available algorithms:"
            ),
            tags$p(
              HTML("The <i>classic</i> method performs enrichment analysis by testing the over-representation of GO terms within the group of diferentially expressed genes.")
            ),
            tags$p(
              HTML("The <i>elim</i> method is more conservative then the classic method and you may expect the p-values returned by the former method to be lower bounded by the p-values returned by the later method.")
            ),
            tags$p(
              "The default algorithm used by the topGO package is a mixture between the <i>elim</i> and <i>weight</i> algorithms."
            ),
            tags$p(
              HTML("<b>Tip:</b> refer to the "),
              tags$a("topGO", href = "https://bioconductor.org/packages/devel/bioc/vignettes/topGO/inst/doc/topGO.pdf"),
              " manual for a description of the algorithms and test statistics."
            ),
            tags$p(
              "Select a test statistic:"
            ),
            radioButtons(
              inputId = "testStat",
              label = NULL,
              choices = c("Fisher" = "fisher",
                "Kolmogorov-Smirnov" = "ks"),
              selected = "Fisher"
            ),
            tags$br(),
            tags$p(
              "Available test statistics:"
            ),
            tags$p(
              HTML("<b>1.</b> Fisher's exact test is based on gene counts")
            ),
            tags$p(
              HTML("<b>2.</b> Kolmogorov-Smirnov like test computes enrichment based on gene scores")
            ),
            tags$p(
              "It is possible to use both the above tests since each gene has a score, which represents how it is diferentially expressed."
            ),
            tags$hr(),
            tags$p(
              align = "center",
              HTML("<b>Range of GO Term P-Values</b>")
            ),
            tags$br(),
            tags$p(
              "The following histograms show the range and frequency of p-values from the enrichment tests for each GO level (BP, MF, or CC)."
            ),
            fluidRow(
              column(
                width = 4,
                plotOutput(outputId = "BPHist"),
                downloadButton(outputId = "downloadBPHist", label = "Download Plot")
              ),
              column(
                width = 4,
                plotOutput(outputId = "MFHist"),
                downloadButton(outputId = "downloadMFHist", label = "Download Plot")
              ),
              column(
                width = 4,
                plotOutput(outputId = "CCHist"),
                downloadButton(outputId = "downloadCCHist", label = "Download Plot")
              )
            ),
            tags$hr(),
            tags$p(
              align = "center",
              HTML("<b>Subgraphs of Most Significant GO Terms</b>")
            ),
            tags$br(),
            tags$p(
              "Select the number of top GO terms:"
            ),
            sliderInput(
              inputId = "sigNodes",
              label = NULL,
              min = 1,
              max = 10,
              value = 5,
              step = 1
            ),
            tags$br(),
            tags$p(
              "The subgraph induced by the selected number of top (most significant) GO terms identifed by the elim algorithm for scoring GO terms for enrichment.",
              "Rectangles indicate the most signifcant terms with colors representing the relative signifcance, which ranges from dark red (most signifcant) to bright yellow (least signifcant).",
              "For each node, some basic information is displayed.",
              "The frst two lines show the GO identifer and a trimmed GO name.",
              "In the third line the raw p-value is shown.",
              "The forth line is showing the number of signifcant genes and the total number of genes annotated to the respective GO term."
            ),
            fluidRow(
              column(
                width = 4,
                tags$p(
                  HTML("<b>Download BP Subgraphs:</b>")
                ),
                downloadButton(outputId = "downloadBPSubgraphs", label = "Download PDF")
              ),
              column(
                width = 4,
                tags$p(
                  HTML("<b>Download MF Subgraphs:</b>")
                ),
                downloadButton(outputId = "downloadMFSubgraphs", label = "Download PDF")
              ),
              column(
                width = 4,
                tags$p(
                  HTML("<b>Download CC Subgraphs:</b>")
                ),
                downloadButton(outputId = "downloadCCSubgraphs", label = "Download PDF")
              )
            ),
            tags$hr(),
            tags$p(
              align = "center",
              HTML("<b>Density Plots of Most Significant GO Terms</b>")
            ),
            tags$br(),
            tags$p(
              "The following density plots show the distribution of the gene's rank for the top GO term of each GO level (BP, MF, or CC). The gene's rank is compared with the null distribution."
            ),
            fluidRow(
              column(
                width = 4,
                tags$p(
                  align = "center",
                  HTML("<b>BP Density Plot</b>")
                ),
                plotOutput(outputId = "BPDensity"),
                downloadButton(outputId = "downloadBPDensity", label = "Download Plot")
              ),
              column(
                width = 4,
                tags$p(
                  align = "center",
                  HTML("<b>MF Density Plot</b>")
                ),
                plotOutput(outputId = "MFDensity"),
                downloadButton(outputId = "downloadMFDensity", label = "Download Plot")
              ),
              column(
                width = 4,
                tags$p(
                  align = "center",
                  HTML("<b>CC Density Plot</b>")
                ),
                plotOutput(outputId = "CCDensity"),
                downloadButton(outputId = "downloadCCDensity", label = "Download Plot")
              )
            ),
            tags$hr(),
            tags$p(
              align = "center",
              HTML("<b>Dot Plot of Most Significant GO Terms</b>")
            ),
            plotOutput(outputId = "dotPlot"),
            downloadButton(outputId = "downloadDotPlot", label = "Download Plot"),
            tags$p(
              HTML("The above dot plot shows the <i>up to the top 5</i> most enriched GO terms for each level (BP, MF, CC). The dots are colored by the enrichment test p-values.")
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
  
  # render example gene score table
  output$exampleDGEScore <- renderTable({
    # create example score table
    exCountsTable <- data.frame(
      Gene = c("gene-1", "gene-2", "gene-3", "gene-4", "gene-5"),
      SampleOne = c(55.4739214515074,55.4922106735603,50.8277794324053,49.0577237706748,35.0116413707558),
      SampleThree = c(60.6342862376941,58.6332792022524,66.8786571479017,55.1899392420091,47.5157990031686),
      SampleFour = c(41.2829182894939,77.4796903744049,73.1206651483725,52.7370530534754,59.1863461267538),
      SampleFive = c(169.001946747616,187.417088878628,135.540745153081,199.9102243655,132.544070903575),
      SampleSix = c(21.9315503412936,24.0815253866394,33.8851862882702,34.3404066394723,26.6755362824806),
      SampleSeven = c(1.29009119654668,3.14106852869209,1.78343085727738,1.66722101765504,0.89916090304527),
      SampleEight = c(32.2522799136671,42.9279365587919,60.6366491474309,33.1139635452055,32.5108098442732)
    )
  })
  
  # render example gene score table
  output$exampleDGESubset <- renderTable({
    # create example score table
    exCountsTable <- data.frame(
      Gene = c("gene-1", "gene-2", "gene-3", "gene-4", "gene-5"),
      SampleOne = c(55.4739214515074,55.4922106735603,50.8277794324053,49.0577237706748,35.0116413707558),
      SampleThree = c(60.6342862376941,58.6332792022524,66.8786571479017,55.1899392420091,47.5157990031686),
      SampleFour = c(41.2829182894939,77.4796903744049,73.1206651483725,52.7370530534754,59.1863461267538),
      SampleFive = c(169.001946747616,187.417088878628,135.540745153081,199.9102243655,132.544070903575),
      SampleSix = c(21.9315503412936,24.0815253866394,33.8851862882702,34.3404066394723,26.6755362824806),
      SampleSeven = c(1.29009119654668,3.14106852869209,1.78343085727738,1.66722101765504,0.89916090304527),
      SampleEight = c(32.2522799136671,42.9279365587919,60.6366491474309,33.1139635452055,32.5108098442732)
    )
  })
  
  # render example gene score table
  output$exampleWGCNAScore <- renderTable({
    # create example score table
    exCountsTable <- data.frame(
      Gene = c("geneA", "geneB", "geneC"),
      sample_1 = c(25.4739214515074,25.4922106735603,20.8277794324053),
      sample_2 = c(30.6342862376941,28.6332792022524,36.8786571479017),
      sample_3 = c(11.2829182894939,37.4796903744049,33.1206651483725),
      sample_4 = c(133.001946747616,145.417088878628,121.540745153081),
      sample_5 = c(21.9315503412936,24.0815253866394,33.8851862882702),
      sample_6 = c(32.2522799136671,42.9279365587919,60.6366491474309),
      sample_7 = c(1.29009119654668,3.14106852869209,1.78343085727738),
      sample_8 = c(60.6342862376941,58.6332792022524,66.8786571479017),
      sample_9 = c(41.2829182894939,77.4796903744049,73.1206651483725),
      sample_10 = c(6.6342862376941,5.6332792022524,6.8786571479017),
      sample_11 = c(169.001946747616,187.417088878628,135.540745153081),
      sample_12 = c(55.4739214515074,55.4922106735603,50.8277794324053)
    )
  })
  
  # render example gene score table
  output$exampleWGCNASubset <- renderTable({
    # create example score table
    exCountsTable <- data.frame(
      Gene = c("geneA", "geneB", "geneC"),
      sample_1 = c(25.4739214515074,25.4922106735603,20.8277794324053),
      sample_2 = c(30.6342862376941,28.6332792022524,36.8786571479017),
      sample_3 = c(11.2829182894939,37.4796903744049,33.1206651483725),
      sample_4 = c(133.001946747616,145.417088878628,121.540745153081),
      sample_5 = c(21.9315503412936,24.0815253866394,33.8851862882702),
      sample_6 = c(32.2522799136671,42.9279365587919,60.6366491474309),
      sample_7 = c(1.29009119654668,3.14106852869209,1.78343085727738),
      sample_8 = c(60.6342862376941,58.6332792022524,66.8786571479017),
      sample_9 = c(41.2829182894939,77.4796903744049,73.1206651483725),
      sample_10 = c(6.6342862376941,5.6332792022524,6.8786571479017),
      sample_11 = c(169.001946747616,187.417088878628,135.540745153081),
      sample_12 = c(55.4739214515074,55.4922106735603,50.8277794324053)
    )
  })
  
  # render first example mappings table
  output$exampleDesignOne <- renderTable({
    # create example mappings table
    exDesignTable <- data.frame(
      Sample = c("SampleOne", "SampleTwo", "SampleThree", "SampleFour", "SampleFive", "SampleSix"),
      Group = c("1", "1", "1", "2", "2", "2")
    )
  })
  
  
  ##
  # Data Setup
  ##
  
  # retrieve input data
  inputAnalysisTable <- reactive({
    # require input data
    req(input$analysisTable)
    # check the input table is not null
    if(is.null(input$analysisTable)){
      return(NULL)
    }
    # check if the input table contains the selected gene score
    if(!(input$scoreStat %in% colnames(dat))){
      return(NULL)
    }
    # TO-DO: add note about first column is expected to contain gene names
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
    # retrieve selected gene score statistic column
    list_genes <- as.numeric(`$`(resultsTable , input$scoreStat))
    # create named list of all genes (gene universe) and values
    # the gene universe is set to be the list of all genes contained in the gene2GO list of annotated genes
    list_genes <- setNames(list_genes, rownames(resultsTable))
    list_genes_filtered <- list_genes[names(list_genes) %in% names(GOmaps)]
    # return list
    list_genes_filtered
  }
  
  # function to retrieve interesting genes
  retrieveInteresting <- function(){
    # require input
    req(input$universeCut)
    # function that returns list of interesting DE genes (0 == not significant, 1 == significant)
    get_interesting_DE_genes <- function(geneUniverse){
      interesting_DE_genes <- rep(0, length(geneUniverse))
      for(i in 1:length(geneUniverse)){
        if(eval(parse(text = paste(geneUniverse[i], tmpInput, sep=" ")))){
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
    # require input
    req(input$testStat, input$testAlg)
    # retrieve topGOdata object
    GO_data <- createOntology(ontologyID)
    # perform GO enrichment using the topGOdata objects
    GOResults <- runTest(GO_data, algorithm = input$testAlg, statistic = input$testStat)
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
    # create title
    titleName <- paste("Range of", ontologyID, "P-Values", sep=" ")
    # plot histogram to see range of p-values
    hist(pvalGO, 35, xlab = "p-values", main = titleName)
  }
  
  # render BP p-value histogram
  output$BPHist <- renderPlot({
    # create the plot
    createPHist("BP")
  })
  
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
  output$MFHist <- renderPlot({
    # create the plot
    createPHist("MF")
  })
  
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
  output$CCHist <- renderPlot({
    # create the plot
    createPHist("CC")
  })
  
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
  
  # function to plot BP, MF, or CC subgraphs
  createSubgraphs <- function(ontologyID){
    # require input
    req(input$sigNodes)
    # retrieve topGOdata object
    GO_data <- createOntology(ontologyID)
    # retrieve results
    GOResults <- performGO(ontologyID)
    # plot subgraphs induced by the most significant GO terms
    printGraph(GO_data, GOResults, firstSigNodes = input$sigNodes, 
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
  
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)
          
          