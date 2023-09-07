# load packages 
library(shiny)
library(shinythemes)
library(ggplot2)
library(ghibli)
library(ggVennDiagram)
library(edgeR)

# set the working directory
#setwd("/YOUR/PATH/")
setwd("/Users/bamflappy/Desktop/NFCDSWorkshop_Fall2022-main/data")

##
# Data
##
# import gene count data
tribolium_counts <- read.csv("TriboliumCounts.csv", row.names="X")

##
# Pairwise Setup
##
# add grouping factor
group <- factor(c(rep("cntrl_4h",3), rep("treat_4h",3), rep("cntrl_24h",3), rep("treat_24h",3)))
# begin to construct the DGE list object
list <- DGEList(counts=tribolium_counts,group=group)


# Define UI 
ui <- fluidPage(
  # view available themes
  #shinythemes::themeSelector(),
  
  # use the superhero theme
  theme = shinytheme("superhero"),
  
  # add application title
  titlePanel("Differential Gene Expression (DGE) Analysis - Pairwise Comparisons"),
  
  # setup sidebar layout
  sidebarLayout(
    # setup sidebar panel
    sidebarPanel(
      # Select variable for the first level
      selectInput(
        inputId = "levelOne",
        label = "First Level",
        choices = c("cntrl_4h", "treat_4h", "cntrl_24h", "treat_24h"),
        selected = "cntrl_24h"),
      # Select variable for the second level
      selectInput(
        inputId = "levelTwo",
        label = "Second Level",
        choices = c("cntrl_4h", "treat_4h", "cntrl_24h", "treat_24h"),
        selected = "treat_24h")
    ),
    
    # setup main panel layout
    mainPanel(
      # use tab panels to organize outputs
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Data Normalization & Exploration", 
          plotOutput(outputId = "librarySizes"),
          plotOutput(outputId = "MDS"),
          plotOutput(outputId = "heatmap"),
          plotOutput(outputId = "BCV")),
        tabPanel(
          "Pairwise Analysis", 
          span(textOutput(outputId = "pairwise"), align="center"),
          plotOutput(outputId = "MD"),
          plotOutput(outputId = "volcano")),
        tabPanel(
          "Summary", 
          #tags$p(
          #  align="center",
          #  "Data Normalization"),
          #tableOutput(outputId = "numNorm"),
          span(textOutput(outputId = "pairwise"), align="center"),
          tableOutput(outputId = "pairwiseSummary")),
          #tableOutput(outputId = "pairwiseResults")),
        tabPanel(
          "Reference",
          tags$p(
            "The RNA-seq data and background information was obtained from",
            tags$a("ScienceDirect", href = "https://www.sciencedirect.com/science/article/pii/S0147651319302684"), "and",
            tags$a("NCBI", href = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA504739/"), "."),
          tags$p(
            "Gene tables were created by processing the RNA-seq data as described in the", 
            tags$a("Bioinformatics Analysis of Omics Data with the Shell & R", href = "https://morphoscape.wordpress.com/2022/07/28/bioinformatics-analysis-of-omics-data-with-the-shell-r/"), "."),
          tags$p(
            "A tutorial of the analysis performed in this application is provided in the", 
            tags$a("Downstream Bioinformatics Analysis of Omics Data with edgeR", href = "https://morphoscape.wordpress.com/2022/08/09/downstream-bioinformatics-analysis-of-omics-data-with-edger/"), "."
          )
        )
      )
    )
  )
)

# define server 
server <- function(input, output, session) {
  # render text with pairwise comparison
  output$pairwise <- renderText({
    paste(input$levelTwo, input$levelOne, sep = " vs ")
  })
  
  ##
  # Pairwise Normalization
  ##
  # render plot of library sizes before normalization
  output$librarySizes <- renderPlot({
    barplot(list$samples$lib.size*1e-6, names=1:12, ylab="Library size (millions)")
  })
  # filter the list of gene counts based on expression levels
  keep <- filterByExpr(list)
  # render table with number of filtered genes
  #output$numNorm <- renderTable({
    # view the number of filtered genes
    #table(keep)
  #})
  # remove genes that are not expressed in either experimental condition
  list <- list[keep, , keep.lib.sizes=FALSE]
  # calculate scaling factors
  list <- calcNormFactors(list)
  # compute counts per million (CPM) using normalized library sizes
  normList <- cpm(list, normalized.lib.sizes=TRUE)
  
  ##
  # Plotting Palettes
  ##
  # retrieve the vector of colors associated with PonyoMedium
  ghibli_colors <- ghibli_palette("PonyoMedium", type = "discrete")
  # vector with a subset of colors associated with PonyoMedium
  ghibli_subset <- c(ghibli_colors[3], ghibli_colors[6], ghibli_colors[4])
  
  ##
  # Pairwise Data Exploration
  ##
  # vector of shape numbers for the MDS plot
  points <- c(0,1,15,16)
  # vector of colors for the MDS plot
  colors <- rep(c(ghibli_colors[3], ghibli_colors[6]), 2)
  # render MDS plot
  output$MDS <- renderPlot({
    # add extra space to right of plot area and change clipping to figure
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    # MDS plot with distances approximating log2 fold changes
    plotMDS(list, col=colors[group], pch=points[group])
    # place the legend outside the right side of the plot
    legend("topright", inset=c(-0.4,0), legend=levels(group), pch=points, col=colors)
    # close the plot
    #dev.off()
  })
  # calculate the log CPM of the gene count data
  logcpm <- cpm(list, log=TRUE)
  # render heatmap of individual RNA-seq samples using moderated log CPM
  output$heatmap <- renderPlot({
    #heatmap(logcpm, main = "Heatmap of RNA-seq Samples Using Moderated Log CPM")
    heatmap(logcpm)
  })
  
  ##
  # Pairwise Fitting
  ##
  # estimate common dispersion and tagwise dispersions to produce a matrix of pseudo-counts
  list <- estimateDisp(list)
  # render plot of dispersion estimates and biological coefficient of variation
  output$BCV <- renderPlot({
    plotBCV(list)
  })
  
  ##
  # Pairwise Contrasts
  ##
  # render plot of log-fold change against log-counts per million with DE genes highlighted
  output$MD <- renderPlot({
    # perform exact test
    tested <- exactTest(list, pair=c(input$levelOne, input$levelTwo))
    # create MD plot
    #plotMD(tested, main = "Mean-Difference (MD) Plot")
    plotMD(tested)
    # add blue lines to indicate 2-fold changes
    abline(h=c(-1, 1), col="blue")
  })
  # render volcano plot
  output$volcano <- renderPlot({
    # perform exact test
    tested <- exactTest(list, pair=c(input$levelOne, input$levelTwo))
    # create a results table of DE genes
    resultsTbl <- topTags(tested, n=nrow(tested$table), adjust.method="fdr")$table
    # add column for identifying direction of DE gene expression
    resultsTbl$topDE <- "NA"
    # identify significantly up DE genes
    resultsTbl$topDE[resultsTbl$logFC > 1 & resultsTbl$FDR < 0.05] <- "Up"
    # identify significantly down DE genes
    resultsTbl$topDE[resultsTbl$logFC < -1 & resultsTbl$FDR < 0.05] <- "Down"
    # create volcano plot
    ggplot(data=resultsTbl, aes(x=logFC, y=-log10(FDR), color = topDE)) + 
      geom_point() +
      theme_minimal() +
      scale_colour_discrete(type = ghibli_subset, breaks = c("Up", "Down")) +
      ggtitle("Volcano Plot")
  })
  # render table of DE genes
  output$pairwiseSummary <- renderTable({
    # perform exact test
    tested <- exactTest(list, pair=c(input$levelOne, input$levelTwo))
    # view the total number of differentially expressed genes at a p-value of 0.05
    summary(decideTests(tested))
  })
  # render table of top 10 DE genes
  #output$pairwiseResults <- renderTable({
    # perform exact test
    #tested <- exactTest(list, pair=c(input$levelOne, input$levelTwo))
    # view results table of top 10 DE genes
    #topTags(tested, n=10, adjust.method="fdr")$table
  #})
}

# create the Shiny app object 
shinyApp(ui = ui, server = server)