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
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for the first level
      selectInput(
        inputId = "levelOne",
        label = "First Level",
        choices = c("cntrl_4h", "treat_4h", "cntrl_24h", "treat_24h"),
        selected = "cntrl_24h"
      ),
      # Select variable for the second level
      selectInput(
        inputId = "levelTwo",
        label = "Second Level",
        choices = c("cntrl_4h", "treat_4h", "cntrl_24h", "treat_24h"),
        selected = "treat_24h"
      )
    ),
    
    # Output: Show plots
    mainPanel(
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
          plotOutput(outputId = "MD"),
          plotOutput(outputId = "volcano")),
        tabPanel(
          "Summary", 
          tableOutput(outputId = "summary")),
        tabPanel(
          "Reference",
          tags$p(
            "The data and background information was obtained from",
            tags$a("ScienceDirect", href = "https://www.sciencedirect.com/science/article/pii/S0147651319302684"), "and",
            tags$a("NCBI", href = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA504739/"), "."
          ),
          tags$p(
            "A tutorial of the analysis performed in R is provided in the", 
            tags$a("Downstream Bioinformatics Analysis of Omics Data with edgeR", href = "https://morphoscape.wordpress.com/2022/08/09/downstream-bioinformatics-analysis-of-omics-data-with-edger/"), "."
          )
        )
      )
    )
  )
)

# Define server 
server <- function(input, output, session) {
  ##
  # Pairwise Normalization
  ##
  
  # render plot of library sizes before normalization
  output$librarySizes <- renderPlot({
    barplot(list$samples$lib.size*1e-6, names=1:12, ylab="Library size (millions)")
  })
  
  # filter the list of gene counts based on expression levels
  keep <- filterByExpr(list)
  
  # view the number of filtered genes
  table(keep)
  
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
  
  # render table of DE genes
  output$summary <- renderTable({
    # perform exact test
    tested <- exactTest(list, pair=c(input$levelOne, input$levelTwo))
    # view the total number of differentially expressed genes at a p-value of 0.05
    summary(decideTests(tested))
  })
  
  # render plot of log-fold change against log-counts per million with DE genes highlighted
  output$MD <- renderPlot({
    # perform exact test
    tested <- exactTest(list, pair=c(input$levelOne, input$levelTwo))
    # create MD plot
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
      scale_colour_discrete(type = ghibli_subset, breaks = c("Up", "Down"))
  })
  
  # identify significantly DE genes by FDR
  #resultsTbl.keep <- resultsTbl$FDR < 0.05
  
  # create filtered results table of DE genes
  #resultsTbl_filtered <- resultsTbl[resultsTbl.keep,]
  
  
  ##
  # Pairwise Results Exploration
  ##
  
  # retrieve set of DE gene names for 24h contrast
  #geneSet_24h <- rownames(resultsTbl_24h_filtered)
  
  # retrieve set of DE gene names for treat contrast
  #geneSet_treat <- rownames(resultsTbl_treat_filtered)
  
  # retrieve set of DE gene names for cntrl contrast
  #geneSet_cntrl <- rownames(resultsTbl_cntrl_filtered)
  
  # create combined list of DE gene names
  #list_venn <- list(h24 = geneSet_24h, 
  #                  treat = geneSet_treat, 
  #                  cntrl = geneSet_cntrl)
  
  # create venn diagram
  #ggVennDiagram(list_venn, label_alpha=0.25, category.names = c("24h","treat","cntrl")) +
  #  scale_color_brewer(palette = "Paired")
}

# Create a Shiny app object 

shinyApp(ui = ui, server = server)