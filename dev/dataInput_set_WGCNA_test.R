#!/usr/bin/env Rscript

# script to create a network for a subset of samples using WGNCA

# install pacakges, if necessary
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("WGCNA")

#Load the WGCNA and edgeR packages
library(WGCNA)

#The following setting is important, do not omit.
options(stringsAsFactors = FALSE)

#Import normalized gene count data
#inputTable_subset <- read.csv(file="/Users/bamflappy/PfrenderLab/OLYM_dMelUV/KAP4/ensembl/GCA_021134715.1/biostatistics/DEAnalysis/Genotypes/glmQLF_normalizedCounts.csv", row.names="gene", header=TRUE)
inputTable_subset <- read.csv(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/WGCNA/example3_daphnia_normalizedCounts.csv", row.names=1)

# load in the trait data
#allTraits = read.csv("/Users/bamflappy/Repos/TranscriptomeAnalysisPipeline_DaphniaUVTolerance/InputData/expDesign_treatment_WGCNA_Olympics.csv")
allTraits <- read.csv("/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/WGCNA/example3_daphnia_design_WGCNA.csv", row.names=1)

dim(allTraits)
names(allTraits)

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

# cluster the samples to see if there are any obvious outliers
sampleTree = hclust(dist(datExpr0), method = "average")
# Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# The user should change the dimensions if the window is too large or too small.
#exportFile <- paste(tag, "sampleClustering.png", sep="_")
#png(file = exportFile, width = 12, height = 9, units="in", res=150)
sizeGrWindow(12,9)
par(cex = 0.6)
par(mar = c(0,4,2,0))
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
     cex.axis = 1.5, cex.main = 2)
# Plot a line to show the cut
#abline(h = 15, col = "red")
#dev.off()

# Determine cluster under the line
#clust = cutreeStatic(sampleTree, cutHeight = 15, minSize = 10)
#table(clust)
# clust 1 contains the samples we want to keep.
#keepSamples = (clust==1)

# filter out samples
datExpr <- datExpr0
#datExpr = datExpr0[keepSamples, ]
nGenes = ncol(datExpr)
nSamples = nrow(datExpr)

# Form a data frame analogous to expression data that will hold the clinical traits
samples = rownames(datExpr)
traitRows = match(samples, rownames(allTraits))
datTraits = allTraits[traitRows,]
collectGarbage()

# Re-cluster samples
#exportFile <- paste(tag, "sampleDendrogram_traitHeatmap.png", sep="_")
#png(file = exportFile, width = 10, height = 7, units="in", res=150)
sizeGrWindow(10,7)
sampleTree2 = hclust(dist(datExpr), method = "average")
# Convert traits to a color representation: white means low, red means high, grey means missing entry
traitColors = numbers2colors(datTraits, signed = FALSE)
# Plot the sample dendrogram and the colors underneath.
plotDendroAndColors(sampleTree2, traitColors,
                    groupLabels = names(datTraits),
                    main = "Sample dendrogram and trait heatmap")
#dev.off()

# save the relevant expression and trait data for use in the next steps
#exportFile <- paste(tag, "dataInput.RData", sep="-")
#save(datExpr, datTraits, file = exportFile)
