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

# Allow multi-threading within WGCNA. At present this call is necessary.
# Any error here may be ignored but you may want to update WGCNA if you see one.
# Caution: skip this line if you run RStudio or other third-party R environments.
# See note above.
enableWGCNAThreads()


##
# Data Input
##

#Import normalized gene count data
#inputTable_subset <- read.csv(file="/Users/bamflappy/PfrenderLab/OLYM_dMelUV/KAP4/ensembl/GCA_021134715.1/biostatistics/DEAnalysis/Genotypes/glmQLF_normalizedCounts.csv", row.names="gene", header=TRUE)
inputTable_subset <- read.csv(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/WGCNA/example3_daphnia_normalizedCounts.csv", row.names=1)

# load in the trait data
#allTraits = read.csv("/Users/bamflappy/Repos/TranscriptomeAnalysisPipeline_DaphniaUVTolerance/InputData/expDesign_treatment_WGCNA_Olympics.csv")
allTraits <- read.csv("/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/WGCNA/example3_daphnia_design_WGCNA.csv", row.names=1)

dim(allTraits)
names(allTraits)


##
# Data Prep
##

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


##
# Pick Soft Thresholding Powers
##

# Choose a set of soft-thresholding powers
powers = c(c(1:10), seq(from = 12, to=36, by=2))
# Call the network topology analysis function
sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)
# Plot the results
cex1 = 0.9
#exportFile <- paste(genotype, "SoftPowers.png", sep="_")
#png(file = exportFile, wi = 9, he = 5, units="in", res=150)
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
#dev.off()


