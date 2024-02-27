#!/usr/bin/env Rscript

# created by: Elizabeth Brooks
# last update: 26 Feb 2024

# install any missing packages
packageList <- c("BiocManager", "shiny", "shinythemes", "ggplot2", "rcartocolor", "dplyr", "statmod")
biocList <- c("edgeR")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
newBioc <- biocList[!(biocList %in% installed.packages()[,"Package"])]
if(length(newPackages)){
  install.packages(newPackages)
}
if(length(newBioc)){
  BiocManager::install(newBioc)
}

#Turn off scientific notation
options(scipen = 999)

#Load the edgeR library
library(edgeR)
library(statmod)
library(ggplot2)
library(rcartocolor)
library(dplyr)

#  plotting palettes
plotColors <- carto_pal(12, "Safe")
plotColorSubset <- c(plotColors[4], plotColors[5], plotColors[6])


##
# Data Input
##

# import gene count data
inputTable <- read.csv(file="/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/edgeR/example3_daphnia_counts.csv", row.names=1)

# trim the data table
countsTable <- head(inputTable, - 5)

# import grouping factor
targets <- read.csv(file="/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/edgeR/example3_daphnia_design_edgeR.csv", row.names=1)


##
# Analysis
##

#Setup a design matrix
group <- factor(targets[,1])
#cbind(targets,Group=group)
#Create DGE list object
list <- DGEList(counts=countsTable,group=group)
colnames(list) <- rownames(targets)

#Plot the library sizes before normalization
#jpeg("glmQLF_plotBarsBefore.jpg")
barplot(list$samples$lib.size*1e-6, names=1:ncol(list), ylab="Library size (millions)")
#dev.off()

#Retain genes only if it is expressed at a minimum level
keep <- filterByExpr(list)
summary(keep)
list <- list[keep, , keep.lib.sizes=FALSE]

#Use TMM normalization to eliminate composition biases
# between libraries
list <- calcNormFactors(list)
#list$samples
#Write normalized counts to file
normList <- cpm(list, normalized.lib.sizes=TRUE)
# add gene row name tag
normList <- as_tibble(normList, rownames = "gene")
#write.table(normList, file="glmQLF_normalizedCounts.csv", sep=",", row.names=FALSE, quote=FALSE)

#Write log transformed normalized counts to file
normListLog <- cpm(list, normalized.lib.sizes=TRUE, log=TRUE)
#write.table(normListLog, file="glmQLF_normalizedCounts_logTransformed.csv", sep=",", row.names=TRUE, quote=FALSE)

#Verify TMM normalization using a MD plot
#Write plot to file
#jpeg("glmQLF_plotMDBefore.jpg")
plotMD(cpm(list, log=TRUE), column=1)
abline(h=0, col=plotColorSubset[3], lty=2, lwd=2)
#dev.off()

#Use a MDS plot to visualizes the differences
# between the expression profiles of different samples
points <- c(0,1,2,3,15,16,17,18)
colors <- rep(c(plotColors[4], plotColors[5], plotColors[6], plotColors[11]), 2)
#Write plot with legend to file
#jpeg("glmQLF_plotMDS.jpg")
par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
plotMDS(list, col=colors[group], pch=points[group])
legend("topright", inset=c(-0.8,0), legend=levels(group), pch=points, col=colors, ncol=2)
#legend("topleft", legend=levels(group), pch=points, col=colors, ncol=2)
dev.off()
#Write plot without legend to file
#jpeg("glmQLF_plotMDS_noLegend.jpg")
plotMDS(list, col=colors[group], pch=points[group])
#dev.off()

# Create a PCA plot with a legend
#jpeg("glmQLF_plotPCA.jpg")
par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
plotMDS(list, col=colors[group], pch=points[group], gene.selection="common")
legend("topright", inset=c(-0.8,0), legend=levels(group), pch=points, col=colors, ncol=2)
#legend("topleft", legend=levels(group), pch=points, col=colors, ncol=2)
dev.off()

# Create a PCA plot without a legend
#jpeg("glmQLF_plotPCA_noLegend.jpg")
plotMDS(list, col=colors[group], pch=points[group], gene.selection="common")
#dev.off()

##
#The experimental design is parametrized with a one-way layout, 
# where one coefficient is assigned to each group
design <- model.matrix(~ 0 + group)
colnames(design) <- levels(group)
#design

#Next, the NB dispersion is estimated
list <- estimateDisp(list, design, robust=TRUE)
#list$common.dispersion
#Visualize the dispersion estimates with a BCV plot
#Write plot to file
#jpeg("glmQLF_plotBCV.jpg")
plotBCV(list)
#dev.off()

#Now, estimate and plot the QL dispersions
fit <- glmQLFit(list, design, robust=TRUE)
#head(fit$coefficients)
#Write plot to file
#jpeg("glmQLF_plotQLDisp.jpg")
plotQLDisp(fit)
#dev.off()

# view column order
colnames(fit)


# testing explicit nested contrast
con.all.nest <- makeContrasts(treatment = (treat.high + treat.low) - (cntrl.high + cntrl.low),
                              levels=design)
# summary table
treat.anov.treatment <- glmTreat(fit, contrast=con.all.nest[,"treatment"], lfc=log2(1.2))
summary(decideTests(treat.anov.treatment))


# export tables of DE genes
#Write tags table of DE genes to file
tagsTblANOVATreatment <- topTags(treat.anov.treatment, n=nrow(treat.anov.treatment$table), adjust.method="fdr")$table
#write.table(tagsTblANOVATreatment, file="glmQLF_2WayANOVA_treatment_topTags_LFC1.2.csv", sep=",", row.names=TRUE, quote=FALSE)


# MD plots
#Write plot to file
#jpeg("glmQLF_2WayANOVA_treatment_plotMD_LFC1.2.jpg")
plotMD(treat.anov.treatment)
abline(h=c(-1, 1), col="blue")
#dev.off()


# Volcano plots
# add column for identifying direction of DE gene expression
tagsTblANOVATreatment$topDE <- "NA"
# identify significantly up DE genes
tagsTblANOVATreatment$topDE[tagsTblANOVATreatment$logFC > 1 & tagsTblANOVATreatment$FDR < 0.05] <- "UP"
# identify significantly down DE genes
tagsTblANOVATreatment$topDE[tagsTblANOVATreatment$logFC < -1 & tagsTblANOVATreatment$FDR < 0.05] <- "DOWN"
# create volcano plot
#jpeg("glmQLF_2WayANOVA_treatment_volcano_LFC1.2.jpg")
ggplot(data=tagsTblANOVATreatment, aes(x=logFC, y=-log10(FDR), color = topDE)) + 
  geom_point() +
  theme_minimal() +
  scale_colour_discrete(type = plotColorSubset, breaks = c("Up", "Down"))
#dev.off()
# create volcano plot with labels
labelSetTreatment <- tagsTblANOVATreatment[tagsTblANOVATreatment$topDE == "UP" | tagsTblANOVATreatment$topDE == "DOWN",]
#jpeg("glmQLF_2WayANOVA_treatment_volcanoLabeled_LFC1.2.jpg")
ggplot(data=tagsTblANOVATreatment, aes(x=logFC, y=-log10(FDR), color = topDE)) + 
  geom_point() +
  ggrepel::geom_text_repel(data = labelSetTreatment, aes(label = row.names(labelSetTreatment))) +
  theme_minimal() +
  scale_colour_discrete(type = plotColorSubset, breaks = c("Up", "Down"))
#dev.off()
# identify significantly DE genes by FDR
tagsTblANOVATreatment.glm_keep <- tagsTblANOVATreatment$FDR < 0.05
# create filtered results table of DE genes
tagsTblANOVATreatment.filtered <- tagsTblANOVATreatment[tagsTblANOVATreatment.glm_keep,]

# heatmap
# view DGE genes
# subset counts table by DE gene set
DGESubset <- tagsTblANOVATreatment[!grepl("NA", tagsTblANOVATreatment$topDE),]
logcounts = cpm(list, log=TRUE)
logcountsSubset <- subset(logcounts,
                          grepl(
                            paste0(rownames(DGESubset), collapse = "|"),
                            rownames(logcounts),
                            ignore.case = TRUE
                          )
                        )
heatmap(logcountsSubset, main= "Heatmap of DGE")
