#!/usr/bin/env Rscript

# created by: Elizabeth Brooks
# last update: 19 Feb 2024

#if (!requireNamespace("BiocManager", quietly=TRUE))
#   install.packages("BiocManager")
#BiocManager::install('_______')

#Load the libraries
#library(filesstrings)
library(topGO)
#library(edgeR)
#library(GO.db)
#library(reshape2)
library(ggplot2)
library(Rgraphviz)
#library(statmod)
#library("animation")


# turn off scientific notation
options(scipen = 999)

# the following setting is important, do not omit.
options(stringsAsFactors = FALSE)


# retrieve input DE results
#DGE_results_table <- read.csv(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example5_mycobacterium_topDEGs.csv", row.names=1)
DGE_results_table <- read.csv(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example6_daphnia_topDEGs.csv", row.names=1)

# retrieve mappings created by pannzer2
#GOmaps_pannzer <- read.delim(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example5_mycobacterium_GO.out.txt", sep = "", row.names=NULL, colClasses = c(qpid = "character", goid = "character"))
GOmaps_pannzer <- read.delim(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example6_daphnia_GO.out.txt", sep = "", row.names=NULL, colClasses = c(qpid = "character", goid = "character"))

# re-format mappings from pannzer2
GOmaps_pannzer_fmt <- split(GOmaps_pannzer$goid,GOmaps_pannzer$qpid)

# create data frame with formtted mappings
GOmaps_pannzer_out <- as.data.frame(unlist(lapply(names(GOmaps_pannzer_fmt), function(x){gsub(" ", "", toString(paste("GO:", GOmaps_pannzer_fmt[[x]], sep="")))})))
rownames(GOmaps_pannzer_out) <- names(GOmaps_pannzer_fmt)
colnames(GOmaps_pannzer_out) <- NULL

# output re-formatted mappings from pannzer2
#write.table(GOmaps_pannzer_out, file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example4_daphnia_GO.fmt.txt", sep = "\t", quote = FALSE)
write.table(GOmaps_pannzer_out, file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example6_daphnia_GO.fmt.txt", sep = "\t", quote = FALSE)

# retrieve gene to GO map
#GOmaps <- readMappings(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example4_daphnia_GO.fmt.txt")
GOmaps <- readMappings(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example6_daphnia_GO.fmt.txt")


## GO enrichment
# create named list of all genes (gene universe) and p-values
# the gene universe is set to be the list of all genes contained in the gene2GO list of annotated genes
list_genes <- as.numeric(DGE_results_table$FDR)
list_genes <- setNames(list_genes, rownames(DGE_results_table))
list_genes_filtered <- list_genes[names(list_genes) %in% names(GOmaps)]

testInput <- "== 1"
testExp <- strsplit(testInput, split = " ")[[1]][1]
testComp <- strsplit(testInput, split = " ")[[1]][2]

testCheck <- try(
  if(eval(parse(text = paste(which(colors() == "1"), testExp, which(colors() == testComp), sep=" ")))){
    print("yes")
  },
  silent = TRUE
)
if(class(testCheck) == "try-error"){
  if(eval(parse(text = paste("1", testExp, testComp, sep=" ")))){
    print("yes")
  }
}

if(eval(parse(text = paste(which(colors() == "purple3"), "==", which(colors() == "purple3"), sep=" ")))){
  print("yes")
}


# create function to return list of interesting DE genes (0 == not significant, 1 == significant)
get_interesting_DE_genes <- function(geneUniverse){
  interesting_DE_genes <- rep(0, length(geneUniverse))
  for(i in 1:length(geneUniverse)){
    tmpInput <- "< 0.05"
    if(eval(parse(text = paste(geneUniverse[i], tmpInput, sep=" ")))){
      interesting_DE_genes[i] = 1
    }
  }
  interesting_DE_genes <- setNames(interesting_DE_genes, names(geneUniverse))
  return(interesting_DE_genes)
}

# create topGOdata objects for enrichment analysis (1 for each ontology)
BP_GO_data <- new('topGOdata', ontology = 'BP', allGenes = list_genes_filtered, 
                  geneSel = get_interesting_DE_genes, nodeSize = 10, annot = annFUN.gene2GO, 
                  gene2GO = GOmaps)
#MF_GO_data <- new('topGOdata', ontology = 'MF', allGenes = list_genes_filtered, 
#                  geneSel = get_interesting_DE_genes, nodeSize = 10, annot = annFUN.gene2GO, 
#                  gene2GO = GOmaps)
#CC_GO_data <- new('topGOdata', ontology = 'CC', allGenes = list_genes_filtered, 
#                  geneSel = get_interesting_DE_genes, nodeSize = 10, annot = annFUN.gene2GO, 
#                  gene2GO = GOmaps)

#Summary functions
#numGenes(BP_GO_data)
#length(sigGenes(BP_GO_data))
#numGenes(MF_GO_data)
#length(sigGenes(MF_GO_data))
#numGenes(CC_GO_data)
#length(sigGenes(CC_GO_data))

# performGO enrichment using the topGOdata objects
BP_GO_results <- runTest(BP_GO_data, statistic = 'ks')
#BP_GO_results <- runTest(BP_GO_data, statistic = 'fisher')
#MF_GO_results <- runTest(MF_GO_data, statistic = 'Fisher')
#CC_GO_results <- runTest(CC_GO_data, statistic = 'Fisher')

# check the names of GO terms
#head(names(BP_GO_results@score))
#geneData(BP_GO_results)

# store p-values as named list... ('score(x)' or 'x@score' returns named list of p-val's 
# where names are the GO terms)
pval_BP_GO <- score(BP_GO_results)
#pval_MF_GO <- score(MF_GO_results)
#pval_CC_GO <- score(CC_GO_results)

# plot histogram to see range of p-values
#exportFile <- paste(set, "pValueRanges.pdf", sep="_")
#pdf(file=exportFile)
#par(mfrow=c(3, 1),mar=c(1,1,1,1))
hist(pval_BP_GO, 35, xlab = "p-values", main = "Range of BP GO term p-values")
#hist(pval_MF_GO, 35, xlab = "p-values", main = "Range of MF GO term p-values")
#hist(pval_CC_GO, 35, xlab = "p-values", main = "Range of CC GO term p-values")
#dev.off()

# get statistics on GO terms
list_BP_GO_terms <- usedGO(BP_GO_data)
#list_MF_GO_terms <- usedGO(MF_GO_data)
#list_CC_GO_terms <- usedGO(CC_GO_data)

BP_GO_results_table <- GenTable(BP_GO_data, weightFisher = BP_GO_results, orderBy = 'weightFisher', 
                                topNodes = length(list_BP_GO_terms))
#MF_GO_results_table <- GenTable(MF_GO_data, weightFisher = MF_GO_results, orderBy = 'weightFisher', 
#                                topNodes = length(list_MF_GO_terms))
#CC_GO_results_table <- GenTable(CC_GO_data, weightFisher = CC_GO_results, orderBy = 'weightFisher', 
#                                topNodes = length(list_CC_GO_terms))

# write table of GO terms to a CSV file
#write.table(BP_GO_results_table, file=paste(set, "BP_GO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)
#write.table(MF_GO_results_table, file=paste(set, "MF_GO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)
#write.table(CC_GO_results_table, file=paste(set, "CC_GO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)

# create table of significant GO terms
BP_sigGO_results_table <- BP_GO_results_table[BP_GO_results_table$weightFisher <= 0.05, ]
#MF_sigGO_results_table <- MF_GO_results_table[MF_GO_results_table$weightFisher <= 0.05, ]
#CC_sigGO_results_table <- CC_GO_results_table[CC_GO_results_table$weightFisher <= 0.05, ]

# write table of significant GO terms to a CSV file
#write.table(BP_sigGO_results_table, file=paste(set, "BP_sigGO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)
#write.table(MF_sigGO_results_table, file=paste(set, "MF_sigGO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)
#write.table(CC_sigGO_results_table, file=paste(set, "CC_sigGO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)

# retrieve most significant GO term
BP_topSigGO_ID <- BP_GO_results_table[1, 'GO.ID']
#MF_topSigGO_ID <- MF_GO_results_table[1, 'GO.ID']
#CC_topSigGO_ID <- CC_GO_results_table[1, 'GO.ID']

# create density plots
#pdf(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/tmp/example3_daphnia_topSigGO_Density.pdf")
showGroupDensity(BP_GO_data, whichGO = BP_topSigGO_ID, ranks = TRUE)
#showGroupDensity(MF_GO_data, whichGO = MF_topSigGO_ID, ranks = TRUE)
#showGroupDensity(CC_GO_data, whichGO = CC_topSigGO_ID, ranks = TRUE)
#dev.off()

# plot subgraphs induced by the most significant GO terms and save to a PDF file
#printGraph(BP_GO_data, BP_GO_results, firstSigNodes = 5, 
#           fn.prefix = "BP_sigGO_subgraphs", useInfo = "all", pdfSW = TRUE)
#printGraph(MF_GO_data, MF_GO_results, firstSigNodes = 5, 
#           fn.prefix ="MF_sigGO_subgraphs", useInfo = "all", pdfSW = TRUE)
#printGraph(CC_GO_data, CC_GO_results, firstSigNodes = 5, 
#           fn.prefix = "CC_sigGO_subgraphs", useInfo = "all", pdfSW = TRUE)


## 
# testing
##

#showSigOfNodes(BP_GO_data, score(BP_GO_results), firstSigNodes = 5, useInfo = 'all')

#im.convert("BP_sigGO_subgraphs_weight01_5_all.pdf", output = "BP_sigGO_subgraphs_weight01_5_all.png", extra.opts="-density 150")

# function to retrieve interesting genes
retrieveInteresting <- function(){
  # split the input expression string
  inputExp <- strsplit(input$universeCut, split = " ")[[1]][1]
  inputStr <- strsplit(input$universeCut, split = " ")[[1]][2]
  # retrieve color list
  colorList <- colors()
  # function that returns list of interesting DE genes (0 == not significant, 1 == significant)
  get_interesting_DE_genes <- function(geneUniverse){
    interesting_DE_genes <- rep(0, length(geneUniverse))
    # check for color string inputs
    testCheck <- try(
      if(eval(parse(text = paste(which(colorList == geneUniverse[1]), inputExp, which(colorList == inputStr), sep=" ")))){
        print("yes")
      },
      silent = TRUE
    )
    if(class(testCheck) == "try-error"){
      for(i in 1:length(geneUniverse)){
        if(eval(parse(text = paste(geneUniverse[1], inputExp, inputStr, sep=" ")))){
          interesting_DE_genes[i] = 1
        }
      }
    }else{
      for(i in 1:length(geneUniverse)){
        if(eval(parse(text = paste(which(colorList == geneUniverse[1]), inputExp, which(colorList == inputStr), sep=" ")))){
          interesting_DE_genes[i] = 1
        }
      }
    }
    interesting_DE_genes <- setNames(interesting_DE_genes, names(geneUniverse))
    return(interesting_DE_genes)
  }
}
