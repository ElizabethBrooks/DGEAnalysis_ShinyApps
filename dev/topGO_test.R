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


# turn off scientific notation
options(scipen = 999)

# the following setting is important, do not omit.
options(stringsAsFactors = FALSE)


# retrieve gene to GO map
GOmaps <- readMappings(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example4_daphnia_geneToGO.txt")

# retrieve input DE results
DGE_results_table <- read.csv(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/topGO/example4_daphnia_topDEGs.csv", row.names=1)


## GO enrichment
# create named list of all genes (gene universe) and p-values
# the gene universe is set to be the list of all genes contained in the gene2GO list of annotated genes
list_genes <- as.numeric(DGE_results_table$FDR)
list_genes <- setNames(list_genes, rownames(DGE_results_table))
list_genes_filtered <- list_genes[names(list_genes) %in% names(GOmaps)]

# create function to return list of interesting DE genes (0 == not significant, 1 == significant)
get_interesting_DE_genes <- function(geneUniverse){
  interesting_DE_genes <- rep(0, length(geneUniverse))
  for(i in 1:length(geneUniverse)){
    if(geneUniverse[i] < 0.05){
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
MF_GO_data <- new('topGOdata', ontology = 'MF', allGenes = list_genes_filtered, 
                  geneSel = get_interesting_DE_genes, nodeSize = 10, annot = annFUN.gene2GO, 
                  gene2GO = GOmaps)
CC_GO_data <- new('topGOdata', ontology = 'CC', allGenes = list_genes_filtered, 
                  geneSel = get_interesting_DE_genes, nodeSize = 10, annot = annFUN.gene2GO, 
                  gene2GO = GOmaps)

#Summary functions
#numGenes(BP_GO_data)
#length(sigGenes(BP_GO_data))
#numGenes(MF_GO_data)
#length(sigGenes(MF_GO_data))
#numGenes(CC_GO_data)
#length(sigGenes(CC_GO_data))

# performGO enrichment using the topGOdata objects
BP_GO_results <- runTest(BP_GO_data, statistic = 'Fisher')
MF_GO_results <- runTest(MF_GO_data, statistic = 'Fisher')
CC_GO_results <- runTest(CC_GO_data, statistic = 'Fisher')

# check the names of GO terms
#head(names(BP_GO_results@score))
#geneData(BP_GO_results)

# store p-values as named list... ('score(x)' or 'x@score' returns named list of p-val's 
# where names are the GO terms)
pval_BP_GO <- score(BP_GO_results)
pval_MF_GO <- score(MF_GO_results)
pval_CC_GO <- score(CC_GO_results)

# plot histogram to see range of p-values
#exportFile <- paste(set, "pValueRanges.pdf", sep="_")
#pdf(file=exportFile)
#par(mfrow=c(3, 1),mar=c(1,1,1,1))
hist(pval_BP_GO, 35, xlab = "p-values", main = "Range of BP GO term p-values")
hist(pval_MF_GO, 35, xlab = "p-values", main = "Range of MF GO term p-values")
hist(pval_CC_GO, 35, xlab = "p-values", main = "Range of CC GO term p-values")
#dev.off()

# get statistics on GO terms
list_BP_GO_terms <- usedGO(BP_GO_data)
list_MF_GO_terms <- usedGO(MF_GO_data)
list_CC_GO_terms <- usedGO(CC_GO_data)

BP_GO_results_table <- GenTable(BP_GO_data, weightFisher = BP_GO_results, orderBy = 'weightFisher', 
                                topNodes = length(list_BP_GO_terms))
MF_GO_results_table <- GenTable(MF_GO_data, weightFisher = MF_GO_results, orderBy = 'weightFisher', 
                                topNodes = length(list_MF_GO_terms))
CC_GO_results_table <- GenTable(CC_GO_data, weightFisher = CC_GO_results, orderBy = 'weightFisher', 
                                topNodes = length(list_CC_GO_terms))

# write table of GO terms to a CSV file
#write.table(BP_GO_results_table, file=paste(set, "BP_GO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)
#write.table(MF_GO_results_table, file=paste(set, "MF_GO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)
#write.table(CC_GO_results_table, file=paste(set, "CC_GO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)

# create table of significant GO terms
BP_sigGO_results_table <- BP_GO_results_table[BP_GO_results_table$weightFisher <= 0.05, ]
MF_sigGO_results_table <- MF_GO_results_table[MF_GO_results_table$weightFisher <= 0.05, ]
CC_sigGO_results_table <- CC_GO_results_table[CC_GO_results_table$weightFisher <= 0.05, ]

# write table of significant GO terms to a CSV file
#write.table(BP_sigGO_results_table, file=paste(set, "BP_sigGO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)
#write.table(MF_sigGO_results_table, file=paste(set, "MF_sigGO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)
#write.table(CC_sigGO_results_table, file=paste(set, "CC_sigGO_terms.csv", sep="_"), sep=",", row.names=FALSE, quote=FALSE)

# retrieve most significant GO term
BP_topSigGO_ID <- BP_GO_results_table[1, 'GO.ID']
MF_topSigGO_ID <- MF_GO_results_table[1, 'GO.ID']
CC_topSigGO_ID <- CC_GO_results_table[1, 'GO.ID']

# create density plots
#pdf(file = "/Users/bamflappy/Repos/DGEAnalysis_ShinyApps/data/tmp/example3_daphnia_topSigGO_Density.pdf")
showGroupDensity(BP_GO_data, whichGO = BP_topSigGO_ID, ranks = TRUE)
showGroupDensity(MF_GO_data, whichGO = MF_topSigGO_ID, ranks = TRUE)
showGroupDensity(CC_GO_data, whichGO = CC_topSigGO_ID, ranks = TRUE)
#dev.off()

# plot subgraphs induced by the most significant GO terms and save to a PDF file
printGraph(BP_GO_data, BP_GO_results, firstSigNodes = 5, 
           fn.prefix = "BP_sigGO_subgraphs", useInfo = "all", pdfSW = TRUE)
printGraph(MF_GO_data, MF_GO_results, firstSigNodes = 5, 
           fn.prefix ="MF_sigGO_subgraphs", useInfo = "all", pdfSW = TRUE)
printGraph(CC_GO_data, CC_GO_results, firstSigNodes = 5, 
           fn.prefix = "CC_sigGO_subgraphs", useInfo = "all", pdfSW = TRUE)

# testing
#showSigOfNodes(BP_GO_data, score(BP_GO_results), firstSigNodes = 5, useInfo = 'all')
