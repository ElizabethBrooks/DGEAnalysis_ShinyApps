# DGEAnalysis_ShinyApps

These are R shiny applications for analyzing count data produced by genomic sequencing technologies, such as RNA-seq. The applications guide users through biological data assessment, processing and analysis. The different analyses available include differential expression (DE), network, and functional analysis.

## Installation

Each of the R shiny applications can be run locally on your computer using R and Posit.

<b>First,</b> download this GitHub repository using the git clone command in the terminal as follows.

The latest version of this application may be downloaded from this repository by clicking the green "< > Code" button near the top of the page, and then clicking "Download ZIP".

To download the code onto a local computer or server space, click the green "< > Code" button and copy the link. Then, in the terminal:

git clone https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps.git

<b>Second</b>, if running the app locally, you will need to install or update [R and Posit](https://posit.co/download/rstudio-desktop/).

<b>Third</b>, before clicking the "Run App" button in Posit, make sure to install all of the necessary R packages for each of the applications.

### Differential Gene Expression (DGE) Analysis with edgeR
```
packageList <- c("BiocManager", "shiny", "shinythemes", "ggplot2", "rcartocolor", "dplyr", "statmod", "pheatmap", "ggplotify")
biocList <- c("edgeR")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
newBioc <- biocList[!(biocList %in% installed.packages()[,"Package"])]
if(length(newPackages)){
  install.packages(newPackages)
}
if(length(newBioc)){
  BiocManager::install(newBioc)
}
```

### Weighted Gene Co-Expression Network Analysis (WGCNA)
```
packageList <- c("BiocManager", "shiny", "shinythemes", "dplyr", "matrixStats", "Hmisc", "splines", "foreach", "doParallel", "fastcluster", "dynamicTreeCut", "survival")
biocList <- c("WGCNA", "GO.db", "impute", "preprocessCore")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
newBioc <- biocList[!(biocList %in% installed.packages()[,"Package"])]
if(length(newPackages)){
  install.packages(newPackages)
}
if(length(newBioc)){
  BiocManager::install(newBioc)
}
```

### Functional Enrichment Analysis with topGO
```
packageList <- c("BiocManager", "shiny", "shinythemes", "ggplot2", "rcartocolor", "tidyr")
biocList <- c("topGO", "Rgraphviz")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
newBioc <- biocList[!(biocList %in% installed.packages()[,"Package"])]
if(length(newPackages)){
  install.packages(newPackages)
}
if(length(newBioc)){
  BiocManager::install(newBioc)
}
```

### Analysis of Set Relationships with ggVennDiagram
```
packageList <- c("BiocManager", "shiny", "shinythemes", "ggplot2", "rcartocolor", "ggVennDiagram", "gplots")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
if(length(newPackages)){
  install.packages(newPackages)
}
```

## Tutorials

### Web

Helpful information for downloading the applications in this repository can be found in the tutorial [GitHub Version Control Quick Start Guide](https://morphoscape.wordpress.com/2024/02/28/github-version-control-quick-start-guide/) 

A tutorial of the biostatistical analysis performed in this application is provided in [Downstream Bioinformatics Analysis of Omics Data with edgeR](https://morphoscape.wordpress.com/2022/08/09/downstream-bioinformatics-analysis-of-omics-data-with-edger/).

Gene tables were may be created from RNA-seq data as described in [Bioinformatics Analysis of Omics Data with the Shell & R](https://morphoscape.wordpress.com/2022/07/28/bioinformatics-analysis-of-omics-data-with-the-shell-r/).


### PDF 

The tutorials for using the applications or creating scripts for the different analyses can be found in the [tutorials](https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps/tree/main/tutorials) folder of this repository.

## Example Data Sets

Example gene counts and experimental design tables are also provided in the [data](https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps/tree/main/data) folder of this repository.

An example RNA-seq data set may be obtained from [ScienceDirect](https://www.sciencedirect.com/science/article/pii/S0147651319302684) and [NCBI](https://www.ncbi.nlm.nih.gov/bioproject/PRJNA504739/).

## Acknowledgements

The applications were created by [Elizabeth Brooks](https://www.linkedin.com/in/elizabethmbrooks/).

This project was funded by the National Science Foundation grant "Collaborative Research: EDGE FGT: Genome-wide Knock-out mutant libraries for the microcrustacean Daphnia"  (2220695/2324639  to Sen Xu and 2220696 to Michael E. Pfrender).

## Methods

Differential expression (DE) analysis to identify UVR responsive genes across D. melanica genotypes was conducted using the edgeR package v3.42.2 (Chen, Lun & Smyth, 2016) in R v4.3.0 (R Core Team, 2023). Library sizes were calculated for each RNA-seq sample before normalizing with trimmed mean of M-values (TMM) between each pair of samples. The clustering of samples with a PCA was performed using edgeR to create a multidimensional scaling (MDS) plot of the distances between gene expression profiles, in which the same genes were selected for all comparisons. Two-way ANOVAs were calculated using generalized linear models (GLMs) to identify genes with significant DE above a 1.2 log2 fold-change (LFC) threshold using t-tests relative to a threshold (TREAT) with the glmTreat function of edgeR (McCarthy & Smyth, 2009). We considered genes with a FDR adjusted p < 0.05 and above a 1.2 fold-change to be significantly DE.

## Analysis Pipeline 
![DGE Analysis Pipeline](RNASeqWorkflow_1Sep2023.jpg)

## References

