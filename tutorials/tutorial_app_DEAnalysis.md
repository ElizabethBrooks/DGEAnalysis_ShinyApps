## Differential Expression (DE) Analysis

A common task when working with transcriptomic data is the identification of differentially expressed (DE) genes or tags between groups. In this tutorial users will learn how to perform biostatistical analysis with the edgeR shiny app, including pairwise and analysis of variance (ANOVA) like comparisons to identify significantly DE genes.

### Objectives

<i>After completing this tutorial</i> you will know how to:
* analyze quantified transcriptomic data
* make comparisons using the experimental design
* perform pairwise comparisons
* perform ANOVA like comparisons
* use different types of plots to explore the data and results
* filter tables of DE genes by statistical and biological significance 
* retrieve tables of DE genes

## edgeR

In this tutorial we will be performing differential expression (DE) analysis using [edgeR](https://bioconductor.org/packages/release/bioc/html/edgeR.html) R package. 

Note that the DESeq2 and edgeR packages perform similarly and have the same underlying hypothesis that most genes are not DE, but use different normalization methods that may change the number of detected DE genes.

## Installation

> [!TIP]
> The edgeR app can be run on a computer locally using Posit. To run the app locally we need to download the edgeR R Shiny app script. The script is in a GitHub repository and can be downloaded [HERE](https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps/tree/main).

<b>First,</b> download the GitHub repository using the git clone command in the terminal, for example:

```
git clone https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps.git
```

<b>Next,</b> if running the app locally, we will need to install or update [R and Posit](https://posit.co/download/rstudio-desktop/) (formerly RStudio).

<b>Lastly,</b> we need to install all of the necessary R packages with the software needed to run edgeR and create plots with the results:

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

The above code can also be found at the top of the R script for the edgeR app, which is located in the <i>apps</i> directory of the GitHub repository that we downloaded.

## Data Format

Before running the edgeR DE analysis app, make sure to have ready the two .csv files with the <b>gene counts</b> and <b>experimental design</b>. Example data for the edgeR app can be found in the <i>data/DEAnalysis</i> directory of the GitHub repository.

The following is a small example gene counts table:

```
Gene,SampleOne,SampleTwo,SampleThree,SampleFour,SampleFive,SampleSix
gene-1,0,10,111,1,0,1000,11,0
gene-2,0,20,222,2,0,2000,12,0
gene-3,0,30,333,3,0,3000,13,0
gene-4,0,40,444,4,0,4000,14,0
gene-5,0,50,555,5,0,5000,15,0
```

The experimental design file must also be in the format required by edgeR, for example: 

```
Sample,Group
SampleOne,cntrl
SampleTwo,cntrl
SampleThree,cntrl
SampleFour,treat
SampleFive,treat
SampleSix,treat
```

## Analysis Workflow

To run the edgeR app, open the R <b>script app_shiny_DEAnalysis.R</b> in Posit and press the <i>Run App</i> button in the upper right corner of the [source pane](https://docs.posit.co/ide/user/ide/guide/ui/ui-panes.html).

### Part One: Getting Started

Start in the left-hand sidebar by:

1. browsing for a .csv file with the gene counts
2. browsing for a .csv file with the experimental design
3. clicking the <i>Run Analysis</i> button, which appears after the input files are verified as valid for analysis

### Part Two: Data Normalization

After uploading the gene counts and experimental design, the sample libraries are normalized using Trimmed Mean of M-values (TMM) normalization. Normalized values are calcuated in counts per million (CPM) using the normalized library sizes. Note, however, that TMM normalization factors do not take into account library sizes.

> [!NOTE]
> A library is the collection of RNA-seq reads associated with a sample.

Here filtering is also performed to remove genes that were identified as not sufficiently expressed under the experimental conditions. The negative binomial distribution is used to identify genes with sufficiently large counts to be considered a real signal.

### Part Three: Data Exploration

#### PCA

PCAs are commonly used to visualize the signal to noise relationship within a data set. For example, the patterns of variation between and within groups.

In a principal component analysis (PCA) plot the distances between samples approximate the expression differences. The expression differences were calculated as the the average of the largest absolute LFCs between each pair of samples and the same genes were selected for all comparisons. 

Note that the points are replaced by the sample name and colored by the associated factor level.

#### MDS

In a multidimensional scaling (MDS) plot the distances between samples approximate the expression differences. The expression differences were calculated as the the average of the largest absolute LFCs between each pair of samples and the top genes were selected separately for each pairwise comparison. 

Note that the points are replaced by the sample name and colored by the associated factor level.

#### Heatmap

The heatmap shows the hierarchical clustering of individual samples by the log2 CPM expression values. Furthermore, the log2 CPM that has undefined values avoided and poorly defined LFC for low counts shrunk towards zero

#### BCV

The biological coefficient of variation (BCV) plot shows the square root of the dispersion parameter under the negative binomial model, which is equivalent to estimating the dispersions of the negative binomial model.

The negative binomial distribution is used to identify genes with sufficiently large counts to be considered a real signal and measures what it expects to be missing data, or a measure of dispersion. For example, a BCV^2 of 0.4 indicates a 20% difference between samples.

The negative binomial distribution models biological noise rather than sequencing noise (e.g., library size normalization).

### Part Four: Analysis & Results

#### pairwise

Exact tests are performed to identify differences in the means between two groups of negative-binomially distributed counts. A comparison or contrast is a linear combination of means for groups of samples.

#### GLM

The GLM is used to perform an ANOVA-like analysis to identify any significant main effect associated with an explanatory variable. An explanatory variable may be a categorical factor with two or more levels, such as treat and cntrl.

Additionally, genes above the input log2 fold change (LFC) threshold are identified as significantly DE using t-tests relative to a threshold (TREAT) with the glmTreat function of edgeR. If the input LFC cut off is set to 0 in the app then the glmQLFTest function is used instead.

## Example Script

> [!NOTE]
> An example R script named <b>example_script_DEAnalysis.R</b> with code for performing the same analysis as in the edgeR R Shiny app can be found in the <i>scripts</i> directory of the GitHub repository. 

The script is hard coded to work for an example data set and will need to be customized for use with other data sets.
