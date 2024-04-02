## Functional Analysis



### Objectives

<i>After completing this tutorial</i> you will know how to:
* EXAMPLE

## EXAMPLE



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

The above code can also be found at the top of the R script for the edgeR app, which is located in the <i>apps</i> directory of the GitHub repository that we downloaded.

## Data Format

Before running the topGO functional analysis app, make sure to have ready the two tables with the <b>gene scores</b> and <b>mappings</b>. Example data for the topGO app can be found in the <i>data/functionalAnalysis</i> directory of the GitHub repository.



## Analysis Workflow

To run the EXAMPLE app, open the R <b>script app_shiny_EXAMPLE.R</b> in Posit and press the <i>Run App</i> button in the upper right corner of the [source pane](https://docs.posit.co/ide/user/ide/guide/ui/ui-panes.html).

### Part One: Getting Started

Start in the left-hand sidebar by:
1. entering the statistic for gene scoring, for example:
* FDR from DGE results
* module number from WGCNA results
2. entering the expression for gene scoring, for example:
* < 0.05 for specifying significant DEGs using a FDR cut off
* == 1 for specifying a specific module number from WGCNA
3. uploading a gene score table .csv file with the unfiltered results table from DGE or WGCNA
4. uploading a mappings table .txt file with the gene-to-GO term annotation mappings formatted as either:
* topGO expected gene-to-GO mappings
* pannzer2 resulting GO prediction details
5. clicking the Upload button to check that the inputs are valid, which appears after the format of the inputs are checked
6. clicking the Run Analysis button, which appears after the input files are verified as valid for analysis

#### Helpful Tips
1. The topGO package expects gene-to-GO mappings files to be specifically formatted where:
* the first column must contain gene IDs and the second column GO terms
* the second column of GO terms must be in a comma separated list format
* the first column must be tab or space separated from the second column
* the first column of gene IDs must match the gene IDs contained in the gene score table
2. It is possible to create a gene-to-GO term annotations table with PANNZER2 by:
* first, navigating to the Annotate tab
* second, uploading a list of protein sequences where the sequence names must match the gene names in the input gene score table
* third, selecting Batch queue and entering your email
* fourth, selecting the GO prediction details link after recieving the pannzer2 results
* fifth, right clicking and selecting Save As... to download the GO.out.txt annotations table
3. The input gene score table should not be filtered in advance. The functional analysis requires the complete gene universe, which includes all genes detected in the experiment regardless of signifigance in DGE or WGCNA.
4. The input gene score statistic must match the name of a column in the input gene score table.
5. The first column of the gene score table is expected to contain gene IDs.
6. The gene score tables are required to contain two columns with gene IDs and gene scores at minimum.

### Part Two: Data Normalization



### Part Three: Data Exploration



### Part Four: Analysis & Results



## Example Script

> [!NOTE]
> An example R script named <b>example_script_EXAMPLE.R</b> with code for performing the same analysis as in the edgeR R Shiny app can be found in the <i>scripts</i> directory of the GitHub repository. 

The script is hard coded to work for an example data set and will need to be customized for use with other data sets.
