## Functional Analysis

Functional analysis is a typical approach to interpreting gene expression data and identify potential biological mechanisms. After determining sets significantly of DE genes or genes associated with different network modules, it is possible to perform enrichment or over-representation analysis. 

### Objectives

<i>After completing this tutorial</i> you will know how to:
* perform enrichment and over-representation analysis
* use different types of plots to explore the data and results
* retrieve tables of significant biological process, molecular function, and cellular component GO terms

### Background



## topGO



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

To run the functional analysis app, open the R <b>script app_shiny_functionalAnalysis.R</b> in Posit and press the <i>Run App</i> button in the upper right corner of the [source pane](https://docs.posit.co/ide/user/ide/guide/ui/ui-panes.html).

After the app is launched you will see the following pages:
1. <i>Getting Started</i> page with information for uploading data to start the analysis
2. <i>Processing</i> page that indicates the analysis has begun running
3. a page with separate tabs for each step in the analysis workflow, including:
* Tips 
* Analysis
* Results
* Information

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
3. Valid statistic for gene scoring include FDR from DGE or module number from WGCNA, and must be the same name as a column in the input gene score table.
4. Valid expressions for gene scoring include:
* < 0.05 for specifying significant DEGs using a FDR cut off
* == 1 for specifying a specific module number from WGCNA
5. The input gene score table should not be filtered in advance. The functional analysis requires the complete gene universe, which includes all genes detected in the experiment regardless of signifigance in DGE or WGCNA.
6. The input gene score statistic must match the name of a column in the input gene score table.
7. The first column of the gene score table is expected to contain gene IDs.
8. The gene score tables are required to contain two columns with gene IDs and gene scores at minimum.

### Part Two: Analysis

#### Functional Analysis

Begin the functional enrichment or over-representation analysis by selecting a test statistic and algorithm.

> [!NOTE]
> Further details about the available types of enrichment tests can be found in the topGO manual (e.g., section 6).
> Also, refer to the topGO manual for more information regarding the available algorithms and test statistics.

Available algorithms:
1. Default algorithm used by the topGO package is a mixture between the elim and weight algorithms
2. Classic algorithm performs functional analysis by testing the over-representation of GO terms within the group of diferentially expressed genes
3. Elim algorithm is more conservative then the classic method and you may expect the p-values returned by the former method to be lower bounded by the p-values returned by the later method

Available test statistics:
1. Fisher's exact test is based on gene counts and can be used to perform over representation analysis of GO terms
2. Kolmogorov-Smirnov (KS) like test computes enrichment or rank based on gene scores and can be used to perform gene set enrichment analysis (GSEA)

> [!TIP]
> It is possible to use both the fisher's and KS tests for DE analysis results since each gene in the results table has a score, which represents how it is diferentially expressed.

#### Range of GO Term P-Values

The histograms shows the range and frequency of p-values from the enrichment tests for each GO level (BP, MF, or CC).

#### Density Plots of GO Terms

To draw the density plot for a particular GO term, select a GO term category and enter a GO term ID.

> [!TIP]
> Only significant GO terms may be plotted.

The density plot shows the distribution of the gene's rank for the top GO term of each GO level (BP, MF, or CC). The gene's rank is compared with the null distribution.

> [!CAUTION]
> Make sure that the GO category is valid for the input GO term ID.

#### Dot Plot of Most Significant GO Terms

The dot plot of most significant GO terms requires the selection of a p-value cut off.

> [!NOTE]
> Note that the computed p-values are unadjusted for multiple testing.

The dot plot shows the <i>up to the top 5</i> most enriched or overrepresented GO terms for each level (BP, MF, CC). The size of the dots indicate the number of significant genes annotated to the GO term. The dots are colored by the enrichment test p-values.

#### Subgraphs of Most Significant GO Terms

To draw the subgraphs of most significant GO terms, select a GO term category and the number of nodes.

The subgraph induced by the selected number of top (most significant) GO terms identifed by the selected algorithm for scoring GO terms for enrichment. Rectangles indicate the most signifcant terms with colors representing the relative signifcance, which ranges from dark red (most signifcant) to bright yellow (least signifcant).

For each node, some basic information is displayed. The frst two lines show the GO identifer and a trimmed GO name. In the third line the raw p-value is shown. The forth line is showing the number of signifcant genes and the total number of genes annotated to the respective GO term.

> [!NOTE]
> The subgraphs must be downloaded to be viewed.

### Part Three: Results

#### Functional Analysis Results

Results from the GO term enrichment analysis may be downloaded by selecting a GO term category and p-value cut off.

It is also possible to download the table of gene-to-GO term annotation mappings for each gene, which has been formatted for use with topGO.

> [!NOTE]
> Note that the computed p-values are unadjusted for multiple testing.

## Example Script

> [!NOTE]
> An example R script named <b>script_EXAMPLE.R</b> with code for performing the same analysis as in the edgeR R Shiny app can be found in the <i>scripts</i> directory of the GitHub repository. 

The script is hard coded to work for an example data set and will need to be customized for use with other data sets.
