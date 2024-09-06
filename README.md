# smaR: Simple Metagenomic Analysis in R

<!-- badges: start -->
![image](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
<!-- badges: end -->

## Overview

smaR (Simple Metagenomic Analysis in R) is an R package designed to streamline and **simplify metagenomic data analysis workflows.**
It provides a set of tools for processing, analyzing, and visualizing metagenomic sequencing data, making it accessible to
researchers with varying levels of bioinformatics expertise.

It starts from already calculated and merged taxonomic profiles, i.e. a table with samples as rows and species/OTUs/ASVs/ as columns,
or viceversa, and numeric values that represent relative abundances. smaR contains functions to easily check the correctness of
the matrix, calculate alpha- and beta-diversity and, if a valid relative metadata table is provided, can perform Non-metric
Multidimensional Scaling (NMDS) and differential abundance analysis (DAA) with different methods, as well as plot their results.
In particular, it can calculate and represent the consensus between the results obtained by Maaslin2 and MaAsLin3.
The package aims at simplifying and homogenizing common metagenomic workflows.

## Installation

smaR is an R package, you can install the development version
from GitHub running in R or Rstudio:

``` r
devtools::install_github("NiCarlino/smaR")
```

### Requirements

smaR requires an R version > 4.4. See the DESCRIPTION file for the full list of other packages dependencies.


## Usage example

As a demonstration of the function included in smaR, here a basic example. Please look here  
for a complete [demo](https://github.com/NiCarlino/smaR/wiki) and [tutorial](https://github.com/NiCarlino/smaR/tree/main/vignettes).

```{r example}
library(smaR)
abundance_table <- import_abundance_table("path_to_your_table.tsv", rowtype = "samples")
```
It uploads an abundance table and check that the table is consistent with relative abundance data 
(e.g. numeric values, normalize, fill NAs) and set the correct orientation for the following steps.

In order to calculate the Shannon Index for all the samples included in the abundance table, run:

```{r}
df_alpha <- calculate_alpha_diversity(abundance_table, metric ="shannon")
```

You can also specify other metrics ("richness" or "simpson"), or even more than one if you want
to compare the results of different approaches.

Similarly, to calculate the distances among all samples you can run

```{r}
df_beta <- calculate_beta_diversity(abundance_table, metric ="shannon")
```

and you can plot these results together with metadata, in order to study their behaviour with respective to any variable

```{r}
plot_alpha_diversity(df_alpha, metadata_table, plot_type = "boxplot", plot_variables = c("disease_subtype"))

plot_beta_diversity(df_beta$result_dist, metadata_table, plot_variables =  c("study_condition", "antibiotics_current_use"))
```

Plot Alpha Diversity      |  Plot Beta Diversity
:-------------------------:|:-------------------------:
![image](/inst/images/alpha_diversity_plot_simple.png) | ![image](/inst/images/beta_diversity_plot_simple.png)

In order to study the association between single features and sample information one can run a differential
abundance analysis, identifying among the metadata which are the important variables with a fixed effect and what
are those introducing random effects.

```{r}
out_maas2 <- run_maaslin2(abundance_table, metadata_table, var_fix, var_ran)

out_maas3 <- run_maaslin3(abundance_table, metadata_table, var_fix, var_ran)
```

Being many the possible tools to use, with a large variety of different models, one way to preoceed is to
look for consensus results, comparing the output of both MaAsLin2 and MaAsLin3

```{r}
analyse_and_plot_consistent_maaslin_results(out_maas2, out_maas3, pthresh = 0.05, qthresh = 0.1, top_features = 20 )
```
![image](inst/images/main_plot.png?raw=true)

### Input data ###

smaR requires one input file:

1. Relative abundance data frame
    * Formatted with features as columns and samples as rows.
    * The transpose of this format is also okay.
    * Possible features include taxonomy (i.e. species/OTUs/ASVs) or genes (though not for all functions)
    * These can be relative abundances, not necessarily normalized.
    * This can be a filepath to a comma-delimited (csv), tab-delimited (tsv) or excel file.

For further analysis, a metadata table can be provided:

2. Metadata data frame
    * Formatted with variables as columns and samples as rows.
    * Possible metadata include disease, country, gender or age.


See an example of a an input [abundance table](https://github.com/NiCarlino/smaR/blob/main/inst/extdata/ex_abundancetable.tsv)
and [metadata_table](https://github.com/NiCarlino/smaR/blob/main/inst/extdata/ex_metadata.tsv).


### Output files ###

smaR generates two types of output files: data tables and
visualizations. In the repository [inst/outdata](https://github.com/NiCarlino/smaR/tree/main/inst/outdata)
is reported the output generated with the tutorial from 'vignettes'.

1. Data output files
    * ``alpha_diversity_matrix.csv``: values of alpha diversity calculated by ``calculate_alpha_diversity()``, table with one row per each sample, one column for each metric
    * ``beta_diversity_matrix.csv``: values of beta diversity calculated by ``calculate_beta_diversity()``, table with one row and one column per each sample, symmetric matrix with 0 diagonal.
    * ``common_features_maaslin2_maaslin3.csv``: all the statistical significant common association given by the comparison of the output of Maaslin2 vs MaAsLin3, i.e. species for which the tools show same sign of beta with respect to the same variable and the same variable value and both have p-value < p_threshold and q_value (FDR-corrected p-value) < q_threshold. Calculated by ``maas_consistency()``.
    * ``top_common_features_maaslin2_maaslin3.csv``: association from ``common_features_maaslin2_maaslin3.csv`` with the highest (in absolute value) beta-coefficient. Number of top N is chosen by the user as parameter of ``maas_consistency()`` inside of ``analyse_and_plot_consistent_maaslin_results()``.

2. Visualization output files
    * ``alpha_diversity_plot.png``: boxplot or violinplot of the distrubution of the alpha diversity according to chosen metadata variables.
    * ``beta_diversity_plot.png``: 2-dimensional scatterplot of the NMDS of the beta diversity.
    * ``venn_diagram_maaslin.png``: Numerical representation of the statistical significant associations found by MaAsLin2 and MaAsLin3. The intersection represents the dimension of ``common_features_maaslin2_maaslin3.csv``.
    * ``consensus_maaslin_plot_top_features.png``: representation of the ``top_common_features_maaslin2_maaslin3.csv`` reporting species, variable, value and quantification of the association as well as of its uncertainty and statistical significance.

In addition, the output representing the results given by the models of MaAsLin2 and MaAsLin3 can be saved in output,
similarly to what one would have running the single tool.

## Running a demo

Please look here for a complete [demo](https://github.com/NiCarlino/smaR/wiki) and [tutorial](https://github.com/NiCarlino/smaR/tree/main/vignettes).


## License

This project is licensed under the [GPL-3 License](https://github.com/NiCarlino/smaR/blob/main/LICENSE).


## References

William A. Nickols et al., [MaAsLin 3: Refining and extending generalized multivariate linear models for meta-omic association discovery.](https://github.com/biobakery/biobakery/wiki/MaAsLin3), (In progress).

Mallick H. et al., [Multivariable Association Discovery in Population-scale Meta-omics Studies](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009442), PLoS Computational Biology, (2021), 17(11):e1009442.

Carlino et al., [Unexplored microbial diversity from 2,500 food metagenomes and links with the human microbiome](https://linkinghub.elsevier.com/retrieve/pii/S009286742400833X), Cell, 2024 (https://doi.org/10.1016/j.cell.2024.07.039).

[Human Microbiome Project Consortium. A framework for human microbiome research.](https://www.nature.com/articles/nature11209) Nature. 2012 Jun 13;486(7402):215-21. doi: 10.1038/nature11209. PMID: 22699610; PMCID: PMC3377744.

Blanco-Miguez A. et al., [Extending and improving metagenomic taxonomic profiling with uncharacterized species using MetaPhlAn 4.](https://www.nature.com/articles/s41587-023-01688-w) , Nature Biotechnology (2023)

## Contact

Niccolò Carlino

URL: https://github.com/NiCarlino/smaR
