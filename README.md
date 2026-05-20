# seafar: Sparse Exploratory Approximate Factor Analysis for High-dimensional Data in R
The package contains functions to perform a Sparse Exploratory Approximate Factor Analysis (SEAFA). These methods are implementations and extensions of the Regularized ESEM method ([Le et al. (2026)](https://doi.org/10.3758/s13428-026-02960-y)) that uses approximate factor model with regularization (cardinality constraint and the LASSO penalty) to obtain simple structure for the loading matrix. A vignette was created as a tutorial paper for interested users.  

Please cite the package as shown in `CITATION.cff`.

## Installation instructions

This package requires `PCAtools` which was not available on [CRAN](https://cran.r-project.org/) at the time of writing. This can lead to problems when installing `seafar` from GitHub. We therefore recommend installing `PCAtools` from [Bioconductor](https://cran.r-project.org/web/packages/BiocManager/vignettes/BiocManager.html) before installing `seafar`.

```r
install.packages("BiocManager")
BiocManager::install("PCAtools")
```

Currently the only way to install the package with the vignette is to build it from source. This might take a few minutes depending on your system.

```r
install.packages("remotes")
remotes::install_github(
  "trale97/seafar@*release",
  build_vignettes = TRUE,
  dependencies = TRUE
)
```

You can also skip the vignette for a quicker installation:

```r
install.packages("remotes")
remotes::install_github("trale97/seafar@*release")
```

## Getting started

The best way to get started with `seafar` is to follow the tutorial paper we published for the package. You can access it in R like this:

```r
library(seafar)
browseVignettes("seafar")
```

## Reproducibility statement

The tutorial paper can be recreated from source by following the steps below:

1, Make sure you have `seafar` installed.

1. Install [Quarto](https://quarto.org/docs/get-started/).

2. If you don't have a LaTeX distribution yet, we recommend TinyTeX:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

3. Navigate into `vignettes/`

```bash
cd vignettes
```

4. Render the document by running the following command:

```bash
quarto render SEAFAR_tutorial.qmd --to apaquarto-pdf
```

## Terms of use

This package is licensed under the MIT License.

The autism dataset is licensed under the terms specified in data/README.md

The [APA Quarto template](https://github.com/wjschne/apaquarto) by W. J. Schneider is licensed under [CC0-1.0](https://creativecommons.org/publicdomain/zero/1.0/).
