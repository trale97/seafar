# seafar: Sparse Exploratory Approximate Factor Analysis for High-dimensional Data in R
The package contains functions to perform a Sparse Exploratory Approximate Factor Analysis (SEAFA). These methods are implementations and extensions of the Regularized ESEM method ([Le et al. (2026)](https://doi.org/10.3758/s13428-026-02960-y)) that uses approximate factor model with regularization (cardinality constraint and the LASSO penalty) to obtain simple structure for the loading matrix. A vignette was created as a tutorial paper for interested users.  

## Installation instructions

This package requires `PCAtools` which is not available via CRAN, which can lead to problems when installing it from GitHub. We therefore recommend installing `PCAtools` from BiocManager before installing `seafar`.

```r
install.packages("BiocManager")
BiocManager::install("PCAtools")
remotes::install_github("trale97/seafar")
```

## Reproducibility statement

The vignette can be recreated from source by following the steps below

1. Install Quarto

2. Install Tinytex

3. Navigate into `vignette/`

4. Render the document by running the following command:

```bash
quarto preview SEAFAR_tutorial.qmd --to apaquarto-pdf --no-watch-inputs --no-browse
```

## Terms of use

This package is licensed under the MIT License.

The autism dataset is licensed under the terms specified in data/README.md

The [APA Quarto template](https://github.com/wjschne/apaquarto/tree/main) is licensed under CC0-1.0.
