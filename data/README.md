# Dataset provenance of `autism.rda`

This dataset is a preprocessed version of the GEO dataset [GSE7329: Gene expression profiles of lymphoblastoid cells](https://ftp.ncbi.nlm.nih.gov/geo/series/GSE7nnn/GSE7329/) from [NCBI Gene Expression Omnibus (GEO)](https://www.ncbi.nlm.nih.gov/geo/).
This repository shares the processed data object for reproducibility and convenience under the terms of use mentioned below.

## Recreating the dataset from source

The dataset was derived from the GEO **Series Matrix** file of the dataset in the following manner:  

1.  downloads the original compressed Series Matrix file from NCBI GEO,
2.  extracts sample labels and expression values,
3.  removes mislabeled individuals,
4.  transposes and standardizes the expression matrix,
5.  removes features with missing values after scaling, and
6.  saves the resulting processed matrix for use in analyses and vignettes.

To recreate the preprocessed dataset from its source you can run `autism <- reproduce_autism_dataset()`. This is equivalent to making the dataset available via `data("autism")`, as we do in the tutorial paper.

## Terms of use

The original dataset is hosted by NCBI on their public ftp infrastructure. Their general terms of use state that data may be [used and distributed without restrictions](https://www.ncbi.nlm.nih.gov/geo/info/disclaimer.html). On their ftp server itself, they state that all data there is "[public, non-sensitive, unrestricted scientific data intended for sharing among scientific communities](https://ftp.ncbi.nlm.nih.gov/README.ftp)". Please adhere to these terms when sharing `seafar`, the preprocessed datasets or modified versions of it. 
