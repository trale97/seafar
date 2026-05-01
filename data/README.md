# Dataset provenance of `autism.rda`

The original data set from @nishimura2007genome is publicly available and can be downloaded from \href{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE7329}{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE7329}. We followed the steps done by @guerra2023sparsifying to obtain and process the data prior to the analyses in this manuscript.

## Recreating the dataset from source

The dataset was derived from the GEO **Series Matrix** file of the dataset in the following manner:  

1.  download the original compressed Series Matrix file from NCBI GEO,
2.  extract sample labels and expression values,
3.  remove mislabeled individuals,
4.  transpose and standardizes the expression matrix,
5.  remove features with missing values after scaling, and
6.  save the resulting processed matrix for further use.

To recreate the preprocessed dataset from its source you can run `autism <- reproduce_autism_dataset()`. This is equivalent to making the dataset available via `data("autism")`, as we do in the tutorial paper.

## Terms of use

The original dataset is hosted by NCBI on their public ftp infrastructure. Their general terms of use state that data may be [used and distributed without restrictions](https://www.ncbi.nlm.nih.gov/geo/info/disclaimer.html). On their ftp server itself, they state that all data there is "[public, non-sensitive, unrestricted scientific data intended for sharing among scientific communities](https://ftp.ncbi.nlm.nih.gov/README.ftp)". Please adhere to these terms when sharing `seafar`, the preprocessed datasets or modified versions of it. 
