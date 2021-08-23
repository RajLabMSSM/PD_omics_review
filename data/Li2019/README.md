## Description 

Results from [Garfield](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6908448/) enrichment analyses across multiple GWAS.

## [File descriptions](https://bioconductor.org/packages/release/bioc/manuals/garfield/man/garfield.pdf)

Output files: 

**out.file.prep**: contains the genomic positions of pruned variants, p-values for association with the trait of interest, number of LD tags (r2>0.8), MAF, distance to the nearest TSS andbinary representation of annotation information (with LD-tagging r2>0.8). 

**out.file.perm**: contains enrichment analysis results for each annotation, where PThresh is the GWAS p-value threshold used for analysis, FE denotes the fold enrichment statistic (equals -1 if no sufficient data was available for the FE calculation), EmpPval shows the empirical p-value of enrichment (equals -1 if FE is calculated but significance of enrichment analysis is not run at that threshold), NAnnotThresh - the number of variants at the threshold which are annotated with the given feature, NAnnot - the total number of annotated variants, NThresh - the total number of variants at that threshold and N - the total number of pruned variants. The remaining columns show additional information on the annotations used for analysis.


## Publication 

Li, Y.I., Wong, G., Humphrey, J. et al. Prioritizing Parkinson’s disease genes using population-scale transcriptomic data. Nat Commun 10, 994 (2019). https://doi.org/10.1038/s41467-019-08912-9

## Location on Mount Sinai's HPC (Minerva) 

/sc/arion/projects/ad-omics/wongg05/garfield/garfield-data/output
 