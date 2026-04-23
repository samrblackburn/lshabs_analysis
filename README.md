# Analysis of Lake Superior, St. Louis River, and Connected Tributary Nutrient, Chlorophyll, and Cyanobacterial Bloom Data

This work is an extension of the [related data synthesis project](https://github.com/Lake-Superior-Reserve/ls_habs_synthesis/tree/main).

It documents the code used to create summaries and figures from that dataset which are used in Blackburn et al.

## Structure

- `/data` has csv versions of data objects created in the data synthesis pipeline. It also has flowlines for tributaries, used in a map figure
- `/R` has the actual code. `analysis.R` creates summarized and filtered data used in the figures and referenced in the paper. `/figures` has code to create each figure, separated by figure and data type.
- `/figures` has pregenerated versions of the figures.


## Running Pipeline

This repository is setup as an automated pipeline using the [`targets`](https://books.ropensci.org/targets/) R package.
Run the following snippet to create all data objects and figures:

``` r
# install required packages
install.packages("targets", "tidyverse", "sf", "maptiles", "ggspatial", "patchwork", "khroma", "ggnewscale", "ggrepel", "ggpmisc", "climaemet")

# run pipeline
targets::tar_make()
```