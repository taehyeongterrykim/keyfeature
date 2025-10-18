
# keyfeature

<!-- badges: start -->
<!-- badges: end -->

**keyfeature** is an R package that runs *Key Feature (KF) analysis* — a corpus-based method for identifying lexico-grammatical features that distinguish a **target** corpus and a **reference** corpus (Egbert & Biber, 2023).

The package reads two files (`.csv`, `.xlsx`, or `.xls`), each containing normalized rates of linguistic feature occurrences (rows = texts, columns = features). It computes descriptive statistics and effect sizes (Cohen’s *d*) for each feature and visualizes the results with a plot.

---

## Features

- Reads two `.csv`, `.xlsx`, and `.xls` files  
- If unspecified in the argument, automatically selects overlapping (numeric) features across two files  
- Computes for each feature:
  - Mean and SD for target and reference corpora  
  - Pooled SD = √((SDₜ² + SDᵣ²)/2)  
  - Cohen’s *d* = (meanₜ − meanᵣ) / pooled SD  
- Produces a plot  
- Returns a data frame sorted by Cohen’s *d* values

## Installation

To install the development version of **keyfeature**, open R or RStudio and run one of the following commands (they install the package directly from [GitHub](https://github.com/taehyeongterrykim/keyfeature) using [`pak`](https://pak.r-lib.org/) or [`devtools`](https://devtools.r-lib.org/)):

```r
# Using pak
install.packages("pak")
pak::pak("taehyeongterrykim/keyfeature")

# Or using devtools
install.packages("devtools")
devtools::install_github("taehyeongterrykim/keyfeature")
```

## Example

``` r
# After installing the package

library(keyfeature)

# Example 1: Automatically include all overlapping (numeric) features in the files
# (If files are not in the working directory, use full file paths)
keyfeature("target.xlsx", "reference.xlsx")

# Example 2: Specify features explicitly
keyfeature(
  target    = "target.csv",
  reference = "reference.csv",
  features  = c("feature_a", "feature_b", "feature_c")
)

# Example 3: Adjust plot range and axis interval
keyfeature(
  target     = "target.xlsx",
  reference  = "reference.xlsx",
  xlim       = c(-3, 3),      # adjust X-axis range
  xinterval  = 0.25           # adjust X-axis interval
)

```

## Reference

Egbert, J., & Biber, D. (2023). Key feature analysis: A simple, yet powerful method for comparing text varieties. *Corpora, 18*(1), 121–133. https://doi.org/10.3366/cor.2023.0275

