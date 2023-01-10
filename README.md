
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SDMWorkflows

<!-- badges: start -->

[![R-CMD-check](https://github.com/derek-corcoran-barrios/SDMWorkflows/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/derek-corcoran-barrios/SDMWorkflows/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of SDMWorkflows is to make a workflow for Species distribution
models for Sustainscapes

## Installation

You can install the development version of SDMWorkflows like so:

``` r
remotes::install_github("derek-corcoran-barrios/SDMWorkflows")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SDMWorkflows)
## basic example code
```

We will start by generating a list of species to be checked

``` r
Spp <- c("Abies concolor", "Abies lowiana", "Canis lupus", "Cannis lupus")
```

In this case we have four taxa to check, however we know that *Abies
concolor* and *Abies lowiana* are synonyms for the accepted name of
*Abies concolor* and that the second version of *Canis lupus* is badly
written, it would be expected that the `Clean_Taxa()` function will
recognize that there are only 2 species here and return a data frame
with these together with their taxonomy resolution,

``` r
Final_List <- Clean_Taxa(Spp)
```

Which yields the following table, the resolved species is the Species
Column

| matched_name2  | confidence | kingdom  | phylum       | order     | family   | genus | species        |
|:---------------|-----------:|:---------|:-------------|:----------|:---------|:------|:---------------|
| Abies concolor |         98 | Plantae  | Tracheophyta | Pinales   | Pinaceae | Abies | Abies concolor |
| Abies lowiana  |         98 | Plantae  | Tracheophyta | Pinales   | Pinaceae | Abies | Abies concolor |
| Canis lupus    |         99 | Animalia | Chordata     | Carnivora | Canidae  | Canis | Canis lupus    |
