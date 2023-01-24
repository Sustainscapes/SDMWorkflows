
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SDMWorkflows

<!-- badges: start -->

[![R-CMD-check](https://github.com/Sustainscapes/SDMWorkflows/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Sustainscapes/SDMWorkflows/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of SDMWorkflows is to make a workflow for Species distribution
models for Sustainscapes

## Installation

You can install the development version of SDMWorkflows like so:

``` r
remotes::install_github("Sustainscapes/SDMWorkflows")
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
#> Joining, by = "Taxa"
#> Joining, by = "matched_name2"
```

Which yields the following table, the resolved species is the Species
Column

| Taxa           | matched_name2  | confidence | canonicalName  | kingdom  | phylum       | order     | family   | genus | species        | rank    |
|:---------------|:---------------|-----------:|:---------------|:---------|:-------------|:----------|:---------|:------|:---------------|:--------|
| Abies concolor | Abies concolor |         98 | Abies concolor | Plantae  | Tracheophyta | Pinales   | Pinaceae | Abies | Abies concolor | SPECIES |
| Abies lowiana  | Abies lowiana  |         98 | Abies lowiana  | Plantae  | Tracheophyta | Pinales   | Pinaceae | Abies | Abies concolor | SPECIES |
| Canis lupus    | Canis lupus    |         99 | Canis lupus    | Animalia | Chordata     | Carnivora | Canidae  | Canis | Canis lupus    | SPECIES |
