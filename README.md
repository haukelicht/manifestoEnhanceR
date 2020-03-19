# The {manifestoEnhanceR} package

The {manifestoEnhanceR} package implements functionality to enhance {manifestoR} data objects.
This includes functions to convert manifestoR data obejcts to tidy data frames, and a function to convet tidy manifesto data frames to XML documents.

## Functions

- `as_tibble` methods for {manifestoR} 'ManifestoCorpus' and 'ManifestoDocument' classes
- `enhance_manifesto_df()`: takes a manifesto data frame (see `as_tibble.ManifestoCorpus` and `as_tibble.ManifestoDocument`) and enhances it with running quasi-sentence and sentence counters, and other usefull text-level information; returns a `manifesto.df` object (inherits from `tibble`)
- `manifesto_df_to_xml()` converts `manifesto.df` objects to XML documents

## Vignetts

A short tutorial using the Swiss manifesto corpus: `r vignette("tutorial", "manifestoEnhanceR")`

## Installation

In R, type
```r
library(devtools)
install_github("haukelicht/manifestoEnhanceR")
```

## To-Dos

- write Intro vignette
- implement tests
- implement `manifesto_xml_to_df()`
