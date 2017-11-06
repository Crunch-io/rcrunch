---
title: "Importing datasets"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Importing datasets}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[Previous: Getting started](getting-started.md)



# Datasets in `crunch`

To get started working with Crunch datasets in R, we need to get a dataset in our local R session. This means we either need to create a new one or load an existing dataset.

## Uploading datasets

To create new datasets, multiple paths exist. In the web application, you can upload files to create datasets. From `crunch`, there are two methods for creating datasets: one for `data.frames` and one for files of other (non-R) formats.

### 1. newDataset

The `crunch` package contains a sample dataset that we'll use throughout the vignettes: a sample drawn from a political survey fielded by YouGov on behalf of the _Economist_.


```r
load("../vignettes/economist.RData")
dim(economist)
```

```
## [1] 250  61
```

You can create a dataset from any `data.frame` you have in your R session with `newDataset`. Let's use that sample dataset:


```r
ds <- newDataset(economist, name="Economist/YouGov Weekly Survey")
dim(ds)
```

```
## [1] 250  61
```

`newDataset` translates R data types into their analogous types in Crunch.

* character --> Text Variable
* numeric, integer --> Numeric Variable
* factor --> Categorical Variable
* Date --> Datetime Variable

### 2. newDatasetFromFile

Alternatively, `newDatasetFromFile` essentially does what you would do in the web application: uploads your file and creates a dataset from it. If you have an SPSS or CSV file, you can upload it with that without first reading it into R.

## Loading existing Crunch datasets

Datasets already existing on the Crunch server can be loaded with `loadDataset`. The function takes either the dataset's name, or the position within the dataset list returned by `listDatasets`:


```r
listDatasets()
```
```
## [1] "Economist/YouGov Weekly Survey"
```

```r
ds <- loadDataset("Economist/YouGov Weekly Survey")
```

```r
is.dataset(ds)
```

```
## [1] TRUE
```

## Dataset properties

Dataset have metadata beyond what a `data.frame` has. Datasets have a human-readable `name`, which you specified when you created it, and a `description`.


```r
name(ds)
```

```
## [1] "Economist/YouGov Weekly Survey"
```

```r
description(ds)
```

```
## [1] ""
```

Both can be set with `<-` assignment. Let's give our dataset an informative description:


```r
description(ds) <- "U.S. nationally representative sample, 1000 respondents"
description(ds)
```
```
## [1] "U.S. nationally representative sample, 1000 respondents"
```

Note that this assignment doesn't just modify our local dataset object: it sends the new description to the server. If we pull a fresh copy of the dataset from the server, with `refresh`, we'll see the description is there:


```r
ds <- refresh(ds)
description(ds)
```
```
## [1] "U.S. nationally representative sample, 1000 respondents"
```

## Archiving and deleting datasets

Datasets that you don't need anymore can be either archived or deleted. Archiving removes the dataset from the primary listings of datasets, but it is not a permanently destructive action. You can archive and restore archived datasets in the web application.

Datasets can also be deleted permanently. This action cannot be undone, so it should not be done lightly. `crunch` provides two ways to delete a dataset: a `delete` method on a dataset object, like


```r
## Not run
delete(ds)
```

The second way to delete is `deleteDataset`, which works like `loadDataset`: you supply a dataset name. This way is faster if you have not already loaded the dataset object into your R session: no need to fetch something from the server just to then tell the server to delete it.

For details on both, see their help pages.

[Next: Variable metadata](variables.md)
