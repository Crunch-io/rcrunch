---
title: "Adding Variables"
description: "The crunch package makes it easy for you to derive new variables in your dataset. These derived variables retain their functional connection on the server, such that changes to the input variables automatically propagate to the derived variable."
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Adding Variables}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[Previous: variable organization](variable-order.md)



One of the powerful features of working with Crunch is that you and all of your collaborators work off of the same data, whether you use the web client or use R. Circumstances may arise in which you are working in R and want to add or modify variables in a Crunch dataset, and the `crunch` package facilitates that. It provides an idiomatic R interface to manipulating a Dataset, doing so efficiently, without copying data off of the server to transform it. In addition, when you create variables that are derived from other variables, they retain their functional connection on the server, such that changes to the input variables automatically propagate to the derived variable.

## Creating derived variables
In our sample dataset, we don't have an "Age" variable, but we do have "Birth Year" (`birthyr`). We can create an age variable simply by taking the current year and subtracting the "Birth Year" variable from it--just as you would do if you were working with a `data.frame`:


```r
ds$age <- 2015 - ds$birthyr
```

Now, we have an age variable, and it looks like we'd expect it would relative to `birthyr`:


```r
summary(ds$age)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   22.00   38.00   48.50   49.62   59.75   90.00
```

```r
summary(ds$birthyr)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1925    1955    1966    1965    1977    1993
```

Age is now a derived variable, functionally linked to `birthyr` on the server. Note that in order to create `age`, we didn't pull any values off of the server; we just supplied the derivation expression.


```r
class(2015 - ds$birthyr)
```

```
## [1] "CrunchExpr"
## attr(,"package")
## [1] "crunch"
```

## Updating values

We can also use these expressions to update values. Suppose, just for demonstration purposes, that we want to truncate the range of the `birthyr` variable and set it to 1945 everywhere where it is less than 1945. We can once again write the R code we'd write if we were working with a `data.frame`, even though we're working with a Crunch dataset:


```r
ds$birthyr[ds$birthyr < 1945] <- 1945
```

If we look at the summary again, we'll see that our data have been updated, and the minimum Birth Year is now 1945.


```r
summary(ds$birthyr)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1945    1955    1966    1966    1977    1993
```

Not only that: since `age` is a function of `birthyr` on the server, its values also update now that we've modified `birthyr`. Max age is now 70, or 2015 - 1945.


```r
summary(ds$age)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   22.00   38.00   48.50   48.98   59.75   70.00
```

Once again, we've done this operation by turning idiomatic R expressions into Crunch expression syntax.


```r
class(ds$birthyr < 1945)
```

```
## [1] "CrunchLogicalExpr"
## attr(,"package")
## [1] "crunch"
```

The work gets done without having to pull data off of the server.

[Next: analyzing data](analyze.md)
