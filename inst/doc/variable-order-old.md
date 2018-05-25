---
title: "Reordering Variables"
description: "Variables within datasets are organized in folders. The crunch package provides tools for creating folders and moving variables among them."
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Reordering Variables}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[Previous: array variables](array-variables.md)



> This ordering API is being phased out in favor of the new "variable folders" API, which is currently in a beta testing phase. This vignette is left here for reference, but you are encouraged to try the new methods. See [the new vignette](variable-order.md) for details.

In the web application, variables in a dataset are displayed in a list on the left side of the screen.

<!-- screenshot -->

Typically, when you import a dataset, the variable list is flat, but it can be organized into an accordion-like hierarchy. The variable organizer in the GUI allows you to perform this organization visually, but you can also manage this metadata from `crunch`.

## Viewing the order

Picking up with the dataset we used in the [array variables vignette](array-variables.md), we can fetch the `ordering` property of the dataset to view the current organization:


```r
ordering(ds)
```

```
## perc_skipped
## newsint2
## Direction of country
## Favorability of Edward Snowden
## Approval of Snowden's Leak
## Support for Prosecution of Snowden
## Penalty for Snowden
## manningknowledge
## manningfavorability
## manningguilt
## manningpenalty
## Issue importance
## imissf
## obamaapp
## Approval of Obama on issues
## congapp
## ideo5
## ideoobama
## saysobama
## likeobama
## birthyr
## gender
## pid3
## pid7
## pid7others
## race
## educ
## marstat
## phone
## faminc
## region
## state
## weight
## votereg_new
## is_voter
## votereg_old
## votereg
```

It's flat. If you're importing data from a `data.frame` or a file, like an SPSS file, this is where you'll begin.

## Creating groups
Since I know how this dataset is organized, I'm going quickly toss the variables into one of a small number of groups, instantiated with `VariableGroup`, and collect them in a container object called `VariableOrder`.


```r
ordering(ds) <- VariableOrder(
        VariableGroup("Demos", ds[c(1, 21:37)]),
        VariableGroup("Tracking questions", ds[c(2,3, 11:20)]),
        VariableGroup("This week", ds[4:11])
    )
```

Now, our variable tree has some structure:


```r
ordering(ds)
```

```
## [+] Demos
##     perc_skipped
##     birthyr
##     gender
##     pid3
##     pid7
##     pid7others
##     race
##     educ
##     marstat
##     phone
##     faminc
##     region
##     state
##     weight
##     votereg_new
##     is_voter
##     votereg_old
##     votereg
## [+] Tracking questions
##     newsint2
##     Direction of country
##     manningpenalty
##     Issue importance
##     imissf
##     obamaapp
##     Approval of Obama on issues
##     congapp
##     ideo5
##     ideoobama
##     saysobama
##     likeobama
## [+] This week
##     Favorability of Edward Snowden
##     Approval of Snowden's Leak
##     Support for Prosecution of Snowden
##     Penalty for Snowden
##     manningknowledge
##     manningfavorability
##     manningguilt
```

Groups can be created with several kinds of inputs. You can specify a vector or list of variable references (URLs), as returned from the `self` method of variables. That list of references may also contain `VariableGroup` objects as well, which will nest those Groups inside the Group you're creating (see "Nested Groups" below). You can also provide a list of variable entities or, as in this example, a subset of a dataset entity (which can be thought of as a list of variables).

## Group names
We can use the `names` method to access and modify these group names:


```r
names(ordering(ds))
```

```
## [1] "Demos"              "Tracking questions" "This week"
```

Let's rename the first group. No need to abbreviate "Demographics".


```r
names(ordering(ds))[1] <- "Demographics"
names(ordering(ds))
```

```
## [1] "Demographics"       "Tracking questions" "This week"
```

## Reordering groups and entities

`VariableOrder` and `VariableGroup` support standard R forms of indexing for extracting and reordering.

Let's move "Demographics" to the end:


```r
ordering(ds) <- ordering(ds)[c(2, 3, 1)]
names(ordering(ds))
```

```
## [1] "Tracking questions" "This week"          "Demographics"
```

## Nested groups

We can create groups within groups as well. In the "This week" group, we have a set of questions about Edward Snowden: the first four variables in the group. Let's nest them inside their own group inside "This week":


```r
ordering(ds)[["This week"]][["Snowden"]] <- ordering(ds)[["This week"]][1:4]
ordering(ds)
```

```
## [+] Tracking questions
##     newsint2
##     Direction of country
##     manningpenalty
##     Issue importance
##     imissf
##     obamaapp
##     Approval of Obama on issues
##     congapp
##     ideo5
##     ideoobama
##     saysobama
##     likeobama
## [+] This week
##     manningknowledge
##     manningfavorability
##     manningguilt
##     [+] Snowden
##         Favorability of Edward Snowden
##         Approval of Snowden's Leak
##         Support for Prosecution of Snowden
##         Penalty for Snowden
## [+] Demographics
##     perc_skipped
##     birthyr
##     gender
##     pid3
##     pid7
##     pid7others
##     race
##     educ
##     marstat
##     phone
##     faminc
##     region
##     state
##     weight
##     votereg_new
##     is_voter
##     votereg_old
##     votereg
```

Note a few things there. First, groups can be extracted by list-like indexing by name. `ordering(ds)[["This week"]]` gives us the `VariableGroup` named "This week".

Second, we can create a new group, nested or otherwise, by assigning in to an Order/Group by a new name. This is similar to extending a named list object by name in base R. In this case, we created a group called "Snowden" inside "This week", and we assigned into it the first four entries of the "This week" group.

Third, the four variable references we put into "Snowden" were also removed from their positions in the "This week" group: they were "moved" rather than copied. This is what we might expect from a files-and-folders form of organization. If you wish to make copies, you can set `duplicates(ordering(ds)) <- TRUE`. If we had done that in this example, we'd have the four Snowden variables appearing both in the "This week" group and in the nested "Snowden" group. (More accurately, and keeping with the file-system metaphor, setting duplicates to `TRUE` lets you create symbolic links rather than actually copying the variables. Only the references are copied; there is only one variable behind them.)

[Next: transforming and deriving](derive.md)
