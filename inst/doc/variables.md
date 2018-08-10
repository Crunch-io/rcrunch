---
title: "Crunch Variables"
description: "Datasets contain variables, which in Crunch contain rich metadata so that they display and export with human-friendly labeling. This set of tools enables you to clean and refine that metadata."
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Crunch Variables}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[Previous: create and load datasets](datasets.md)



Once you've uploaded data to Crunch, you'll likely want to spend some time cleaning up the metadata associated with your dataset. Legacy statistical programs offer only limited support for variable metadata, both in type and quality. Because Crunch facilitates data visualization, collaboration, and sharing results with others, making your dataset presentation-quality is worthwhile, and this often requires additional work after uploading a data file.

Most of the operations described below can also be accomplished in the web client. However, users comfortable with scripting may find it faster or easier to automate these actions with `crunch`. Such edits made within R are carried out on the remote Crunch dataset, thereby keeping data in sync across all client applications.

## A rose by any other moniker: "names" and "aliases"

Crunch takes the principled stand that working with data in the 21st Century should not be constrained by legacies of the punch-card era. Variables should have "names" that are human-readable and searchable by their meaning---there is no reason to constrain variable names to be eight characters, all caps, etc. "Aided awareness: coffee roasters" is much nicer and more presentable than "Q2B_V1".

At the same time, shorter, user-defined, unique identifiers for variables do have their uses. For one, it's what most any legacy statistical software uses for its identifiers, so retaining them on import will help us line up variables when appending a subsequent batch of imported data, for example. For another, when interacting with a dataset from the command line, it can be useful to have shorter, machine-friendlier references.

So, Crunch stores two user-settable identifiers for variables. What you may have thought of as a variable "label", Crunch elevates to the status of "name". What you may be used to thinking of as a variable "name", Crunch calls "alias".

Aliases and names have slightly different validation constraints. Aliases must be unique across the entire dataset, including among array [subvariables](array-variables.md). "Names", however, only must be unique within [variable order groups](variable-order.md). Any string is valid for either alias or name, though you may want more machine-friendly strings as aliases. In most cases, you probably won't even set aliases, though: they'll be set when you import your dataset and will be whatever the names were in your source data.

In sum, name is crunch alias, label is crunch name.

Except in one place in `crunch`: referencing variables within a dataset.

### Accessing variables within a dataset

When dealing with variables within a dataset, the alias is used to identify variables. This is because (1) aliases are typically what were used to identify variables in whatever format from which the dataset was imported, and consequently, (2) aliases are typically more machine-friendly as names, less likely to contain characters that are not valid as variable names in R.

Because the `names` attribute is used for indexing elements in R, if we want to extract variables based on alias, it means that the `names` attribute of dataset must actually expose aliases. This may be dissonant, but it has some nice properties. For one, comparing our Crunch dataset with the `data.frame` from which it was created, their `names` attribute have the same contents:


```r
identical(names(ds), names(economist))
```
```
## [1] TRUE
```

You can reference and extract variables from a dataset as if it were a `data.frame`, using the `$`, `[`, and `[[` methods.


```r
track.var <- ds$track
track.var
```

```
## track (categorical)
## 
##                                         Count
## Off on the wrong track                    137
## Generally headed in the right direction    80
## Not sure                                   33
```

Like datasets, variables have various attributes like `name` and `description` that can be set naturally.


```r
name(track.var) <- "Direction of country"
description(track.var) <- "In your opinon, is the country going in the right direction, or is it on the wrong track?"
```

Two caveats. First, because we first extracted the variable from the dataset before making edits, the dataset object has stale metadata for this variable.


```r
name(track.var) == name(ds$track)
```
```
## [1] FALSE
```

If we had instead modified `track` within `ds`, like


```r
## Not run
name(ds$track) <- "Direction of country"
```

`ds` would be up to date.

This can be remedied one of two ways. We could either assign `track.var` back to `ds`, as in


```r
ds$track <- track.var
```

or we can just refresh the dataset and fetch data from the server again:


```r
ds <- refresh(ds)
```

Now, `ds` has our edits:


```r
name(track.var) == name(ds$track)
```
```
## [1] TRUE
```


### Addressing the variable catalog

It is not always convenient that the `names` attribute of the dataset actually yields *aliases*. Moreover, if we want to edit the Crunch names of many variables, we need a way of accessing the Crunch metadata more directly. It will be very slow to edit each variable in the dataset individually, referencing them with `$`, because each edit would send a request to the server. Instead, we'd rather bundle those into a single request. To do this, we can access the `variables` attribute of the dataset, which is a "variable catalog":


```r
class(variables(ds))
```

```
## [1] "VariableCatalog"
## attr(,"package")
## [1] "crunch"
```

In the variable catalog, Crunch names are names, and aliases are aliases. Hence,


```r
identical(names(ds), aliases(variables(ds)))
```

```
## [1] TRUE
```

but


```r
identical(names(ds), names(variables(ds)))
```

```
## [1] FALSE
```

because "Direction of country" is the name for `ds$track`


```r
head(names(variables(ds)), 10)
```

```
##  [1] "perc_skipped"         "newsint2"             "Direction of country" "snowdenfav"           "snowdenleakapp"      
##  [6] "snowdenpros"          "snowdenpenalty"       "manningknowledge"     "manningfavorability"  "manningguilt"
```

These attributes all allow assignment with `<-`. The methods `names` and `aliases` yield character vectors, and they take characters in assignment. Hence, you can use any vectorized string manipulation tools available in R, such as regular expressions, to edit variable names efficiently. You can also just supply a replacement vector, like


```r
names(variables(ds))[4:7] <- c("Favorability of Edward Snowden",
                               "Approval of Snowden's Leak",
                               "Support for Prosecution of Snowden",
                               "Penalty for Snowden")
head(names(variables(ds)), 10)
```

```
##  [1] "perc_skipped"                       "newsint2"                           "Direction of country"              
##  [4] "Favorability of Edward Snowden"     "Approval of Snowden's Leak"         "Support for Prosecution of Snowden"
##  [7] "Penalty for Snowden"                "manningknowledge"                   "manningfavorability"               
## [10] "manningguilt"
```

## Categorical variables

Many variables in survey data are categorical: respondents have a finite set of answers to the survey question, and the answers are first and foremost of a nominal, not quantitative nature. In R, this data type is represented as a `factor`. The response options, are contained in the factor's "levels" as a character vector. Manipulation of these levels is limited and often challenging.

In Crunch, categorical variables' "categories" are objects with richer metadata.


```r
is.Categorical(track.var)
```

```
## [1] TRUE
```

```r
categories(track.var)
```

```
##   id                                    name value missing
## 1  1 Generally headed in the right direction     1   FALSE
## 2  2                  Off on the wrong track     2   FALSE
## 3  3                                Not sure     3   FALSE
## 4 -1                                 No Data    NA    TRUE
```

### Category attributes

Categories have `names`, the factor's levels; numeric `values` which can be used when interpreting the categorical variable as numeric; and `ids`, which are analogous to the integer values that underlie an R factor. Categories also have their own "missing" status. Indeed, because Crunch supports more complex missing value support than does R, multiple categories can be marked as missing: there's not a single "NA" value.


```r
names(categories(track.var))
```

```
## [1] "Generally headed in the right direction" "Off on the wrong track"                 
## [3] "Not sure"                                "No Data"
```

```r
values(categories(track.var))
```

```
## [1]  1  2  3 NA
```

```r
ids(categories(track.var))
```

```
## [1]  1  2  3 -1
```

```r
is.na(categories(track.var))
```

```
## Generally headed in the right direction                  Off on the wrong track                                Not sure 
##                                   FALSE                                   FALSE                                   FALSE 
##                                 No Data 
##                                    TRUE
```

Names and values can be assigned into categories, but ids cannot: they are immutable references to values within the column of data on the server. Missingness can be set with `is.na`. Character values assigned will mark those categories as missing, leaving other categories unchanged. Logical values assigned will set the missing TRUE/FALSE accordingly.


```r
names(categories(track.var))[1:2] <- c("Right track", "Wrong track")
values(categories(track.var)) <- c(1, -1, 0)
is.na(categories(track.var)) <- "Not sure"
categories(track.var)
```

```
##   id                                    name value missing
## 1  1 Generally headed in the right direction     1   FALSE
## 2  3                                Not sure     0    TRUE
## 3  2                             Wrong track    -1   FALSE
## 4 -1                                 No Data    NA    TRUE
```


```r
ids(categories(track.var)) <- sample(ids(categories(track.var)), replace=FALSE)
```
```
## Error : Cannot modify category ids
```

### Reordering categories

Categories can also be reordered by index, like any list object


```r
categories(track.var) <- categories(track.var)[c(1,3,2)]
categories(track.var)
```

```
##   id                                    name value missing
## 1  1 Generally headed in the right direction     1   FALSE
## 2  2                             Wrong track    -1   FALSE
## 3  3                                Not sure     0    TRUE
```

As with all other metadata edits discussed, updating with these methods automatically sends the changes to the server, so your local edits are reflected in the cloud.

## Hiding variables

Datasets often contain variables that you may want to use -- perhaps through a derived variable, a transformation, or a recode -- or that may simply not be relevant for the analytic tasks at hand. In short, you want to hide them. They aren't deleted, so you can restore them if you need them later, but they no longer clutter the dataset "workspace".

As when working with a `data.frame`, you typically assign the return of a dataset-level function back to the variable representing the dataset in your R script or session.

In our example dataset, we have two copies of a voter-registration variable, "votereg_new" and "votereg_old". Let's hide the old version:


```r
ds <- hideVariables(ds, "votereg_old")
hiddenVariables(ds)
```
```
## [1] "votereg_old"
```

As with the `is.na` function, you can update a variable by assigning it to the hidden variables list.


```r
hiddenVariables(ds) <- "pid7others"
hiddenVariables(ds)
```
```
## [1] "votereg_old"  "pid7others"
```

These variables are now hidden, both locally in your R session and remotely on the server, which you can see in the web application. And, just as you could restore them there, you can also restore them from R:


```r
ds <- unhideVariables(ds, "pid7others")
hiddenVariables(ds)
```
```
## [1] "votereg_old"
```

## Deleting variables

Sometimes you do want to delete variables permanently. Doing so is easy, but we have some protections in place to keep you from accidentally deleting things from a dataset that may be shared with many people on the server.

To delete, you can assign `NULL` in to the dataset for that variable, just like you were removing a column from a `data.frame`. Let's kill the "votereg_old" variable permanently:


```r
ds$votereg_old <- NULL
```
```
## Really delete "votereg_old"?
```

The delete function requires confirmation when you're running from an interactive session, just to make sure you aren't accidentally assigning something in that is NULL and deleting your variable. If you know that you want to delete the variable, you can give your approval in advance by wrapping it in a `with` statement, using the `consent` context manager:


```r
with(consent(), ds$votereg_old <- NULL)
"votereg_old" %in% names(ds)
```
```
## [1] FALSE
```

The `with(consent(), ...)` pattern works everywhere in `crunch` that requires confirmation to do an action, such as deleting datasets.

[Next: create and manipulate array variables](array-variables.md)
