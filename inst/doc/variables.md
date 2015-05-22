<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Variables: cleaning and defining metadata}
-->

[Previous: create and load datasets](datasets.md)


```
## Error in library(crunch): there is no package called 'crunch'
```

# Manipulating variables within datasets

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
identical(names(ds), names(df))
```
```
## [1] TRUE
```

You can reference and extract variables from a dataset as if it were a `data.frame`, using the `$`, `[`, and `[[` methods. 


```r
track.var <- ds$track
```

```r
track.var
```

```
## Loading required package: crunch
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'crunch'
```

```
## Error in .requirePackage(package): unable to find required package 'crunch'
```

```
## 
##                  Off on the wrong track 
##                                     576 
## Generally headed in the right direction 
##                                     285 
##                                Not sure 
##                                     139 
## attr(,"class")
## [1] "CategoricalVariableSummary" "array"                     
## attr(,"varname")
## [1] "track"         "(categorical)"
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
## Error in eval(expr, envir, enclos): could not find function "variables"
```

In the variable catalog, Crunch names are names, and aliases are aliases. Hence,


```r
identical(names(ds), aliases(variables(ds)))
```

```
## Loading required package: crunch
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'crunch'
```

```
## Error in .requirePackage(package): unable to find required package 'crunch'
```

but


```r
identical(names(ds), names(variables(ds)))
```

```
## Loading required package: crunch
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'crunch'
```

```
## Error in .requirePackage(package): unable to find required package 'crunch'
```

because "Direction of country" is the name for `ds$track`


```r
head(names(variables(ds)), 10)
```

```
##  [1] "starttime"            "endtime"              "perc_skipped"        
##  [4] "newsint2"             "Direction of country" "snowdenfav"          
##  [7] "snowdenleakapp"       "snowdenpros"          "snowdenpenalty"      
## [10] "manningknowledge"
```

These attributes all allow assignment with `<-`. The methods `names` and `aliases` yield character vectors, and they take characters in assignment. Hence, you can use any vectorized string manipulation tools available in R, such as regular expressions, to edit variable names efficiently. You can also just supply a replacement vector, like


```r
names(variables(ds))[6:9] <- c("Favorability of Edward Snowden", 
                               "Approval of Snowden's Leak",
                               "Support for Prosecution of Snowden",
                               "Penalty for Snowden")
head(names(variables(ds)), 10)
```

```
##  [1] "starttime"                         
##  [2] "endtime"                           
##  [3] "perc_skipped"                      
##  [4] "newsint2"                          
##  [5] "Direction of country"              
##  [6] "Favorability of Edward Snowden"    
##  [7] "Approval of Snowden's Leak"        
##  [8] "Support for Prosecution of Snowden"
##  [9] "Penalty for Snowden"               
## [10] "manningknowledge"
```

## Categorical variables

Many variables in survey data are categorial: respondents have a finite set of answers to the survey question, and the answers are first and foremost of a nominal, not quantitative nature. In R, this data type is represented as a `factor`. The response options, are contained in the factor's "levels" as a character vector. Manipulation of these levels is limited and often challenging.

In Crunch, categorical variables' "categories" are objects with richer metadata. 


```r
is.Categorical(track.var)
```

```
## Error in eval(expr, envir, enclos): could not find function "is.Categorical"
```

```r
categories(track.var)
```

```
## Error in eval(expr, envir, enclos): could not find function "categories"
```

### Category attributes

Categories have `names`, the factor's levels; numeric `values` which can be used when interpreting the categorical variable as numeric; and `ids`, which are analogous to the integer values that underly an R factor. Categories also have their own "missing" status. Indeed, because Crunch supports more complex missing value support than does R, multiple categories can be marked as missing: there's not a single "NA" value.


```r
names(categories(track.var))
```

```
## Error in eval(expr, envir, enclos): could not find function "categories"
```

```r
values(categories(track.var))
```

```
## Error in eval(expr, envir, enclos): could not find function "values"
```

```r
ids(categories(track.var))
```

```
## Error in eval(expr, envir, enclos): could not find function "ids"
```

```r
is.na(categories(track.var))
```

```
## Error in eval(expr, envir, enclos): could not find function "categories"
```

Names and values can be assigned into categories, but ids cannot: they are immutable references to values within the column of data on the server. Missingness can be set with `is.na`. Character values assigned will mark those categories as missing, leaving other categories unchanged. Logical values assigned will set the missing TRUE/FALSE accordingly.


```r
names(categories(track.var))[1:2] <- c("Right track", "Wrong track")
values(categories(track.var)) <- c(1, -1, 0)
is.na(categories(track.var)) <- "Not sure"
categories(track.var)
```

```
## Loading required package: crunch
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'crunch'
```

```
## Error in .requirePackage(package): unable to find required package 'crunch'
```
<!-- MAKE THAT -->

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
## Loading required package: crunch
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'crunch'
```

```
## Error in .requirePackage(package): unable to find required package 'crunch'
```

As with all other metadata edits discussed, updating with these methods automatically sends the changes to the server, so your local edits are reflected in the cloud.

## Hiding variables

Datasets often contain variables that you may want to use -- perhaps through a derived variable, a transformation, or a recode -- or that may simply not be relevant for the analytic tasks at hand. In short, you want to hide them. They aren't deleted, so you can restore them if you need them later, but they no longer clutter the dataset "workspace".

As when working with a `data.frame`, you typically assign the return of a dataset-level function back to the variable representing the dataset in your R script or session. 


```r
ds <- hideVariables(ds, "comments")
hiddenVariables(ds)
```
```
## [1] "comments"
```

As with the `is.na` function, you can update a variable by assigning it to the hidden variables list.


```r
hiddenVariables(ds) <- "pid7others"
hiddenVariables(ds)
```
```
## [1] "comments"  "pid7others"
```

These variables are now hidden, both locally in your R session and remotely on the server, which you can see in the web application. And, just as you could restore them there, you can also restore them from R:


```r
ds <- unhideVariables(ds, "pid7others")
hiddenVariables(ds)
```
```
## [1] "comments"
```

[Next: create and manipulate array variables](array-variables.md)
