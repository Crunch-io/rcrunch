---
title: "Array Variables"
description: "Crunch supports several types of 'array' variables, which group together multiple columns of data in ways common in survey research."
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Array Variables}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[Previous: variables](variables.md)




Crunch supports more types of variables than many of the data formats from which you can create datasets. Plain-text .csv files, for example, can't express that some columns are actually indicator matrices of multiple selections (Multiple Response Variables). Most SPSS .sav files do not indicate Categorical Arrays (otherwise known as "grids") as being part of a group---they are simply several categorical variables. The same is true for an R `data.frame`.

You can use `crunch` to "bind" categorical variables into Multiple Response and Categorical Array variables.

## Creating an array

One of the reasons to use R with Crunch is to leverage the power of scripting for tasks that would be repetitive in a GUI. Many crunch functions operating on Crunch datasets have an optional `pattern` argument that lets you use regular expressions for these "bulk" operations.

In our sample _Economist_ dataset, we have set of variables prefixed with "imiss":


```r
grep("^imiss_", names(ds), value=TRUE)
```

```
##  [1] "imiss_b" "imiss_c" "imiss_d" "imiss_f" "imiss_g" "imiss_h" "imiss_j"
##  [8] "imiss_m" "imiss_p" "imiss_q" "imiss_r" "imiss_s" "imiss_t"
```

These correspond to a survey grid question about how important respondents view a set of issues. Examining one of them, we see


```r
ds$imiss_b
```

```
## imiss_b (categorical)
## 
##                    Count
## Very Important       196
## Somewhat Important    44
## Not very Important     8
## Unimportant            1
```

All of these "imiss" categorical variables have the same structure. We can combine them into a categorical array variable with `makeArray`:


```r
ds$imiss <- makeArray(ds[grep("^imiss_", names(ds))], name="Issue importance")
ds$imiss
```

```
## Issue importance (categorical_array)
## Subvariables:
##   $`imiss_b`
##   $`imiss_c`
##   $`imiss_d`
##   $`imiss_f`
##   $`imiss_g`
##   $`imiss_h`
##   $`imiss_j`
##   $`imiss_m`
##   $`imiss_p`
##   $`imiss_q`
##   $`imiss_r`
##   $`imiss_s`
##   $`imiss_t`
```

The set of "important issue" variables have gone from thirteen separate categorical variable cards to just one, where the subvariables are shown as rows, and the common categories across all of them are shown as columns. Categorical array variables look something like this in the web application:

<!-- ![Categorical Array variable card](http://support.crunch.io/crunch/images/VariableCardArray.png) -->

In our example dataset, the categorical variables `imiss_*` are now not visible in the dataset directly, but we can access them as "subvariables" of the array we just created.


```r
subvariables(ds$imiss)
```

```
## Subvariables:
##   $`imiss_b`
##   $`imiss_c`
##   $`imiss_d`
##   $`imiss_f`
##   $`imiss_g`
##   $`imiss_h`
##   $`imiss_j`
##   $`imiss_m`
##   $`imiss_p`
##   $`imiss_q`
##   $`imiss_r`
##   $`imiss_s`
##   $`imiss_t`
```

We can also step into the subvariables and access the underlying categorical variables:


```r
ds$imiss$imiss_b
```

```
## imiss_b (categorical)
## 
##                    Count
## Very Important       196
## Somewhat Important    44
## Not very Important     8
## Unimportant            1
```

## Manipulating subvariables

The names `imiss_b` through `imiss_t` are unsatisfying from a human-readability perspective: you can't tell which political issues correspond to the variables. Unfortunately, we didn't have additional metadata on these survey questions in the `data.frame` we imported initially. However, we can rectify this.

Subvariables have methods similar to those for [categories](variables.md). They have a names attribute that we can get:


```r
names(subvariables(ds$imiss))
```

```
##  [1] "imiss_b" "imiss_c" "imiss_d" "imiss_f" "imiss_g" "imiss_h" "imiss_j"
##  [8] "imiss_m" "imiss_p" "imiss_q" "imiss_r" "imiss_s" "imiss_t"
```

We can set it, too:


```r
names(subvariables(ds$imiss)) <- c("The economy", "Immigration",
    "The environment", "Terrorism", "Gay rights", "Education",
    "Health care", "Social security", "The budget deficit",
    "The war in Afghanistan", "Taxes", "Medicare", "Abortion")
subvariables(ds$imiss)
```

```
## Subvariables:
##   $`The economy`
##   $`Immigration`
##   $`The environment`
##   $`Terrorism`
##   $`Gay rights`
##   $`Education`
##   $`Health care`
##   $`Social security`
##   $`The budget deficit`
##   $`The war in Afghanistan`
##   $`Taxes`
##   $`Medicare`
##   $`Abortion`
```

Another useful thing we can do with array subvariables is reorder them. Let's alphabetize the subvariables:


```r
sorting <- order(names(subvariables(ds$imiss)))
subvariables(ds$imiss) <- subvariables(ds$imiss)[sorting]
subvariables(ds$imiss)
```

```
## Subvariables:
##   $`Abortion`
##   $`Education`
##   $`Gay rights`
##   $`Health care`
##   $`Immigration`
##   $`Medicare`
##   $`Social security`
##   $`Taxes`
##   $`Terrorism`
##   $`The budget deficit`
##   $`The economy`
##   $`The environment`
##   $`The war in Afghanistan`
```

## Creating multiple response variables

Just as we created a categorical array, we can create a multiple response variable. Like categorical arrays, multiple responses contain a set of subvariables, categorical variables with a common list of categories. However, the subvariables in a multiple response are treated as dichotomous indicators. We specify one or more categories that indicate "selected" versus "not selected". Hence, when a multiple response appears in the web app, it looks like a single categorical variable, each subvariable shown like a category. Unlike a categorical variable, though, the multiple responses are not mutually exclusive, so tabulations with them may not sum to 100 percent.

In the _Economist_ dataset, we have another set of parallel categorical variables, "boap", which indicate approval of President Obama on a range of issues.


```r
ds$boap_4
```

```
## boap_4 (categorical)
## 
##                     Count
## Somewhat approve       68
## Strongly disapprove    67
## No opinion             46
## Somewhat disapprove    43
## Strongly approve       26
```

In the questionnaire that collected this data, "boap" appeared as a grid question, just as "imiss" did. But, for illustration purposes---and to show how you can convert between categorical array and multiple response---let's treat this as multiple response.

The function `makeMR` works like `makeArray` but with an additional argument, "selections", in which you specify the category name(s) that identify which category or categories should be the dichotomous indicator.


```r
ds$boap <- makeMR(ds[grep("^boap_[0-9]+", names(ds))],
    name="Approval of Obama on issues",
    selections=c("Strongly approve", "Somewhat approve"))
ds$boap
```

```
## Approval of Obama on issues (multiple_response)
## Subvariables:
##   $`boap_2`
##   $`boap_3`
##   $`boap_4`
##   $`boap_5`
##   $`boap_6`
##   $`boap_7`
##   $`boap_8`
##   $`boap_9`
##   $`boap_10`
##   $`boap_11`
##   $`boap_12`
##   $`boap_13`
##   $`boap_14`
```

## (un)dichotomize

Multiple response variables can be thought of as categorical arrays that have extra metadata indicating which categories are "selected". This metadata can be manipulated, and we can thus transform categorical arrays into multiple response and vice versa.

The function `undichotomize` removes the dichotomization metadata:


```r
ds$boap <- undichotomize(ds$boap)
ds$boap
```

```
## Approval of Obama on issues (categorical_array)
## Subvariables:
##   $`boap_2`
##   $`boap_3`
##   $`boap_4`
##   $`boap_5`
##   $`boap_6`
##   $`boap_7`
##   $`boap_8`
##   $`boap_9`
##   $`boap_10`
##   $`boap_11`
##   $`boap_12`
##   $`boap_13`
##   $`boap_14`
```

We can add that information with `dichotomize`. Taking our categorical array "boap," let's make it into a multiple response, but this time, let's only include the "Strongly approve" category:


```r
ds$boap <- dichotomize(ds$boap, "Strongly approve")
ds$boap
```

```
## Approval of Obama on issues (multiple_response)
## Subvariables:
##   $`boap_2`
##   $`boap_3`
##   $`boap_4`
##   $`boap_5`
##   $`boap_6`
##   $`boap_7`
##   $`boap_8`
##   $`boap_9`
##   $`boap_10`
##   $`boap_11`
##   $`boap_12`
##   $`boap_13`
##   $`boap_14`
```

## Splitting arrays back into categoricals

As noted above, when we make an array, its subvariables no longer appear in the dataset outside of the array.


```r
grep("boap", names(ds), value=TRUE)
```

```
## [1] "boap"
```

We can access the subvariables and do things with them directly via the `subvariables` method, but the case may arise in which we want to undo our binding of these subvariables into the array. The function `unbind` deletes the array variable and restores the subvariables as top-level variables.


```r
unbind(ds$boap)
ds <- refresh(ds)
```


Note the use of `refresh`. Most functions that modify objects on the server refresh their local copies in our R session automatically; however, because `unbind` doesn't assign back into `ds`, the local dataset object doesn't get updated with the change, so we need to refresh it manually.

Now, if we check the names of `ds`, we see our full set of `boap_*` former subvariables:


```r
grep("boap", names(ds), value=TRUE)
```

```
##  [1] "boap_2"  "boap_3"  "boap_4"  "boap_5"  "boap_6"  "boap_7"  "boap_8" 
##  [8] "boap_9"  "boap_10" "boap_11" "boap_12" "boap_13" "boap_14"
```

[Next: variable organization](variable-order.md)
