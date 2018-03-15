---
title: "Subtotals and Headings"
description: "Modify subtotals and headings from R"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Subtotals}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[Previous: exporting data](export.md)


A common task in the market research world is to collapse two or more categories together to see how the collapsed categories compare to one another. For example, if you asked people to rate their preference on a scale of 1 to 10, you might want to see how the people who provide a rating between 1 and 5 compare to those who rated it between 6 and 10. This goes by a number of names, including "_Top Box_" or "_Nets_", depending on the use case. In Crunch, we call this family of features _Subtotals_. This vignette shows how to define, manage, and analyze variables with subtotals.

# Getting and setting subtotals

Subtotals can be applied to any Categorical or Categorical Array variable. In R, we can view and set subtotal definitions with the `subtotals()` function. If there are no subtotals, the function will return `NULL`:


```r
subtotals(ds$obamaapp)
```

```
## NULL
```

To add subtotals, we can assign a list of `Subtotal` objects. Each `Subtotal` object has three things: a `name` to identify it; a set of `categories` to pool together, referenced either by category name or id; and a location to show it, either `after` a given category or with `position="top"` or `"bottom"` to pin it first or last in the list.


```r
subtotals(ds$DiversityImportant) <- list(
    Subtotal(name = "Follows closely",
             categories = c("Strongly closely", "Very closely"),
             after = "Somewhat closely"),
    Subtotal(name = "Generally disagree",
             categories = c("Not very closely", "Not at all"),
             after = "Not at all")
)
```

Now, if we check `subtotals()`, we can see that we have saved them. In this output we see a few different aspects of subtotals: the `anchor` is the id of the category to put the subtotal after (matching the `after` or `position` argument in `Subtotal()`), name, aggregation functions and `args`, which in the this case are the category ids to include in the subtotal.


```r
subtotals(ds$manningknowledge)
```

```
##   anchor                   name     func    args
## 1      2        Follows closely subtotal 2 and 1
## 2      4 Doesn't Follow Closely subtotal 3 and 4
```

This shows up in the Categorical variable card on the web app like this:

![plot of chunk crunch app output](images/manning_knowledge_subtotals.png)

We can also add _headings_, which are similar to subtotals in that they are additions to categorical variables that will be displayed in the app, but they don't sum up any categories. For a variable with many categories, they can help group variables visually. Here we add some guides to Obama's approval rating.


```r
subtotals(ds$obamaapp) <- list(
    Heading(name = "Approves",
            after = 0),
    Heading(name = "Disapprove",
            after = "Somewhat Approve"),
    Heading(name = "No Answer",
            after = "Strongly Disapprove")
)

subtotals(ds$obamaapp)
```

```
##   anchor       name func args
## 1      0   Approves   NA   NA
## 2      2 Disapprove   NA   NA
## 3      4  No Answer   NA   NA
```

Again, this shows up in the Categorical variable card on the web app:

![plot of chunk crunch app output headings](images/obama_headings.png)

# Removing subtotals

Subtotals and headings can be removed by assigning a `NULL` value.


```r
subtotals(ds$YearsCodedJob) <- NULL
```

```
## NULL
```

# Setting many subtotals

In the _Economist_ survey, there are a number of questions that have the same response categories. If the category names (or ids, if we're using those) are the same, we can use the same set of subtotals across multiple variables.


```r
approve_subtotals <- list(
    Subtotal(name = "Approves",
            categories = c("Somewhat approve", "Strongly approve"),
            after = "Somewhat approve"),
    Subtotal(name = "Disapprove",
            categories = c("Somewhat disapprove", "Strongly disapprove"),
            after = "Strongly disapprove"))
```


```r
subtotals(ds$snowdenleakapp) <- approve_subtotals
subtotals(ds$congapp) <- approve_subtotals
```

Notice here, because each of the categories for these variables has slightly different ids, the `args` in the output differs slightly. But, because we used category names when we were constructing our list of subtotals, when we store them on the variable itself, Crunch does the right thing and converts them over to the correct ids.


```r
subtotals(ds$snowdenleakapp)
subtotals(ds$congapp)
```

```
##   anchor       name     func    args
## 1      2   Approves subtotal 2 and 1
## 2      4 Disapprove subtotal 3 and 4
```

```
##   anchor       name     func    args
## 1      2   Approves subtotal 2 and 1
## 2      5 Disapprove subtotal 4 and 5
```


# Computing with subtotals

Now that we have defined subtotals on the congressional approval question, if we use it in a crosstab, we can see the subtotals.


```r
crtabs(~congapp + gender, data = ds)
```

```
##                               gender          
## congapp                                    Male           Female
##               Strongly approve                0                0
##               Somewhat approve 2.51012984049177 5.46240518719819
##                       Approves 2.51012984049177 5.46240518719819
## Neither approve nor disapprove 11.2916608660399 18.8687169535911
##            Somewhat disapprove 12.2769754589437 24.3223013661526
##            Strongly disapprove 62.6898938858094 38.2775564880149
##                     Disapprove 74.9668693447531 62.5998578541675
##                       Not sure 15.2265010614957 25.8714100226452
```

We can even get just the subtotals as an array from the result if we want to ignore the constituent groups:


```r
subtotalArray(crtabs(~congapp + gender, data = ds))
```

```
##             gender
## congapp          Male    Female
##   Approves    2.51013  5.462405
##   Disapprove 74.96687 62.599858
```

If you don't want to see the subtotals as part of these summaries, you can suppress them from display with the `noTransforms()` function around `crtabs()`.


```r
noTransforms(crtabs(~congapp + gender, data = ds))
```

```
##                                 gender
## congapp                              Male    Female
##   Strongly approve                0.00000  0.000000
##   Somewhat approve                2.51013  5.462405
##   Neither approve nor disapprove 11.29166 18.868717
##   Somewhat disapprove            12.27698 24.322301
##   Strongly disapprove            62.68989 38.277556
##   Not sure                       15.22650 25.871410
```

This does not modify the variable---the subtotals are still defined and visible in the web app---but they are removed from the current analysis.

[Next: combining answers and variables](combine-cut-interact.md)
