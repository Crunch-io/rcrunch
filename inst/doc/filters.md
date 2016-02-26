<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Working with filters}
-->

[Previous: analyze data](analyze.md)


# Working with Filters

Sometimes you only want to work with a subset of your data. With the `crunch` package, you can both filter the views of data you work with in your R session and manage the filters that you and your collaborators see in the web application.

## Filtering and subsetting in R

As we've seen in previous vignettes, making logical expressions with Crunch datasets and variables is natural. We showed how to [update a selection of values](derive.md) on the server, as well as how to [crosstab in a subset of a dataset](analyze.md). Other applications work just as intuitively.

<!-- Add links with anchors to "Updating values" and to "Subsetting data" -->

Filtering like this works by creating a dataset or variable object that has the filter embedded in it:


```r
dems <- ds[ds$pid3 == "Democrat",]
dems
```

```
## Dataset “Economist/YouGov Weekly Survey”
## 
## Contains 96 rows of 38 variables:
## $newsint2: newsint2 (categorical)
## $track: Direction of country (categorical)
## $imiss: Issue importance (multiple_response)
## $imissf: imissf (categorical)
## $obamaapp: obamaapp (categorical)
## $boap: Approval of Obama on issues (multiple_response)
## $congapp: congapp (categorical)
## $ideo5: ideo5 (categorical)
## $ideoobama: ideoobama (categorical)
## $saysobama: saysobama (categorical)
## $likeobama: likeobama (categorical)
## $manningknowledge: manningknowledge (categorical)
## $manningfavorability: manningfavorability (categorical)
## $manningguilt: manningguilt (categorical)
## $manningpenalty: manningpenalty (categorical)
## $snowdenfav: Favorability of Edward Snowden (categorical)
## $snowdenleakapp: Approval of Snowden's Leak (categorical)
## $snowdenpros: Support for Prosecution of Snowden (categorical)
## $snowdenpenalty: Penalty for Snowden (categorical)
## $perc_skipped: perc_skipped (numeric)
## $birthyr: birthyr (numeric)
## $gender: gender (categorical)
## $pid3: pid3 (categorical)
## $pid7: pid7 (categorical)
## $pid7others: pid7others (categorical)
## $race: race (categorical)
## $educ: educ (categorical)
## $marstat: marstat (categorical)
## $phone: phone (categorical)
## $faminc: faminc (categorical)
## $region: region (numeric)
## $state: state (categorical)
## $weight: weight (numeric)
## $votereg_new: votereg_new (numeric)
## $is_voter: is_voter (numeric)
## $votereg_old: votereg_old (numeric)
## $votereg: votereg (numeric)
## $age: age (numeric)
```


```r
round(crtabs(mean(track) ~ educ + gender, data=dems), 2)
```

```
##                       gender
## educ                    Male Female
##   No HS                  NaN   1.00
##   High school graduate  0.64  -0.08
##   Some college          0.73   0.30
##   2-year                1.00   0.00
##   4-year               -0.12   0.40
##   Post-grad             0.33   0.12
```

When you extract a variable from a filtered dataset, it too is filtered. So


```r
table(dems$educ)
```

```
## educ
##                No HS High school graduate         Some college               2-year               4-year 
##             5.990781            23.817206            19.000165             4.962549            18.238301 
##            Post-grad 
##             6.278870
```

is the same as


```r
table(ds$educ[ds$pid3 == "Democrat",])
```

```
## educ
##                No HS High school graduate         Some college               2-year               4-year 
##             5.990781            23.817206            19.000165             4.962549            18.238301 
##            Post-grad 
##             6.278870
```

As an aside, if you prefer using the `subset` function, that works just the same as the `[` extract method on datasets:


```r
identical(subset(ds, ds$pid3 == "Democrat"), dems)
```

```
## [1] TRUE
```

## Filter entities

In the web application, you can save filter definitions with names for easy reuse. You can also share these filter definitions with other viewers of the dataset.

To do so, we work with the dataset's filter catalog. To start, our filter catalog will be empty:


```r
filters(ds)
```

```
## data frame with 0 columns and 0 rows
```

Create a filter by assigning a Crunch logical expression to the catalog by the name we want to give it, using `$` or `[[`:


```r
filters(ds)[["Young males"]] <- ds$gender == "Male" & ds$age < 30
filters(ds)[["Young males"]]
```

```
## Crunch filter “Young males”
## Expression: gender == "Male" & age < 30L
```

This new filter now appears in our filter catalog.


```r
filters(ds)
```

```
##          name                               id is_public
## 1 Young males 1efb8332a78c46abaae7438091aa7eda     FALSE
```

This filter is now available for you to use in the web application. If you want to make the filter available to all viewers of the dataset, make it "public":


```r
is.public(filters(ds)[["Young males"]]) <- TRUE
filters(ds)
```

```
##          name                               id is_public
## 1 Young males 1efb8332a78c46abaae7438091aa7eda      TRUE
```

## Exclusion filters

One other application for filtering is the dataset exclusion filter. The exclusion allows you to suppress from view rows that match a certain condition. Exclusions are set on the dataset with a Crunch logical expression:


```r
dim(ds)
```

```
## [1] 250  38
```


```r
exclusion(ds) <- ds$perc_skipped > 15
exclusion(ds)
```

```
## Crunch logical expression: perc_skipped > 15L
```


```r
dim(ds)
```

```
## [1] 229  38
```

All viewers of the dataset will see the dataset as if those rows do not exist; however, as the editor of the dataset, you can remove the exclusion filter to see them if you need:


```r
exclusion(ds) <- NULL
dim(ds)
```

```
## [1] 250  38
```

### Alternative: `dropRows`

If you do know that you never want to see those rows again, you can permanently delete them with `dropRows`:


```r
## Not run
ds <- dropRows(ds, ds$perc_skipped > 15)
```
