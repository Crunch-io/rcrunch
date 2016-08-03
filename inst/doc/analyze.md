<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Analyzing data with crunch}
-->

[Previous: transform and derive](derive.md)


# Computing with R and Crunch
With `crunch`, you can harness the power of R to do computations with your datasets in Crunch that would be difficult or impossible to accomplish in a graphical user interface.

## Crosstabbing
While the web application certainly supports crosstabbing, you may want to do aggregations like this in R. Crosstabbing in R with `crunch` may allow you to easily do additional computations on the result, for example.

`crunch` contains the `crtabs` (Crunch-tabs) function, which largely emulates the design of the `xtabs` function in base R. In essence, you define a formula and provide data in which to evaluate it. In this case, we'll be providing a `CrunchDataset`.

### Basic examples

Like `xtabs`, `crtabs` takes a formula and a data argument. Dimensions of your crosstab go on the right side of the `~`. For a univariate table of frequencies by education, we can do


```r
tab1 <- crtabs(~ educ, data=ds)
tab1
```

```
## educ
##                No HS High school graduate         Some college               2-year               4-year 
##                   12                   71                   61                   24                   57 
##            Post-grad 
##                   25
```

Additional dimensions are added with `+`. For a two-way table of education and gender,


```r
tab2 <- crtabs(~ educ + gender, data=ds)
tab2
```

```
##                       gender
## educ                   Male Female
##   No HS                   6      6
##   High school graduate   33     38
##   Some college           26     35
##   2-year                  6     18
##   4-year                 25     32
##   Post-grad              15     10
```

### Weighting
`crtabs` takes advantage of several Crunch features that `xtabs` does not support. First, it respects weight variables that have been set on the server. This dataset is not currently weighted


```r
weight(ds)
```

```
## NULL
```

but we can very easily change that. Let's use the "weight" variable that already exists in the dataset:


```r
weight(ds) <- ds$weight
```

Now, if we request the same two-way table as before, we'll get weighted results:


```r
crtabs(~ educ + gender, data=ds)
```

```
##                       gender
## educ                        Male    Female
##   No HS                12.628200  8.698227
##   High school graduate 43.315170 43.297542
##   Some college         31.612096 29.963579
##   2-year                8.567314 15.260707
##   4-year               16.492669 22.581293
##   Post-grad             8.935983  8.647220
```

If we want unweighted data, that's easy enough:


```r
crtabs(~ educ + gender, data=ds, weight=NULL)
```

```
##                       gender
## educ                   Male Female
##   No HS                   6      6
##   High school graduate   33     38
##   Some college           26     35
##   2-year                  6     18
##   4-year                 25     32
##   Post-grad              15     10
```

### Proportion tables
As with any `array` data type, we can compute margin tables, and the `prop.table` function in R provides a convenient way for sweeping a table by a margin. These work on the output of `crtabs`, too:


```r
prop.table(tab1)
```

```
## educ
##                No HS High school graduate         Some college               2-year               4-year 
##                0.048                0.284                0.244                0.096                0.228 
##            Post-grad 
##                0.100
```

For column proportions, specify margin=2 (by rows, margin=1):


```r
prop.table(tab2, 2)
```

```
##                       gender
## educ                         Male     Female
##   No HS                0.05405405 0.04316547
##   High school graduate 0.29729730 0.27338129
##   Some college         0.23423423 0.25179856
##   2-year               0.05405405 0.12949640
##   4-year               0.22522523 0.23021583
##   Post-grad            0.13513514 0.07194245
```

Let's make that more readable:


```r
round(100*prop.table(tab2, 2))
```

```
##                       gender
## educ                   Male Female
##   No HS                   5      4
##   High school graduate   30     27
##   Some college           23     25
##   2-year                  5     13
##   4-year                 23     23
##   Post-grad              14      7
```

### Complex data types
`crtabs` also comfortably handles the more complex data types that Crunch supports, including categorical array and multiple response variables. In the [array variables vignette](array-variables.md), we created a categorical array, "imiss", for "Important issues". We can crosstab with arrays just as we do non-arrays.


```r
tab3 <- crtabs(~ imiss + gender, data=ds)
tab3
```

```
## , , gender = Male
## 
##                         imiss
## imiss                    Very Important Somewhat Important Not very Important Unimportant
##   Abortion                     31.11036           27.53957          30.125371   32.776132
##   Education                    47.47270           46.51134          11.613407   15.953981
##   Gay rights                   11.31668           28.19149          26.431299   55.117086
##   Health care                  75.85298           39.27766           3.819861    2.600933
##   Immigration                  49.31502           37.34169          17.139246   17.755480
##   Medicare                     59.80997           47.99707           9.550301    4.194091
##   Social security              68.15899           41.79910           6.273460    5.319881
##   Taxes                        65.01490           41.59154           6.653878    7.761953
##   Terrorism                    45.64391           42.62133          11.107822   19.924924
##   The budget deficit           59.03394           30.34666          16.027038   16.143793
##   The economy                  90.19129           18.19050          13.169648    0.000000
##   The environment              30.96798           59.36010          19.180623   12.042728
##   The war in Afghanistan       21.66667           51.95812          20.996176   25.220936
## 
## , , gender = Female
## 
##                         imiss
## imiss                    Very Important Somewhat Important Not very Important Unimportant
##   Abortion                     52.59306           45.01694          17.535359   12.266433
##   Education                    70.03272           45.04846           6.944584    6.422805
##   Gay rights                   33.97914           34.23703          24.418990   35.813411
##   Health care                  90.70215           29.57668           3.697671    3.856843
##   Immigration                  48.19794           48.71002          17.758330   11.222931
##   Medicare                     72.39412           38.22034          12.111045    4.282138
##   Social security              73.35809           35.84414          13.522809    5.723524
##   Taxes                        78.98925           29.93235          11.747300    7.779673
##   Terrorism                    63.89258           37.68492          14.613920   12.257150
##   The budget deficit           58.46827           45.55364          15.087550    9.339109
##   The economy                  93.86900           25.30076           5.421963    3.084749
##   The environment              53.05919           43.99564          15.071708   11.953823
##   The war in Afghanistan       48.27161           48.41640          17.997678   13.762882
```

Note that even though we specified two variables in our formula, because "imiss" itself is two dimensional, our result is a three-dimensional array.

To illustrate working with multiple response variables, let's convert "imiss" to multiple response, selecting its positive categories as indicating selection:


```r
ds$imiss <- dichotomize(ds$imiss, c("Very Important", "Somewhat Important"))
```

Now, when we crosstab it, we'll get a two-dimensional table because multiple response variables present a one-dimensional interface:


```r
tab3mr <- crtabs(~ imiss + gender, data=ds)
tab3mr
```

```
##                         gender
## imiss                         Male    Female
##   Abortion                58.64993  97.61001
##   Education               93.98404 115.08118
##   Gay rights              39.50817  68.21617
##   Health care            115.13064 120.27883
##   Immigration             86.65671  96.90796
##   Medicare               107.80704 110.61446
##   Social security        109.95809 109.20223
##   Taxes                  106.60644 108.92159
##   Terrorism               88.26523 101.57750
##   The budget deficit      89.38060 104.02191
##   The economy            108.38178 119.16976
##   The environment         90.32808  97.05483
##   The war in Afghanistan  73.62479  96.68801
```

It's worth noting here that the result of `crtabs` isn't an `array` object but a `CrunchCube` object.


```r
class(tab3mr)
```

```
## [1] "CrunchCube"
## attr(,"package")
## [1] "crunch"
```

This allows us to do the appropriate calculations on arrays and multiple response variables when `prop.table` is called. To compute a margin table over a multiple response variable, summing along the dimension would give an incorrect value because the responses in a multiple response are not mutually exclusive--they can't be assumed to sum to 100 percent. However, the `margin.table` method on `CrunchCubes` can compute the correct margin, so `prop.table` gives correct proportions:


```r
round(100*prop.table(tab3mr, 2))
```

```
##                         gender
## imiss                    Male Female
##   Abortion                 48     76
##   Education                77     90
##   Gay rights               33     53
##   Health care              95     94
##   Immigration              71     75
##   Medicare                 89     86
##   Social security          90     85
##   Taxes                    88     85
##   Terrorism                73     79
##   The budget deficit       74     81
##   The economy              89     93
##   The environment          74     76
##   The war in Afghanistan   61     75
```

Finally, just as we saw in the [array variables](array-variables.md) vignette, we can grab individual subvariables and crosstab with them:


```r
crtabs(~ imiss$Education + gender, data=ds)
```

```
##                     gender
## imiss_h                  Male    Female
##   Very Important     47.47270 70.032722
##   Somewhat Important 46.51134 45.048457
##   Not very Important 11.61341  6.944584
##   Unimportant        15.95398  6.422805
```

### N-way tables

It's worth noting that we can extend the crosstabbing to higher dimensions, just by adding more terms on the right-hand side of the formula:


```r
round(crtabs(~ imiss + educ + gender, data=ds))
```

```
## , , gender = Male
## 
##                         educ
## imiss                    No HS High school graduate Some college 2-year 4-year Post-grad
##   Abortion                   6                   17           21      1      9         5
##   Education                 13                   25           27      9     13         8
##   Gay rights                 1                   10           18      1      4         5
##   Health care               12                   43           28      9     15         8
##   Immigration               10                   25           23      9     12         8
##   Medicare                  12                   39           27      8     14         7
##   Social security           12                   41           25      9     14         9
##   Taxes                     13                   31           31      9     15         8
##   Terrorism                 12                   25           24      9     12         7
##   The budget deficit        13                   24           24      8     13         7
##   The economy               13                   32           31      9     16         9
##   The environment           12                   31           24      7      9         8
##   The war in Afghanistan    11                   17           24      2     13         6
## 
## , , gender = Female
## 
##                         educ
## imiss                    No HS High school graduate Some college 2-year 4-year Post-grad
##   Abortion                   5                   31           24     10     19         8
##   Education                  9                   32           28     15     23         9
##   Gay rights                 4                   17           21      5     15         6
##   Health care                9                   38           29     15     21         9
##   Immigration                6                   28           27     10     17         9
##   Medicare                   9                   34           26     15     19         7
##   Social security            9                   34           27     13     19         7
##   Taxes                      9                   32           24     14     22         8
##   Terrorism                  5                   35           22     14     18         7
##   The budget deficit         6                   34           23     14     19         7
##   The economy                9                   37           27     15     23         9
##   The environment            6                   29           19     14     21         8
##   The war in Afghanistan     8                   27           27     13     15         7
```

<!--
### Special functions
#### bin
#### rollup

-->
### Numeric aggregations
`crtabs` can also compute quantities other than counts. Using the left-hand side of the formula, we can specify other aggregations to put in the cells of the table. For example, in the [deriving variables vignette](derive.md), we created an "age" variable. We can easily compute the average age by gender and education:


```r
crtabs(mean(age) ~ educ + gender, data=ds)
```

```
##                       gender
## educ                       Male   Female
##   No HS                54.00829 56.02205
##   High school graduate 50.31550 48.09241
##   Some college         40.84733 43.06316
##   2-year               55.25996 46.68702
##   4-year               50.14993 40.75736
##   Post-grad            50.85838 48.64874
```

Other supported aggregations include `min`, `max`, `sd`, and `sum`. For the minimum age by gender and education,


```r
crtabs(min(age) ~ educ + gender, data=ds)
```

```
##                       gender
## educ                   Male Female
##   No HS                  42     45
##   High school graduate   35     27
##   Some college           22     24
##   2-year                 35     26
##   4-year                 32     24
##   Post-grad              26     27
```

We can get unconditional (univariate) statistics by making the right-hand side of your formula be just the number `1`:


```r
crtabs(min(age) ~ 1, data=ds)
```

```
## [1] 22
```

Numeric aggregation functions also work with categorical variables that have numeric values defined for their categories; this is the reason why numeric values for categories are defined, in fact. In the [variables vignette](variables.md), we worked with the "On the right track" question and set some numeric values:


```r
categories(ds$track)
```

```
## [ 1 ]  Generally headed in the right direction
## [ 0 ]  Not sure
## [ -1 ]  Wrong track
## [ NA ]  No Data
```

We can use these numeric values to compute an "on the right track index" by averaging them. If the index is greater than zero, more people thing things are going well, and if it is negative, more respondents are pessimistic.


```r
round(crtabs(mean(track) ~ educ + gender, data=ds), 2)
```

```
##                       gender
## educ                    Male Female
##   No HS                -0.39  -0.05
##   High school graduate -0.69  -0.22
##   Some college         -0.05  -0.14
##   2-year               -0.56  -0.40
##   4-year               -0.57  -0.08
##   Post-grad            -0.17   0.02
```

Looks like most people surveyed thought that the U.S. is on the wrong track, but that pessimism is less pronounced for women with higher levels of education.

### Subsetting data

We can also specify a subset of `ds` to analyze, just as if it were a data.frame. Let's do the same calculation for Democrats only:


```r
round(crtabs(mean(track) ~ educ + gender, data=ds[ds$pid3 == "Democrat",]), 2)
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

Not surprisingly, Democrats were less pessimistic about the direction of the country than the general population.

A few final observations about `crtabs`. First, all of these calculations have been weighted by the weight variable we set above. We set it and could then forget about it--we didn't have to litter all of our expressions with `ds$weight` and extra arithmetic to do the weighting. Crunch handles this for us.

Second, none of these aggregations required pulling case-level data to your computer. `crtabs` sends Crunch expressions to the server and receives in return an `n`-D array of results. The only computations happening locally are the margin tables and sweeping in `prop.table`, computing on the aggregate results. Your computer would work exactly as hard with this example dataset of 1000 rows as it would with a dataset of 100 million rows.  

## Statistical modeling
Any statistical modeling function that takes a `data` argument should happily accept a `CrunchDataset` and just do the right thing--no extra effort or thought required.

Let's fit a basic Ordinary Least Squares (OLS) model. In our dataset, we have a few questions about Edward Snowden, such as:


```r
ds$snowdenleakapp
```

```
## Approval of Snowden's Leak (categorical)
## 
##                        Count
## Strongly disapprove 86.00644
## Not sure            55.82542
## Somewhat approve    43.19385
## Somewhat disapprove 40.26587
## Strongly approve    24.70842
```

We can use `lm` to fit our model. Let's explore the relationship between approval of Snowden's leak and respondents' interest in current events, party identification, gender, and age.


```r
ols1 <- lm(I(snowdenleakapp == "Strongly approve") ~ newsint2 + pid3 + gender + age,
    data=ds)
summary(ols1)
```

```
## 
## Call:
## lm(formula = I(snowdenleakapp == "Strongly approve") ~ newsint2 + 
##     pid3 + gender + age, data = ds)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.33261 -0.15753 -0.12930 -0.06006  0.94924 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                0.133723   0.093700   1.427    0.155
## newsint2Some of the time   0.016157   0.050750   0.318    0.750
## newsint2Only now and then -0.007068   0.074048  -0.095    0.924
## newsint2Hardly at all     -0.058744   0.091630  -0.641    0.522
## pid3Republican            -0.088822   0.058002  -1.531    0.127
## pid3Independent            0.022139   0.052650   0.420    0.675
## pid3Other                  0.185892   0.198541   0.936    0.350
## pid3Not sure              -0.049288   0.103442  -0.476    0.634
## genderFemale              -0.019411   0.045856  -0.423    0.672
## age                        0.000361   0.001651   0.219    0.827
## 
## Residual standard error: 0.3366 on 240 degrees of freedom
## Multiple R-squared:  0.02533,	Adjusted R-squared:  -0.01122 
## F-statistic: 0.6931 on 9 and 240 DF,  p-value: 0.7149
```

Looks like partisanship is weakly associated with approval of the NSA leak, but overall the model isn't a great fit, given our data. (For what it's worth, we're working on a randomly drawn subset of the survey so that the size of data included with package is small. Results are more meaningful with the full dataset.) Nevertheless, this example illustrates how straightforward it is to do statistical analysis with data in Crunch. Even though your dataset lives on the server, you can think of it like a local `data.frame`. Note, for example, that our categorical variables (News Interest, Party ID, and Gender) expand their categories out as dichotomous indicators, just as if they were `factor` variables in a `data.frame`.

Given that we're estimating a model with a dichotomous dependent variable, perhaps a logit would be more appropriate than a strict linear predictor. We can use `glm` instead:


```r
logit1 <- glm(I(snowdenleakapp == "Strongly approve") ~ newsint2 + pid3 + gender + age,
    family=binomial(link="logit"), data=ds)
summary(logit1)
```

```
## 
## Call:
## glm(formula = I(snowdenleakapp == "Strongly approve") ~ newsint2 + 
##     pid3 + gender + age, family = binomial(link = "logit"), data = ds)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.8982  -0.5873  -0.5249  -0.3413   2.4261  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)  
## (Intercept)               -1.883494   0.847876  -2.221   0.0263 *
## newsint2Some of the time   0.141215   0.439458   0.321   0.7480  
## newsint2Only now and then -0.075103   0.700499  -0.107   0.9146  
## newsint2Hardly at all     -0.734607   1.084190  -0.678   0.4980  
## pid3Republican            -1.077062   0.673224  -1.600   0.1096  
## pid3Independent            0.156299   0.439101   0.356   0.7219  
## pid3Other                  1.056509   1.273998   0.829   0.4069  
## pid3Not sure              -0.535621   1.119244  -0.479   0.6323  
## genderFemale              -0.176401   0.410364  -0.430   0.6673  
## age                        0.003547   0.015024   0.236   0.8134  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 191.28  on 249  degrees of freedom
## Residual deviance: 184.52  on 240  degrees of freedom
## AIC: 204.52
## 
## Number of Fisher Scoring iterations: 5
```

As before, not a particularly interesting result, but this is just the beginning of the analysis process. Using `crunch`, you can keep exploring the data and perhaps find a better fit.

Unlike the previous examples, these modeling functions do have to pull columns of data from the server to your local machine. However, only the columns of data you reference in your formula are copied, and if you specify a subset of the dataset to regress on (as we did above with `crtabs` when we looked at just Democrats), only those rows are retrieved. This helps minimize the time spent shipping data across the network. Moreover, because of the `crunch` package's query cache, subsequent models that incorporate any of those variables will not have to go to the server to get them.

[Next: filtering datasets](filters.md)
