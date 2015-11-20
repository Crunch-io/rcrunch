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
##                   47                  278                  253                   83                  228 
##            Post-grad 
##                  111
```

Additional dimensions are added with `+`. For a two-way table of education and gender, 


```r
tab2 <- crtabs(~ educ + gender, data=ds)
tab2
```

```
##                       gender
## educ                   Male Female
##   No HS                  23     24
##   High school graduate  124    154
##   Some college          110    143
##   2-year                 32     51
##   4-year                115    113
##   Post-grad              63     48
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
##   No HS                 57.78851  51.70880
##   High school graduate 152.08593 155.76219
##   Some college         126.99383 143.98321
##   2-year                35.00172  36.41431
##   4-year                78.47811  88.75691
##   Post-grad             37.43805  35.58841
```

If we want unweighted data, that's easy enough:


```r
crtabs(~ educ + gender, data=ds, weight=NULL)
```

```
##                       gender
## educ                   Male Female
##   No HS                  23     24
##   High school graduate  124    154
##   Some college          110    143
##   2-year                 32     51
##   4-year                115    113
##   Post-grad              63     48
```

### Proportion tables
As with any `array` data type, we can compute margin tables, and the `prop.table` function in R provides a convenient way for sweeping a table by a margin. These work on the output of `crtabs`, too:


```r
prop.table(tab1)
```

```
## educ
##                No HS High school graduate         Some college               2-year               4-year 
##                0.047                0.278                0.253                0.083                0.228 
##            Post-grad 
##                0.111
```

For column proportions, specify margin=2 (by rows, margin=1):


```r
prop.table(tab2, 2)
```

```
##                       gender
## educ                         Male     Female
##   No HS                0.04925054 0.04502814
##   High school graduate 0.26552463 0.28893058
##   Some college         0.23554604 0.26829268
##   2-year               0.06852248 0.09568480
##   4-year               0.24625268 0.21200750
##   Post-grad            0.13490364 0.09005629
```

Let's make that more readable:


```r
round(100*prop.table(tab2, 2))
```

```
##                       gender
## educ                   Male Female
##   No HS                   5      5
##   High school graduate   27     29
##   Some college           24     27
##   2-year                  7     10
##   4-year                 25     21
##   Post-grad              13      9
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
##   Abortion                    162.12515          208.09776           0.000000  134.988838
##   Education                     0.00000          107.24213          77.846505    0.000000
##   Gay rights                   39.36797            0.00000           4.006600    5.899711
##   Health care                 249.27835          311.70813           0.000000  149.975893
##   Immigration                   0.00000           49.29305          29.862339    0.000000
##   Medicare                     15.59800            0.00000           3.271685    4.575016
##   Social security              89.37703          110.79498           0.000000  126.656090
##   Taxes                         0.00000           91.30474         123.073140    0.000000
##   Terrorism                   130.67483            0.00000           4.704833    6.056686
##   The budget deficit          309.00483          371.86255           0.000000  130.563156
##   The economy                   0.00000           38.21869          10.108529    0.000000
##   The environment              11.12973            0.00000           3.271685    5.606140
##   The war in Afghanistan      195.29567          179.08374           0.000000  185.411293
## 
## , , gender = Female
## 
##                         imiss
## imiss                    Very Important Somewhat Important Not very Important Unimportant
##   Abortion                      0.00000           71.04937          76.601528    0.000000
##   Education                    30.59193            0.00000           4.526322   11.261814
##   Gay rights                  232.66403          296.29635           0.000000  171.901199
##   Health care                   0.00000           59.70127          44.960749    0.000000
##   Immigration                  18.78344            0.00000           1.000690    6.745825
##   Medicare                    262.64142          302.56883           0.000000  154.105889
##   Social security               0.00000           49.45844          42.933408    0.000000
##   Taxes                        13.99751            0.00000           3.271685    3.789242
##   Terrorism                   275.06635          300.65116           0.000000  148.382303
##   The budget deficit            0.00000           44.73307          32.880000    0.000000
##   The economy                  15.54603            0.00000           4.240972    9.624116
##   The environment             215.59557          256.47697           0.000000  162.970011
##   The war in Afghanistan        0.00000           64.45748          55.872378    0.000000
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
## imiss                        Male   Female
##   Abortion               297.1140 389.0997
##   Education              399.2542 462.1785
##   Gay rights             216.0331 252.4092
##   Health care            439.5680 485.3694
##   Immigration            380.7070 393.7586
##   Medicare               404.5652 441.7238
##   Social security        416.7473 451.4937
##   Taxes                  423.4487 454.1637
##   Terrorism              378.5656 418.2717
##   The budget deficit     391.4612 433.0006
##   The economy            462.2432 486.2133
##   The environment        375.9582 392.7234
##   The war in Afghanistan 311.3608 399.2116
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
##   Abortion                 61     76
##   Education                82     91
##   Gay rights               44     50
##   Health care              90     95
##   Immigration              78     77
##   Medicare                 83     87
##   Social security          86     89
##   Taxes                    87     89
##   Terrorism                78     82
##   The budget deficit       80     85
##   The economy              95     95
##   The environment          77     77
##   The war in Afghanistan   64     78
```

Finally, just as we saw in the [array variables](array-variables.md) vignette, we can grab individual subvariables and crosstab with them:


```r
crtabs(~ imiss$Education + gender, data=ds)
```

```
##                     gender
## imiss_h                   Male    Female
##   Very Important     249.27835 311.70813
##   Somewhat Important 149.97589 150.47036
##   Not very Important  49.29305  29.86234
##   Unimportant         35.96718  15.59800
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
##   Abortion                  48                   41            0     83    113         0
##   Education                116                    0           18     29      0        43
##   Gay rights                 0                   19           29      0      0         0
##   Health care               51                   51            0    114    135         0
##   Immigration              132                    0           31     32      0        68
##   Medicare                   0                   34           32      0      0         0
##   Social security           29                   22            0     47     64         0
##   Taxes                     82                    0           10     17      0        37
##   Terrorism                  0                   20           20      0      0         0
##   The budget deficit        57                   51            0    137    141         0
##   The economy              140                    0           31     36      0        69
##   The environment            0                   34           34      0      0         0
##   The war in Afghanistan    47                   38            0    106    115         0
## 
## , , gender = Female
## 
##                         educ
## imiss                    No HS High school graduate Some college 2-year 4-year Post-grad
##   Abortion                 127                    0           28     34      0        65
##   Education                  0                   32           30      0      0         0
##   Gay rights                57                   52            0    135    141         0
##   Health care              128                    0           30     33      0        66
##   Immigration                0                   34           31      0      0         0
##   Medicare                  50                   52            0    126    133         0
##   Social security          122                    0           28     35      0        72
##   Taxes                      0                   35           34      0      0         0
##   Terrorism                 56                   41            0    112    135         0
##   The budget deficit       120                    0           31     32      0        59
##   The economy                0                   28           29      0      0         0
##   The environment           56                   45            0    110    132         0
##   The war in Afghanistan   124                    0           34     34      0        59
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
##   No HS                50.82525 44.68583
##   High school graduate 52.15276 51.32068
##   Some college         40.29562 41.17773
##   2-year               46.40166 45.33917
##   4-year               42.32709 39.58522
##   Post-grad            49.87801 48.03841
```

Other supported aggregations include `min`, `max`, `sd`, and `sum`. For the minimum age by gender and education, 


```r
crtabs(min(age) ~ educ + gender, data=ds)
```

```
##                       gender
## educ                   Male Female
##   No HS                  25     24
##   High school graduate   32     22
##   Some college           22     21
##   2-year                 25     25
##   4-year                 24     24
##   Post-grad              26     27
```

We can get unconditional (univariate) statistics by making the right-hand side of your formula be just the number `1`:


```r
crtabs(min(age) ~ 1, data=ds)
```

```
## [1] 21
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
##   No HS                -0.83  -0.74
##   High school graduate -0.50  -0.45
##   Some college         -0.31  -0.35
##   2-year               -0.38  -0.35
##   4-year               -0.30  -0.28
##   Post-grad            -0.26  -0.26
```

Looks like most people surveyed thought that the U.S. is on the wrong track, but that pessimism is less pronounced for those with higher levels of education.

### Subsetting data

We can also specify a subset of `ds` to analyze, just as if it were a data.frame. Let's do the same calculation for Democrats only:


```r
round(crtabs(mean(track) ~ educ + gender, data=ds[ds$pid3 == "Democrat",]), 2)
```

```
##                       gender
## educ                    Male Female
##   No HS                -0.90   1.00
##   High school graduate  0.22  -0.03
##   Some college          0.55   0.20
##   2-year               -0.35   0.32
##   4-year                0.36   0.07
##   Post-grad             0.42   0.02
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
## snowdenleakapp (categorical)
## 
##                        Count
## Strongly disapprove 288.3561
## Somewhat approve    220.3593
## Not sure            200.1591
## Strongly approve    159.1486
## Somewhat disapprove 131.4862
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
## -0.51183 -0.19709 -0.13374 -0.08017  0.96392 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                0.248796   0.049545   5.022 6.08e-07 ***
## newsint2Some of the time  -0.025779   0.027641  -0.933  0.35123    
## newsint2Only now and then -0.034062   0.040342  -0.844  0.39869    
## newsint2Hardly at all     -0.091411   0.052376  -1.745  0.08125 .  
## pid3Republican            -0.031757   0.031030  -1.023  0.30636    
## pid3Independent            0.091577   0.028140   3.254  0.00118 ** 
## pid3Other                  0.313302   0.097186   3.224  0.00131 ** 
## pid3Not sure               0.014837   0.065847   0.225  0.82177    
## genderFemale              -0.042308   0.024297  -1.741  0.08194 .  
## age                       -0.001436   0.000876  -1.640  0.10142    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3683 on 989 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.04013,	Adjusted R-squared:  0.0314 
## F-statistic: 4.595 on 9 and 989 DF,  p-value: 5.785e-06
```

Looks like partisanship is associated with approval of the NSA leak, but overall the model isn't a great fit, given our data. Nevertheless, this example illustrates how straightforward it is to do statistical analysis with data in Crunch. Even though your dataset lives on the server, you can think of it like a local `data.frame`. Note, for example, that our categorical variables (News Interest, Party ID, and Gender) expand their categories out as dichotomous indicators, just as if they were `factor` variables in a `data.frame`. 

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
## -1.2651  -0.6418  -0.5229  -0.4278   2.3800  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)   
## (Intercept)               -1.031957   0.361199  -2.857  0.00428 **
## newsint2Some of the time  -0.190179   0.205651  -0.925  0.35509   
## newsint2Only now and then -0.271006   0.315410  -0.859  0.39022   
## newsint2Hardly at all     -0.843629   0.496533  -1.699  0.08931 . 
## pid3Republican            -0.302909   0.263112  -1.151  0.24963   
## pid3Independent            0.612542   0.200765   3.051  0.00228 **
## pid3Other                  1.608627   0.546297   2.945  0.00323 **
## pid3Not sure               0.144096   0.519991   0.277  0.78169   
## genderFemale              -0.309956   0.179389  -1.728  0.08402 . 
## age                       -0.010655   0.006548  -1.627  0.10368   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 905.03  on 998  degrees of freedom
## Residual deviance: 866.64  on 989  degrees of freedom
##   (1 observation deleted due to missingness)
## AIC: 886.64
## 
## Number of Fisher Scoring iterations: 5
```

As before, not a particularly interesting result, but this is just the beginning of the analysis process. Using `crunch`, you can keep exploring the data and perhaps find a better fit.

Unlike the previous examples, these modeling functions do have to pull columns of data from the server to your local machine. However, only the columns of data you reference in your formula are copied, and if you specify a subset of the dataset to regress on (as we did above with `crtabs` when we looked at just Democrats), only those rows are retrieved. This helps minimize the time spent shipping data across the network. Moreover, because of the `crunch` package's query cache, subsequent models that incorporate any of those variables will not have to go to the server to get them. 
