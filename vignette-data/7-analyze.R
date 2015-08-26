library(crunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

load("../vignettes/derive.RData")

tab1 <- crtabs(~ educ, data=ds)

tab2 <- crtabs(~ educ + gender, data=ds)

### Weighting

weight(ds) <- ds$weight

tab2weighted <- crtabs(~ educ + gender, data=ds)


### Complex data types

tab3 <- crtabs(~ imiss + gender, data=ds)

ds$imiss <- dichotomize(ds$imiss, c("Very Important", "Somewhat Important"))
tab3mr <- crtabs(~ imiss + gender, data=ds)

tab4 <- crtabs(~ imiss + educ + gender, data=ds)

### Numeric aggregations

tab5 <- crtabs(mean(age) ~ educ + gender, data=ds)

tab6 <- crtabs(min(age) ~ educ + gender, data=ds)

tab6a <- crtabs(min(age) ~ 1, data=ds)

track.cats <- categories(ds$track)

tab7 <- crtabs(mean(track) ~ educ + gender, data=ds)

tab8 <- crtabs(mean(track) ~ educ + gender, data=ds[ds$pid3 == "Democrat",])

snowdenleakapp.var <- capture.output(print(ds$snowdenleakapp))

ols1 <- lm(I(snowdenleakapp == "Strongly approve") ~ newsint2 + pid3 + gender + age, data=ds)

logit1 <- glm(I(snowdenleakapp == "Strongly approve") ~ newsint2 + pid3 + gender + age, family=binomial(link="logit"), data=ds)

save.image(file="../vignettes/analyze.RData")

delete(ds) ## cleanup