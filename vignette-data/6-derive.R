library(crunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

load("../pkg/vignettes/variable-order.RData")

ds$age <- 2015 - ds$birthyr

age.summary.before <- summary(ds$age)
by.summary.before <- summary(ds$birthyr)

print(age.summary.before)

ds$birthyr[ds$birthyr < 1945] <- 1945

age.summary.after <- summary(ds$age)
by.summary.after <- summary(ds$birthyr)

print(age.summary.after)

cr.expr <- 2015 - ds$birthyr
cr.log.expr <- ds$birthyr < 1945

save.image(file="../pkg/vignettes/derive.RData")