library(rcrunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

load("../pkg/vignettes/array-variables.RData")

rm(df)
save.image(file="../pkg/vignettes/variable-order.RData")