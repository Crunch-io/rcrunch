library(crunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

load("../pkg/vignettes/array-variables.RData")

step0 <- ordering(ds)

ordering(ds) <- VariableOrder(
        VariableGroup("Demos", ds[c(1:3, 93:118)]),
        VariableGroup("Tracking questions", ds[c(4,5,52:92)]),
        VariableGroup("This week", ds[6:51])
    )
step1 <- ordering(ds)

names(ordering(ds))[1] <- "Demographics"
step2 <- ordering(ds)

ordering(ds) <- ordering(ds)[c(2, 3, 1)]
step3 <- ordering(ds)

save.image(file="../pkg/vignettes/variable-order.RData")