## ----, results='hide', echo=FALSE, message=FALSE, eval=FALSE-------------
#  library(crunch)
#  load("variable-order.RData")

## ----, eval=FALSE--------------------------------------------------------
#  ordering(ds)

## ----, echo=FALSE--------------------------------------------------------
step0

## ----, eval=FALSE--------------------------------------------------------
#  ordering(ds) <- VariableOrder(
#          VariableGroup("Demos", ds[c(1:3, 93:118)]),
#          VariableGroup("Tracking questions", ds[c(4,5,52:92)]),
#          VariableGroup("This week", ds[6:51])
#      )

## ----, eval=FALSE--------------------------------------------------------
#  ordering(ds)

## ----, echo=FALSE--------------------------------------------------------
step1

## ----, eval=FALSE--------------------------------------------------------
#  names(ordering(ds))

## ----, echo=FALSE--------------------------------------------------------
names(step1)

## ----, eval=FALSE--------------------------------------------------------
#  names(ordering(ds))[1] <- "Demographics"
#  names(ordering(ds))

## ----, echo=FALSE--------------------------------------------------------
names(step2)

## ----, eval=FALSE--------------------------------------------------------
#  ordering(ds) <- ordering(ds)[c(2, 3, 1)]
#  names(ordering(ds))

## ----, echo=FALSE--------------------------------------------------------
names(step3)

