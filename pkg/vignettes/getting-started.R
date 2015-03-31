## ----, message=FALSE-----------------------------------------------------
library(crunch)

## ----, eval=FALSE--------------------------------------------------------
#  login(email="xkcd@crunch.io", password="correct horse battery staple")

## ----, results='hide', echo=FALSE, message=FALSE-------------------------
## Because the vignette tasks require communicating with a remote host,
## we do all the work ahead of time and save a workspace, which we load here.
## We'll then reference saved objects in that as if we had just retrieved them
## from the server
load("getting-started.RData")

## ------------------------------------------------------------------------
load("economist.RData")
dim(df)

## ----, eval=FALSE--------------------------------------------------------
#  ds <- newDataset(df, name="Economist/YouGov Weekly Survey")

## ------------------------------------------------------------------------
dim(ds)

## ----, eval=FALSE--------------------------------------------------------
#  listDatasets()

## ----, eval=FALSE--------------------------------------------------------
#  ds <- loadDataset("Economist/YouGov Weekly Survey")

## ------------------------------------------------------------------------
is.dataset(ds)

## ----, eval=FALSE--------------------------------------------------------
#  ds <- hideVariables(ds, "comments")
#  hiddenVariables(ds)

## ----, eval=FALSE--------------------------------------------------------
#  hiddenVariables(ds) <- "pid7others"
#  hiddenVariables(ds)

## ----unhide, eval=FALSE--------------------------------------------------
#  ds <- unhideVariables(ds, "pid7others")
#  hiddenVariables(ds)

