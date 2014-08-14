library(rcrunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

load("../pkg/vignettes/economist.RData")
ds <- newDataset(df, name="Economist/YouGov Weekly Survey")

.listDatasets <- "Economist/YouGov Weekly Survey"
save(ds, .listDatasets, file="../pkg/vignettes/getting-started.RData")
