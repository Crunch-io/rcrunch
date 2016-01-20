library(crunch)
library(foreign)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

economist <- read.spss("ECON0003_WTS_W235.sav", to.data.frame=TRUE)
economist <- economist[-grep("^pp", names(economist))]
economist <- economist[-setdiff(grep("_t$", names(economist)), match("imiss_t", names(economist)))]
economist <- economist[-c(1:3, 7:40, grep("^qualityControl", names(economist)), grep("religpew", names(economist)))]
economist$econid <- NULL
economist$boap_issues <- NULL
economist$imiss <- NULL
economist$track[is.na(economist$track)] <- "Not sure"
# economist <- economist[1:100,]

# load("../vignettes/economist.RData")
ds <- newDataset(economist, name="A Toy, Economist/YouGov Weekly Survey")
dim.ds <- dim(ds)

.listDatasets <- "A Toy, Economist/YouGov Weekly Survey" # the dot avoids confusing with real fxn?
#save(ds, .listDatasets, dim.ds, file="../vignettes/getting-started.RData")
save(economist, file="../vignettes/economist.RData")
