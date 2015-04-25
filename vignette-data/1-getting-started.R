library(crunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

library(foreign)
df <- read.spss("ECON0003_WTS_W235.sav", to.data.frame=TRUE)
df <- df[-grep("^pp", names(df))]
df <- df[-setdiff(grep("_t$", names(df)), match("imiss_t", names(df)))]
df <- df[-c(1:3, 7:40, grep("^qualityControl", names(df)), grep("religpew", names(df)))]
df$econid <- NULL
df$boap_issues <- NULL
df$imiss <- NULL
df$track[is.na(df$track)] <- "Not sure"
# df <- df[1:100,]

# load("../pkg/vignettes/economist.RData")
ds <- newDataset(df, name="Economist/YouGov Weekly Survey")

.listDatasets <- "Economist/YouGov Weekly Survey"
save(ds, .listDatasets, file="../pkg/vignettes/getting-started.RData")
