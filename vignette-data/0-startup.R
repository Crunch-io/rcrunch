library(foreign)
df <- read.spss("ECON0003_WTS_W235.sav", to.data.frame=TRUE)
df <- df[-grep("^pp", names(df))]
df <- df[-setdiff(grep("_t$", names(df)), match("imiss_t", names(df)))]
df <- df[-c(1:3, 7:40, grep("^qualityControl", names(df)), grep("religpew", names(df)))]
df$econid <- NULL
df$boap_issues <- NULL
df$imiss <- NULL
df <- df[1:100,]
save(df, file="../pkg/vignettes/economist.RData")