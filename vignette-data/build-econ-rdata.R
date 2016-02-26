set.seed(999)
library(foreign)
economist <- read.spss("../../ECON0003_WTS_W235.sav", to.data.frame=TRUE)
economist <- economist[-grep("^pp", names(economist))]
economist <- economist[-setdiff(grep("_t$", names(economist)), match("imiss_t", names(economist)))]
economist <- economist[-c(1:3, 7:40, grep("^qualityControl", names(economist)), grep("religpew", names(economist)))]
economist$econid <- NULL
economist$boap_issues <- NULL
economist$imiss <- NULL
economist$track[is.na(economist$track)] <- "Not sure"
economist <- economist[-c(1:2, 14:51, 94:116)] ## and ^pew_
economist$comments <- NULL
economist$inputstate <- NULL
economist$imissf2 <- NULL
economist <- economist[-c(grep("^fav", names(economist)), grep("^pew_", names(economist)))]
economist$faminc2 <- NULL
economist$obamaGroup <- NULL
economist$obamaVote <- NULL

economist <- economist[sample(seq_len(nrow(economist)), 250, replace=FALSE),]
economist$weight <- economist$weight/mean(economist$weight)
save(economist, file="../data/economist.RData")
