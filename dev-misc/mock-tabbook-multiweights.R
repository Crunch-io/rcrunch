library(httptest)
rm(list = ls())

system("rm -rf tmp")

# ds <- newExampleDataset()
ds <- loadDataset("Example dataset")
weight(ds) <- ds$weight1
#ds$weight1 <- NULL
#ds$weight2 <- NULL

# Dummy weights for testing
#ds$weight1 <- makeWeight(ds$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
#ds$weight2 <- makeWeight(ds$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight2')

#is.weight(ds$weight1) <- TRUE
#is.weight(ds$weight2) <- TRUE



httpcache::clearCache()
start_capturing("tmp")
login() 
ds <- loadDataset("Example dataset")
w <- list(weight1 = c('allpets', 'q1'), weight2 = 'q1')
tabFramePrepare(
    ds[c("q1", "allpets", "weight2", "weight1")], w, FALSE)
tabFramePrepare(ds, w, TRUE)
tabFrame <- tabFramePrepare(ds, w, TRUE)
multitable <- newMultitable("~ `allpets`", ds)
r <- tabBook(multitable, dataset = ds, w)
stop_capturing()


# Drop weights and capture again
weight(ds) <- NULL

start_capturing("tmp_unweighted")
login()
ds <- loadDataset("Example dataset")

login() 
ds <- loadDataset("Example dataset")
w <- list(weight1 = c('allpets', 'q1'), weight2 = 'q1')
tabFramePrepare(
    ds[c("q1", "allpets", "weight2", "weight1")], w, FALSE)
tabFramePrepare(ds, w, TRUE)
tabFrame <- tabFramePrepare(ds, w, TRUE)
multitable <- newMultitable("~ `allpets`", ds)
r <- tabBook(multitable, dataset = ds, w)

stop_capturing()


