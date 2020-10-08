library(httptest)
rm(list = ls())

system("rm -rf tmp tmp_unweighted")
login()
# ds <- newExampleDataset()
ds <- loadDataset("Example dataset")
weight(ds) <- ds$weight1
with_consent(ds$weight1 <- NULL)
with_consent(ds$weight2 <- NULL)

# Dummy weights for testing
ds$weight1 <- makeWeight(ds$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
ds$weight2 <- makeWeight(ds$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight2')

is.weight(ds$weight2) <- TRUE
is.weight(ds$weight1) <- TRUE
weight(ds) <- ds$weight1

httpcache::clearCache()

start_capturing("tmp")
login() 
ds <- loadDataset("Example dataset")
w <- list(weight1 = c('allpets', 'q1'), weight2 = 'q1')
tabFramePrepare(
    ds[c("q1", "allpets", "weight2", "weight1")], w)
ds[["weight1"]]
ds[["weight2"]]
tabFramePrepare(ds, w)
tabFrame <- tabFramePrepare(ds, w)
multitable <- newMultitable("~ `allpets`", ds)
r <- tabBook(multitable, dataset = ds, w)
stop_capturing()


rn(list = ls())
with_consent(deleteDataset("Example dataset"))
ds <- newExampleDataset()
ds$weight1 <- makeWeight(ds$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
ds$weight2 <- makeWeight(ds$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight2')
is.weight(ds$weight2) <- TRUE
is.weight(ds$weight1) <- TRUE
weight(ds) <- NULL

httpcache::clearCache()
start_capturing("tmp_unweighted")
login()
ds <- loadDataset("Example dataset")
ds[["weight1"]]
ds[["weight2"]]
# Drop weights and capture again

w <- list(weight1 = c('allpets', 'q1'), weight2 = 'q1')
tabFramePrepare(
    ds[c("q1", "allpets", "weight2", "weight1")], w)
tabFramePrepare(ds, w)
tabFrame <- tabFramePrepare(ds, w)
multitable <- newMultitable("~ `allpets`", ds)
r <- tabBook(multitable, dataset = ds, w)
stop_capturing()


# Copy from player/export/tabbooks/export-*/*.json into  app.crunch.io/api/datasets/<ID>/multitables/<MID>/