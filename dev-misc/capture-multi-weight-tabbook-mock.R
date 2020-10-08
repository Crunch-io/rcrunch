library(httptest)
library(jsonlite)
library(readr)
library(stringr)
library(fs)
library(crunch)

temp_dir <- tempfile()
dir.create(temp_dir)

# Find old mock and delete files associated with it
old_mock_id <- read_json("mocks/app.crunch.io/api/datasets/by_name/multiweight%20tabbook%20ds.json")$index[[1]]$id
dir_delete(paste0("mocks/app.crunch.io/api/datasets/", old_mock_id))
file_delete(paste0("mocks/app.crunch.io/api/datasets/", old_mock_id, ".json"))
file_delete("mocks/app.crunch.io/api/datasets/by_name/multiweight%20tabbook%20ds.json")


# Prepare dataset from scratch
login()
ds <- newExampleDataset()
name(ds) <- "multiweight tabbook ds"

# Dummy weights for testing (need to exist, but we don't want them set)
ds$weight1 <- makeWeight(ds$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
ds$weight2 <- makeWeight(ds$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight2')

is.weight(ds$weight2) <- TRUE
is.weight(ds$weight1) <- TRUE
weight(ds) <- NULL

# Make a multitable so we don't have to in tests
newMultitable("~ `allpets`", ds)


# Capture mocks
httpcache::clearCache()
start_capturing(temp_dir)
login()
ds <- loadDataset("multiweight tabbook ds")
w <- list(weight1 = c('allpets', 'q1'), weight2 = 'q1')
tabBookWeightSpec(ds[c("q1", "allpets", "weight2", "weight1")], w)

multitable <- multitables(ds)[[1]]
r <- tabBook(multitable, dataset = ds, w)
stop_capturing()

# Copy over new mocks into right place
new_mock_id <- read_json(paste0(temp_dir, "/app.crunch.io/api/datasets/by_name/multiweight%20tabbook%20ds.json"))$index[[1]]$id

file_copy(
    paste0(temp_dir, "/app.crunch.io/api/datasets/by_name/multiweight%20tabbook%20ds.json"),
    "mocks/app.crunch.io/api/datasets/by_name/multiweight%20tabbook%20ds.json"
)

file_copy(
    paste0(temp_dir, "/app.crunch.io/api/datasets/", new_mock_id, ".json"),
    paste0("mocks/app.crunch.io/api/datasets/", new_mock_id, ".json")
)

dir_copy(
    paste0(temp_dir, "/app.crunch.io/api/datasets/", new_mock_id),
    paste0("mocks/app.crunch.io/api/datasets/", new_mock_id),
)

# Need to move and rename the tabbook result files
tabbook_files <- dir_ls(
    paste0(temp_dir, "/player-crunch-io.s3.amazonaws.com"),
    recurse = TRUE,
    type = "file"
)

# Move them into the multitable's folder so get the id
new_mt_id <- dir_ls(
    paste0(temp_dir, "/app.crunch.io/api/datasets/", new_mock_id, "/multitables"),
    type = "dir"
)
new_mt_id <- str_replace(new_mt_id, ".+/", "")


for (file in tabbook_files) {
    weight <- read_json(file)$meta$analyses[[1]]$weight

    if (is.null(weight)) weight <- "unweighted"

    file_copy(
        file,
        paste0("mocks/app.crunch.io/api/datasets/", new_mock_id, "/multitables/", new_mt_id, "/tabbook-", weight, ".json")
    )
}
