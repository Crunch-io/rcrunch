# Use vegetables dataset to create cube fixtures that include numeric arrays
library(crunch)
library(httptest)
library(fs)
library(here)


login()
temp_dir <- tempfile()
ds <- loadDataset("Vegetables example")


# Numeric array alone (numa.json) ----
httpcache::clearCache()
dir_create(temp_dir)

start_capturing(temp_dir)
crtabs(mean(ds$ratings_numa) ~ 1, ds)
stop_capturing()

cube_path <- dir_ls(
    temp_dir,
    regexp = "app.crunch.io/api/datasets/.{6}/cube-.{6}\\.json",
    recurse = TRUE
)
file_copy(cube_path, here("mocks", "cubes", "numa.json"), overwrite = TRUE)

dir_delete(temp_dir)


# Numeric array by categorical (numa-x-cat.json) ----
httpcache::clearCache()
dir_create(temp_dir)

start_capturing(temp_dir)
crtabs(mean(ds$ratings_numa) ~ ds$healthy_eater, ds)
stop_capturing()

cube_path <- dir_ls(
    temp_dir,
    regexp = "app.crunch.io/api/datasets/.{6}/cube-.{6}\\.json",
    recurse = TRUE
)
file_copy(cube_path, here("mocks", "cubes", "numa-x-cat.json"), overwrite = TRUE)

dir_delete(temp_dir)


# Numeric array by categorical (numa-x-mr.json) ----
httpcache::clearCache()
dir_create(temp_dir)

start_capturing(temp_dir)
crtabs(mean(ds$ratings_numa) ~ ds$enjoy_mr, ds)
stop_capturing()

cube_path <- dir_ls(
    temp_dir,
    regexp = "app.crunch.io/api/datasets/.{6}/cube-.{6}\\.json",
    recurse = TRUE
)
file_copy(cube_path, here("mocks", "cubes", "numa-x-mr.json"), overwrite = TRUE)

dir_delete(temp_dir)

