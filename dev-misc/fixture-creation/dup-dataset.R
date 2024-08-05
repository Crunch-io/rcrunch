library(crunch)
library(here)
library(fs)
library(httptest)
library(purrr)

setupCrunchAuth("team")

source(here("dev-misc/fixture-creation/redactors.R"))

# Make a dataset with duplicate aliases (in subvariables)
ds <- newDataset(data.frame(
    x1 = 1:3,
    x2 = 2:4,
    y1 = factor(letters[1:3], levels = letters[1:5]),
    y2 = factor(letters[2:4], levels = letters[1:5]),
    z = factor(letters[11:13], levels = letters[11:15])
), "dup test")

ds$x <- deriveArray(
    list(
        VarDef(ds$x1, name = "x1", alias = "x1"),
        VarDef(ds$x2, name = "x2_derived", alias = "x2_derived")
    ),
    name = "x",
    numeric = TRUE
)

ds$y <- deriveArray(
    list(
        VarDef(ds$y1, name = "y1", alias = "y1"),
        VarDef(ds$y2, name = "z", alias = "z")
    ),
    name = "y",
    numeric = FALSE
)

mv(projects()[["Vegetables fixture"]], ds, projects()[["Vegetables fixture"]])
ds <- refresh(ds)
ds_url <- self(ds)

# Capture fixtures ----
set_redactor(response_redactor(ds, "dup"))
set_requester(request_redactor(ds, "dup"))
## Capture dataset fixtures ----
### General dataset capture ----
temp_dir <- tempfile()
httpcache::clearCache()
dir_create(temp_dir)
start_capturing(temp_dir)

ds <- loadDataset(ds_url)
aliases(allVariables(ds))
# Don't actually export because httptest doesn't get it right
# but we do need the export views and metadata
exporters <- crGET(shojiURL(ds, "views", "export"))
var_meta <- variableMetadata(ds)

stop_capturing()

stabilize_json_files(
    temp_dir,
    list(
        "app.crunch.io/api/datasets/dup.json",
        list(list("body", "current_editor_name"), "User"),
        list(list("body", "owner_name"), "User"),
        list(list("body", "creation_time"), "2024-01-01T21:25:59.791000"),
        list(list("body", "modification_time"), "2024-01-01T21:26:43.038000"),
        list(list("body", "access_time"), "2024-01-01T21:26:43.038000"),
        list(
            # --- Only keep the palettes from the project folder so changes to crunch org
            # --- don't affect fixtures. Maybe it'd be better to ask for a rcrunch test
            # --- account, but this is okay for now
            list("body", "palette", "analysis"),
            function(x) {
                purrr::keep(x, ~.$name %in% c("Default green palette for fixture", "purple palette for fixture"))
            }
        ),
        list(list("urls", "owner_url"), "https://app.crunch.io/api/projects/pid/")
    )
)

dir_delete(here("mocks/app.crunch.io/api/datasets/dup/"))
file_copy(
    path(temp_dir, "app.crunch.io/api/datasets/dup.json"),
    here("mocks/app.crunch.io/api/datasets/dup.json"),
    overwrite = TRUE
)
dir_copy(
    path(temp_dir, "app.crunch.io/api/datasets/dup/"),
    here("mocks/app.crunch.io/api/datasets/dup/"),
    overwrite = TRUE
)


write.csv(
    ds,
    here("mocks", "dataset-fixtures", "dup.csv"),
    categorical = "id",
    include.hidden = TRUE,
    missing_values = ""#,
    # header_field = "qualified_alias" # This will only work after #188045851 ships
)

# Mock what header_field="qualified_alias" will look like after #188045851 ships
lines <- readLines(here("mocks", "dataset-fixtures", "dup.csv"))
lines[1] <- "x1,x2,y1,y2,z,x[x1],x[x2_derived],y[y1],y[z]"
writeLines(lines, here("mocks", "dataset-fixtures", "dup.csv"))


with_consent(delete(ds))
