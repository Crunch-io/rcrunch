run.integration.tests <- Sys.getenv("INTEGRATION") == "TRUE"
Sys.setlocale("LC_COLLATE", "C") ## What CRAN does

skip_on_jenkins <- function (...) {
    if (nchar(Sys.getenv("JENKINS_HOME"))) {
        skip(...)
    }
}

skip_locally <- function (...) {
    if (startsWith(getOption("crunch.api"), "http://local")) {
        skip(...)
    }
}

set.seed(666)

fromJSON <- jsonlite::fromJSON
loadLogfile <- httpcache::loadLogfile
cacheLogSummary <- httpcache::cacheLogSummary
requestLogSummary <- httpcache::requestLogSummary
uncached <- httpcache::uncached
newDataset <- function (...) suppressMessages(crunch::newDataset(...))

envOrOption <- function (opt) {
    ## .Rprofile options are like "test.api", while env vars are "R_TEST_API"
    envvar.name <- paste0("R_", toupper(gsub(".", "_", opt, fixed=TRUE)))
    envvar <- Sys.getenv(envvar.name)
    if (nchar(envvar)) {
        ## Let environment variable override .Rprofile, if defined
        return(envvar)
    } else {
        return(getOption(opt))
    }
}

## .onAttach stuff, for testthat to work right
options(
    crunch.api=envOrOption("test.api"),
    warn=1,
    crunch.debug=FALSE,
    digits.secs=3,
    crunch.timeout=15, ## In case an import fails to start, don't wait forever
    # httpcache.log="",
    crunch.require.confirmation=TRUE,
    crunch.namekey.dataset="alias",
    crunch.namekey.array="alias",
    crunch.email=envOrOption("test.user"),
    crunch.pw=envOrOption("test.pw")
)
set_config(crunchConfig())

## Test serialize and deserialize
cereal <- function (x) fromJSON(toJSON(x), simplifyVector=FALSE)

newDatasetFromFixture <- function (filename) {
    ## Grab csv and json from "dataset-fixtures" and make a dataset
    m <- fromJSON(file.path("dataset-fixtures", paste0(filename, ".json")),
        simplifyVector=FALSE)
    return(suppressMessages(createWithMetadataAndFile(m,
        file.path("dataset-fixtures", paste0(filename, ".csv")))))
}

releaseAndReload <- function (dataset) {
    .releaseDataset(dataset)
    return(refresh(dataset))
}

## Data frames to make datasets with
df <- data.frame(v1=c(rep(NA_real_, 5), rnorm(15)),
                 v2=c(letters[1:15], rep(NA_character_, 5)),
                 v3=8:27,
                 v4=as.factor(LETTERS[2:3]),
                 v5=as.Date(0:19, origin="1955-11-05"),
                 v6=TRUE,
                 stringsAsFactors=FALSE)

mrdf <- data.frame(mr_1=c(1,0,1,NA_real_),
                   mr_2=c(0,0,1,NA_real_),
                   mr_3=c(0,0,1,NA_real_),
                   v4=as.factor(LETTERS[2:3]),
                   stringsAsFactors=FALSE)

testfile.csv <- "fake.csv"
testfile.df <- read.csv(testfile.csv)

mrdf.setup <- function (dataset, pattern="mr_", name=ifelse(is.null(selections),
                        "CA", "MR"), selections=NULL) {
    cast.these <- grep(pattern, names(dataset))
    dataset[cast.these] <- lapply(dataset[cast.these],
        castVariable, "categorical")
    if (is.null(selections)) {
        dataset[[name]] <- makeArray(dataset[cast.these], name=name)
    } else {
        dataset[[name]] <- makeMR(dataset[cast.these], name=name,
            selections=selections)
    }
    return(dataset)
}

## Global teardown
bye <- new.env()
with_test_authentication({
    datasets.start <- urls(datasets())
    users.start <- urls(getUserCatalog())
    projects.start <- urls(session()$projects)
})
reg.finalizer(bye,
    function (x) {
        with_test_authentication({
            datasets.end <- urls(datasets())
            leftovers <- setdiff(datasets.end, datasets.start)
            if (length(leftovers)) {
                stop(length(leftovers),
                    " dataset(s) created and not destroyed: ",
                    serialPaste(dQuote(names(datasets()[leftovers]))),
                    call.=FALSE)
            }
            users.end <- urls(getUserCatalog())
            leftovers <- setdiff(users.end, users.start)
            if (length(leftovers)) {
                stop(length(leftovers),
                    " users(s) created and not destroyed: ",
                    serialPaste(dQuote(names(getUserCatalog()[leftovers]))),
                    call.=FALSE)
            }
            projects.end <- urls(session()$projects)
            leftovers <- setdiff(projects.end, projects.start)
            if (length(leftovers)) {
                stop(length(leftovers),
                    " projects(s) created and not destroyed: ",
                    serialPaste(dQuote(names(session()$projects[leftovers]))),
                    call.=FALSE)
            }
        })
    },
    onexit=TRUE)
