run.integration.tests <- Sys.getenv("LOCALONLY") == "FALSE"
set.seed(666)

## .onAttach stuff, for testthat to work right
options(crunch.api=getOption("test.api"), 
        warn=1,
        crunch.debug=FALSE,
        digits.secs=3,
        crunch.timeout=15,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))

is.tap.reporter <- grepl('reporter ?= ?"tap"', 
    paste(deparse(sys.calls()[[1]]), collapse=""))
if (is.tap.reporter) {
    skip <- function (..., reason="") invisible() # cat("skip ", reason, "\n")
} else {
    ## for the test running...S.....
    skip <- function (...) cat(colourise("S", "yellow"))
}

#####################
## Test decorators ##
#####################
setup.and.teardown <- function (setup, teardown, obj.name=".setup") {
    structure(list(setup=setup, teardown=teardown, obj.name=obj.name),
        class="SUTD")
}

with.SUTD <- function (data, expr, ...) {
    env <- parent.frame()
    on.exit(data$teardown())
    assign(data$obj.name, data$setup(), envir=env) ## rm this after running?
    try(eval(substitute(expr), envir=parent.frame()))
}

## note that this works because testthat evals within package namespace
addFakeHTTPVerbs <- function () {
    http_verbs$GET <- function (url, ...) {
        handleShoji(fromJSON(url, simplifyWithNames=FALSE))
    }
    http_verbs$PUT <- function (...) invisible()
    http_verbs$PATCH <- function (...) invisible()
    http_verbs$POST <- function (...) invisible()
    session_store$urls <- list(datasets_url="api/datasets.json")
    session_store$cookie <- 12345 ## so it thinks we're authenticated
    try(updateDatasetList())
}

## Mock backend
fake.HTTP <- setup.and.teardown(addFakeHTTPVerbs, addRealHTTPVerbs)

timingTracer <- function (filename=tempfile(), append=FALSE) {
    return(function () {
        suppressMessages(trace("crunchAPI", 
            exit=quote(cat(paste(c(http.verb, url, x$status_code,
                ifelse(is.null(x$headers$`content-length`), 
                NA, x$headers$`content-length`), 
                format(x$times, scientific=FALSE)),
                collapse="\t"), "\n")),
            print=FALSE, where=CrunchDataset))
        message("Writing HTTP timings to ", filename)
        sink(filename, append=append)
        if (!append) {
            cat(paste("method", "url", "status", "content_length", "redirect",
                "namelookup", "connect", "pretransfer", "starttransfer", "total",
                sep="\t"), "\n")
        }
    })
}

startTiming <- function (filename=tempfile(), append=FALSE) {
    timingTracer(filename, append)()
}

stopTiming <- function () {
    sink()
    suppressMessages(untrace("crunchAPI", where=CrunchDataset))
}

timed.HTTP <- function (filename=tempfile(), append=FALSE) {
    return(setup.and.teardown(timingTracer(filename, append), stopTiming))
}

silencer <- setup.and.teardown(function () {
    showerrs <- getOption("show.error.messages")
    options(show.error.messages=FALSE, show.err.msg.orig=showerrs)
}, function () {
    options(show.error.message=getOption("show.err.msg.orig"))
})

## Auth setup-teardown
test.authentication <- setup.and.teardown(
    function () suppressMessages(login()), 
    logout)

uniqueDatasetName <- now

## Create a test dataset and then destroy it after tests
datasets_to_purge <- c()
new.dataset.with.setup <- function (df=NULL, ...) {
    unique.name <- uniqueDatasetName()
    if (is.null(df)) {
        out <- createDataset(name=unique.name, ...)
    } else {
        out <- newDataset(df, name=unique.name, ...)
    }
    datasets_to_purge <<- c(datasets_to_purge, self(out))
    return(out)
}

purge <- function () {
    len <- length(datasets_to_purge)
    if (len) {
        try(DELETE(datasets_to_purge[len]), silent=TRUE)
        datasets_to_purge <<- datasets_to_purge[-len]
    }
}

test.dataset <- function (df=NULL, obj.name="ds", ...) {
    return(setup.and.teardown(
        function () new.dataset.with.setup(df, ...),
        purge,
        obj.name
    ))
}

## Expectations

does_not_give_warning <- function () {
    function (expr) {
        warnings <- evaluate_promise(expr)$warnings
        expectation(length(warnings) == 0, 
                paste0(length(warnings), " warnings created"),
                "no warnings given")
    }
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
        var <- makeArray(pattern=pattern, dataset=dataset, name=name)
    } else {
        var <- makeMR(pattern=pattern, dataset=dataset, name=name,
            selections=selections)
    }
    return(refresh(dataset))
}
