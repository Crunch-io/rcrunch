run.integration.tests <- Sys.getenv("INTEGRATION") == "TRUE"
Sys.setlocale("LC_COLLATE", "C") ## What CRAN does

skip_on_jenkins <- function (...) {
    if (nchar(Sys.getenv("JENKINS_HOME"))) {
        skip(...)
    }
}

set.seed(666)

cacheOn()
# startLog ("~/c/rcrunch/test.log")

fromJSON <- jsonlite::fromJSON

## .onAttach stuff, for testthat to work right
options(crunch.api=getOption("test.api"), 
        warn=1,
        crunch.debug=TRUE,
        digits.secs=3,
        crunch.timeout=15,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
set_config(crunchConfig())

print(sessionInfo())

## Test serialize and deserialize
cereal <- function (x) fromJSON(toJSON(x), simplifyVector=FALSE)

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
    tryCatch(eval(substitute(expr), envir=parent.frame()),
        error=function (e) {
            expect_that(stop(e$message), does_not_throw_error())
        })
}

## note that this works because testthat evals within package namespace
addFakeHTTPVerbs <- function () {
    http_verbs$GET <- function (url, ...) {
        if (is.null(url)) {
            stop("No URL found", call.=FALSE)
        }
        url <- unlist(strsplit(url, "?", fixed=TRUE))[1] ## remove query params
        url <- sub("\\/$", ".json", url)
        url <- sub("^\\/", "", url) ## relative to cwd
        out <- handleShoji(fromJSON(url, simplifyVector=FALSE))
        return(out)
    }
    http_verbs$PUT <- function (url, body, ...) {
        stop("PUT ", url, " ", body, call.=FALSE)
    }
    http_verbs$PATCH <- function (url, body, ...) {
        stop("PATCH ", url, " ", body, call.=FALSE)
    }
    http_verbs$POST <- function (url, body, ...) {
        stop("POST ", url, " ", body, call.=FALSE)
    }
    http_verbs$DELETE <- function (...) function (url, ...) {
        stop("DELETE ", url, call.=FALSE)
    }
    session_store$root <- getAPIroot("/api/root.json")
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
    if (is.dataset(df)) {
        ## Passing a dataset already made in, just to ensure its cleanup
        ## Just return it
        out <- df
    } else if (is.null(df)) {
        out <- createDataset(name=unique.name, ...)
    } else {
        out <- suppressMessages(newDataset(df, name=unique.name, ...))
    }
    datasets_to_purge <<- c(datasets_to_purge, self(out))
    return(out)
}

purge.dataset <- function () {
    len <- length(datasets_to_purge)
    if (len) {
        try(crDELETE(datasets_to_purge[len]), silent=TRUE)
        datasets_to_purge <<- datasets_to_purge[-len]
    }
}

test.dataset <- function (df=NULL, obj.name="ds", ...) {
    return(setup.and.teardown(
        function () new.dataset.with.setup(df, ...),
        purge.dataset,
        obj.name
    ))
}

uniqueEmail <- function () paste0("test+", as.numeric(Sys.time()), "@crunch.io")
users_to_purge <- c()
new.user.with.setup <- function (email=uniqueEmail(), name=email, ...) {
    u.url <- invite(email, name=name, notify=FALSE, ...)
    users_to_purge <<- c(users_to_purge, u.url)
    return(u.url)
}

purge.user <- function () {
    len <- length(users_to_purge)
    if (len) {
        u.url <- users_to_purge[len]
        try(crDELETE(u.url))
        users_to_purge <<- users_to_purge[-len]
    }
}

test.user <- function (email=uniqueEmail(), name=email, obj.name="u", ...) {
    return(setup.and.teardown(
        function () new.user.with.setup(email, name, ...),
        purge.user,
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

does_not_show_message <- function () {
    function (expr) {
        warnings <- evaluate_promise(expr)$messages
        expectation(length(messages) == 0, 
                paste0(length(messages), " messages created"),
                "no messages shown")
    }
}

does_not_throw_error <- function () {
    function (expr) {
        res <- try(force(expr), TRUE)
        error <- inherits(res, "try-error")
        failure.msg <- "threw an error"
        if (error) {
            ## Append the error message
            failure.msg <- paste0(failure.msg, ": ", 
                attr(res, "condition")$message)
        }
        expectation(!error, failure.msg, "no error thrown")
    }
}

is_not_an_error <- function () {
    ## Like does_not_throw_error, but for an error already caught
    function (expr) {
        expectation(!is.error(expr), 
            paste("is an error:", attr(expr, "condition")$message), 
            "no error thrown")
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

validImport <- function (ds) {
    ## Pull out common tests that "df" was imported correctly
    expect_true(is.dataset(ds))
    expect_identical(names(df), names(ds))
    expect_identical(dim(ds), dim(df))
    expect_true(is.Numeric(ds[["v1"]]))
    expect_true(is.Text(ds[["v2"]]))
    expect_identical(name(ds$v2), "v2")
    expect_true(is.Numeric(ds[["v3"]]))
    expect_identical(description(ds$v3), "")
    expect_equivalent(as.array(crtabs(mean(v3) ~ v4, data=ds)),
        tapply(df$v3, df$v4, mean, na.rm=TRUE))
    expect_true(is.Categorical(ds[["v4"]]))
    expect_equivalent(as.array(crtabs(~ v4, data=ds)), 
        array(c(10, 10), dim=2L, dimnames=list(v4=c("B", "C"))))
    expect_true(all(levels(df$v4) %in% names(categories(ds$v4))))
    expect_identical(categories(ds$v4), categories(refresh(ds$v4)))
    expect_identical(ds$v4, refresh(ds$v4))
    expect_true(is.Datetime(ds$v5))
    expect_true(is.Categorical(ds$v6))
    expect_identical(showVariableOrder(ordering(ds)), names(variables(ds)))
}

## Global teardown proof of concept
# bye <- new.env()
# reg.finalizer(bye, function (x) print("Cleaning..."), 
#     onexit=TRUE)
