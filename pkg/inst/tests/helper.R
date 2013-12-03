run.only.local.tests <- FALSE

## .onAttach stuff, for testthat to work right
options(crunch.api.endpoint=getOption("test.api"), 
        warn=1,
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
assign("application/json", parseJSONresponse, envir=httr:::parsers)

## for the test running...S.....
skip <- function (...) cat(colourise("S", "yellow"))

#####################
## Test decorators ##
#####################
setup.and.teardown <- function (setup, teardown) {
    structure(list(setup=setup, teardown=teardown), class="SUTD")
}

##' @S3method with SUTD
with.SUTD <- function (data, expr, ...) {
    env <- parent.frame()
    on.exit(data$teardown())
    env$.setup <- data$setup() ## rm this after running?
    eval(substitute(expr), envir=parent.frame())
}

## note that this works because testthat evals within package namespace
addFakeHTTPVerbs <- function () {
    http_verbs$GET <- function (url, ...) url
    http_verbs$PUT <- function (...) crunchAPI("PUT", ...)
    http_verbs$POST <- function (...) crunchAPI("POST", ...)
}

## Mock backend
fake.HTTP <- setup.and.teardown(addFakeHTTPVerbs, addRealHTTPVerbs)

## Auth setup-teardown
test.authentication <- setup.and.teardown(
    function () suppressMessages(login()), 
    logout)

## Create a test dataset and then destroy it after tests
preexisting_datasets <- NULL
new.dataset.with.setup <- function (...) {
    if (is.null(preexisting_datasets)) {
        p <- GET(sessionURL("datasets_url"))$entities
        if (isTRUE(getOption('crunch.debug'))) message("Setting ", length(p), " preexisting datasets")
        preexisting_datasets <<- p
    }
    return(newDataset(...))
}

purge <- function () {
    if (isTRUE(getOption('crunch.debug'))) message("Reading ", length(preexisting_datasets), " preexisting datasets")
    for (i in setdiff(GET(sessionURL("datasets_url"))$entities, preexisting_datasets)) {
        try(DELETE(i))
    }
}

test.dataset <- function (df, ...) {
    return(setup.and.teardown(
        function () new.dataset.with.setup(df, ...),
        purge
    ))
}

###################
## Mock fixtures ##
###################

## Datasets
ds <- loadJSONMocks("dataset.json")
class(ds) <- "shoji"

## Variables
vars <- loadJSONMocks("variables.json")
names(vars) <- selectFrom("alias", vars)
vars <- lapply(vars, function (x) structure(list(body=x), class="shoji"))

vars2 <- lapply(vars, as.variable)

## Summaries
sums <- loadJSONMocks("summaries.json")
sums <- lapply(sums, function (x) structure(list(body=x), class="shoji"))

df <- data.frame(v1=c(rep(NA_real_, 5), rnorm(15)), 
                 v2=c(letters[1:15], rep(NA_character_, 5)), 
                 v3=8:27,
                 v4=as.factor(LETTERS[2:3]),
                 v5=as.Date(1:20, origin="1955-11-05"),
                 stringsAsFactors=FALSE)

mrdf <- data.frame(mr_1=c(1,0,1,NA_real_),
                   mr_2=c(0,0,1,NA_real_),
                   mr_3=c(0,0,1,NA_real_),
                   v4=as.factor(LETTERS[2:3]),
                   stringsAsFactors=FALSE)

## Values
setGeneric("mockValues", function (x, n, ...) standardGeneric("mockValues"))
setMethod("mockValues", "NumericVariable", function (x, n, ...) rnorm(n, ...))
setMethod("mockValues", "TextVariable", function (x, n, ...) {
    sample(letters, n, replace=TRUE, ...)
})
setMethod("mockValues", "CategoricalVariable", function (x, n, ...) {
    sample(names(categories(x)), n, replace=TRUE, ...)
})
setMethod("mockValues", "DatetimeVariable", function (x, n, ...) {
    as.character(sample(as.Date(1:100, origin="1955-11-05"), n, replace=TRUE, ...))
})
setMethod("mockValues", "CrunchVariable", function (x, n, ...) {
    sample(letters, n, replace=TRUE, ...)
})

vals <- lapply(vars2, mockValues, n=25)
vars2 <- mapply(function (var, val) {
    var@urls$values_url <- val
    return(var)
}, var=vars2, val=vals)
