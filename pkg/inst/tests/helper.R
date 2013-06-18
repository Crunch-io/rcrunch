run.only.local.tests <- FALSE

## .onAttach stuff, for testthat to work right
options(crunch.api.endpoint="http://localhost:8080/api/", 
        warn=1, 
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
assign("application/json", parseJSONresponse, envir=httr:::parsers)

## Dataset fixture
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
                 v4=as.factor(LETTERS[2:3]), stringsAsFactors=FALSE)

## Values
setGeneric("mockValues", function (x, n, ...) standardGeneric("mockValues"))
setMethod("mockValues", "NumericVariable", function (x, n, ...) rnorm(n, ...))
setMethod("mockValues", "TextVariable", function (x, n, ...) {
    sample(letters, n, replace=TRUE, ...)
})
setMethod("mockValues", "CategoricalVariable", function (x, n, ...) {
    sample(ids(categories(x)), n, replace=TRUE, ...)
})
vals <- lapply(vars2, mockValues, n=25)
vars2 <- mapply(function (var, val) {
    var@urls$values_url <- val
    return(var)
}, var=vars2, val=vals)
