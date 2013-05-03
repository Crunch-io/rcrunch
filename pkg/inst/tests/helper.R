run.only.local.tests <- TRUE

## .onAttach stuff, for testthat to work right
options(crunch.api.endpoint="http://localhost:8080/api/", 
        warn=1, 
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
assign("application/json", parseJSONresponse, envir=httr:::parsers)
test.user <- "***REMOVED***"
test.pw <- "***REMOVED***"

## Dataset fixture
ds <- fromJSON(system.file("dataset.json", package="rcrunch", 
    mustWork=TRUE), simplifyWithNames=FALSE)
class(ds) <- "shoji"

## Variables
vars <- fromJSON(system.file("variables.json", package="rcrunch",
    mustWork=TRUE), simplifyWithNames=FALSE)
names(vars) <- selectFrom("alias", vars)
vars <- lapply(vars, function (x) structure(list(body=x), class="shoji"))

vars2 <- lapply(vars, as.variable)

## Summaries
sums <- fromJSON(system.file("summaries.json", package="rcrunch",
    mustWork=TRUE), simplifyWithNames=FALSE)
sums <- lapply(sums, function (x) structure(list(body=x), class="shoji"))

df <- data.frame(v1=rnorm(20), v2=letters[1:20], v3=8:27)