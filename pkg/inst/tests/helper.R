options(crunch.api.endpoint="http://localhost:8080/api/", warn=1)
assign("application/json", parseJSONresponse, envir=httr:::parsers)
test.user <- "***REMOVED***"

## Dataset fixture
ds <- fromJSON(system.file("dataset.json", package="rcrunch", 
    mustWork=TRUE), simplifyWithNames=FALSE)
class(ds) <- "shoji"

## Variables
vars <- fromJSON(system.file("variables.json", package="rcrunch",
    mustWork=TRUE), simplifyWithNames=FALSE)
names(vars) <- selectFrom("alias", vars)
vars <- lapply(vars, function (x) structure(list(body=x), class="shoji"))

## Summaries
sums <- fromJSON(system.file("summaries.json", package="rcrunch",
    mustWork=TRUE), simplifyWithNames=FALSE)
sums <- lapply(sums, function (x) structure(list(body=x), class="shoji"))