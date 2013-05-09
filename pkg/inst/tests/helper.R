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

df <- data.frame(v1=rnorm(20), v2=letters[1:20], v3=8:27,
                 v4=as.factor(LETTERS[2:3]), stringsAsFactors=FALSE)