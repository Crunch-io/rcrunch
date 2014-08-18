## "Ops" for Crunch Variables
##
## Most of the indirection here is to programatically create the Ops methods
## for the right combinations of multiple-dispatch signatures

math.exp <- function (e1, e2, operator) {
    ## Generic function that creates ZCL of `e1 %operator% e2`
    ex <- zfunc(operator, e1, e2)
    ds.url <- unique(unlist(lapply(list(e1, e2), datasetReference))) %||% ""
    CrunchExpression(expression=ex, dataset_url=ds.url)
}

vxr <- function (i) {
    ## Create math.exp of Variable x R.object
    force(i)
    return(function (e1, e2) math.exp(e1, typeof(e2, e1), i))
}

rxv <- function (i) {
    ## Create math.exp of R.object x Variable
    force(i)
    return(function (e1, e2) math.exp(typeof(e1, e2), e2, i))
}

vxv <- function (i) {
    ## Create math.exp of two non-R.objects
    force(i)
    return(function (e1, e2) math.exp(e1, e2, i))
}

.sigs <- list(
    c("TextVariable", "character"),
    c("NumericVariable", "numeric"),
    c("DatetimeVariable", "Date"),
    c("DatetimeVariable", "POSIXt"),
    c("CategoricalVariable", "numeric")
)

.rtypes <- unique(vapply(.sigs, function (a) a[[2]], character(1)))
.nomath <- which(!vapply(.sigs, 
    function (a) a[[1]] %in% c("TextVariable", "CategoricalVariable"),
    logical(1)))

for (i in c("+", "-", "*", "/", "<", ">", ">=", "<=")) {
    for (j in .nomath) {
        setMethod(i, .sigs[[j]], vxr(i))
        setMethod(i, rev(.sigs[[j]]), rxv(i))
    }
    for (j in setdiff(.rtypes, "character")) {
        setMethod(i, c("CrunchExpression", j), vxv(i)) # no typeof?
        setMethod(i, c(j, "CrunchExpression"), vxv(i)) # no typeof?
    }    
    setMethod(i, c("CrunchVariable", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchExpression", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchVariable", "CrunchExpression"), vxv(i))
}

.catmeth <- function (i, Rarg=1) {
    force(i)
    force(Rarg)
    return(function (e1, e2) {
        if (Rarg == 1) {
            e1 <- typeof(n2i(as.character(e1), categories(e2)), e2)
        } else {
            e2 <- typeof(n2i(as.character(e2), categories(e1)), e1)
        }
        return(math.exp(e1, e2, i))
    })
}

for (i in c("==", "!=")) {
    for (j in seq_along(.sigs)) {
        setMethod(i, .sigs[[j]], vxr(i))
        setMethod(i, rev(.sigs[[j]]), rxv(i))
    }
    setMethod(i, c("CategoricalVariable", "character"), .catmeth(i, 2))
    setMethod(i, c("CategoricalVariable", "factor"), .catmeth(i, 2))
    setMethod(i, c("character", "CategoricalVariable"), .catmeth(i, 1))
    setMethod(i, c("factor", "CategoricalVariable"), .catmeth(i, i))
    for (j in .rtypes) {
        setMethod(i, c("CrunchExpression", j), vxv(i)) # no typeof?
        setMethod(i, c(j, "CrunchExpression"), vxv(i)) # no typeof?
    }
    setMethod(i, c("CrunchVariable", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchExpression", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchVariable", "CrunchExpression"), vxv(i))
}

setMethod("&", c("CrunchExpression", "CrunchExpression"), vxv("and"))
setMethod("|", c("CrunchExpression", "CrunchExpression"), vxv("or"))
setMethod("!", c("CrunchExpression"), 
    function (x) {
        CrunchExpression(expression=zfunc("not", x),
            dataset_url=datasetReference(x))
    })


.inCrunch <- function (x, table) math.exp(x, typeof(table, x), "contains")

##' @export
setMethod("%in%", c("CategoricalVariable", "character"), 
    function (x, table) .inCrunch(x, n2i(table, categories(x))))
setMethod("%in%", c("CategoricalVariable", "factor"), 
    function (x, table) x %in% as.character(table))
for (i in seq_along(.sigs)) {
    setMethod("%in%", .sigs[[i]], .inCrunch)
}

setMethod("datasetReference", "CrunchExpression", function (x) x@dataset_url)

##' @export
setMethod("as.vector", "CrunchExpression", function (x, mode) {
    payload <- list(command="select", variables=list(out=zcl(x)))
    if (length(x@filter)) {
        payload[["filter"]] <- x@filter
    }
    out <- POST(paste0(x@dataset_url, "table/"), body=toJSON(payload))
    ## pass in the variable metadata to the column parser
    variable <- as.variable(structure(list(body=out$metadata$out),
        class="shoji"))
    return(columnParser(out$metadata$out$type)(out$data$out, variable))
})

setMethod("is.na", "CrunchVariable", function (x) {
    CrunchExpression(expression=zfunc("is_missing", x),
        dataset_url=datasetReference(x))
})
