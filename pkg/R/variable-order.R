init.VariableGrouping <- function (.Object, ...) {
    .Object@.Data <- lapply(list(...), function (x) {
        if (inherits(x, "VariableGroup")) return(x)
        do.call(VariableGroup, x)
    })
    return(.Object)
}
setMethod("initialize", "VariableGrouping", init.VariableGrouping)

init.VariableGroup <- function (.Object, group, entities, ...) {
    if (is.list(entities)) {
        entities <- vapply(entities, function (x) self(x), character(1), USE.NAMES=FALSE)
    }
    dots <- list(...)
    if ("name" %in% names(dots)) group <- dots$name
    .Object@group <- group
    .Object@entities <- entities
    return(.Object)
}
setMethod("initialize", "VariableGroup", init.VariableGroup)

setMethod("entities", "VariableGroup", function (x) x@entities)
setMethod("entities", "VariableGrouping", function (x) {
    ## To get a flattened view
    es <- lapply(x, function (a) entities(a))
    return(unique(unlist(es)))
})
setMethod("entities<-", "VariableGroup", function (x, value) {
    if (is.list(value)) {
        value <- vapply(value, function (x) self(x), character(1),
            USE.NAMES=FALSE)
    }
    x@entities <- value
    return(x)
})

setMethod("name", "VariableGroup", function (x) x@group)
setMethod("name<-", "VariableGroup", function (x, value) {
    x@group <- value ## Should check that we're not renaming "ungrouped"
    return(x)
})

setMethod("names", "VariableGrouping", 
    function (x) vapply(x, function (a) name(a), character(1)))
setMethod("names<-", "VariableGrouping", 
    function (x, value) {
        x@.Data <- mapply(
            function (y, v) {
                y@group <- v
                return(y)
            }, y=x, v=value, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        return(x)
    })
setMethod("toJSON", "VariableGrouping", function (x, ...) toJSON(x@.Data, ...))
    ## need that toJSON method so that names don't get assigned bc of the names() method
setMethod("toJSON", "VariableGroup", function (x, ...) {
    toJSON(list(group=x@group, entities=I(x@entities)))
})

getVariableOrderURL <- function (dataset) {
    u <- dataset@urls$variables_url
    catalog <- GET(u)
    return(catalog$views$hierarchical_order)
}

getVariableOrder <- function (dataset) {
    if (is.null(dataset@urls$order_url)) {
        ## Something of a hack since we don't have caching.
        dataset@urls$order_url <- getVariableOrderURL(dataset)
    }
    return(do.call(VariableGrouping, GET(dataset@urls$order_url)$groups))
}

setVariableOrder <- function (x, value) {
    if (is.null(x@urls$order_url)) {
        ## Something of a hack since we don't have caching.
        x@urls$order_url <- getVariableOrderURL(x)
    }
    PUT(x@urls$order_url, body=toJSON(list(groups=value))) ## not yet supported...
    invisible(x)
}

# getDatasetVariables <- function (x) {
#     u <- x@urls$variables_url
#     catalog <- GET(u)
#     varIndex <- catalog$index
#     varOrder <- VariableGrouping(GET(catalog$views$hierarchical_order)$groups)
#     varIndex <- varIndex[entities(varOrder)]
#     return(list(variables=varIndex, order=varOrder))
# }