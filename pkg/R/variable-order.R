init.VariableOrder <- function (.Object, ...) {
    .Object@.Data <- lapply(list(...), function (x) {
        if (inherits(x, "VariableGroup")) return(x)
        do.call(VariableGroup, x)
    })
    return(.Object)
}
setMethod("initialize", "VariableOrder", init.VariableOrder)

init.VariableGroup <- function (.Object, group, entities, ...) {
    dots <- list(...)
    if ("variables" %in% names(dots)) entities <- dots$variables
    if (is.list(entities)) {
        entities <- vapply(entities, function (x) self(x), character(1), USE.NAMES=FALSE)
    } else if (is.dataset(entities)) {
        entities <- names(entities@variables@index)
    } else if (is.variable(entities)) {
        entities <- self(entities)
    }
    if ("name" %in% names(dots)) group <- dots$name
    .Object@group <- group
    .Object@entities <- entities
    return(.Object)
}
setMethod("initialize", "VariableGroup", init.VariableGroup)

##' @export
setMethod("entities", "VariableGroup", function (x) x@entities)
setMethod("entities", "VariableOrder", function (x) {
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

setMethod("names", "VariableOrder", 
    function (x) vapply(x, function (a) name(a), character(1)))
setMethod("names<-", "VariableOrder", 
    function (x, value) {
        x@.Data <- mapply(
            function (y, v) {
                y@group <- v
                return(y)
            }, y=x, v=value, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        return(x)
    })
setMethod("toJSON", "VariableOrder", function (x, ...) toJSON(x@.Data, ...))
    ## need that toJSON method so that names don't get assigned bc of the names() method
setMethod("toJSON", "VariableGroup", function (x, ...) {
    ents <- x@entities
    if (length(ents) == 0) {
        ## toJSON(character(0)) is [""], which is length 1 :(
        ents <- list() ## but toJSON(list()) is []
    }
    toJSON(list(group=x@group, entities=I(ents)))
})

##' @export
setMethod("[", c("VariableOrder", "ANY"), function (x, i, ..., drop=FALSE) {
    x@.Data <- x@.Data[i]
    return(x)
})
##' @export
setMethod("[", c("VariableOrder", "character"), function (x, i, ..., drop=FALSE) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        stop("Undefined groups selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})

##' @export
printVariableOrder <- function (x) {
    ## VariableOrder should get a proper show method
    if (is.dataset(x)) {
        return(printVariableOrder(variables(x)))
    }
    stopifnot(inherits(x, "VariableCatalog"))
    invisible(lapply(x@order, printVariableGroup, index=x@index))
}

printVariableGroup <- function (group, index) {
    cat(name(group), "\n")
    print(vapply(index[entities(group)], function (x) x[["name"]] %||% "(Hidden variable)", character(1), USE.NAMES=FALSE))
    invisible()
}

##' Get un(grouped) VariableGroups
##'
##' "ungrouped" is a magic VariableGroup that contains all variables not found
##' in any of the other groups. Regardless of whether you create it, the server
##' will return it. You can modify the order of the variables within it, but 
##' which variables are present in it is determined server-side.
##' @param var.order an object of class VariableOrder
##' @return For grouped(), a VariableOrder with "ungrouped" omitted. For
##' ungrouped(), a VariableGroup.
##' @export
grouped <- function (var.order) {
    var.order[names(var.order) != "ungrouped"]
}

##' @rdname grouped
##' @export
ungrouped <- function (var.order) {
    ind <- match("ungrouped", names(var.order))
    if (is.na(ind)) {
        return(VariableGroup(name="ungrouped", entities=character(0)))
    } else {
        return(var.order[[ind]])
    }
}