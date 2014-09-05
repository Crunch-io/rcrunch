init.VariableOrder <- function (.Object, ...) {
    .Object@.Data <- lapply(list(...), function (x) {
        if (inherits(x, "VariableGroup")) return(x)
        do.call(VariableGroup, x)
    })
    return(.Object)
}
setMethod("initialize", "VariableOrder", init.VariableOrder)

.initEntities <- function (x) {
    if (is.list(x)) {
        raw.groups <- vapply(x, 
            function (a) {
                is.list(a) && setequal(c("group", "entities"), names(a))
            }, logical(1))
        x[raw.groups] <- lapply(x[raw.groups], 
            function (a) do.call(VariableGroup, a))
        nested.groups <- vapply(x, 
            function (a) inherits(a, "VariableGroup"), logical(1))
        if (any(nested.groups)) {
            x[!nested.groups] <- lapply(x[!nested.groups], .initEntities)
        } else {
            x <- vapply(x, .initEntities, character(1), USE.NAMES=FALSE)
        }
    } else if (is.dataset(x)) {
        x <- names(x@variables@index)
    } else if (is.variable(x)) {
        x <- self(x)
    } else if (!(is.character(x) || inherits(x, "VariableGroup"))) {
        halt("")
    }
    return(x)
}

init.VariableGroup <- function (.Object, group, entities, ...) {
    dots <- list(...)
    if ("variables" %in% names(dots)) entities <- dots$variables
    if ("name" %in% names(dots)) group <- dots$name
    .Object@group <- group
    .Object@entities <- .initEntities(entities)
    return(.Object)
}
setMethod("initialize", "VariableGroup", init.VariableGroup)

setMethod("toJSON", "VariableOrder", function (x, ...) toJSON(x@.Data, ...))
    ## need that toJSON method so that names don't get assigned bc of the names() method

.jsonprep.vargroup <- function (x) {
    ents <- x@entities
    if (length(ents) == 0) {
        ## toJSON(character(0)) is [""], which is length 1 :(
        ents <- list() ## but toJSON(list()) is []
    } else if (is.list(ents)) {
        nested.groups <- vapply(ents, inherits, logical(1),
            what="VariableGroup")
        ents[nested.groups] <- lapply(ents[nested.groups], .jsonprep.vargroup)
    }
    return(list(group=x@group, entities=I(ents)))
}
setMethod("toJSON", "VariableGroup", function (x, ...) {
    toJSON(.jsonprep.vargroup(x), ...)
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
        halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
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
    ## extend this to display nested groups
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
##' @seealso VariableOrder
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