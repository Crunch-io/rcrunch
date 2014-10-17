init.VariableOrder <- function (.Object, ...) {
    vg <- function (x, u) {
        if (inherits(x, "VariableGroup")) return(x)
        do.call(VariableGroup, c(x, url.base=u))
    }
    .Object <- callNextMethod(.Object, ...)
    dots <- list(...)
    if (length(dots) && !is.shoji(dots[[1]])) {
        .Object@value$groups <- lapply(dots, vg, u=NULL)
    } else {
        .Object@value$groups <- lapply(.Object@value$groups, vg, u=.Object@self)
    }
    return(.Object)
}
setMethod("initialize", "VariableOrder", init.VariableOrder)

.initEntities <- function (x, url.base=NULL) {
    if (is.list(x)) {
        raw.groups <- vapply(x, 
            function (a) {
                is.list(a) && setequal(c("group", "entities"), names(a))
            }, logical(1))
        x[raw.groups] <- lapply(x[raw.groups], 
            function (a) do.call(VariableGroup, c(a, url.base=url.base)))
        nested.groups <- vapply(x, 
            function (a) inherits(a, "VariableGroup"), logical(1))
        if (any(nested.groups)) {
            x[!nested.groups] <- lapply(x[!nested.groups], .initEntities, url.base=url.base)
        } else {
            x <- vapply(x, .initEntities, character(1), USE.NAMES=FALSE, url.base=url.base)
        }
    } else if (is.dataset(x)) {
        x <- urls(allVariables(x))
    } else if (is.variable(x)) {
        x <- self(x)
    } else if (!(is.character(x) || inherits(x, "VariableGroup"))) {
        print(x)
        halt("")
    }
    if (!is.null(url.base)) x <- absolutizeURLs(x, url.base)
    return(x)
}

init.VariableGroup <- function (.Object, group, entities, url.base=NULL, ...) {
    dots <- list(...)
    if ("variables" %in% names(dots)) entities <- dots$variables
    if ("name" %in% names(dots)) group <- dots$name
    .Object@group <- group
    .Object@entities <- .initEntities(entities, url.base)
    return(.Object)
}
setMethod("initialize", "VariableGroup", init.VariableGroup)

setMethod("toJSON", "VariableOrder",
    function (x, ...) toJSON(x@value, ...))

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
as.list.VariableOrder <- function (x, ...) x@value$groups

##' @export
setMethod("length", "VariableOrder", function (x) length(x@value$groups))

##' @export
setMethod("[", c("VariableOrder", "ANY"), function (x, i, ..., drop=FALSE) {
    x@value$groups <- x@value$groups[i]
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

setMethod("[[", c("VariableOrder", "ANY"), function (x, i, ..., drop=FALSE) {
    x@value$groups[[i]]
})

setMethod("[<-", c("VariableOrder", "character", "missing", "VariableOrder"), 
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })
setMethod("[<-", c("VariableOrder", "ANY", "missing", "VariableOrder"), 
   function (x, i, j, value) {
       x@value$groups[i] <- value@value$groups
       return(x)
   })
setMethod("[[<-", c("VariableOrder", "character", "missing", "VariableGroup"), 
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined group selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })
setMethod("[[<-", c("VariableOrder", "ANY", "missing", "VariableGroup"), 
   function (x, i, j, value) {
       x@value$groups[[i]] <- value
       return(x)
   })

##' @export
setMethod("$", "VariableOrder", function (x, name) x[[name]])
##' @export
setMethod("$<-", "VariableOrder", function (x, name, value) {
    x[[name]] <- value
    return(x)
})

##' @export
printVariableOrder <- function (x) {
    ## VariableOrder should get a proper show method
    if (is.dataset(x)) {
        return(printVariableOrder(variables(x)))
    }
    stopifnot(inherits(x, "VariableCatalog"))
    invisible(lapply(x@order, printVariableGroup, index=index(x)))
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