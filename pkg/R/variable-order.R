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
    ## Sanitize the inputs in VariableGroup construction/updating
    ## Result should be a list, each element being either a URL (character) 
    ## or VariableGroup
    
    ## Valid inputs:
    ## 1) A dataset: take all urls
    ## 2) A character vector of urls
    ## 3) A list of
    ## a) variables: take self
    ## b) mixed character and VariableGroups
    ## c) mixed character and lists that should be VariableGroups (fromJSON)
    if (is.dataset(x)) {
        return(.initEntities(urls(allVariables(x)), url.base=url.base))
    }
    if (is.character(x)) {
        return(.initEntities(as.list(x), url.base=url.base))
    }
    if (is.list(x)) {
        ## Init raw (fromJSON) groups
        raw.groups <- vapply(x, 
            function (a) {
                is.list(a) && setequal(c("group", "entities"), names(a))
            }, logical(1))
        x[raw.groups] <- lapply(x[raw.groups], 
            function (a) do.call(VariableGroup, c(a, url.base=url.base)))
        ## Get self if any are Variables
        vars <- vapply(x, is.variable, logical(1))
        x[vars] <- lapply(x[vars], self)
        ## Now everything should be valid
        nested.groups <- vapply(x, 
            function (a) inherits(a, "VariableGroup"),
            logical(1))
        string.urls <- vapply(x, 
            function (a) is.character(a) && length(a) == 1,
            logical(1))
        stopifnot(all(string.urls | nested.groups))
        
        ## Absolutize if needed
        if (!is.null(url.base)) {
            x[string.urls] <- lapply(x[string.urls], absolutizeURLs,
                base=url.base)
        }
        return(x)
    } 
    halt(class(x), " is an invalid input for entities")
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

##' @rdname tojson-crunch
##' @export
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
##' @rdname tojson-crunch
##' @export
setMethod("toJSON", "VariableGroup", function (x, ...) {
    toJSON(.jsonprep.vargroup(x), ...)
})

##' @export
as.list.VariableOrder <- function (x, ...) x@value$groups

##' Length of VariableOrder
##' @param x a VariableOrder
##' @return Integer: the number of VariableGroups in the VariableOrder
##' @export
setMethod("length", "VariableOrder", function (x) length(x@value$groups))

##' Extract and update in VariableOrder and VariableGroup
##'
##' @param x a VariableOrder or VariableGroup
##' @param i an index. Numeric and logical indexing supported for both classes;
##' character indexing supported for VariableOrder, matching on VariableGroup
##' names
##' @param name Same as i but for \code{\$}
##' @param j Invalid
##' @param value For update methods, an object equivalent in class to what is
##' being updated
##' @param ... additional arguments
##' @param drop Ignored
##' @return \code{[[} and \code{\$} on a VariableOrder return the VariableGroup.
##' \code{[[} on VariableGroup returns the entity within, either a character
##' (URL) or nested VariableGroup. \code{[} and assignment methods return
##' objects of the same class as \code{x}
##' @rdname variable-order-extract
##' @aliases variable-order-extract
##' @export
setMethod("[", c("VariableOrder", "ANY"), function (x, i, ..., drop=FALSE) {
    x@value$groups <- x@value$groups[i]
    return(x)
})
##' @rdname variable-order-extract
##' @export
setMethod("[", c("VariableOrder", "character"), function (x, i, ..., drop=FALSE) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})

##' @rdname variable-order-extract
##' @export
setMethod("[[", c("VariableOrder", "ANY"), function (x, i, ...) {
    x@value$groups[[i]]
})

##' @rdname variable-order-extract
##' @export
setMethod("[[", c("VariableOrder", "character"), function (x, i, ...) {
    w <- match(i, names(x))
    callNextMethod(x, w, ..., drop=drop)
})

##' @rdname variable-order-extract
##' @export
setMethod("$", "VariableOrder", function (x, name) x[[name]])

##' @rdname variable-order-extract
##' @export
setMethod("[<-", c("VariableOrder", "character", "missing", "VariableOrder"), 
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })
##' @rdname variable-order-extract
##' @export
setMethod("[<-", c("VariableOrder", "ANY", "missing", "VariableOrder"), 
   function (x, i, j, value) {
       x@value$groups[i] <- value@value$groups
       return(x)
   })
##' @rdname variable-order-extract
##' @export
setMethod("[[<-", c("VariableOrder", "character", "missing", "VariableGroup"), 
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined group selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })
##' @rdname variable-order-extract
##' @export
setMethod("[[<-", c("VariableOrder", "ANY", "missing", "VariableGroup"), 
    function (x, i, j, value) {
        x@value$groups[[i]] <- value
        return(x)
    })
##' @rdname variable-order-extract
##' @export
setMethod("[[<-", c("VariableOrder", "ANY", "missing", "ANY"), 
    function (x, i, j, value) {
        halt("Cannot assign an object of class ", dQuote(class(value)), 
            " into a VariableOrder")
    })

##' @rdname variable-order-extract
##' @export
setMethod("[[<-", c("VariableOrder", "ANY", "missing", "NULL"), 
    function (x, i, j, value) {
        x@value$groups[[i]] <- value
        return(x)
    })
##' @rdname variable-order-extract
##' @export
setMethod("[[<-", c("VariableOrder", "character", "missing", "NULL"), 
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined group selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })

##' @rdname variable-order-extract
##' @export
setMethod("$", "VariableOrder", function (x, name) x[[name]])
##' @rdname variable-order-extract
##' @export
setMethod("$<-", "VariableOrder", function (x, name, value) {
    x[[name]] <- value
    return(x)
})

##' @rdname variable-order-extract
##' @export
setMethod("[[", "VariableGroup", function (x, i, ...) {
    entities(x)[[i]]
})
##' @rdname variable-order-extract
##' @export
setMethod("[[<-", c("VariableGroup", "ANY", "missing", "VariableGroup"), function (x, i, j, value) {
    entities(x)[[i]] <- value
    return(x)
})

printVariableOrder <- function (x) {
    ## VariableOrder should get a proper show method
    if (is.dataset(x)) {
        return(printVariableOrder(variables(x)))
    }
    stopifnot(inherits(x, "VariableCatalog"))
    out <- showVariableOrder(x@order, vars=index(x))
    cat(out, sep="\n")
    cat("\n")
    invisible(out)
}

printVariableGroup <- function (x, index) {
    if (inherits(x, "VariableGroup")) {
        ents <- entities(x)
        if (length(ents)) {
            group <- unlist(lapply(ents, printVariableGroup, index=index))
        } else {
            group <- "(Empty group)"
        }
        out <- c(paste0("[+] ", name(x)), paste0("    ", group))
    } else {
        tup <- index[[x]] %||% list()
        out <- tup[["name"]] %||% "(Hidden variable)"
    }
    return(out)
}

showVariableOrder <- function (x, vars=x@vars) {
    return(unlist(lapply(x, printVariableGroup, index=vars)))
}

##' @rdname show-crunch
##' @export
setMethod("show", "VariableOrder", function (object) {
    out <- showVariableOrder(object)
    cat(out, sep="\n")
    cat("\n")
    invisible(out)
})

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