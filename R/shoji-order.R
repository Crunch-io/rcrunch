.initEntities <- function (x, url.base=NULL, group.class="OrderGroup", entity.class="ShojiObject") {
    ## Sanitize the inputs in OrderGroup construction/updating
    ## Result should be a list, each element being either a URL (character)
    ## or OrderGroup

    ## Valid inputs:
    ## 1) A catalog: take all urls
    ## 2) A character vector of urls
    ## 3) A list of
    ## a) entities: take self
    ## b) mixed character and OrderGroups
    ## c) mixed character and lists that should be OrderGroups (from JSON)
    if (is.catalog(x)) {
        return(.initEntities(urls(x), url.base=url.base))
    }
    if (is.character(x)) {
        return(.initEntities(as.list(x), url.base=url.base))
    }
    if (is.list(x)) {
        ## Init raw (fromJSON) groups, which have lists inside of lists
        raw.groups <- vapply(x, is.list, logical(1))
        x[raw.groups] <- lapply(x[raw.groups],
            function (a) do.call(group.class,
                list(group=names(a), entities=a[[1]], url.base=url.base)))

        ## Get self if any are entities
        vars <- vapply(x, inherits, logical(1), what=entity.class)
        x[vars] <- lapply(x[vars], self)

        ## Now everything should be valid
        nested.groups <- vapply(x,
            function (a) inherits(a, group.class),
            logical(1))
        string.urls <- vapply(x,
            function (a) is.character(a) && length(a) == 1,
            logical(1))
        stopifnot(all(string.urls | nested.groups))

        ## Absolutize if needed
        if (!is.null(url.base)) {
            x[string.urls] <- lapply(x[string.urls], absoluteURL,
                base=url.base)
        }
        ## Make sure there are no names on the list--will throw off toJSON
        names(x) <- NULL
        return(x)
    }
    halt(class(x), " is an invalid input for entities")
}

##' @export
as.list.ShojiOrder <- function (x, ...) x@graph

##' @export
as.list.OrderGroup <- function (x, ...) x@entities

##' Length of an Order
##' @param x a ShojiOrder
##' @return Integer: the number of elements in the Order
##' @name ShojiOrder-length
NULL

##' @rdname ShojiOrder-length
##' @export
setMethod("length", "ShojiOrder", function (x) length(entities(x)))


##' Extract and update in VariableOrder and VariableGroup
##'
##' @param x a VariableOrder or VariableGroup
##' @param i an index. Numeric and logical indexing supported for both classes;
##' character indexing supported for VariableOrder, matching on VariableGroup
##' names
##' @param name Same as i but for \code{$}
##' @param j Invalid
##' @param value For update methods, an object equivalent in class to what is
##' being updated
##' @param ... additional arguments
##' @param drop Ignored
##' @return \code{[[} and \code{$} on a VariableOrder return the VariableGroup.
##' \code{[[} on VariableGroup returns the entity within, either a character
##' (URL) or nested VariableGroup. \code{[} and assignment methods return
##' objects of the same class as \code{x}
##' @name ShojiOrder-extract
##' @aliases ShojiOrder-extract
NULL

###############################
# 1. Extract from VariableOrder
###############################

##' @rdname ShojiOrder-extract
##' @export
setMethod("[", c("ShojiOrder", "ANY"), function (x, i, ..., drop=FALSE) {
    x@graph <- x@graph[i]
    return(x)
})
##' @rdname ShojiOrder-extract
##' @export
setMethod("[", c("ShojiOrder", "character"), function (x, i, ..., drop=FALSE) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[", c("ShojiOrder", "ANY"), function (x, i, ...) {
    x@graph[[i]]
})

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[", c("ShojiOrder", "character"), function (x, i, ...) {
    w <- match(i, names(x))
    callNextMethod(x, w, ..., drop=drop)
})

##' @rdname ShojiOrder-extract
##' @export
setMethod("$", "ShojiOrder", function (x, name) x[[name]])

###############################
# 2. Assign into VariableOrder
###############################

##' @rdname ShojiOrder-extract
##' @export
setMethod("[<-", c("ShojiOrder", "character", "missing", "ShojiOrder"),
    function (x, i, j, value) {
        stopifnot(class(x) == class(value)) ## So we don't cross subclasses
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })
##' @rdname ShojiOrder-extract
##' @export
setMethod("[<-", c("ShojiOrder", "ANY", "missing", "ShojiOrder"),
   function (x, i, j, value) {
       stopifnot(class(x) == class(value)) ## So we don't cross subclasses
       x@graph[i] <- value@graph
       ## Ensure duplicates setting persists
       duplicates(x) <- duplicates(x)
       return(x)
   })


##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("ShojiOrder", "character", "missing", "OrderGroup"),
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined group selected: ", serialPaste(i[is.na(w)]))
        }
        ## NextMethod: c("ShojiOrder", "ANY", "missing", "OrderGroup")
        callNextMethod(x, w, value=value)
    })

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("VariableOrder", "ANY", "missing", "ANY"),
    function (x, i, j, value) {
        ## These have to be on the subclasses, otherwise dispatch is screwy
        halt("Cannot assign an object of class ", dQuote(class(value)),
            " into a ShojiOrder")
    })

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("DatasetOrder", "ANY", "missing", "ANY"),
    function (x, i, j, value) {
        ## These have to be on the subclasses, otherwise dispatch is screwy
        halt("Cannot assign an object of class ", dQuote(class(value)),
            " into a ShojiOrder")
    })

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("ShojiOrder", "ANY", "missing", "NULL"),
    function (x, i, j, value) {
        x@graph[[i]] <- value
        return(x)
    })
##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("ShojiOrder", "character", "missing", "NULL"),
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined group selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })

##' @rdname ShojiOrder-extract
##' @export
setMethod("$<-", "ShojiOrder", function (x, name, value) {
    x[[name]] <- value
    return(x)
})


###############################
# 3. Extract from OrderGroup
###############################

##' @rdname ShojiOrder-extract
##' @export
setMethod("[", c("OrderGroup", "ANY"), function (x, i, ..., drop=FALSE) {
    x@entities <- x@entities[i]
    return(x)
})
##' @rdname ShojiOrder-extract
##' @export
setMethod("[", c("OrderGroup", "character"), function (x, i, ..., drop=FALSE) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[", c("OrderGroup", "character"), function (x, i, ...) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[", c("OrderGroup", "ANY"), function (x, i, ...) {
    x@entities[[i]]
})

##' @rdname ShojiOrder-extract
##' @export
setMethod("$", "OrderGroup", function (x, name) x[[name]])

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("OrderGroup", "ANY", "missing", "OrderGroup"),
    function (x, i, j, value) {
        stopifnot(class(x) == class(value)) ## So we don't cross subclasses
        entities(x)[[i]] <- value
        return(x)
    })

##' @rdname ShojiOrder-extract
##' @export
setMethod("$<-", "OrderGroup", function (x, name, value) {
    x[[name]] <- value
    return(x)
})

setdiff_entities <- function (x, ents, remove.na=FALSE) {
    ## Remove "ents" (variable references) anywhere they appear in x (Order)
    if (!is.character(ents)) {
        ## Get just the entity URLs
        ents <- entities(ents, simplify=TRUE)
    }

    if (inherits(x, "ShojiOrder") || inherits(x, "OrderGroup")) {
        entities(x) <- setdiff_entities(entities(x), ents)
    } else if (is.list(x)) {
        ## We're inside entities, which may have nested groups
        grps <- vapply(x, inherits, logical(1), what="OrderGroup")
        x[grps] <- lapply(x[grps], setdiff_entities, ents)
        matches <- unlist(x[!grps]) %in% ents
        if (any(matches)) {
            ## Put in NAs so that any subsequent assignment into this object
            ## assigns into the right position. Then strip NAs after
            x[!grps][matches] <- rep(list(NA_character_), sum(matches))
        }
    }
    if (remove.na) {
        x <- removeMissingEntities(x)
    }
    return(x)
}

removeMissingEntities <- function (x) {
    ## Remove NA entries, left by setdiff_entities, from @graph/entities
    if (inherits(x, "ShojiOrder") || inherits(x, "OrderGroup")) {
        entities(x) <- removeMissingEntities(entities(x))
    } else if (is.list(x)) {
        ## We're inside entities, which may have nested groups
        grps <- vapply(x, inherits, logical(1), what="OrderGroup")
        x[grps] <- lapply(x[grps], removeMissingEntities)
        drops <- vapply(x[!grps], is.na, logical(1))
        if (any(drops)) {
            x <- x[-which(!grps)[drops]]
        }
    }
    return(x)
}
