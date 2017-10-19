setMethod("initialize", "ShojiOrder", function (.Object, ..., duplicates=FALSE,
                                                catalog_url="") {
    .Object <- callNextMethod(.Object, ...)
    dots <- list(...)
    ents <- entitiesInitializer(.Object)
    if (length(dots) && !is.shoji(dots[[1]])) {
        .Object@graph <- ents(dots, url.base=NULL)
    } else {
        .Object@graph <- ents(.Object@graph, url.base=.Object@self)
    }
    duplicates(.Object) <- duplicates
    .Object@catalog_url <- catalog_url
    return(.Object)
})

setMethod("initialize", "OrderGroup", function (.Object, group, entities,
                                                url.base=NULL, duplicates=FALSE,
                                                ...) {
    dots <- list(...)
    ents <- entitiesInitializer(.Object)
    if ("variables" %in% names(dots)) entities <- dots$variables
    if ("name" %in% names(dots)) group <- dots$name
    .Object@group <- group
    .Object@entities <- ents(entities, url.base)
    duplicates(.Object) <- duplicates
    return(.Object)
})

setMethod("groupClass", "ShojiOrder", function (x) "OrderGroup")
setMethod("groupClass", "OrderGroup", function (x) "OrderGroup")
setMethod("entityClass", "ShojiOrder", function (x) "ShojiObject")
setMethod("entityClass", "OrderGroup", function (x) "ShojiObject")

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

orderEntitiesInit <- function (x) {
    gc <- groupClass(x)
    ec <- entityClass(x)
    return(function (entities, ...) {
        return(.initEntities(entities, ..., group.class=gc, entity.class=ec))
    })
}

setMethod("entitiesInitializer", "ShojiOrder", orderEntitiesInit)
setMethod("entitiesInitializer", "OrderGroup", orderEntitiesInit)

.setNestedGroupByName <- function (x, i, j, value) {
    ents <- entitiesInitializer(x)
    w <- match(i, names(x))
    value <- ents(value)
    if (!duplicates(x)) {
        x <- setdiff_entities(x, value)
    }
    if (any(is.na(w))) {
        ## New group.
        entities(x) <- c(entities(x), do.call(groupClass(x), list(name=i, entities=value)))
    } else {
        ## Existing group. Assign entities
        entities(x[[w]]) <- value
    }
    ## Ensure duplicates setting persists
    duplicates(x) <- duplicates(x)
    return(removeMissingEntities(x))
}

#' @export
as.list.ShojiOrder <- function (x, ...) x@graph

#' @export
as.list.OrderGroup <- function (x, ...) x@entities

#' Length of an Order
#' @param x a ShojiOrder
#' @return Integer: the number of elements in the Order
#' @name ShojiOrder-length
NULL

#' @rdname ShojiOrder-length
#' @export
setMethod("length", "ShojiOrder", function (x) length(entities(x)))

#' @rdname ShojiOrder-length
#' @export
setMethod("length", "OrderGroup", function (x) length(entities(x)))

#' Extract and update in VariableOrders and VariableGroups
#'
#' @param x a VariableOrder or VariableGroup
#' @param i an index. Numeric and logical indexing supported for both classes;
#' character indexing supported for VariableOrder, matching on VariableGroup
#' names
#' @param name Same as i but for \code{$}
#' @param j Invalid
#' @param value For update methods, an object equivalent in class to what is
#' being updated
#' @param ... additional arguments
#' @param drop Ignored
#' @return `[[` and `$` on a VariableOrder return the VariableGroup.
#' `[[` on VariableGroup returns the entity within, either a character
#' (URL) or nested VariableGroup. `[` and assignment methods return
#' objects of the same class as `x`
#' @name ShojiOrder-extract
#' @aliases ShojiOrder-extract
NULL

###############################
# 1. Extract from ShojiOrder
###############################

#' @rdname ShojiOrder-extract
#' @export
setMethod("[", c("ShojiOrder", "ANY"), function (x, i, ..., drop=FALSE) {
    x@graph <- x@graph[i]
    return(x)
})
#' @rdname ShojiOrder-extract
#' @export
setMethod("[", c("ShojiOrder", "character"), function (x, i, ..., drop=FALSE) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[", c("ShojiOrder", "ANY"), function (x, i, ...) {
    x@graph[[i]]
})

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[", c("ShojiOrder", "character"), function (x, i, ...) {
    ## i may be a path string, so split on the delimiter (default is "/")
    i <- unlist(strsplit(i, getOption("crunch.delimiter", "/"), fixed=TRUE))
    for (segment in i) {
        ## since i may be a path vector, iterate over it
        x <- x[[match(segment, names(x))]]
    }
    return(x)
})

#' @rdname ShojiOrder-extract
#' @export
setMethod("$", "ShojiOrder", function (x, name) x[[name]])

###############################
# 2. Assign into ShojiOrder
###############################

#' @rdname ShojiOrder-extract
#' @export
setMethod("[<-", c("ShojiOrder", "character", "missing", "ShojiOrder"),
    function (x, i, j, value) {
        stopifnot(class(x) == class(value)) ## So we don't cross subclasses
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })
#' @rdname ShojiOrder-extract
#' @export
setMethod("[<-", c("ShojiOrder", "ANY", "missing", "ShojiOrder"),
   function (x, i, j, value) {
       stopifnot(class(x) == class(value)) ## So we don't cross subclasses
       x@graph[i] <- value@graph
       ## Ensure duplicates setting persists
       duplicates(x) <- duplicates(x)
       return(x)
   })

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("ShojiOrder", "character", "missing", "list"),
   .setNestedGroupByName)

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("ShojiOrder", "character", "missing", "character"),
   .setNestedGroupByName)

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("ShojiOrder", "character", "missing", "OrderGroup"),
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined group selected: ", serialPaste(i[is.na(w)]))
        }
        ## NextMethod: c("ShojiOrder", "ANY", "missing", "OrderGroup")
        callNextMethod(x, w, value=value)
    })

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("ShojiOrder", "ANY", "missing", "OrderGroup"),
    function (x, i, j, value) {
        if (!duplicates(x) && length(entities(value))) {
            x <- setdiff_entities(x, value)
        }
        x@graph[[i]] <- value
        ## Ensure duplicates setting persists
        duplicates(x) <- duplicates(x)
        return(removeMissingEntities(x))
    })

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("ShojiOrder", "ANY", "missing", "ANY"),
    function (x, i, j, value) {
        halt("Cannot assign an object of class ", dQuote(class(value)),
            " into a ", class(x))
    })

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("ShojiOrder", "ANY", "missing", "NULL"),
    function (x, i, j, value) {
        x@graph[[i]] <- value
        return(x)
    })
#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("ShojiOrder", "character", "missing", "NULL"),
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined group selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("ShojiOrder", "character", "missing", "ShojiOrder"),
    function (x, i, j, value) {
        .setNestedGroupByName(x, i, j, entities(value))
    })

#' @rdname ShojiOrder-extract
#' @export
setMethod("$<-", "ShojiOrder", function (x, name, value) {
    x[[name]] <- value
    return(x)
})


###############################
# 3. Extract from OrderGroup
###############################

#' @rdname ShojiOrder-extract
#' @export
setMethod("[", c("OrderGroup", "ANY"), function (x, i, ..., drop=FALSE) {
    x@entities <- x@entities[i]
    return(x)
})
#' @rdname ShojiOrder-extract
#' @export
setMethod("[", c("OrderGroup", "character"), function (x, i, ..., drop=FALSE) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[", c("OrderGroup", "character"), function (x, i, ...) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        halt("Undefined groups selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[", c("OrderGroup", "ANY"), function (x, i, ...) {
    x@entities[[i]]
})

#' @rdname ShojiOrder-extract
#' @export
setMethod("$", "OrderGroup", function (x, name) x[[name]])

###############################
# 4. Assign into ShojiGroup
###############################

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("OrderGroup", "character", "missing", "list"),
    .setNestedGroupByName)

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("OrderGroup", "character", "missing", "character"),
    .setNestedGroupByName)

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("OrderGroup", "character", "missing", "ShojiOrder"),
    function (x, i, j, value) {
        .setNestedGroupByName(x, i, j, entities(value))
    })

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("OrderGroup", "character", "missing", "OrderGroup"),
    function (x, i, j, value) {
        .setNestedGroupByName(x, i, j, entities(value))
    })

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("OrderGroup", "ANY", "missing", "OrderGroup"),
    function (x, i, j, value) {
        stopifnot(class(x) == class(value)) ## So we don't cross subclasses
        entities(x)[[i]] <- value
        return(x)
    })

#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("OrderGroup", "numeric", "missing", "NULL"),
    function (x, i, j, value) {
        if (length(i) > 1 || i < 0) {
            halt("Illegal subscript")
        }
        entities(x) <- entities(x)[-i]
        return(x)
    })
#' @rdname ShojiOrder-extract
#' @export
setMethod("[[<-", c("OrderGroup", "character", "missing", "NULL"),
    function (x, i, j, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined group selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value=value)
    })

#' @rdname ShojiOrder-extract
#' @export
setMethod("$<-", "OrderGroup", function (x, name, value) {
    x[[name]] <- value
    return(x)
})

setdiff_entities <- function (x, ents, remove.na=FALSE) {
    ## Remove "ents" (entity references) anywhere they appear in x (Order)
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

intersect_entities <- function (x, ents, remove.na=TRUE) {
    ## Keep only the part of x (Order) containing "ents" (entity references)
    if (!is.character(ents)) {
        ## Get just the entity URLs
        ents <- urls(ents)
    }

    if (inherits(x, "ShojiOrder") || inherits(x, "OrderGroup")) {
        entities(x) <- intersect_entities(entities(x), ents)
    } else if (is.list(x)) {
        ## We're inside entities, which may have nested groups
        grps <- vapply(x, inherits, logical(1), what="OrderGroup")
        x[grps] <- lapply(x[grps], intersect_entities, ents)
        matches <- unlist(x[!grps]) %in% ents
        if (any(!matches)) {
            ## Put in NAs so that any subsequent assignment into this object
            ## assigns into the right position. Then strip NAs after
            x[!grps][!matches] <- rep(list(NA_character_), sum(!matches))
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

#' Remove OrderGroups with no entities
#'
#' This function recurses through a \code{ShojiOrder}/\code{OrderGroup} and
#' removes any groups that contain no entities.
#'
#' @param x VariableOrder, DatasetOrder, VariableGroup, or DatasetGroup
#' @return \code{x} with empty groups removed.
#' @export
removeEmptyGroups <- function (x) {
    if (inherits(x, "ShojiOrder") || inherits(x, "OrderGroup")) {
        entities(x) <- removeEmptyGroups(entities(x))
    } else if (is.list(x)) {
        ## We're inside entities, which may have nested groups
        grps <- vapply(x, inherits, logical(1), what="OrderGroup")
        if (any(grps)) {
            empties <- vapply(x[grps], function (g) length(urls(g)) == 0,
                logical(1))
            ## Recurse through non-empty groups
            if (any(!empties)) {
                nonempty <- which(grps)[!empties]
                x[nonempty] <- lapply(x[nonempty], removeEmptyGroups)
            }
            ## Drop empty groups
            if (any(empties)) {
                x <- x[-which(grps)[empties]]
            }
        }
    }
    return(x)
}

#' Remove duplicated entities from an order/group
#'
#' This function recurses through a `ShojiOrder` or `OrderGroup` and
#' removes any duplicate entities. As with [`base::duplicated`],
#' the first appearance of an entity is kept, and subsequent occurrences are marked as duplicated
#' and removed. (Unlike `duplicated`, there is no option to reverse that order.)
#' The first occurrence of an entity is determined by the function's recursion:
#' within each group, nested groups are processed first, in order, and
#' their nested groups are processed recursively. See the test suite, in
#' test-variable-order.R, for an example that illustrates which entities are
#' dropped as duplicate.
#'
#' @param x VariableOrder, DatasetOrder, VariableGroup, or DatasetGroup
#' @return `x` with duplicate entities removed.
#' @seealso [`duplicates`], which when set to `FALSE` also calls this function.
#' @export
dedupeOrder <- function (x) {
    ## Collect seen urls outside, diff out urls, recurse into groups, update seen urls
    seen <- c()

    .dedupe <- function (x) {
        if (inherits(x, "ShojiOrder") || inherits(x, "OrderGroup")) {
            entities(x) <- .dedupe(entities(x))
        } else if (is.list(x)) {
            ## We're inside entities, which may have nested groups
            grps <- vapply(x, inherits, logical(1), what="OrderGroup")

            ## First, recurse through groups:
            x[grps] <- lapply(x[grps], .dedupe)

            ## Then, dedupe URLs at this level, and drop any that we've already seen
            ents <- unlist(x[!grps])
            badents <- duplicated(ents) | ents %in% seen
            if (any(badents)) {
                x <- x[-which(!grps)[badents]]
            }

            ## Update "seen" with the URLs we didn't drop
            seen <<- c(seen, ents[!badents])
        }
        return(x)
    }
    return(.dedupe(x))
}

#' Remove nesting of groups within an order/group
#'
#' This function reduces a potentially nested order to its flattened
#' representation, containing no nested groups. Entities are ordered in the
#' result by their first appearance in the order object.
#'
#' @param x VariableOrder, DatasetOrder, VariableGroup, or DatasetGroup; or a
#' CrunchDataset or catalog that has an `ordering` property.
#' @return `x`, or its order resource, flattened.
#' @export
flattenOrder <- function (x) {
    if (!(inherits(x, "ShojiOrder") || inherits(x, "OrderGroup"))) {
        ## Perhaps it's a dataset or catalog. Get its "ordering"
        x <- ordering(x)
    }
    entities(x) <- urls(x)
    return(x)
}

#' Get grouped or ungrouped OrderGroups
#'
#' "ungrouped" is an OrderGroup that contains all entities not found
#' in groups at a given level of nesting.
#' @param order.obj an subclass of ShojiOrder or OrderGroup
#' @return For `grouped()`, an Order/Group, respectively, with "ungrouped"
#' omitted. For `ungrouped()`, an OrderGroup subclass.
#' @seealso [`VariableOrder`]
#' @export
grouped <- function (order.obj) {
    Filter(Negate(is.character), order.obj)
}

#' @rdname grouped
#' @export
ungrouped <- function (order.obj) {
    return(do.call(groupClass(order.obj), list(name="ungrouped",
        entities=entities(Filter(is.character, order.obj)))))
}

#' Move entities to a group
#'
#' Shoji entities can be placed into groups, this is mostly used for grouping
#' variables for display in the app, but is technically possible for any of the
#' order catalogs. This function moves an entity to one of these groups.
#'
#' The function has two versions: a regular function and a setter. They do the
#' same thing, but the setter is probably more succinct.
#'
#' @param x VariableGroup
#' @param value Variable, VariableCatalog subset, or Dataset subset
#' @return `x` with the entities in `value` appended to it. If the
#' containing order object has `duplicates=FALSE`, the entities will be "moved"
#' to this group. Otherwise, their references will be copied to the group.
#' @examples
#' \dontrun{
#' moveToGroup(ordering(ds)[["Demographics"]]) <- ds[c("gender", "age")]
#'}
#' @export
moveToGroup <- function (x, value) {
    if (!inherits(value, "OrderGroup")) {
        ## If it's a Group, let's move it as is. If not, get the URLs
        ## TODO: this won't do the right thing for moving Dataset to DatasetGroup
        value <- urls(value)
    }
    entities(x) <- c(entities(x), value)
    return(x)
}

#' @rdname moveToGroup
#' @export
"moveToGroup<-" <- moveToGroup

#' Find an entity in an order object
#'
#' @param x Variable or Dataset, depending on the type of order, or URL for it
#' @param ord ShojiOrder (VariableOrder or DatasetOrder)
#' @return If `x` is found in `ord`, a character vector of group names
#' that provide the "path" to the entity. The length of the vector corresponds
#' to the depth of nesting. If not found, `NA` is returned
#' @export
locateEntity <- function (x, ord) {
    if (!is.character(x)) x <- self(x)
    out <- character(0)

    .locateInGroups <- function (x, ord) {
        allurls <- urls(ord)
        if (x %in% allurls) {
            us <- vapply(grouped(ord), function (g) x %in% urls(g), logical(1))
            if (any(us)) {
                ## Only looks for first match
                ind <- which(us)[1]
                out <<- c(out, name(grouped(ord)[[ind]]))
                .locateInGroups(x, grouped(ord)[[ind]])
            }
        } else if (inherits(ord, "ShojiOrder")) {
            ## We're at the top level and it wasn't found at all
            out <<- NA_character_
        }
        invisible()
    }

    .locateInGroups(x, ord)
    return(out)
}
