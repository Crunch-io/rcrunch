setMethod("initialize", "ShojiCatalog", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    names(.Object@index) <- absoluteURL(names(.Object@index), .Object@self)
    return(.Object)
})

is.catalog <- function(x) inherits(x, "ShojiCatalog")

getIndexSlot <- function(x, i, what = character(1), ifnot = NA_character_) {
    getListSlot(index(x), i, what, ifnot)
}

getListSlot <- function(x, i, what = character(1), ifnot = NA_character_) {
    ## Wrapper to vapply over a list data structure and select a value
    vapply(x, function(a) a[[i]] %||% ifnot, what, USE.NAMES = FALSE)
}

setIndexSlot <- function(x, i, value, unique = FALSE) {
    scalar_value <- length(value) == 1
    if (!scalar_value) {
        stopifnot(length(x) == length(value))
    }

    duples <- duplicated(urls(x))
    if (any(duples)) {
        ## This is probably a mistake, a selection from a catalog that wasn't
        ## unique. Make sure that the user isn't trying to assign different
        ## values for the duplicated entries, and if not, clean it up
        if (!scalar_value && !identical(duples, duplicated(paste(urls(x), value)))) {
            halt("Can't update the same index item with more than one entry")
        }
        x <- x[!duples]
        if (!scalar_value) {
            value <- value[!duples]
        }
    }

    if (scalar_value) {
        value <- rep(value, length(x))
    }

    old <- index(x)
    index(x) <- mapply(function(a, v) {
        a[[i]] <- v
        return(a)
    }, a = index(x), v = value, SIMPLIFY = FALSE)
    if (unique) {
        ## Check to see if any of the value is duplicated after updating
        newvals <- getIndexSlot(x, i) ## Assumes "character". Revisit if need unique for non-char
        dups <- duplicated(newvals)
        if (any(dups)) {
            halt(
                "Duplicate values not permitted: ",
                serialPaste(unique(newvals[dups]))
            )
        }
    }
    to.update <- dirtyElements(old, index(x))
    if (any(to.update)) {
        ## Make sure certain fields are [] in the JSON
        ensure <- c("subvariables")
        payload <- lapply(index(x)[to.update], function(p) {
            p <- p[i] ## Let's only PATCH the field we're changing
            these <- intersect(ensure, names(p))
            if (length(these)) p[these] <- lapply(p[these], I)
            return(p)
        })
        crPATCH(self(x), body = toJSON(wrapCatalogIndex(payload)))
    }
    return(x)
}

setIndexSlotOnEntity <- function(x, i, value, ...) {
    ## For catalog setters where you can't PATCH the catalog
    old <- getIndexSlot(x, i, ...)
    changes <- dirtyElements(old, value)
    mapply(function(m, v) setEntitySlot(m, i, v),
        m = x[changes], v = value[changes]
    )
    return(refresh(x))
}

dirtyElements <- function(x, y) {
    !mapply(identical, x, y, USE.NAMES = FALSE, SIMPLIFY = TRUE)
}

setMethod(
    "whichCatalogEntry", "ShojiCatalog",
    function(x, i, ...) whichNameOrURL(x, i, ...)
)

#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "character"), function(x, i, ...) {
    w <- whichCatalogEntry(x, i, ...)
    if (any(is.na(w))) {
        halt("Undefined elements selected: ", serialPaste(i[is.na(w)]))
    }
    return(x[w])
})
#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "numeric"), function(x, i, ...) {
    bad <- abs(as.integer(i)) > length(x)
    if (any(bad)) {
        halt("Subscript out of bounds: ", capture.output(dput(i[bad])))
    }
    callNextMethod(x, i, value)
})
#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "logical"), function(x, i, ...) {
    if (length(i) > length(x)) {
        halt(
            "Subscript out of bounds: got ", length(i), " logicals, need ",
            length(x)
        )
    }
    index(x) <- index(x)[i]
    return(x)
})
#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "ANY"), function(x, i, ...) {
    index(x) <- index(x)[i]
    return(x)
})
#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiCatalog", "ANY"), function(x, i, ...) {
    ## Note that this returns a bare list, not an IndexTuple
    index(x)[[i]]
})

getTuple <- function(x, i, Constructor = ShojiTuple, ...) {
    b <- index(x)[[i]]
    if (is.null(b)) return(NULL)
    Constructor(index_url = self(x), entity_url = urls(x)[i], body = b)
}

getEntity <- function(x, i, Constructor = ShojiEntity, ...) {
    stopifnot(length(i) == 1)
    url <- urls(x)[i]
    if (is.na(url)) {
        halt("subscript out of bounds: ", i)
    }
    return(Constructor(crGET(url)))
}

#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiCatalog", "character"), function(x, i, ...) {
    stopifnot(length(i) == 1L)
    w <- whichCatalogEntry(x, i, ...)
    if (is.na(w)) {
        return(NULL)
    }
    return(x[[w]])
})

#' @rdname catalog-extract
#' @export
setMethod("$", "ShojiCatalog", function(x, name) x[[name]])

#' @rdname catalog-extract
#' @export
setMethod("$<-", "ShojiCatalog", function(x, name, value) {
    x[[name]] <- value
    return(x)
})

#' @rdname catalog-extract
#' @export
setMethod(
    "[<-", c("ShojiCatalog", "ANY", "missing", "ShojiCatalog"),
    function(x, i, j, value) {
        index(x)[i] <- index(value)[i]
        ## Assume that PATCHing has happened outside this function
        return(x)
    }
)

#' Length of Catalog
#' @param x a Catalog
#' @return Integer: the number of elements in the index list
#' @name catalog-length
NULL

whichNameOrURL <- function(x, i, secondary = names(x), ...) {
    var_matches <- match(i, secondary)
    if (any(is.na(var_matches))) {
        url_matches <- match(i, urls(x))
        # replace NAs in matches with the contents of URL match (either a match or NA)
        var_matches[is.na(var_matches)] <- url_matches[is.na(var_matches)]
    } else {
        ## Warn if duplicated
        dups <- i %in% secondary[duplicated(secondary)]
        if (any(dups)) {
            bads <- i[dups]
            msg <- ifelse(length(bads) > 1,
                " do not uniquely identify elements. Returning the first matches",
                " does not uniquely identify elements. Returning the first match"
            )
            warning(i, msg, call. = FALSE)
        }
    }

    return(var_matches)
}

#' @rdname catalog-length
#' @export
setMethod("length", "ShojiCatalog", function(x) length(index(x)))
setMethod("lapply", "ShojiCatalog", function(X, FUN, ...) lapply(index(X), FUN, ...))

#' Get the body of a Catalog
#'
#' The core of Catalog data is in its "index". These methods get and set that
#' slot.
#' @param x a Catalog (VariableCatalog, Subvariables, or similar object)
#' @param value For the setters, an appropriate-length list to
#' assign
#' @return Getters return the list object in the "index" slot; setters
#' return `x` duly modified.
#' @aliases index index<-
#' @name shoji-index
NULL

#' @rdname shoji-index
#' @export
setMethod("index", "ShojiCatalog", function(x) x@index)
#' @rdname shoji-index
#' @export
setMethod("index<-", "ShojiCatalog", function(x, value) {
    x@index <- value
    return(x)
})

#' Get the URLs contained in a Catalog or Order object
#'
#' Sometimes it is useful to extract flattened vector of URLs from more
#' complex objects for purposes like subsetting or doing set comparisons.
#'
#' @param x a Catalog, Order, or Group object
#' @return A character vector of URLs
#' @aliases urls
#' @keywords internal
#' @rdname urls
#' @export
setMethod("urls", "ShojiCatalog", function(x) names(index(x)))

#' @rdname urls
#' @export
setMethod("urls", "list", function(x) vapply(x, self, character(1))) ## Assumes list of entities

#' @rdname urls
#' @export
setMethod("urls", "character", function(x) x) ## Assumes already are URLs

#' @rdname describe-catalog
#' @export
setMethod("names", "ShojiCatalog", function(x) getIndexSlot(x, "name"))
#' @export
#' @rdname describe-catalog
setMethod("names<-", "ShojiCatalog", function(x, value) {
    setIndexSlot(x, "name", value, unique = TRUE)
})

#' @rdname describe-catalog
#' @export
setMethod("emails", "ShojiCatalog", function(x) getIndexSlot(x, "email"))

#' @export
as.list.ShojiCatalog <- function(x, ...) lapply(names(index(x)), function(i) x[[i]])

#' A utility to return a data.frame from a ShojiCatalog.
#'
#' This is an internal function called by the various `as.data.frame` methods.
#' Use `as.data.frame` instead of calling this directly.
#'
#' Some of the attributes of a `ShojiCatalog` will not naturally fit in
#' a conventional data.frame. For example, an array variable contains a list of
#' subvariables, and these subvariables will not easily fit in a single row of a
#' data.frame. In this case, the list of subvariables are stored in a
#' list-column in the resulting data.frame.
#'
#' @param x `ShojiCatalog` or subclass
#' @param keys character vector of attribute names from each catalog tuple to
#' include in the result. Default is TRUE, which means all.
#' @param rownames See [base::data.frame()] for the `row.names`
#' argument. The difference here is that if `rownames` is explicitly set as
#' `NULL`, the resulting object will not have row names set. By default, row
#' names will be the URLs of the catalog tuples.
#' @param list_columns A character vector of the names of the attributes that
#' should be stored in a list-column. Currently ignored.
#' @param ... additional arguments passed to `data.frame`
#' @return a `data.frame` view of the catalog
#' @keywords internal
catalogToDataFrame <- function(x, keys = TRUE, rownames = NULL,
                               list_columns = c("subvariables", "subvariables_catalog"),
                               ...) {
    if (length(x) == 0) {
        ## If catalog is empty, bail
        return(data.frame())
    } else {
        index <- lapply(index(x), function(a) a[keys])
        ### The following code is equivalent to out <- purrr::map_df(index, entry_to_df)
        ################
        entry_list <- lapply(index, prepareCatalogEntry)
        names <- unique(unlist(lapply(entry_list, names)))
        out <- data.frame(matrix(nrow = length(entry_list), ncol = length(names)), ...)
        names(out) <- names
        for (i in seq_along(entry_list)) {
            for (j in names(entry_list[[i]])) {
                val <- entry_list[[i]][[j]]
                if (identical(val, list())) {
                    val <- NA
                }
                out[[j]][i] <- val
            }
        }
        #################

        ## TODO: ensure something about the elements of a "list column".
        ## i.e. do better than:
        # $ subvariables        :List of 5
        # ..$ : chr NA
        # ..$ : chr NA
        # ..$ : chr "just/one/subvar"
        # ..$ :List of 3
        # .. ..$ : chr "mymrset/subvariables/subvar2/"
        # .. ..$ : chr "mymrset/subvariables/subvar1/"
        # .. ..$ : chr "mymrset/subvariables/subvar3/"
        # ..$ : chr NA
        # Note that the "list_columns" argument is no longer used

        # When a bad key argument is passed to the the shoji index it returns a
        # data.frame with an NA value. This excludes those columns.
        exclude_cols <- grepl("^NA", names(out)) &
            vapply(out, function(x) all(is.na(x)), logical(1))
        out <- out[, !exclude_cols, drop = FALSE]

        if (!isTRUE(keys)) {
            missing_keys <- setdiff(keys, names(out))
            if (length(missing_keys)) {
                if (length(missing_keys) == 1) {
                    error_text <- " is an invalid key for catalogs of class "
                } else {
                    error_text <- " are invalid keys for catalogs of class "
                }
                halt(serialPaste(dQuote(missing_keys)), error_text, class(x), ".")
            }
        }

        # Reorder columns to match the order in which keys were supplied
        # Some catalogs get to this point with no columns, subsetting caused
        # an error in that case.
        if (ncol(out) > 0) {
            out <- out[, keys, drop = FALSE]
        }
        return(out)
    }
}

prepareCatalogEntry <- function(entry) {
    ## Do some standardization so that we can stack up catalog tuples
    entry[vapply(entry, is.null, logical(1))] <- NA
    list_cols <- vapply(entry, length, integer(1)) > 1
    entry[list_cols] <- lapply(entry[list_cols], list)
    return(entry)
}

#' Change the name of the entities in a catalog
#'
#' This is an alternative to assigning `names(catalog) <- something`, suitable
#' for inclusion in a pipeline.
#'
#' @param object A catalog object, such as `VariableFolder`
#' @param nm A character vector of new names of the same length as the number
#'   of entities in the index
#' @return `object`, with the names of its children duly changed
#' @seealso [cd()] and [mv()]
#' @examples
#' \dontrun{
#' ds <- ds %>%
#'     cd("Demographics") %>%
#'     setNames(c("Gender (4 category)", "Birth year", "Race (5 category)"))
#' }
#'
#' @name setNames
#' @export
setGeneric("setNames", function(object, nm) stats::setNames(object, nm))

#' @rdname setNames
#' @export
setMethod("setNames", "ShojiCatalog", function(object, nm) {
    # check lengths to provide a friendly user-facing error message.
    if (length(object) != length(nm)) {
        # TODO: should this check be lower, inside names<- or even lower?
        halt("names must have the same length as the number of children: ", length(object))
    }

    names(object) <- nm
    return(invisible(object))
})
