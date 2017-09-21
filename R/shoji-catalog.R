setMethod("initialize", "ShojiCatalog", function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    names(.Object@index) <- absoluteURL(names(.Object@index), .Object@self)
    return(.Object)
})

is.catalog <- function (x) inherits(x, "ShojiCatalog")

getIndexSlot <- function (x, i, what=character(1), ifnot=NA_character_) {
    getListSlot(index(x), i, what, ifnot)
}

getListSlot <- function (x, i, what=character(1), ifnot=NA_character_) {
    ## Wrapper to vapply over a list data structure and select a value
    vapply(x, function (a) a[[i]] %||% ifnot, what, USE.NAMES=FALSE)
}

setIndexSlot <- function (x, i, value, unique=FALSE) {
    if (length(value) == 1) value <- rep(value, length(x))
    stopifnot(length(x) == length(value))

    old <- index(x)
    index(x) <- mapply(function (a, v) {
        a[[i]] <- v
        return(a)
    }, a=index(x), v=value, SIMPLIFY=FALSE)
    if (unique) {
        ## Check to see if any of the value is duplicated after updating
        newvals <- getIndexSlot(x, i) ## Assumes "character". Revisit if need unique for non-char
        dups <- duplicated(newvals)
        if (any(dups)) {
            halt("Duplicate values not permitted: ",
                serialPaste(unique(newvals[dups])))
        }
    }
    to.update <- dirtyElements(old, index(x))
    if (any(to.update)) {
        ## Make sure certain fields are [] in the JSON
        ensure <- c("subvariables")
        payload <- lapply(index(x)[to.update], function (p) {
            p <- p[i] ## Let's only PATCH the field we're changing
            these <- intersect(ensure, names(p))
            if (length(these)) p[these] <- lapply(p[these], I)
            return(p)
        })
        crPATCH(self(x), body=toJSON(payload))
    }
    return(x)
}

setIndexSlotOnEntity <- function (x, i, value, ...) {
    ## For catalog setters where you can't PATCH the catalog
    old <- getIndexSlot(x, i, ...)
    changes <- dirtyElements(old, value)
    mapply(function (m, v) setEntitySlot(m, i, v),
        m=x[changes], v=value[changes])
    return(refresh(x))
}

dirtyElements <- function (x, y) {
    !mapply(identical, x, y, USE.NAMES=FALSE, SIMPLIFY=TRUE)
}

#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "character"), function (x, i, ...) {
    w <- whichNameOrURL(x, i)
    if (any(is.na(w))) {
        halt("Undefined elements selected: ", serialPaste(i[is.na(w)]))
    }
    return(x[w])
})
#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "numeric"), function (x, i, ...) {
    bad <- abs(as.integer(i)) > length(x)
    if (any(bad)) {
        halt("Subscript out of bounds: ", capture.output(dput(i[bad])))
    }
    callNextMethod(x, i, value)
})
#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "logical"), function (x, i, ...) {
    if (length(i) > length(x)) {
        halt("Subscript out of bounds: got ", length(i), " logicals, need ",
            length(x))
    }
    index(x) <- index(x)[i]
    return(x)
})
#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "ANY"), function (x, i, ...) {
    index(x) <- index(x)[i]
    return(x)
})
#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiCatalog", "ANY"), function (x, i, ...) {
    ## Note that this returns a bare list, not an IndexTuple
    index(x)[[i]]
})

getTuple <- function (x, i, Constructor=ShojiTuple, ...) {
    b <- index(x)[[i]]
    if (is.null(b)) return(NULL)
    Constructor(index_url=self(x), entity_url=urls(x)[i], body=b)
}

getEntity <- function (x, i, Constructor=ShojiEntity, ...) {
    stopifnot(length(i) == 1)
    url <- urls(x)[i]
    if (is.na(url)) {
        halt("subscript out of bounds: ", i)
    }
    return(Constructor(crGET(url)))
}

#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiCatalog", "character"), function (x, i, ...) {
    stopifnot(length(i) == 1L)
    w <- whichNameOrURL(x, i)
    if (is.na(w)) {
        return(NULL)
    }
    return(x[[w]])
})

#' @rdname catalog-extract
#' @export
setMethod("$", "ShojiCatalog", function (x, name) x[[name]])

#' @rdname catalog-extract
#' @export
setMethod("$<-", "ShojiCatalog", function (x, name, value) {
    x[[name]] <- value
    return(x)
})

#' @rdname catalog-extract
#' @export
setMethod("[<-", c("ShojiCatalog", "ANY", "missing", "ShojiCatalog"),
    function (x, i, j, value) {
        index(x)[i] <- index(value)[i]
        ## Assume that PATCHing has happened outside this function
        return(x)
    })

#' Length of Catalog
#' @param x a Catalog
#' @return Integer: the number of elements in the index list
#' @name catalog-length
NULL

whichNameOrURL <- function (x, i, secondary=names(x)) {
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
                " does not uniquely identify elements. Returning the first match")
            warning(i, msg, call.=FALSE)
        }
    }

    return(var_matches)
}

#' @rdname catalog-length
#' @export
setMethod("length", "ShojiCatalog", function (x) length(index(x)))
setMethod("lapply", "ShojiCatalog", function (X, FUN, ...) lapply(index(X), FUN, ...))

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
setMethod("index", "ShojiCatalog", function (x) x@index)
#' @rdname shoji-index
#' @export
setMethod("index<-", "ShojiCatalog", function (x, value) {
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
setMethod("urls", "ShojiCatalog", function (x) names(index(x)))

#' @rdname urls
#' @export
setMethod("urls", "list", function (x) vapply(x, self, character(1))) ## Assumes list of entities

#' @rdname urls
#' @export
setMethod("urls", "character", function (x) x) ## Assumes already are URLs

#' @rdname describe-catalog
#' @export
setMethod("names", "ShojiCatalog", function (x) getIndexSlot(x, "name"))
#' @export
#' @rdname describe-catalog
setMethod("names<-", "ShojiCatalog", function (x, value) {
    setIndexSlot(x, "name", value, unique=TRUE)
})

#' @rdname describe-catalog
#' @export
setMethod("emails", "ShojiCatalog", function (x) getIndexSlot(x, "email"))

#' @export
as.list.ShojiCatalog <- function (x, ...) lapply(names(index(x)), function (i) x[[i]])

#' A utility to return a data.frame from a ShojiCatalog.
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
#' should be stored in a list-column.
#' @param ... additional arguments passed to `data.frame`
#' @return a `data.frame` view of the catalog
#' @export
catalogToDataFrame <- function (x, keys=TRUE, rownames = NULL,
    list_columns = c("subvariables", "subvariables_catalog"),
    ...) {

    index <- lapply(index(x), function(a) a[keys])
    ## Return an empty dataframe if the function is called on an empty catalog
    if (length(index) == 0) {
        message("Catalog is empty, returning an empty dataframe")
        return(data.frame())
    } else {
        ### The following code is equivalent to out <- purrr::map_df(index, entry_to_df)
        ################
        entry_list <- lapply(index, entryToDF, list_col_names = list_columns)
        names   <- unique(unlist( lapply(entry_list, names)))
        out <- data.frame(matrix(nrow = length(entry_list), ncol = length(names)))
        names(out) <- names

        for (i in seq_along(entry_list)) {
            for (j in  names(entry_list[[i]])) {
                # Overwriting a entry in a dataframe with a longer list
                # can trigger a warning, which needs to be suppressed.
                suppressWarnings(out[i, j] <- entry_list[[i]][1, j])
            }
        }
        #################

        out <- as.data.frame(out, rownames = rownames, ...)

        # When a bad key argument is passed to the the shoji index it returns a
        # data,frame with an NA value. This excludes those columns.
        exclude_cols <- grepl("^NA", names(out)) &
            vapply(out, function(x)all(is.na(x)), FUN.VALUE = logical(1))
        out <- out[, !exclude_cols, drop = FALSE]

        missing_keys <- dQuote(keys[!(keys %in% names(out))])
        if (any(!(keys %in% names(out))) && keys != TRUE) {
            if (length(missing_keys) == 1) {
                error_text <-  " is an invalid key for catalogs of class "
            } else {
                error_text <- " are invalid keys for catalogs of class "
            }
            halt(serialPaste(missing_keys), error_text, class(x), ".")
        }

        # Reorder columns to match the order in which keys were supplied
        out <- out[, keys, drop = FALSE]
        return(out)
    }
}

#' Turn an entry in the catalog into a one-row data.frame
#'
#' @param entry A single entry in a catalog
#' @param list_col_names Some entries in a catalog do not fit neatly into a
#' traditional data.frame and are instead stored in a list-column. This is
#' controlled by passing the attribute names as the `list_col_names` argument.
#' @return A one row data frame
#' @keywords internal
entryToDF <- function (entry, list_col_names) {
    entry[vapply(entry, is.null, logical(1))] <- NA
    vect_col <- entry[!(names(entry) %in% list_col_names)]
    mislabled_list_cols <- lapply(vect_col, length) > 1

    error_text <- " contain more than one entry and are not included in list_col_names"
    if (sum(mislabled_list_cols) == 1) {
        error_text <- " contains more than one entry and is not included in list_col_names"
    }
    if (any(mislabled_list_cols)) {
        halt(serialPaste(dQuote(names(vect_col[mislabled_list_cols]))),
            error_text)
    }
    entry_df <- as.data.frame(vect_col, stringsAsFactors = FALSE)

    if (any(names(entry) %in% list_col_names)) {
        list_col <- entry[list_col_names]
        list_df <- data.frame(matrix(nrow = 1, ncol = length(list_col)))
        names(list_df) <- names(list_col)
        for (i in seq_along(list_col)) {
            # The column needs to corerced to a list before you
            # can assign a list into it. This can be removed if
            # we add a tibble dependency.
            list_df[[1, i]] <- as.list(list_df[[1, i]])
            list_df[[1, i]] <- list_col[[i]]
        }
        entry_df <- cbind(entry_df, list_df)
    }
    return(entry_df)
}
