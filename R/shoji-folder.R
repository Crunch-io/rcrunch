setMethod("initialize", "ShojiFolder", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@graph <- lapply(.Object@graph, absoluteURL, .Object@self)
    if (length(.Object@graph)) {
        # Root catalogs may not have a graph (right?)
        .Object@index <- .Object@index[unlist(.Object@graph)]
    }
    return(.Object)
})

is.folder <- function(x) inherits(x, "ShojiFolder")

#' @export
#' @rdname describe-catalog
setMethod("types", "ShojiFolder", function(x) getIndexSlot(x, "type"))

#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiFolder", "numeric"), function(x, i, ..., drop = FALSE) {
    out <- index(x)[i]
    if (is.null(out[[1]])) {
        return(NULL)
    }
    return(folderExtraction(x, out))
})

#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiFolder", "character"), function(x, i, ..., drop = FALSE) {
    path <- parseFolderPath(i)
    if (nchar(path[1]) == 0) {
        ## Go to root level
        x <- rootFolder(x)
        path <- path[-1]
    } else if (identical(path[1], "~")) {
        ## Go to personal
        x <- personalFolder(x)
        path <- path[-1]
    }
    create <- isTRUE(list(...)$create)
    while (length(path)) {
        ## Recurse
        segment <- path[1]
        if (segment == "..") {
            ## Go up a level
            this <- folder(x)
            if (is.null(this)) {
                halt(deparse(i), " is an invalid path")
            }
        } else {
            this <- x[[whichCatalogEntry(x, segment)]]
            if (is.null(this) && create) {
                u <- createFolder(x, segment)
                this <- new(class(x), crGET(u))
            } else if (!is.folder(this) && length(path) > 1) {
                ## Can't recurse deeper if this isn't a folder
                halt(deparse(i), " is an invalid path: ", segment, " is not a folder")
            }
        }
        path <- path[-1]
        x <- this
    }
    return(x)
})

parseFolderPath <- function(path) {
    ## path can be "/" separated, and can change that delimiter with
    ## options(crunch.delimiter="|") or something in case you have real "/"
    if (length(path) == 1) {
        path <- unlist(strsplit(path, folderDelimiter(), fixed = TRUE))
    }
    return(path)
}

folderDelimiter <- function() getOption("crunch.delimiter", "/")

parentFolderURL <- function(x) {
    if (is.variable(x) || inherits(x, "VariableFolder")) {
        shojiURL(x, "catalogs", "folder", mustWork=FALSE)
    } else {
        # A dataset or project
        shojiURL(x, "catalogs", "project", mustWork=FALSE)
    }
}

rootFolder <- function(x) {
    this <- folder(x)
    ## If the parent of x is NULL, we're already at top level.
    while (!is.null(this)) {
        x <- this
        this <- folder(x)
    }
    return(x)
}

createFolder <- function(where, name, index, ...) {
    ## TODO: include index of variables/folders in a single request;
    ## turn index into index + graph in payload
    ## TODO: also for reordering, function that takes a list (index) and returns
    ## list(index=index, graph=names(index))
    if (inherits(where, "VariableFolder")) {
        bod <- wrapCatalog(body = list(name = name, ...))
    } else {
        ## Special case: projects strictly require "entity"
        ## Remove after https://www.pivotaltracker.com/story/show/160328444
        bod <- wrapEntity(body = list(name = name, ...))
    }
    crPOST(self(where), body = toJSON(bod))
}

#' @rdname describe
#' @export
setMethod("name<-", "ShojiFolder",
    function(x, value) setEntitySlot(x, "name", value)
)

#' @rdname delete
#' @export
setMethod("delete", "ShojiFolder", function(x, ...) {
    if (is.null(parentFolderURL(x))) {
        halt("Cannot delete root folder")
    }

    # count the variable/folder objects, and warn the user that they will be
    # summarily deleted as well. Projects must be empty to be deleted (which is
    # enforced on the server, so we only need to check VariableFolders) send as
    # a message before the prompt for test-ability, and so the prompt isn't lost
    # at then end of a long line.
    if (inherits(x, "VariableFolder")) {
        obj_names <- names(x)
        num_vars <- length(x)
        obj_word <- if(num_vars > 1) "objects" else "object"

        if (num_vars > 5) {
            obj_string <- serialPaste(dQuote(head(obj_names, 5)), "...")
        } else {
            obj_string <- serialPaste(dQuote(obj_names))
        }
        message(
            "This folder contains ", num_vars, " ", obj_word, ": ", obj_string, 
            ". Deleting the folder will also delete these objects (including ",
            "their contents)."
        )
    }

    if (!askForPermission(paste0("Really delete ", name(x), "?"))) {
        halt("Must confirm deleting folder")
    }
    out <- crDELETE(self(x))
    invisible(out)
})

#' Change the order of entities in folder
#'
#' @param folder A `VariableFolder` or other `*Folder` object
#' @param ord A vector of integer indices or character references to objects
#' contained in the folder
#' @return `folder` with the order dictated by `ord`. The function also persists
#' that order on the server.
#' @export
setOrder <- function(folder, ord) {
    # If ord is character, match against names/aliases/urls
    if (is.character(ord)) {
        nums <- whichCatalogEntry(folder, ord)
        # Validate that none are NA
        bads <- is.na(nums)
        if (any(bads)) {
            halt(
                ifelse(sum(bads) > 1, "Invalid values: ", "Invalid value: "),
                paste(ord[bads], collapse = ", ")
            )
        }
        ord <- nums
    }
    if (!is.numeric(ord)) {
        halt("Order values must be character or numeric, not ", class(ord))
    }
    # Allow for omitted values, and validate
    valid <- seq_len(length(folder))
    bad <- setdiff(ord, valid)
    if (length(bad)) {
        halt(
            ifelse(length(bad) > 1, "Invalid values: ", "Invalid value: "),
            paste(bad, collapse = ", ")
        )
    }
    dupes <- duplicated(ord)
    if (any(dupes)) {
        halt(
            "Order values must be unique: ",
            paste(unique(ord[dupes]), collapse = ", "),
            ifelse(sum(dupes) > 1, " are", " is"), " duplicated"
        )
    }
    ord <- c(ord, setdiff(valid, ord))
    # Only send an update if there's a change
    if (!all(ord == valid)) {
        # Update folder in place
        index(folder) <- index(folder)[ord]
        folder@graph <- as.list(urls(folder))
        # PATCH graph
        crPATCH(self(folder), body = toJSON(wrapCatalog(graph = urls(folder))))
    }
    return(folder)
}

path <- function(x) {
    out <- name(x)
    parent <- folder(x)
    ## If the parent of x is NULL, we're already at top level.
    while (!is.null(parent)) {
        out <- c(name(parent) %||% "", out)
        parent <- folder(parent)
    }
    out <- paste(out, collapse = folderDelimiter())
    if (nchar(out) == 0) {
        ## Root
        out <- folderDelimiter()
    }
    return(out)
}

# setMethod("folderExtraction", "ShojiFolder", function (x, tuple) {
#     ## Default method: return a folder of the same type
#     return(get(class(x))(crGET(names(tuple))))
# })

setMethod("personalFolder", "ShojiFolder", function (x) {
    halt("Not implemented")
})
