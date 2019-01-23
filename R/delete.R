#' Delete a Crunch object from the server
#'
#' These methods delete entities, notably Datasets and Variables within them,
#' from the server. This action is permanent and cannot be undone, so it
#' should not be done lightly. Consider instead using `archive`
#' for datasets and `hide` for variables.
#'
#' Deleting requires confirmation. In an interactive session, you will be asked
#' to confirm. To avoid that prompt, or to delete objects from a
#' non-interactive session, wrap the call in [with_consent()] to give
#' your permission to delete.
#'
#' @param x a Crunch object
#' @param ... additional arguments, generally ignored
#' @seealso [hide()] [deleteDataset()] [deleteVariables()] [deleteSubvariables()]
#' @name delete
#' @aliases delete
setGeneric("delete", function(x, ...) standardGeneric("delete"),
    signature = "x"
)

#' @rdname delete
#' @export
setMethod("delete", "CrunchDataset", function(x, ...) {
    out <- delete(tuple(x), ...)
    invisible(out)
})

confirmDeleteEntity <- function(entity_name, entity_type=NULL) {
    prompt <- paste("Really delete", entity_type, dQuote(entity_name))
    if (!askForPermission(paste0(prompt, "?"))) {
        halt("Must confirm deleting ", entity_type)
    }
}

#' @rdname delete
#' @export
setMethod("delete", "DatasetTuple", function(x, ...) {
    confirmDeleteEntity(name(x), "dataset")
    out <- crDELETE(self(x))
    dropDatasetsCache()
    invisible(out)
})

dropDatasetsCache <- function() {
    # A dataset or project has been deleted, and rather than guessing where it
    # appeared in the HTTP query cache, just drop wherever it could have been:
    # 1) All projects/folders
    dropCache(sessionURL("projects"))
    # 2) The datasets catalog
    dropOnly(sessionURL("datasets"))
    # 3) Search endpoints
    dropSearchCache()
}

dropSearchCache <- function() {
    # TODO: We should drop cache everywhere datasets or variables are modified?
    dropCache(paste0(sessionURL("datasets"), "by_name/"))
    dropCache(sessionURL("search", "views"))
}

#' @rdname delete
#' @export
setMethod("delete", "CrunchDeck", function(x, ...) {
    confirmDeleteEntity(name(x), "deck")
    out <- crDELETE(self(x))
    invisible(out)
})

#' @rdname delete
#' @export
setMethod("delete", "CrunchSlide", function(x, ...) {
    confirmDeleteEntity(title(x), "slide")
    out <- crDELETE(self(x), drop = dropCache(absoluteURL("../", self(x))))
    return(invisible(out))
})

#' @rdname delete
#' @export
setMethod("delete", "Multitable", function(x, ...) {
    confirmDeleteEntity(name(x), "multitable")
    out <- crDELETE(self(x))
    invisible(out)
})

#' @rdname delete
#' @export
setMethod("delete", "CrunchTeam", function(x, ...) {
    confirmDeleteEntity(name(x), "team")
    u <- self(x)
    out <- crDELETE(u)
    dropCache(absoluteURL("../", u))
    invisible(out)
})

#' @rdname delete
#' @export
setMethod("delete", "CrunchVariable", function(x, ...) {
    out <- delete(tuple(x), ...)
    dropCache(absoluteURL("../", self(x)))
    invisible(out)
})

#' @rdname delete
#' @export
setMethod("delete", "VariableTuple", function(x, ...) {
    confirmDeleteEntity(name(x), "variable")
    out <- crDELETE(self(x))
    invisible(out)
})

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
        obj_word <- pluralize("object", num_vars)

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
    confirmDeleteEntity(name(x), "folder")
    out <- crDELETE(self(x))
    invisible(out)
})

#' @rdname delete
#' @export
setMethod("delete", "ShojiTuple", function(x, ...) {
    crDELETE(x@entity_url, drop = dropCache(x@index_url))
})

#' @rdname delete
#' @export
setMethod("delete", "ShojiObject", function(x, ...) invisible(crDELETE(self(x))))

#' @rdname delete
#' @export
setMethod("delete", "ANY", function(x, ...) halt("'delete' only valid for Crunch objects"))

#' Delete a dataset from the dataset list
#'
#' This function lets you delete a dataset without first loading it. If you
#' have a dataset that somehow is corrupted and won't load, you can delete it
#' this way.
#'
#' The function also works on CrunchDataset objects, just like
#' [delete()], which may be useful if you have loaded another
#' package that masks the [delete()] method.
#' @param x The name (character) of a dataset, its (numeric) position in the
#' return of [listDatasets()], or an object of class
#' `CrunchDataset`. x can only be of length 1--this function is not
#' vectorized (for your protection).
#' @param ... additional parameters passed to `delete`
#' @return (Invisibly) the API response from deleting the dataset
#' @seealso [delete()]
#' @export
deleteDataset <- function(x, ...) {
    if (is.dataset(x)) {
        return(delete(x, ...))
    }

    if (is.character(x)) {
        if (is.datasetURL(x) && identical(x, datasetReference(x))) {
            url <- x
        } else {
            # Assume it is a path or name
            found <- lookupDataset(x)
            if (length(found) != 1) {
                halt(x, " identifies ", length(found),
                    " datasets. To delete, please identify the dataset uniquely by URL or path.")
            }
            ## We know there is just one now
            url <- urls(found)
        }
        ## Now, delete it
        confirmDeleteEntity(x, "dataset")
        crDELETE(url)
        dropDatasetsCache()
    } else {
        halt("deleteDataset requires either a Dataset, a unique dataset name, or a URL")
    }
}

#' Delete Variables Within a Dataset
#'
#' This function permanently deletes a variable from a dataset. For a non-destructive
#' alternative see [hide()].
#'
#' In an interactive session, you will be prompted to confirm that you
#' wish to delete the variable. To avoid that prompt, or to delete variables from a
#' non-interactive session, wrap the call in [with_consent()] to give
#' your permission to delete.
#' @param dataset the Dataset to modify
#' @param variables aliases (following `crunch.namekey.dataset`) or indices
#' of variables to delete.
#' @return (invisibly) `dataset` with the specified variables deleted
#' @seealso [hide()] [delete()] [deleteSubvariable()]
#' @export
deleteVariables <- function(dataset, variables) {
    to.delete <- allVariables(dataset[variables])
    if (length(to.delete) == 1) {
        prompt <- paste0("Really delete ", dQuote(names(to.delete)), "?")
    } else {
        prompt <- paste0(
            "Really delete these ", length(to.delete),
            " variables?"
        )
    }
    if (!askForPermission(prompt)) {
        halt("Must confirm deleting variable(s)")
    }
    out <- lapply(unique(urls(to.delete)), crDELETE)
    dropCache(self(to.delete))
    invisible(refresh(dataset))
}

#' @rdname deleteVariables
#' @export
deleteVariable <- deleteVariables

#' Delete subvariables from an array
#'
#' Deleting variables requires confirmation. In an interactive session, you will be asked
#' to confirm. To avoid that prompt, or to delete subvariables from a
#' non-interactive session, wrap the call in [with_consent()] to give
#' your permission to delete.
#'
#' To delete the subvariables the function unbinds the array, deletes the subvariable, and
#' then binds the remaining subvariables into a new array.
#' @param variable the array variable
#' @param to.delete aliases (following `crunch.namekey.dataset`) or indices
#' of variables to delete.
#' @return a new version of variable without the indicated subvariables
#' @export
#' @seealso [deleteVariable()] [delete()]
deleteSubvariables <- function(variable, to.delete) {
    ## Identify subvariable URLs
    delete.these <- urls(variable[, to.delete])

    if (length(delete.these) == 1) {
        subvars <- subvariables(variable)
        subvar.urls <- urls(subvars)
        subvar.names <- names(subvars)
        prompt <- paste0(
            "Really delete ",
            dQuote(subvar.names[match(delete.these, subvar.urls)]), "?"
        )
    } else {
        prompt <- paste0(
            "Really delete these ", length(delete.these),
            " variables?"
        )
    }
    if (!askForPermission(prompt)) {
        halt("Must confirm deleting subvariable(s)")
    }

    lapply(delete.these, crDELETE)
    invisible(refresh(variable))
}

#' @rdname deleteSubvariables
#' @export
deleteSubvariable <- deleteSubvariables
