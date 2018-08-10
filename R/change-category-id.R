#' Change the id of a category for a categorical variable
#'
#' Changes the id of a category from an existing value to a new one. The
#' variable can be a categorical, categorical array, or multiple response
#' variable. The category changed will have the same numeric value and missing
#' status as before. The one exception to this is if the numeric value is the
#' same as the id, then the new numeric value will be the same as the new id.
#'
#' It is highly recommended to disable any exclusion filter before using
#' `changeCategoryID`, especially if it is being called multiple times in quick
#' succession (e.g. as part of an automated script). If a problematic exclusion
#' is encountered `changeCategoryID` will attempt to disable and re-enable the
#' exclusion, but that process will be repeated for every call made which could
#' have adverse consequences (not to mention slow down processing time).
#'
#' @param variable the variable in a crunch dataset that will be changed (note:
#'   the variable must be categorical, categorical array, or multiple response)
#' @param from the (old) id identifying the category you want to change
#' @param to the (new) id for the category
#' @return `variable` with category `from` and all associated data values mapped
#'   to id `to`
#' @examples
#' \dontrun{
#' ds$country <- changeCategoryID(ds$country, 2, 6)
#' }
#' @export
changeCategoryID <- function(variable, from, to) {
    if (!has.categories(variable)) {
        halt("The variable ", name(variable), " doesn't have categories.")
    }

    if (!is.numeric(from) & length(from) == 1) {
        halt("from should be a single numeric")
    }

    if (!is.numeric(to) & length(to) == 1) {
        halt("to should be a single numeric")
    }

    pos.from <- match(from, ids(categories(variable)))
    if (is.na(pos.from)) {
        halt("No category with id ", from)
    }

    if (to %in% ids(categories(variable))) {
        halt("Id ", to, " is already a category, please provide a new category id.")
    }

    ## Add new category
    newcat <- categories(variable)[[pos.from]]
    # if the old id matches the old numeric value, likely the user wants these
    # to be the same, so change the new numeric value to be the same as the
    # new id.
    if (newcat$id == newcat$numeric_value %||% FALSE) {
        newcat$numeric_value <- to
    }
    newcat$id <- to

    names(categories(variable))[pos.from] <- "__TO_DELETE__"
    categories(variable) <- c(categories(variable), newcat)

    ## Move data to that new id
    moveTheData <- function(variable, from, to) {
        if (is.Categorical(variable)) {
            variable[variable == from] <- to
        } else if (is.Array(variable)) {
            # If the variable is an array, then lapply over the subvariables
            # TODO: change iteration over shojicatalogs to allow iterating over the variable directly
            lapply(names(variable), function(subvarname) {
                variable[[subvarname]][variable[[subvarname]] == from] <- to
            })
        }
        return(variable)
    }
    moveTheData(variable, from, to)

    ## Delete old category
    keep <- seq_along(categories(variable))
    keep[pos.from] <- length(keep)
    keep <- keep[-length(keep)]
    tryCatch(categories(variable) <- categories(variable)[keep],
        error = function(e) {
            msg <- conditionMessage(e)
            if (grepl("Cannot delete categories", msg)) {
                # If dropping the to-be-dropped category failed, it could
                # be because there's an exlcusion. If so unset it, or else
                # the updated values below will not include all rows
                ds <- loadDataset(datasetReference(variable))
                if (!is.null(exclusion(ds))) {
                    old_exclusion <- exclusion(ds)
                    exclusion(ds) <- NULL
                    on.exit(exclusion(ds) <- old_exclusion)
                } else {
                    # if there is no exclusion, we don't have an easy fix,
                    # so return the error
                    stop(e)
                }

                # warn the user what's up
                warning(
                    "Temporarily disabling the exclusion while ",
                    "changing category IDs. See `?changeCategoryID` ",
                    "for more information."
                )

                # retry the move and category update without the exclusion
                moveTheData(variable, from, to)
                categories(variable) <- categories(variable)[keep]
            } else {
                # if the error was not that a category couldn't be deleted,
                # we don't have an easy fix, so return the error.
                stop(e)
            }
        }
    )

    invisible(variable)
}
