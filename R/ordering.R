#' Get and set VariableOrder
#'
#' The \code{ordering} methods allow you to get and set a
#' \code{\link{VariableOrder}} on
#' a \code{\link{CrunchDataset}} or on the \code{\link{VariableCatalog}} that
#' the dataset contains.
#'
#' @param x a VariableCatalog or CrunchDataset
#' @param value a valid VariableOrder object
#' @return \code{ordering} returns a VariableOrder object, while
#' \code{ordering<-} sets the VariableOrder in \code{value} on \code{x}
#' @name ordering
#' @aliases ordering ordering<-
NULL

#' @rdname ordering
#' @export
setMethod("ordering", "CrunchDataset", function (x) ordering(variables(x)))

#' @rdname ordering
#' @export
setMethod("ordering<-", "CrunchDataset", function (x, value) {
    ordering(x@variables) <- value
    return(x)
})

#' @rdname ordering
#' @export
setMethod("ordering", "VariableCatalog", function (x) {
    out <- x@order
    out@catalog_url <- self(x)
    return(out)
})

#' @rdname ordering
#' @export
setMethod("ordering<-", "VariableCatalog", function (x, value) {
    stopifnot(inherits(value, "VariableOrder"))

    if (!identical(ordering(x)@graph, value@graph)) {
        ## Validate.
        bad.entities <- setdiff(urls(value), urls(x))
        if (length(bad.entities)) {
            halt("Variable URL", ifelse(length(bad.entities) > 1, "s", ""),
                " referenced in Order not present in catalog: ",
                serialPaste(bad.entities))
        }

        order_url <- shojiURL(x, "orders", "hier")
        ## Update on server
        crPUT(order_url, body=toJSON(value))
        ## Refresh
        x@order <- VariableOrder(crGET(order_url))
    }
    duplicates(x@order) <- duplicates(value)
    return(x)
})

#' @rdname ordering
#' @export
setMethod("ordering", "DatasetCatalog", function (x) {
    out <- DatasetOrder(crGET(shojiURL(x, "orders", "order")))
    out@catalog_url <- self(x)
    return(out)
})

#' @rdname ordering
#' @export
setMethod("ordering<-", "DatasetCatalog", function (x, value) {
    stopifnot(inherits(value, "DatasetOrder"))

    if (!identical(ordering(x)@graph, value@graph)) {
        ## Validate.
        bad.entities <- setdiff(urls(value), urls(x))
        if (length(bad.entities)) {
            halt("Dataset URL", ifelse(length(bad.entities) > 1, "s", ""),
                " referenced in Order not present in catalog: ",
                serialPaste(bad.entities))
        }
        ## Update on server
        crPUT(shojiURL(x, "orders", "order"), body=toJSON(value))
    }
    return(x)
})


# #' Move a variable to after another variable
# #'
# #' @param x the variable group that `after` is in
# #' @param value the variable or dataset subset you would like to move
# #' @param after the variable you want to precede `value`
# #' @return returns a variable group
# #' @examples
# #' \dontrun{
# #' ordering(ds)[['Demographics']] <- moveToAfter(ordering(ds)[['Grp A']], ds['age'], ds$educ)
# #' }
# moveToAfter <- function (x, value, after) {
#     if (!inherits(after, "OrderGroup")) {
#         after <- urls(after)
#     }
#     if (!inherits(value, "OrderGroup")) {
#         value <- urls(value)
#     }
#     whi <- ifelse(inherits(after, "OrderGroup"), which(names(x) %in% name(after)), which(entities(x) %in% after))
#     if ((inherits(value, "OrderGroup") && !name(value) %in% names(x)) || (!inherits(value, "OrderGroup") && !value %in% entities(x))) entities(x) <- c(entities(x), value)
#     whi2 <- ifelse(inherits(value, "OrderGroup"), which(names(x) %in% name(value)), which(entities(x) %in% value))
#     entities(x) <- entities(x)[c(setdiff(1:whi, whi2), whi2, setdiff((whi+1):length(entities(x)), whi2))]
#     return(x)
# }

#' Copy the order from one dataset to another.
#'
#' @param source the dataset you want to copy the order from
#' @param target the dataset you want to copy the order to
#' @return returns an object of class [`VariableOrder`] (which can be assigned
#' to a dataset with [`ordering`])
#' @examples
#' \dontrun{
#' ordering(ds) <- copyOrder(ds1, ds)
#' }
#' @export
copyOrder <- function (source, target) {
    if (!is.dataset(source) | !is.dataset(target)) {
        halt("Both source and target must be Crunch datasets.")
    }

    ord <- entities(ordering(source))

    # make url and alias maps
    url_to_alias_source <- structure(aliases(variables(source)), .Names=urls(variables(source)))
    alias_to_url_target <- structure(urls(variables(target)), .Names=aliases(variables(target)))

    new_ord <- lapply(ord, copyOrderGroup,
                      source_map = url_to_alias_source,
                      target_map = alias_to_url_target)

    # drop any null entities, those that were not found in target but in source
    new_ord <- removeMissingEntities(new_ord)

    return(do.call(VariableOrder, new_ord))
}

#' Copy the order of a `VariableGroup` (or individual variable URL) from `VariableOrder`
#'
#' @param group the group or variable URL to be copied
#' @param source_map url to alias map for source variables
#' @param target_map alias to url map for target variables
#' @return returns either a [`VariableGroup`] (if a group is supplied) or a URL (if just a variable URL is supplied)
#' @keywords internal
copyOrderGroup <- function (group, source_map, target_map) {
    # if there is a single element in group, and it is a character,
    # just return the URL in the target.
    if (length(group) == 1 & is.character(group)) {
        al <- source_map[group]
        if (al %in% names(target_map)) {
            return(target_map[[al]])
        } else {
            return(NA)
        }
    }

    # there are groups, so recurse
    ents <- lapply(entities(group), copyOrderGroup,
                           source_map = source_map, target_map = target_map)

    # drop any na entities - those that were not found in target but in source
    ents <- removeMissingEntities(ents)

    return(VariableGroup(name(group), ents))
}

# given a URL, return the alias from the dataset.
aliasFromURL <- function(url, ds) {
    return(aliases(allVariables(ds))[match(url, urls(allVariables(ds)))])
}
