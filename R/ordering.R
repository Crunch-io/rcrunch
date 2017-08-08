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

#' Move a variable to after another variable
#'
#' @param x the variable group that `after` is in
#' @param value the variable or dataset subset you would like to move
#' @param after the variable you want to precede `value`
#' @return returns a variable group
#' @examples
#' \dontrun{
#' ordering(ds)[['Demographics']] <- moveToAfter(ordering(ds)[['Grp A']], ds['age'], ds$educ)
#' }
#' @export
moveToAfter <- function(x, value, after){
    if (!inherits(after, "OrderGroup")) {
        after <- urls(after)
    }
    if (!inherits(value, "OrderGroup")) {
        value <- urls(value)
    }
    whi <- ifelse(inherits(after, "OrderGroup"), which(names(x) %in% name(after)), which(entities(x) %in% after))
    if ((inherits(value, "OrderGroup") && !name(value) %in% names(x)) || (!inherits(value, "OrderGroup") && !value %in% entities(x))) entities(x) <- c(entities(x), value)
    whi2 <- ifelse(inherits(value, "OrderGroup"), which(names(x) %in% name(value)), which(entities(x) %in% value))
    entities(x) <- entities(x)[c(setdiff(1:whi, whi2), whi2, setdiff((whi+1):length(entities(x)), whi2))]
    return(x)
}

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
copyOrder <- function(source, target){
    if (!is.dataset(source) | !is.dataset(target)) {
        halt("Both source and target must be Crunch datasets.")
    }

    ord <- entities(ordering(source))
    new_ord <- lapply(ord, copyOrderGroup,  source = source, target = target)

    # TODO: copy other VariableOrder features?

    return(do.call(VariableOrder, new_ord))
}

#' Copy the order of a `VariableGroup` (or individual variable url) from `VariableOrder`
#'
#' @param gr the group or variable url to be copied
#' @param source the dataset you want to copy the order from
#' @param target the dataset you want to copy the order to
#' @return returns either a [`VariableGroup`] (if a group is supplied) or a url (if just a variable url is supplied)
#' @examples
#' \dontrun{
#' ordering(ds) <- copyOrder(ds1, ds)
#' }
#' @keywords internal
copyOrderGroup <- function(gr, source, target) {
    # if there is a single element in gr, and it is a character,
    # just return the URL in the target.
    if (length(gr) == 1 & is.character(gr)) {
        al <- aliasFromURL(gr, source)
        return(urls(target[[al[al %in% aliases(allVariables(target))]]]))
    }

    # make mask of which elements of gr are groups and which are variables
    groups <- vapply(gr, class, character(1)) == "VariableGroup"
    # make an empty list the same length as gr
    ents <- vector("list", length(gr))

    # deal with non-groups (if there are no non-groups, nothing will happen here)
    als <- aliasFromURL(entities(gr[!groups]), source)
    ents[!groups] <- urls(target[als[als %in% aliases(allVariables(target))]])

    # deal with groups (if there are no groups, nothing will happen here)
    ents[groups] <- lapply(entities(gr[groups]), copyOrderGroup,  source = source, target = target)

    return(VariableGroup(name(gr), ents))
}

# given a url, return the alias from the dataset.
aliasFromURL <- function(url, ds) {
    return(aliases(allVariables(ds))[match(url, urls(allVariables(ds)))])
}
