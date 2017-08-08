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
#' ordering(ds)[['Demographics']] <- moveToAfter(ordering(ds)[['Group A']], ds[c('Age', 'gender')], ds$educ)
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
#' @param outer to make it recursive
#' @return currently returns a vector for do.call(VariableOrder, )  but should return a reordered dataset
#' @examples
#' \dontrun{
#' ord <- copyOrder(ds1, ds)
#' ordering(ds) <- do.call(VariableOrder, ord)
#' }
#' @export
copyOrder <- function(source, target, outer=NULL){
    if (is.null(outer)) { outer <- ordering(source) }
    ents <- c()
    if (is.character(unlist(entities(outer)))) {
        als <- aliases(allVariables(source))[match(unlist(entities(outer)), urls(allVariables(source)))]
        ents <- urls(allVariables(target))[match(als[als %in% aliases(allVariables(target))], aliases(allVariables(target)))]
    } else {
        for (gr in entities(outer)){
            if (is.character(gr)){
                al <- aliases(allVariables(source))[match(gr, urls(allVariables(source)))]
                if (al %in% aliases(allVariables(target))) {
                    ents <- c(ents, self(target[[al]]))
                }
            } else ents <- c(ents, copyOrder(source, target, gr))
        }
    }
    if (length(ents) > 0 & !is.null(name(outer))) { return(VariableGroup(name(outer), ents)) }
    if (length(ents) > 0) {
        return(do.call(VariableOrder, ents))
        }
}

copyVariableGroup <- function(source, target) {

}
