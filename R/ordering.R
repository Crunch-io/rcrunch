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


copyOrder <- function(ds_old, ds_new, ord_old=NULL){
    if (is.null(ord_old)) ord_old <- ordering(ds_old)
    ents <- c()
    print(name(ord_old))
    if (class(unlist(entities(ord_old))) == 'character') {
        als <- aliases(allVariables(ds_old))[match(unlist(entities(ord_old)), urls(allVariables(ds_old)))]
        ents <- urls(allVariables(ds_new))[match(als[als %in% aliases(allVariables(ds_new))], aliases(allVariables(ds_new)))]
    } else{
        for (gr in entities(ord_old)){
            if (class(gr) == 'character'){
                al <- aliases(allVariables(ds_old))[match(gr, urls(allVariables(ds_old)))]
                if (al %in% aliases(allVariables(ds_new))) ents <- c(ents, self(ds_new[[al]]))
            } else ents <- c(ents, steal_order(ds_old, ds_new, gr))
        }
    }
    if (length(ents) > 0 & !is.null(name(ord_old))) return(VariableGroup(name(ord_old), ents))
    if (length(ents) > 0) return(ents)
}
