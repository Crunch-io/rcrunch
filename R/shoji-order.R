.initEntities <- function (x, url.base=NULL, group.class="VariableGroup", entity.class="ShojiObject") {
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

##' @export
as.list.ShojiOrder <- function (x, ...) x@graph

##' @export
as.list.OrderGroup <- function (x, ...) x@entities

##' Length of an Order
##' @param x a ShojiOrder
##' @return Integer: the number of elements in the Order
##' @name ShojiOrder-length
NULL

##' @rdname ShojiOrder-length
##' @export
setMethod("length", "ShojiOrder", function (x) length(entities(x)))
