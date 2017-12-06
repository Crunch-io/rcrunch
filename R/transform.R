#' Transformations of variable and cube views
#'
#' Transformations allow you to change how a variable or cube is displayed
#' without changing the underlying data.
#'
#' @param data For the constructor function `Transforms` you can either pass in
#' attributes via `...` or you can create the objects with a fully defined
#' `list` representation of the objects via the `data` argument. See the examples.
#' @param ... For the constructor function `Transforms` you can pass
#' in attributes via `...`
#' @param x For the attribute getters and setters, an object of class
#' Transforms
#' @param value For `[<-`, the replacement Transforms to insert
#' @name Transforms
#' @aliases transforms transforms<-
NULL

getTransforms <- function (x) {
    var_entity <- entity(x)
    trans <- var_entity@body$view$transform

    if (is.null(trans) || length(trans) == 0) {
        return(NULL)
    }

    # get the insertions
    inserts <- Insertions(data=trans$insertions)
    # subtype insertions so that Subtotal, Heading, etc. are their rightful selves
    inserts <- subtypeInsertions(inserts)
    
    trans_out <- Transforms(insertions = Insertions(data=inserts),
                            categories = NULL,
                            elements = NULL)
    return(trans_out)
}

#' @rdname Transforms
#' @export
setMethod("transforms", "CrunchVariable", getTransforms)
#' @rdname Transforms
#' @export
setMethod("transforms", "VariableTuple", getTransforms)


#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchVariable", "Transforms"), function (x, value) {
    # ensure that the value is all insertions, and only insertions
    value$insertions <- lapply(value$insertions, makeInsertion, var = x)

    frmt <- wrapEntity("view" = list("transform" = value))
    crPATCH(self(x), body=toJSON(frmt))
    dropCache(cubeURL(x))
    return(invisible(x))
})

#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchVariable", "NULL"), function (x, value) {
    frmt <- wrapEntity("view" = list("transform" = emptyObject()))
    crPATCH(self(x), body=toJSON(frmt))
    dropCache(cubeURL(x))
    return(invisible(x))
})

setValidity("Transforms", function (object) {
    one_of_names <- c("insertions", "categories", "elements") %in% names(object)
    if (!any(one_of_names)) {
        val <- paste("Transforms must have at least one of", serialPaste(dQuote(c("insertions", "categories", "elements")), "or"))
    } else {
        val <- TRUE
    }

    if (!is.null(object[["insertions"]]) && !is.Insertions(object[["insertions"]])) {
        val <- "Transforms insertions element must be of class Insertions"
    }
    if (!is.null(object[["categories"]])) {
        val <- "Transforms categories element must be NULL"
    }
    if (!is.null(object[["elements"]])) {
        val <- "Transforms elements element must be NULL"
    }

    return(val)
})

#' Show the variable transformations on a Categorical variable
#'
#' @param x a Categorical variable
#'
#' @return summary of the variable, with transforms applied
#'
#' @aliases showTransforms
#'
#' @examples
#' \dontrun{
#' showTransforms(ds$variable)
#' }
#'
#' @export
setMethod("showTransforms", "CategoricalVariable", function (x) {
    tab <- table(x)
    tab <- as.array(calcTransforms(tab, transforms(x), categories(x)))

    remove_these <- names(categories(x)[is.na(categories(x))])
    tab <- tab[!(names(tab) %in% remove_these)]
    styles <- transformStyles(transforms(x), categories(x)[!is.na(categories(x))])

    out <- prettyPrint2d(tab, row_styles = styles)
    cat(unlist(out), sep="\n")
    return(invisible(tab))
})
