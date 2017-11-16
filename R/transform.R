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

    trans_out <- Transforms(insertions = Insertions(data=trans$insertions),
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
setMethod("transforms<-", c("CrunchVariable", "ANY"), function (x, value) {
    frmt <- wrapEntity("view" = list("transform" = value))
    crPATCH(self(x), body=toJSON(frmt))
    dropCache(cubeURL(x))
    invisible(x)
})

#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchVariable", "NULL"), function (x, value) {
    frmt <- wrapEntity("view" = list("transform" = emptyObject()))
    crPATCH(self(x), body=toJSON(frmt))
    dropCache(cubeURL(x))
    invisible(x)
})

setValidity("Transforms", function (object) {
    one_of_names <- c("insertions", "categories", "elements") %in% names(object)
    if (!any(one_of_names)) {
        val <- paste("Transforms must have at least one of", serialPaste(dQuote(c("insertions", "categories", "elements")), "or"))
    } else {
        val <- TRUE
    }

    if (!is.null(object[["insertions"]]) && !is.insertions(object[["insertions"]])) {
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


#' Test if an abstract category object is of a conceptual type
#'
#' Convenience functions to test if an abstract category (AbsCat) object is a
#' specific type. These types are defined by the properties of the abstract
#' category object.
#'
#' `is.abscat.subtotal` is `x` a subtotal insertion?
#' `is.abscat.heading` is `x` a heading insertion?
#' `is.abscat.category` is `x` a category?
#'
#' @param x an AbsCat object
#'
#' @return logical if the AbsCat object is
#'
#' @name AbsCat-type-tests
#' @keywords internal
NULL

#' @rdname AbsCat-type-tests
#' @export
is.abscat.subtotal <- function (x) {
    if (is.AbsCats(x)) {
        return(!(is.na(funcs(x))) & funcs(x) == 'subtotal')
    }

    return(!(is.na(func(x))) & func(x) == 'subtotal')
}

#' @rdname AbsCat-type-tests
#' @export
is.abscat.heading <- function (x) {
    if (is.AbsCats(x)) {
        return(is.na(funcs(x)) & !is.na(anchors(x)))
    }

    return(is.na(func(x)) & !is.na(anchor(x)))
}

#' @rdname AbsCat-type-tests
#' @export
is.abscat.category <- function (x) {
    # if the class has already been specified, use that.
    if (!is.null(x$class)) {
        return(x$class == "Category")
    }

    # Otherwise, check if the properties of x look like a category, since
    # value is sometimes legitamately NA, just check id, missing, and name
    if (is.AbsCats(x)) {
        all <- !is.na(ids(x)) & !is.na(is.na(x)) & !is.na(names(x))
    }  else {
        all <- !is.na(id(x)) & !is.na(is.na(x)) & !is.na(name(x))
    }

    return(all)
}
