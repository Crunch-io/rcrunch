#' Transformations of variable and cube views
#'
#' Transformations allow you to change how a variable or cube is displayed
#' without changing the underlying data.
#'
#' @section Getting transformations:
#' The `transforms(x)` methods can be used with Variables and CrunchCubes to get
#' what transformations are currently set. For variables, they return a single
#' `Transforms` object that includes all transformations for the variable. For
#' CrunchCubes, it returns a named list with the same length as the number of
#' dimensions of the cube with each dimension's transformations.
#'
#' Currently, [Insertions] (e.g. [Subtotal()][SubtotalsHeadings] and
#' [Heading()][SubtotalsHeadings]) are the only type of transformations that are
#' supported.
#'
#' @section Setting transformations on a variable:
#' The `transforms(x) <- value` methods can be used to assign transformations
#' for a specific variable. `value`
#' must be a `Transforms` object. This allows you to set transformations on
#' categorical variables. These transformations will automatically show up in
#' any new CrunchCubes that contain this variable.
#'
#' @section Setting transformations on a CrunchCube:
#' The `transforms(x) <- value` methods can also be used to assign
#' transformations to a CrunchCube that has already been calculated. `value`
#' must be a named list of `Transforms` objects. The names of this list must
#' correspond to dimensions in the cube (those dimensions correspondences are
#' matched based on variable aliases). You don't have to provide an entry for
#' each dimension, but any dimension you do provide will be overwritten fully.
#'
#' @section Removing transformations:
#' To remove transformations from a variable or CrunchCube, use
#' `transforms(x) <- NULL`.
#'
#' @param data For the constructor function `Transforms` you can either pass in
#' attributes via `...` or you can create the objects with a fully defined
#' `list` representation of the objects via the `data` argument. See the examples.
#' @param ... For the constructor function `Transforms` you can pass
#' in attributes via `...`
#' @param x For the attribute getters and setters, an object of class
#' Transforms
#' @param i For `[`, the index of the Transforms to select (or insert for <- methods)
#' @param j Not used
#' @param value For `[<-`, the replacement Transforms to insert
#' @name Transforms
#' @aliases transforms transforms<-
NULL

getTransforms <- function(x) {
    var_entity <- entity(x)
    trans <- var_entity@body$view$transform
    if (is.null(trans) || length(trans) == 0) {
        return(NULL)
    }

    if (is.null(trans$insertions) || length(trans$insertions) == 0) {
        inserts <- NULL
    } else {
        inserts <- Insertions(data = trans$insertions)
        # subtype insertions so that Subtotal, Heading, etc. are their rightful
        # selves
        inserts <- subtypeInsertions(inserts)
    }

    # TODO: when other transforms are implemented, this should check those too.

    # if insertions are null return NULL
    if (is.null(inserts)) {
        return(NULL)
    }

    trans_out <- Transforms(
        insertions = inserts,
        categories = NULL,
        elements = NULL
    )
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
setMethod("transforms<-", c("CrunchVariable", "Transforms"), function(x, value) {
    # ensure that the value is all insertions, and only insertions
    value$insertions <- lapply(value$insertions, makeInsertion,
        var_categories = categories(x)
    )

    frmt <- wrapEntity("view" = list("transform" = value))
    crPATCH(self(x), body = toJSON(frmt))
    dropCache(cubeURL(x))
    return(invisible(x))
})

#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchVariable", "NULL"), function(x, value) {
    frmt <- wrapEntity("view" = list("transform" = emptyObject()))
    crPATCH(self(x), body = toJSON(frmt))
    dropCache(cubeURL(x))
    return(invisible(x))
})

setValidity("Transforms", function(object) {
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

#' Show transformations on a Crunch object
#'
#' `showTransforms([variable])` shows a summary of a categorical variable that
#' has transforms with the transforms calculated and applied. This is useful to
#' see what kind transforms exist before including the variable in a CrunchCube.
#'
#' `showTransforms([CrunchCube])` shows the CrunchCube with all transforms
#' calculated and applied. This is the default display method for cubes, so
#' should not be frequently needed.
#'
#' In both cases, an array is returned that includes the values of both the
#' underlying data (either category counts or CrunchCube cell values) as well as
#' the transformations applied.
#'
#' @param x a Categorical variable or CrunchCube
#'
#' @return summary of the variable, or the full CrunchCube with transforms applied
#'
#' @name showTransforms
#' @aliases showTransforms
#'
#' @examples
#' \dontrun{
#' showTransforms(ds$variable)
#' }
NULL

#' @rdname showTransforms
#' @export
setMethod("showTransforms", "CategoricalVariable", function(x) {
    tab <- table(x)

    remove_these <- names(categories(x)[is.na(categories(x))])

    tab <- as.array(calcTransforms(
        tab,
        insert_funcs =  makeInsertionFunctions(
            categories(x),
            transforms(x),
            cats_in_array = names(tab)
        )
    ))

    tab <- tab[!(names(tab) %in% remove_these)]
    styles <- transformStyles(transforms(x), categories(x)[!is.na(categories(x))])

    out <- prettyPrint2d(tab, row_styles = styles)
    cat(unlist(out), sep = "\n")
    return(invisible(tab))
})
