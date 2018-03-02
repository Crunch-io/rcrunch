#' @rdname showTransforms
#' @export
setMethod("showTransforms", "CrunchCube", function (x) {
    if (is.null(transforms(x)[[1]])) {
        print(cubeToArray(x))
        return(invisible(x))
    } else {
        appliedTrans <- applyTransforms(x)
        # if the row dimension is categorical, make styles
        if (index(variables(x))[[1]]$type == "categorical") {
            row_cats <- Categories(data=index(variables(x))[[1]]$categories)
            row_styles <- transformStyles(transforms(x)[[1]], row_cats[!is.na(row_cats)])            
        } else {
            # otherwise punt, because this is an array or MR var.
            row_styles <- NULL
        }

        if (length(dim(appliedTrans)) <= 2) {
            out <- prettyPrint2d(appliedTrans, row_styles = row_styles)
            cat(unlist(out), sep="\n")
        } else {
            # styling is hard
            print(appliedTrans)
        }

        return(invisible(appliedTrans))
    }
})

#' @rdname applyTransforms
#' @export
setMethod("subtotalArray", "CrunchCube", function(x, headings = FALSE) {
    includes <- c("subtotals")
    if (headings) {
        includes <- c(includes, "headings")
    }
    applyTransforms(x, include = includes)
})

#' Calculate the transforms from a CrunchCube
#'
#' `applyTransforms` calculates transforms (e.g. [Subtotals][SubtotalsHeadings]) on a CrunchCube.
#' Currently only the row transforms are supported. This is useful if you want
#' to use the values from the subtotals of the CrunchCube in an analysis.
#'
#' Including the `include` argument allows you to specify which parts of the
#' CrunchCube to return. The options can be any of the following: "cube_cells"
#' for the untransformed values from the cube itself, "subtotals" for the
#' subtotal insertions, and "headings" for any additional headings. Any
#' combination of these can be given, by default all will be given.
#'
#' `subtotalArray(cube)` is a convenience function that is equivalent to
#' `applyTransforms(cube, include = c("subtotals"))`
#'
#' @param x a CrunchCube
#' @param array an array to use, if not using the default array from the cube
#' itself. (Default: not used, pulls an array from the cube directly)
#' @param ... arguments to pass to `calcTransforms`, for example `include`
#' @param headings for `subtotalArray`: a logical indicating if the headings
#' should be included with the subtotals (default: `FALSE`)
#'
#' @return an array with any transformations applied
#'
#' @examples
#' \dontrun{
#' # to get an array of just the subtotals
#' subtotalArray(crtabs(~opinion, ds))
#' #    Agree Disagree
#' #       47       35
#'
#' # to get the full array with the subtotals but not headings
#' applyTransforms(crtabs(~opinion, ds), include = c("cube_cells", "subtotals"))
#' #             Strongly Agree             Somewhat Agree                      Agree
#' #                         23                         24                         47
#' # Neither Agree nor Disagree          Strongly Disagree                   Disagree
#' #                         18                         19                         35
#' #          Somewhat Disagree
#' #                         16
#' # to get the full array with the headings but not subtotals
#' applyTransforms(crtabs(~opinion, ds), include = c("cube_cells", "headings"))
#' #               All opinions             Strongly Agree             Somewhat Agree
#' #                         NA                         23                         24
#' # Neither Agree nor Disagree          Strongly Disagree          Somewhat Disagree
#' #                         18                         19                         16
#' }
#'
#' @aliases subtotalArray
#' @export
applyTransforms <- function (x, array = cubeToArray(x), ...) {
    # if any of the dimensions are subvariables, don't even attempt to calculate
    # transforms or insertions.
    if (any(unlist(lapply(variables(x), function(x) {
       x$type == "subvariable_items"
    })))) {
        return(array)
    }
    
    # if there are row transforms, calculate them and display them
    row_trans <- transforms(x)[[1]]
    if (!is.null(row_trans)) {
        var_cats <- Categories(data=index(variables(x))[[1]]$categories)

        # TODO: calculate category/element changes

        if (length(dim(array)) > 1) {
            off_margins <- seq_along(dim(array))[-1]
            dim_names  <- names(dimnames(array))
            array <- apply(array, off_margins, calcTransforms, row_trans, var_cats, ...)
            names(dimnames(array)) <- dim_names
        } else {
            array <- as.array(calcTransforms(array, row_trans, var_cats, ...))
        }

        array <- subsetTransformedCube(array, x)
    }

    # TODO: calculate column transforms
    return(array)
}

subsetTransformedCube <- function (array, cube) {
    # subset variable categories to only include non-na
    dims <- dim(array)
    keep_all <- lapply(seq_along(dims),
                       function (i) {
                           out <- rep(TRUE, dims[i])
                           names(out) <- dimnames(array)[[i]]
                           return(out)
                       })
    names(keep_all) <- names(dimensions(cube))[seq_along(keep_all)]
    keep_these_cube_dims <- evalUseNA(array, dimensions(cube)[seq_along(keep_all)], cube@useNA)

    # remove the categories determined to be removable above
    keep_these <- mapply(function(x, y) {
        not_ys <- y[!y]
        x[names(not_ys)] <- not_ys
        return(x)
    }, keep_all,
    keep_these_cube_dims,
    SIMPLIFY=FALSE,
    USE.NAMES=TRUE)

    # match up those in keep_these that are already in array
    array_dimnames <- dimnames(array)
    for (i in seq_along(array_dimnames)) {
        to_keep <- names(keep_these[[i]]) %in% array_dimnames[[i]]
        keep_these[[i]] <- keep_these[[i]][to_keep]
    }

    array <- subsetCubeArray(array, keep_these)

    return(array)
}

#' @rdname Transforms
#' @export
setMethod("transforms", "CrunchCube", function(x) { transforms(variables(x)) })

#' @rdname Transforms
#' @export
setMethod("transforms", "VariableCatalog", function (x) {
    transes <- lapply(x, function (i) {i$view$transform})
    
    if (all(unlist(lapply(transes, is.null)))) {
        return(NULL)
    }
    transes_out <- lapply(transes, function (i) {
        # TODO: when other transforms are implemented, this should check those too.

        # if insertions are null return NULL
        if (is.null(i$insertions) || length(i$insertions) == 0) {
            return(NULL)
        }
        
        # get the insertions
        inserts <- Insertions(data=i$insertions)
        # subtype insertions so that Subtotal, Heading, etc. are their rightful selves
        inserts <- subtypeInsertions(inserts)
        
        return(Transforms(insertions = inserts,
                          categories = NULL,
                          elements = NULL)
        )
    })
    
    names(transes_out) <- aliases(x)
    return(transes_out)
})

#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchCube", "list"), function (x, value) {
    dims <- dimensions(x)
    dimnames <- names(dims)
    
    # check if the names of the dimensions and the names of the transforms line up
    if (any(!(names(value) %in% dimnames))) {
        halt("The names of the transforms supplied (", 
             serialPaste(dQuote(names(value)))
             ,") to not match the dimension names (",
             serialPaste(dQuote(dimnames)) ,") of the cube.")
    }

    # replace the transforms for each dimension
    dims <- CubeDims(lapply(dimnames, function (dim_name) {
        dim_out <- dims[[dim_name]]
        if (dim_name %in% names(value)) {
            # grab the matching insertions, make sure they are proper Insertions
            # and then add them to the dimensions to return.
            one_trans <- value[[dim_name]]
            one_trans$insertions <- Insertions(
                data = lapply(one_trans$insertions, makeInsertion,
                              var_categories = categories(variables(x)[[dim_name]]))
            )
            dim_out$references$view$transform <- jsonprep(one_trans)
        }
        return(dim_out)
    }))
    
    # rename, replace the dimensions and return the cube
    names(dims) <- dimnames
    dimensions(x) <- dims
    return(invisible(x))
})

#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchCube", "NULL"), function (x, value) {
    dims <- dimensions(x)
    dimnames <- names(dims)
    dims <- CubeDims(lapply(dims, function (x) {
        if (!is.null(x$references$view$transform)) {
            x$references$view$transform <- value
        }
        return(x)
    }))
    names(dims) <- dimnames
    dimensions(x) <- dims
    return(invisible(x))
})

#' Remove transformations from a CrunchCube
#'
#' @section Removing transforms:
#' `noTransforms()` is useful if you don't want to see or use any transformations like 
#' Subtotals and Headings. This action only applies to the CrunchCube object in 
#' R: it doesn't actually change the variables on Crunch servers or the query 
#' that generated the CrunchCube.
#'
#' @param cube a CrunchCube
#'
#' @return the CrunchCube with no transformations
#'
#' @examples 
#' \dontrun{
#' # A CrunchCube with a heading and subtotals
#' crtabs(~opinion, ds)
#' #               All opinions   
#' #             Strongly Agree 23
#' #             Somewhat Agree 24
#' #                      Agree 47
#' # Neither Agree nor Disagree 18
#' #          Somewhat Disagree 16
#' #          Strongly Disagree 19
#' #                   Disagree 35
#' 
#' noTransforms(crtabs(~opinion, ds))
#' #             Strongly Agree             Somewhat Agree Neither Agree nor Disagree 
#' #                         23                         24                         18 
#' #          Somewhat Disagree          Strongly Disagree 
#' #                         16                         19 
#' }
#'
#' @export
noTransforms <- function (cube) {
    transforms(cube) <- NULL
    return(cube)
}

#' An experimental function to melt a crunch cube into a long dataframe
#'
#' As cubes become more complex, and especially as they involve several multiple
#' response variables, it gets increasingly difficult to reason about them. The
#' reason is that while crunch variables are one dimensional (adding a variable to
#' a cube adds one dimension), MR variables have two dimensions: the value of the selction
#' and whether that value is in fact selected. This means that when you add an MR
#' to a cube you are adding two dimensions, not one. However when we display these
#' cubes to the user we pretend that MRs are just like other variables. This means
#' we have to keep track of two high dimensional arrays. The one the user sees and the
#' one we program with.
#'
#' This function outputs a long data.frame with the same information as the cube.
#' Additional dimensions are represented by additional columns in the data frame.
#' This means that it's easier to feed the output into other packages which require
#' tabular data structures, and might make it easier to validate expectations because
#' you can approach the output just like a database.
#'
#' @param cube a Crunch Cube
#' @export
cubeToLongDF <- function(cube) {
    arr <- cube@arrays$count
    for (i in seq_along(dimnames(arr))) {
        #TODO need more robust way of identifying MR selections
        if (all( c("Selected", "Not Selected") %in% dimnames(arr)[[i]])) {
            names(dimnames(arr))[i] <- paste0(names(dimnames(arr))[i], "_select")
        }
    }
    return(arrayhelpers::array2df(arr, label.x = "value"))
}