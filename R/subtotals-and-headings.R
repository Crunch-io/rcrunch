#' Subtotals and headings
#'
#' Subtotals and headings for categorical Variables and
#' CrunchCubes. These are especially useful for making aggregates across
#' multiple categories (sometimes referred to as _nets_, _top box_, or
#' _top 2 box_).
#'
#' To see the subtotals or headings set for a variable, use `subtotals(variable)`
#'
#' @section Adding Subtotals and Headings:
#' Subtotals and headings can be added either by passing a list of `Subtotal`s
#' or `Heading`s, or they can be added one at a time by passing `Subtotal` or
#' `Heading` to `subtotals(variable)` alone.
#'
#' Adding subtotals or headings is additive; meaning that subtotals or headings
#' that are already set on the variable are not removed when new subtotals or
#' headings are added. To remove all subtotals and headings, set
#' `subtotals(variable)` to `NULL`.
#'
#' To get an array of just the subtotal rows from a CrunchCube, use the function
#' `subtotalArray(CrunchCube)`.
#'
#' @inheritSection noTransforms Removing transforms
#'
#' @section Working with Subtotals and headings:
#' When interacting programmatically with Subtotals and Headings, it can be
#' useful to be able to tell if something is a Subtotal or a Heading. The `is.*`
#' family of methods are useful here: the singular versions (`is.Subtotal` and
#' `is.Heading`) take a single object and returns `TRUE` if the object is either
#'  a Subtotal or a Heading and `FALSE` if not; the plural versions
#'  (`are.Subtotals` and `are.Headings`) take a list of objects (including an
#'  `Insertions` object) and returns a vector of `TRUE`/`FALSE`s.
#'
#' @param x either a variable or CrunchCube object to add or get subtotal
#' transforms for, for `is.Subtotal()` and `is.Heading()` an object to test if
#' it is either a Subtotal or Heading
#' @param value For `[<-`, the replacement Subtotal to insert
#' @param name character the name of the subtotal or heading
#' @param categories character or numeric the category names or ids for subtotal
#' only
#' @param position character one of "relative", "top", or "bottom". Determines
#' the position of the subtotal or heading, either at the top, bottom, or
#' relative to another category in the cube (default).
#' @param after character or numeric if `position` is "relative", then the
#' category name or id to position the subtotal or heading after. If not supplied
#' this defaults to the last of the `categories` supplied to `Subtotal`.
#' @param negative character or numeric of the category names or ids to be subtracted
#' for subtotals only
#' @examples
#' \dontrun{
#' # given a variable ds$opinion, with categories: Strongly Agree, Somewhat
#' # Agree, Neither Agree nor Disagree, Somewhat Disagree, and Strongly Disagree,
#' # to make two subtotals for Agree and Disagree:
#' subtotals(ds$opinion) <- list(
#'     Subtotal(
#'         name = "Agree",
#'         categories = c("Strongly Agree", "Somewhat Agree"),
#'         after = "Somewhat Agree"
#'     ),
#'     Subtotal(
#'         name = "Disagree",
#'         categories = c("Strongly Disagree", "Somewhat Disagree"),
#'         after = "Strongly Disagree"
#'     )
#' )
#'
#' # headings can also be added:
#' subtotals(ds$opinion) <- Heading(name = "All opinions", position = "top")
#'
#' # to see the subtotals and headings associated with a variable
#' subtotals(ds$opinion)
#' #        anchor         name     func    args
#' # 1      2        Agree subtotal 1 and 2
#' # 2      4     Disagree subtotal 4 and 5
#' # 3      0 All opinions     <NA>      NA
#'
#' # when you use a variable with subtotals and headings in a cube, you see them
#' # by default
#' opinion_cube <- crtabs(~opinion, ds)
#' opinion_cube
#' #               All opinions
#' #             Strongly Agree 23
#' #             Somewhat Agree 24
#' #                      Agree 47
#' # Neither Agree nor Disagree 18
#' #          Somewhat Disagree 16
#' #          Strongly Disagree 19
#' #                   Disagree 35
#'
#'
#' # to get just the subtotals,
#' subtotalArray(opinion_cube)
#' #    Agree Disagree
#' #       47       35
#'
#' # to remove all subtotals and headings
#' subtotals(ds$opinion) <- NULL
#' crtabs(~opinion, ds)
#' #             Strongly Agree 23
#' #             Somewhat Agree 24
#' # Neither Agree nor Disagree 18
#' #          Somewhat Disagree 16
#' #          Strongly Disagree 19
#'
#' # if you want to temporarily remove subtotals and headings, you can with `noTransforms`
#' noTransforms(crtabs(~opinion, ds))
#' #             Strongly Agree             Somewhat Agree Neither Agree nor Disagree
#' #                         23                         24                         18
#' #          Somewhat Disagree          Strongly Disagree
#' #                         16                         19
#' }
#'
#' @name SubtotalsHeadings
#' @aliases subtotals subtotals<-
NULL

#' @rdname SubtotalsHeadings
#' @export
Subtotal <- function(name,
                     categories = NULL,
                     position = c("relative", "top", "bottom"),
                     after = NULL,
                     before = NULL,
                     negative = NULL,
                     na.rm = TRUE,
                     id = NULL
) {
    if (is.null(categories) && is.null(negative)) {
        halt("Must specify at least one of categories or negative for a valid Subtotal")
    }
    # match.args position
    position <- match.arg(position)
    validatePosition(position, after, before)

    subtotal_info <- list(
        name = name,
        categories = categories,
        position = position,
        after = after,
        before = before,
        negative = negative,
        na.rm = na.rm,
        id = id
    )
    # Remove NULLs
    if (is.null(subtotal_info$categories)) subtotal_info$categories <- NULL
    if (is.null(subtotal_info$negative)) subtotal_info$negative <- NULL

    return(new("Subtotal", subtotal_info))
}

validatePosition <- function(position, after, before = NULL) {
    # `before` is only supported in MR insertions for now
    if (position != "relative" && (!is.null(after) || !is.null(before))) {
        halt(
            "If position is not relative, you cannot supply a category id",
            " or name to the ", dQuote("after"), " or ", dQuote("before"), " arguments"
        )
    }

    if (!is.null(after) && !is.null(before)) {
        halt(
            "Cannot specify both the ", dQuote("after"), " and ", dQuote("before"),
            "arguments"
        )
    }
}

#' @rdname SubtotalsHeadings
#' @export
is.Subtotal <- function(x) inherits(x, "Subtotal") %||% FALSE
#' @rdname SubtotalsHeadings
#' @export
is.Heading <- function(x) inherits(x, "Heading") %||% FALSE

#' @rdname SubtotalsHeadings
#' @export
are.Subtotals <- function(x) unlist(lapply(x, inherits, "Subtotal")) %||% FALSE
#' @rdname SubtotalsHeadings
#' @export
are.Headings <- function(x) unlist(lapply(x, inherits, "Heading")) %||% FALSE

setMethod("initialize", "Subtotal", function(.Object, ...) {
    .Object <- callNextMethod()
    # unlist, to flatten user inputs like list(1:2)
    .Object$categories <- unlist(.Object$categories)
    return(.Object)
})

#' @rdname SubtotalsHeadings
#' @export
Heading <- function(name,
                    position = c("relative", "top", "bottom"),
                    after = NULL) {
    position <- match.arg(position)
    validatePosition(position, after)

    return(new("Heading", list(
        name = name,
        position = position,
        after = after
    )))
}

getSubtotals <- function(x) {
    inserts <- transforms(x)$insertions

    if (is.null(transforms(x)) || is.null(inserts)) {
        return(NULL)
    }
    # Treat MR insertions of "any"/"anynm" as Subtotals
    subtotal_functions <- c("subtotal", "any_selected", "any_non_missing_selected")
    sub_heads <- lapply(inserts, function(insrt) {
        if (is.null(insrt$`function`) || insrt$`function` %in% subtotal_functions) {
            return(insrt)
        }
        return(NULL)
    })
    # remove NULLs
    sub_heads@.Data <- Filter(Negate(is.null), sub_heads@.Data)

    return(sub_heads)
}

#' @rdname SubtotalsHeadings
#' @export
setMethod("subtotals", "CrunchVariable", getSubtotals)
#' @rdname SubtotalsHeadings
#' @export
setMethod("subtotals", "VariableTuple", getSubtotals)

#' @rdname SubtotalsHeadings
#' @export
setMethod("subtotals<-", c("CrunchVariable", "ANY"), function(x, value) {
    if (is.Insertion(value)) {
        # if the value is not a list, make it into one, in case we got a bare
        # Subtotal() or Heading()
        value <- list(value)
    }
    if (!is.MR(x)) {
        var_items <- categories(x)
    } else {
        var_items <- subvariables(x)
    }
    inserts <- Insertions(
        data = lapply(value, makeInsertion, var_items = var_items, alias = alias(x))
    )

    bd <- list("transform" = list("insertions" = inserts))
    # setEntitySlot manages old inserts so they are not duplicated
    ent <- setEntitySlot(entity(x), "view", bd)
    dropCache(cubeURL(x))
    return(invisible(x))
})

#' @rdname SubtotalsHeadings
#' @export
setMethod("subtotals<-", c("CrunchVariable", "NULL"), function(x, value) {
    # maintain any non-subtotal insertions
    old_inserts <- transforms(x)$insertions
    subtots <- subtotals(x)
    ## If assigning NULL but we already are NULL, there's nothing to do
    if (!is.null(subtots)) {
        inserts <- setdiff(old_inserts, subtots)

        bd <- list("transform" = list("insertions" = inserts))
        ent <- setEntitySlot(entity(x), "view", bd)
        dropCache(cubeURL(x))
    }
    return(invisible(x))
})

#' Convert a child class of Insertion into a proper Insertion
#'
#' The Crunch API expects that [Subtotals, Headings][SubtotalsHeadings], and
#' other insertions all have the same shape. Before sending insertions to the
#' server, we need to call `makeInsertion` on any insertions to make sure they
#' are proper `Insertion`s. This process sometimes requires a variable's
#' categories object (e.g. in order to convert from category names to category
#' ids)
#'
#' @param x an object the is a child of [Insertion][Insertions] (e.g.
#' `Subtotal`, `Heading`)
#' @param var_items Either categories (from `categories(variable)`) or subvariables
#' (from `subvariables(variable)`) to used by
#' `makeInsertions` to make `Insertions` from a child object (e.g. `Subtotal` or
#' `Heading`).
#' @param alias The alias of the variable the insertion is being added to, only
#' needed for MR insertions.
#'
#' @return an Insertion object
#'
#' @name makeInsertion
#' @aliases makeInsertion
#'
#' @keywords internal
NULL

#' @rdname makeInsertion
#' @export
setMethod("makeInsertion", "Subtotal", function(x, var_items, alias) {
    # Can distinguish between categorical and MR insertions by checking if
    # var_items are categories or subvariables
    if (is.AbstractCategories(var_items)) {
        func <- "subtotal"
    } else {
        func <- ifelse(x$na.rm, "any_non_missing_selected", "any_selected")
    }

    out <- .Insertion(
        anchor = anchor(x, var_items),
        name = name(x),
        `function` = func,
        args = arguments(x, var_items),
        kwargs = subtotalTerms(x, var_items, alias)
    )
    out$id <- x$id # Can be NULL in R, but don't send NULL to server, so add afterwards

    # Remove empty lists and ensure keys that should be lists are stored
    # that way even if it is a single element
    list_kws <- c("positive", "negative", "subvariable_ids")
    if (length(out$kwargs) == 0) {
        out$kwargs <- NULL
    } else {
        for (kw in names(out$kwargs)) {
            if (is.null(out$kwargs[[kw]])) {
                out$kwargs[[kw]] <- NULL
            } else if ((kw %in% list_kws) & length(out$kwargs[[kw]]) == 1) {
                out$kwargs[[kw]] <- as.list(out$kwargs[[kw]])
            }
        }
    }

    return(out)
})

#' @rdname makeInsertion
#' @export
setMethod("makeInsertion", "Heading", function(x, var_items, alias) {
    return(.Insertion(anchor = anchor(x, var_items), name = name(x)))
})

# makeInsertion(insertion) simply returns an insertion.
#' @rdname makeInsertion
#' @export
setMethod("makeInsertion", "Insertion", function(x, var_items, alias) return(x))


#' Convert from Insertion to Insertion subtypes
#'
#' Convert from Insertions with only Insertion objects (the only kind that are
#' available from the Crunch API) to the more user-friendly and user-facing
#' Subtotal and Heading object classes.
#'
#' @param inserts an `Insertions` object for `subtypeInsertions`
#' @param insert a single `Insertion` object for `subtypeInsertion`
#'
#' @return an `Insertions` object with members subtyped for `subtypeInsertions`
#' or a `Subtotal`, `Heading`, or `Insertion` for `subtypeInsertion`
#'
#' @name subtypeInsertion
#' @keywords internal
NULL

#' @rdname subtypeInsertion
#' @export
subtypeInsertions <- function(inserts) {
    if (!inherits(inserts, "Insertions")) {
        halt("Must provide an object of type Insertions")
    }
    return(lapply(inserts, subtypeInsertion))
}

#' @rdname subtypeInsertion
#' @export
subtypeInsertion <- function(insert) {
    if (!inherits(insert, "Insertion")) {
        halt("Must provide an object of type Insertion")
    }
    if (inherits(insert, c("Subtotal", "Heading", "SummaryStat"))) {
        # if the insert is already a sub class, return that.
        return(insert)
    }

    anch <- anchor(insert)
    if (is.list(anch)) {
        position <- "relative"
        after <- if (anch$position == "after") anch$alias
        before <- if (anch$position == "before") anch$alias
    } else if (anch %in% c("top", "bottom")) {
        after <- NULL
        before <- NULL
        position <- anch
    } else {
        after <- anch
        before <- NULL
        position <- "relative"
    }

    if (!(is.na(func(insert)))) {
        # there is a function, check the kind.
        if (func(insert) == "subtotal") {
            # we plan to migrate to having `kwargs$positive` instead
            # of `args`. Our migration plan is to have them be duplicated
            # to start, so for now trust the kwargs first
            kwargs <- subtotalTerms(insert) #nolint
            positive <- kwargs$positive %||% arguments(insert)
            insert <- Subtotal(
                name = name(insert), after = after,
                position = position, categories = positive,
                negative = kwargs$negative,
                id = id(insert)
            )
        } else if (func(insert) %in% c("any_selected", "any_non_missing_selected")) {
            # MR insertion
            kwargs <- subtotalTerms(insert) #nolint
            insert <- Subtotal(
                name = name(insert), after = after, before = before,
                position = position, variable = kwargs$variable,
                categories = kwargs$subvar_ids,
                id = id(insert)
            )
        } else if (func(insert) %in% names(summaryStatInsertions)) {
            # this is a summary statistic, make it so
            insert <- SummaryStat(
                name = name(insert),
                stat = func(insert),
                after = after,
                position = position,
                categories = arguments(insert),
                includeNA = insert$includeNA
            )
        }
    } else if (is.na(func(insert)) & !is.na(anchor(insert))) {
        # this is a heading, make it so
        insert <- Heading(
            name = name(insert), after = after,
            position = position
        )
    }
    # when all else fails, just return the insert as is
    return(insert)
}
