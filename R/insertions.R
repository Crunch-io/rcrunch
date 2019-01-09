#' Insert categories in transformations
#'
#' Insertions allow you to insert new categories into a categorical-like
#' response on a variable's [transformations][Transforms].
#'
#' @section Working with Insertions:
#' Insertions are used to add information about a variable or CrunchCube that
#' extends the data in the dataset but does not alter it. This new data
#' includes: aggregations like [subtotals][SubtotalsHeadings] that sum the count
#' of more than on category together or [headings][SubtotalsHeadings] which can
#' be added between categories.
#'
#' `Insertions` objects are containers for individual `Insertion` objects. The
#' individual `Insertion`s contain all the information needed to calculate,
#' apply, and display insertions to CrunchCubes and categorical variables.
#'
#' An `Insertion` must have two properties:
#' * `anchor` - which is the id of the category the insertion should follow
#' * `name` - the string to display
#'
#' Additionally, `Insertions` may also have the following two properties (though
#' if they have one, they must have the other):
#' * `function` - the function to use to aggregate (e.g. "subtotal")
#' * `args` - the category ids to use as operands to the `function` above.
#'
#' Although it is possible to make both subtotals and headings using `Insertion`
#' alone, it is much easier and safer to use the functions
#' [Subtotal()][SubtotalsHeadings] and [Heading()][SubtotalsHeadings] instead.
#' Not only are they more transparent, they also are quicker to type, accept
#' both category names as well as ids, and have easier to remember argument
#' names.
#'
#' @param data For the constructor functions `Insertion` and
#' `Insertions`, you can either pass in attributes via `...` or you
#' can create the objects with a fully defined `list` representation of
#' the objects via the `data` argument. See the examples.
#' @param x For the attribute getters and setters, an object of class
#' Insertion or Insertions
#' @param ... additional arguments to `[`, ignored
#' @param value For `[<-`, the replacement Insertion to insert
#' @param var_categories categories (from [categories()]) to used by the
#' `arguments` and `anchor` methods when needed to translate between category
#' names and category ids.
#' @name Insertions
#' @aliases anchor anchor<- anchors func func<- funcs arguments arguments<-
NULL


is.Insertion <- function(x) inherits(x, "Insertion")

# check insertion validity only when a user creates an Insertion so that the
# child classes like Subtotal and Heading aren't also checked
insertionValidity <- function(object) {
    val <- TRUE

    reqs <- c("anchor", "name")
    mems <- reqs %in% names(object)

    if (!all(mems)) {
        val <- paste0(
            "An Insertion must have at least ",
            serialPaste(dQuote(reqs)), ". Missing: ",
            serialPaste(dQuote(reqs[!mems]))
        )
    }

    if (!is.na(func(object)) && all(is.na(arguments(object)))) {
        # add checking so that args is either a vectorOrList of numerics
        val <- paste0(
            "If an Insertion has a ", dQuote("function"),
            " it must also have ", dQuote("args")
        )
    }

    if (val != TRUE) {
        halt("invalid class ", dQuote("Insertion"), " object: ", val)
    }
}

is.Insertions <- function(x) inherits(x, "Insertions")

setValidity("Insertions", function(object) {
    are.inserts <- vapply(object, is.Insertion, logical(1))
    if (!all(are.inserts)) {
        badcount <- sum(!are.inserts)
        return(paste0(
            "Invalid insertions: ", badcount,
            ifelse(badcount > 1,
                " elements are not Crunch Insertion objects.",
                " element is not a Crunch Insertion object."
            )
        ))
    }
    if (any(duplicated(names(object)))) {
        return("Invalid insertion names: must be unique")
    }
    return(TRUE)
})

validateNewAnchor <- function(anchor) {
    if (is.whole(anchor)) {
        return(anchor)
    }

    if (is.character(anchor) && (anchor == "top" | anchor == "bottom")) {
        return(anchor)
    }

    halt(
        "an anchor must be a numeric or the character ", dQuote("top"),
        " or ", dQuote("bottom")
    )
}

setAnchor <- function(x, value) {
    x[["anchor"]] <- validateNewAnchor(value)
    return(x)
}

setAfter <- function(x, value) {
    # if the value is top/bottom set position instead of after
    if (value %in% c("top", "bottom")) {
        x[["position"]] <- value
        value <- NULL
    } else {
        x[["position"]] <- "relative"
    }

    x[["after"]] <- value
    return(x)
}

validateNewSubtotal <- function(comb) {
    if (!is.numeric(comb)) {
        halt("a subtotal must be a numeric")
    }
    return(comb)
}

setSubtotal <- function(x, value) {
    x[["args"]] <- validateNewSubtotal(value)
    x[["function"]] <- "subtotal"
    return(x)
}

############################################
## Insertion methods
############################################

#' @rdname Insertions
#' @export
setMethod("anchor<-", "Insertion", setAnchor)

#' @rdname Insertions
#' @export
setMethod("anchor<-", "Subtotal", setAfter)

#' @rdname Insertions
#' @export
setMethod("anchor<-", "Heading", setAfter)

#' @rdname Insertions
#' @export
setMethod("anchor<-", "SummaryStat", setAfter)

#' @rdname Insertions
#' @export
setMethod("subtotals<-", "Insertion", setSubtotal)

#' @rdname Insertions
#' @export
setMethod("arguments<-", "Insertion", function(x, value) {
    # TODO: validate that the arguments are valid
    x[["args"]] <- value
    return(x)
})

#' @rdname Insertions
#' @export
setMethod("arguments<-", "Subtotal", function(x, value) {
    # TODO: validate that the arguments are valid
    x[["categories"]] <- value
    return(x)
})

#' @rdname Insertions
#' @export
setMethod("arguments<-", "Heading", function(x, value) {
    halt("Cannot set arguments on Headings.")
})


#' @rdname Insertions
#' @export
setMethod("arguments<-", "SummaryStat", function(x, value) {
    # TODO: validate that the arguments are valid
    x[["categories"]] <- value
    return(x)
})

#' @rdname Insertions
#' @export
setMethod("arguments", "Insertion", function(x) {
    if (all(is.null(x[["args"]]))) {
        return(NA)
    }
    return(x[["args"]])
})

# method for getting an anchor from a user-friendly abstracted Subtotal or Heading
.convertArgs <- function(x, var_categories) {
    if (!is.null(x$categories) && is.character(x$categories)) {
        # TODO: better error if var is null
        n <- ids(var_categories[x$categories])
    } else {
        n <- x$categories
    }
    return(n)
}

#' @rdname Insertions
#' @export
setMethod("arguments", "Subtotal", .convertArgs)

#' @rdname Insertions
#' @export
setMethod("arguments", "Heading", function(x) NA)

#' @rdname Insertions
#' @export
setMethod("arguments", "SummaryStat", function(x, var_categories) {
    if (is.null(x[["categories"]])) {
        # if var_categories is not provided, return the string all this should
        # only happen when showing insertion objects and not when calculating
        if (missing(var_categories)) {
            return("all")
        }

        x[["categories"]] <- ids(var_categories)
    }

    # grab the arguments from the call so that we can optionally pass var_cats
    .args <- as.list(match.call()[-1])
    return(do.call(.convertArgs, .args))
})

#' @rdname Insertions
#' @export
setMethod("anchor", "Insertion", function(x) {
    n <- x[["anchor"]]
    return(ifelse(is.null(n), NA_integer_, n))
})

# method for getting an anchor from a user-friendly abstracted Subtotal or Heading
.convertAnchor <- function(x, var_categories) {
    if (x$position != "relative") {
        return(x$position)
    }

    # If after is null (and position is relative) we default to setting after to
    # be the last category in the Subtotal category.
    if (is.null(x$after)) {
        if (missing(var_categories)) {
            # we don't have the variable this insertion will attach to, so we
            # can't determine which is the last category to use as the anchor.
            # This will be filled in when this insertion is added to a variable
            # (either on the server or in a cube)
            message(
                "Can't determine the anchor position without a ",
                "variable. However, when this is added to a Crunch ",
                "variable or CrunchCube it will follow the last ",
                "category given"
            )
            return(NA_integer_)
        }
        if (is.numeric(x$categories)) {
            var_cats <- ids(var_categories)
        } else {
            var_cats <- names(var_categories)
        }
        sub_cats <- x$categories[x$categories %in% var_cats]
        ordered_cats <- sub_cats[order(match(sub_cats, var_cats))]
        x$after <- rev(ordered_cats)[1]
    }

    # map chars/nums to ids
    if (is.character(x$after)) {
        # TODO: better error handling if var_categories is null
        n <- ids(var_categories[x$after])
    } else {
        n <- x$after
    }
    return(ifelse(is.null(n), NA_integer_, n))
}

#' @rdname Insertions
#' @export
setMethod("anchor", "Subtotal", .convertAnchor)

#' @rdname Insertions
#' @export
setMethod("anchor", "Heading", .convertAnchor)

#' @rdname Insertions
#' @export
setMethod("anchor", "SummaryStat", .convertAnchor)

#' @rdname Insertions
#' @export
setMethod("func", "Insertion", function(x) {
    f <- x[["function"]]
    return(ifelse(is.null(f), NA_character_, f))
})

#' @rdname Insertions
#' @export
setMethod("func", "Subtotal", function(x) return("subtotal"))

#' @rdname Insertions
#' @export
setMethod("func", "Heading", function(x) return(NA))

#' @rdname Insertions
#' @export
setMethod("func", "SummaryStat", function(x) return(x[["stat"]]))

############################################
## Insertions methods
############################################

#' @rdname Insertions
#' @export
setMethod("anchors", "Insertions", function(x) {
    # Since anchors can be either integers or characters top/bottom we can't use
    # vapply(..., integer(1)) or vapply(..., character(1)) here
    f <- unlist(lapply(x, anchor))
    return(f)
})

#' @rdname Insertions
#' @export
setMethod("funcs", "Insertions", function(x) {
    f <- vapply(x, func, character(1))
    return(f)
})
