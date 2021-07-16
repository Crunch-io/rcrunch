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
#' @param var_items categories (from [categories()]) or subvariables (from
#' [subvariables()] to used by the `arguments` and `anchor` methods when
#' needed to translate between category/subvariable names and category
#' ids/aliases.
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

    if (
        !is.na(func(object)) &&
        all(is.na(arguments(object))) &&
        length(subtotalTerms(object)) == 0
    ) {
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

setMethod("subtotalTerms", "Insertion", function(x) {
    return(x$kwargs)
})

# method for getting an anchor from a user-friendly abstracted Subtotal or Heading
.convertArgs <- function(x, var_items) {
    if (!missing(var_items) && inherits(var_items, "Subvariables")) {
        # don't need to get args for MR insertions
        return(NULL)
    }

    if (!is.null(x$categories) && is.character(x$categories)) {
        if (missing(var_items)) {
            halt(
                "Cannot convert insertion arguments from categories if `var_items`",
                " argument is not provided"
            )
        }
        n <- ids(var_items[x$categories])
    } else {
        n <- x$categories
    }
    return(n)
}

#' @rdname Insertions
#' @export
setMethod("arguments", "Subtotal", .convertArgs)

setMethod("subtotalTerms", "Subtotal", function(x, var_items, alias) {
    # We can distinguish between categorical insertions and MR insertions
    # by checking if the var_items are categories or subvariables
    # It's not ideal to default to the categories version, but since
    # we always have `var_items` when we're about to send to the server
    # the only times we don't have it aren't very important (eg printing)
    if (missing(var_items) || is.AbstractCategories(var_items)) {
        categoricalSubtotalTerms(x, var_items)
    } else {
        subvariableSubtotalTerms(x, var_items, alias)
    }
})

categoricalSubtotalTerms <- function(x, var_items) {
    # We will be migrating to having both `positive` and `negative` terms in the kwargs
    # so grab the positive terms
    # x here is a `Subtotal` object, which is not structured the same as the JSON,
    # it has a `categories` object
    positive <- .convertArgs(x, var_items)

    if (!is.null(x$negative) && is.character(x$negative)) {
        if (missing(var_items)) {
            halt(
                "Cannot convert insertion arguments from negative if `var_items`",
                " argument is not provided"
            )
        }
        negative <- ids(var_items[x$negative])
    } else {
        negative <- x$negative
    }

    list(positive = list(positive), negative = negative)
}

subvariableSubtotalTerms <- function(x, var_items, alias) {
    # If all subvar_items match aliases, assume they are aliases. Otherwise check if
    # they are all subvar names and use that
    subvariable_ids <- x$categories # Stored as categories because of legacy
    if (!all(subvariable_ids %in% aliases(var_items))) {
        matched <- match(subvariable_ids, names(var_items))
        if (any(is.na(matched))) {
            non_alias <- subvariable_ids[!(subvariable_ids %in% aliases(var_items))]
            if (length(non_alias) > 0) {
                non_alias <- paste0("\n - ", paste0(dQuote(non_alias), collapse = ", "), " don't match any aliases ")
            } else {
                non_alias <- ""
            }
            non_name <- subvariable_ids[!(subvariable_ids %in% aliases(var_items))]
            if (length(non_name) > 0) {
                non_name <- paste0("\n - ", paste(dQuote(non_name), collapse = ", "), " don't match any names")
            } else {
                non_name <- ""
            }

            halt("`subvariable_ids` must be all aliases or all names, but:", non_alias, non_name)
        }
        subvariable_ids <- aliases(var_items)[matched]
    }

    # Use variable if provided in the insertion, otherwise assume it's what we're inserting into
    alias <- x$variable %||% alias
    list(variable = alias, subvariable_ids = subvariable_ids)
}

#' @rdname Insertions
#' @export
setMethod("arguments", "Heading", function(x) NA)

#' @rdname Insertions
#' @export
setMethod("arguments", "SummaryStat", function(x, var_items) {
    if (is.null(x[["categories"]])) {
        # if var_items is not provided, return the string all this should
        # only happen when showing insertion objects and not when calculating
        if (missing(var_items)) {
            return("all")
        }

        x[["categories"]] <- ids(var_items)
    }

    # grab the arguments from the call so that we can optionally pass var_cats
    .args <- list(x = x)
    if (!missing(var_items)) .args[["var_items"]] <- var_items
    return(do.call(.convertArgs, .args))
})

#' @rdname Insertions
#' @export
setMethod("anchor", "Insertion", function(x, ...) {
    n <- x[["anchor"]]
    if (is.null(n)) NA_integer_ else n
})

# method for getting an anchor from a user-friendly abstracted Subtotal or Heading
# `var_items` can be either categories (regular subtotals) or subvariables
# for MR insertions
.convertAnchor <- function(x, var_items) {
    if (x$position != "relative") {
        return(x$position)
    }

    # If after (&before) is null (and position is relative) we default to setting after to
    # be the last category in the Subtotal category/subvariable
    if (is.null(x$after) && is.null(x$before)) {
        if (missing(var_items)) {
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
        if (is.categories(var_items)) {
            if (is.numeric(x$categories)) {
                var_cats <- ids(Filter(is.category, var_items))
            } else {
                var_cats <- names(Filter(is.category, var_items))
            }
            sub_cats <- x$categories[x$categories %in% var_cats]
            ordered_cats <- sub_cats[order(match(sub_cats, var_cats))]
            x$after <- rev(ordered_cats)[1]
        } else {
            if (all(x$categories %in% aliases(var_items))) {
                subvars <- aliases(var_items)
            } else {
                subvars <- names(var_items)
            }
            sub_subvars <- x$categories[x$categories %in% subvars]
            ordered_subvars <- sub_subvars[order(match(sub_subvars, subvars))]
            x$after <- rev(ordered_subvars)[1]
        }
    }

    # Can distinguish between category insertions and MR insertions by whether
    # we were passed in categories or subvariables as the var_items
    # If no var_items were passed, treat as categorical
    if (missing(var_items)) {
        if (is.character(x$after)) {
            halt("Could not find anchor without categories/subvariables passed in.")
        }
        return(x$after)
    } else if (is.AbstractCategories(var_items)) {
        var_items <- Filter(is.category, var_items) # Can't use other subtotals as anchors
        # map chars/nums to ids
        if (is.character(x$after)) {
            # TODO: better error handling if var_items is null
            n <- ids(var_items[x$after])
        } else {
            n <- x$after
        }
        if (is.null(n)) return(NA_integer_) else return(n)
    } else {
        position <- if (!(is.null(x$after) || is.na(x$after))) "after" else "before"
        subvar <- if (position == "after") x$after else x$before

        if (!subvar %in% aliases(var_items)) {
            if (subvar %in% names(var_items)) {
                subvar <- aliases(var_items)[match(subvar, names(var_items))]
            } else {
                halt("Could not find anchor `", subvar, "` in subvariable aliases or names.")
            }
        }
        return(list(position = position, alias = subvar))
    }
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
