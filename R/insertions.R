#' Insert categories in transformations
#'
#' Insertions allow you to insert new categories into a categorical-like
#' response on a variable's [transform](Transforms).
#'
#' @param data For the constructor functions `Insertion` and
#' `Insertions`, you can either pass in attributes via `...` or you
#' can create the objects with a fully defined `list` representation of
#' the objects via the `data` argument. See the examples.
#' @param x For the attribute getters and setters, an object of class
#' Insertion or Insertions
#' @param ... additional arguments to `[`, ignored
#' @param value For `[<-`, the replacement Insertion to insert
#' @param var_categories categories (from [categories()]) to used by `args` and 
#' `anchor` methods when needed to translate between category names and category
#' ids.
#' @name Insertions
#' @aliases anchor anchor<- anchors func func<- funcs args args<-
NULL


is.Insertion <- function (x) inherits(x, "Insertion")

# check insertion validity only when a user creates an Insertion so that the 
# child classes like Subtotal and Heading aren't also checked
insertionValidity <- function (object) {
    val <- TRUE

    reqs <- c("anchor", "name")
    mems <- reqs %in% names(object)
    
    if (!all(mems)) {
        val <- paste0("An Insertion must have at least ",
                      serialPaste(dQuote(reqs)), ". Missing: ",
                      serialPaste(dQuote(reqs[!mems])))
    }

    if (!is.na(func(object)) && is.na(args(object))) {
        # add checking so that args is either a vectorOrList of numerics
        val <- paste0("If an Insertion has a ", dQuote("function"),
                      " it must also have ", dQuote("args"))
    }
    
    if (val != TRUE) {
        halt("invalid class ", dQuote("Insertion"), " object: ", val)
    }
}

is.Insertions <- function (x) inherits(x, "Insertions")

setValidity("Insertions", function (object) {
    are.inserts <- vapply(object, is.Insertion, logical(1))
    if (!all(are.inserts)) {
        badcount <- sum(!are.inserts)
        return(paste0("Invalid insertions: ", badcount,
                      ifelse(badcount>1,
                             " elements are not Crunch Insertion objects.",
                             " element is not a Crunch Insertion object.")))
    }
    if (any(duplicated(names(object)))) {
        return("Invalid insertion names: must be unique")
    }
    return(TRUE)
})

validateNewAnchor <- function (anchor) {
    if (!is.whole(anchor)) {
        halt("an anchor must be a numeric")
    }

    return(anchor)
}

setAnchor <- function (x, value) {
    x[["anchor"]] <- validateNewAnchor(value)
    return(x)
}

validateNewSubtotal <- function (comb) {
    if (!is.numeric(comb)) {
        halt("a subtotal must be a numeric")
    }
    return(comb)
}

setSubtotal <- function (x, value) {
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
setMethod("subtotals<-", "Insertion", setSubtotal)

#' @rdname Insertions
#' @export
setMethod("args", "Insertion", function (x) {
    if (all(is.null(x[["args"]]))) {
        return(NA)
    }
    return(x[["args"]])
})

# method for getting an anchor from a user-friendly abstracted Subtotal or Heading
.convertArgs <- function (x, var_categories) {
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
setMethod("args", "Subtotal", .convertArgs)

#' @rdname Insertions
#' @export
setMethod("args", "Heading", .convertArgs)

#' @rdname Insertions
#' @export
setMethod("anchor", "Insertion", function (x) {
    n <- x[["anchor"]]
    return(ifelse(is.null(n), NA_integer_, as.integer(n)))
})

# method for getting an anchor from a user-friendly abstracted Subtotal or Heading
.convertAnchor <- function (x, var_categories) {
    # map chars/nums to ids
    if (is.character(x$after)) {
        # TODO: better error if var is null
        n <- ids(var_categories[x$after])
    } else {
        n <- x$after
    }
    return(ifelse(is.null(n), NA_integer_, as.integer(n)))
}

#' @rdname Insertions
#' @export
setMethod("anchor", "Subtotal", .convertAnchor)

#' @rdname Insertions
#' @export
setMethod("anchor", "Heading", .convertAnchor)

#' @rdname Insertions
#' @export
setMethod("func", "Insertion", function (x) {
    f <- x[["function"]]
    return(ifelse(is.null(f), NA_character_, f))
})

#' @rdname Insertions
#' @export
setMethod("func", "Subtotal", function (x) return("subtotal"))

#' @rdname Insertions
#' @export
setMethod("func", "Heading", function (x) return(NULL))

############################################
## Insertions methods
############################################

#' @rdname Insertions
#' @export
setMethod("anchors", "Insertions", function (x) {
    f <- vapply(x, anchor, integer(1))
    return(f)
})

#' @rdname Insertions
#' @export
setMethod("funcs", "Insertions", function (x) {
    f <- vapply(x, func, character(1))
    return(f)
})
