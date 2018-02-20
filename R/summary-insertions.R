#' Summary insertions
#'
#' Just like [subtotals and headings](SubtotalsHeadings), summary statistics can
#' be inserted into cubes.
#'
#' @inheritSection noTransforms Removing transforms
#'
#' @param name character the name of the summary statistic
#' @param stat a function to calculate the summary (e.g. `mean` or `median`)
#' @param categories character or numeric the category names or ids to be
#' included in the summary statistic, if empty all categories
#' @param position character one of "relative", "top", or "bottom". Determines
#' the position of the subtotal or heading, either at the top, bottom, or
#' relative to another category in the cube (default).
#' @param includeNA should missing categories be included in the summary?
#' @param after character or numeric if `position` is "relative", then the
#' category name or id to position the subtotal or heading after
#' @param x for `is.SummaryStat()` only, an object to test if
#' it is a `SummaryStat` object
#'
#' @name SummaryStatInsertions
#' @aliases summaryStat
NULL

#' @rdname SummaryStatInsertions
#' @export
SummaryStat <- function (name,
                         stat,
                         categories = NULL,
                         position = c("relative", "top", "bottom"),
                         after = NULL,
                         includeNA = FALSE) {
    # match.args position
    position <- match.arg(position)
    validatePosition(position, after)

    if (!(stat %in% names(summaryStatInsertions))) {
        halt(dQuote(stat), " is not a known summary statistic for insertions. ",
             "Available stats are: ", serialPaste(names(summaryStatInsertions)))
    }

    return(new("SummaryStat", list(name = name,
                                   stat = stat,
                                   categories = categories,
                                   position = position,
                                   after = after,
                                   includeNA = includeNA)))
}

#' @importFrom stats weighted.mean
# a list of possible summary statistics to use as an insertion
meanInsert <- function (element, var_cats, vec, includeNA = FALSE) {
    # grab category combinations, and then sum those categories.
    combos <- unlist(arguments(element, var_cats))
    which.cats <- names(var_cats[ids(var_cats) %in% combos])
    num_values <- values(var_cats[which.cats])
    counts <- vec[which.cats]

    # TODO: do something smarter about NA categories that have counts / have
    # numeric values if a user cares
    ok <- !is.na(counts)
    return(weighted.mean(num_values[ok], counts[ok], includeNA = includeNA))
}

medianInsert <- function (element, var_cats, vec, includeNA = FALSE) {
    # grab category combinations, and then sum those categories.
    combos <- unlist(arguments(element, var_cats))
    which.cats <- names(var_cats[ids(var_cats) %in% combos])
    num_values <- values(var_cats[which.cats])
    counts <- vec[which.cats]

    # TODO: do something smarter about NA categories that have counts / have
    # numeric values if a user cares
    ok <- !is.na(counts)

    # weighted median function
    num_values <- num_values[ok]
    counts <- counts[ok]
    o <- order(num_values)
    num_values <- num_values[o]
    counts <- counts[o]
    perc <- cumsum(counts)/sum(counts)

    # if any of the bins are 0.5, return the mean of that and the one above it.
    if (any(perc == 0.5)) {
        n <- which(perc == 0.5)
        return((num_values[n]+num_values[n+1])/2)
    }

    # otherwise return the first bin that is more than 50%
    over0.5 <- which(perc > 0.5)
    return(num_values[min(over0.5)])
}

summaryStatInsertions <- list(
    "mean" = meanInsert,
    "median" = medianInsert
)

#' @rdname SummaryStatInsertions
#' @export
is.SummaryStat <- function (x) inherits(x, "SummaryStat") %||% FALSE

#' @rdname SummaryStatInsertions
#' @export
are.SummaryStats <- function (x) unlist(lapply(x, inherits, "SummaryStat")) %||% FALSE

setMethod("initialize", "SummaryStat", function (.Object, ...) {
    .Object <- callNextMethod()
    # unlist, to flatten user inputs like list(1:2)
    .Object$categories <- unlist(.Object$categories)
    return(.Object)
})

#' @rdname makeInsertion
#' @export
setMethod("makeInsertion", "SummaryStat", function (x, var_categories) {
    if (is.null(arguments(x))) {
        # if there are no arguments, use all category ids
        args <- ids(var_categories)
    } else {
        args <- arguments(x, var_categories)
    }

    return(.Insertion(anchor = anchor(x, var_categories),
                      name = name(x),
                      `function` = x$stat,
                      args = arguments(x, var_categories),
                      includeNA = x$includeNA))
})
