# Generics ---------------------------------------------------------------
# Generics for slides are located in the decks.R file

# Slide Catalog -----------------------------------------------------------

setMethod("initialize", "SlideCatalog", function(.Object, ...) {
    .Object <- callNextMethod()
    order <- crGET(.Object@orders$flat)
    order <- unlist(order$graph)
    .Object@index <- .Object@index[match(order, names(.Object@index))]
    return(.Object)
})

#' @rdname describe-catalog
#' @export
setMethod("names", "SlideCatalog", function(x) titles(x))
#' @rdname describe-catalog
#' @export
setMethod("names<-", "SlideCatalog", function(x, value) titles(x) <- value)

#' @rdname deck-titles
#' @export
setMethod("titles", "SlideCatalog", function(x) getIndexSlot(x, "title"))
#' @rdname deck-titles
#' @export
setMethod("titles<-", "SlideCatalog", function(x, value) {
    setIndexSlot(x, "title", value)
})

#' @rdname deck-titles
#' @export
setMethod("subtitles", "SlideCatalog", function(x) getIndexSlot(x, "subtitle"))
#' @rdname deck-titles
#' @export
setMethod("subtitles<-", "SlideCatalog", function(x, value) {
    setIndexSlot(x, "subtitle", value)
})

#' @rdname crunch-extract
#' @export
setMethod("[[", "SlideCatalog", function(x, i, ...) {
    getEntity(x, i, CrunchSlide)
})

#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("SlideCatalog", "numeric", "missing", "CrunchSlide"),
    function(x, i, j, value) {
        if (length(i) > 1) {
            # TODO, allow assignment of more than one slide
        }
        if (i > length(x) + 1) {
            # TODO what to do with missing slide entries
            i <- length(x) + 1
        }

        n_slides <- length(x)

        payload <- value@body[c("title", "subtitle")]
        anCat <- analyses(value)
        payload$analyses <- lapply(seq_along(anCat), function(i) {
            out <- anCat[[i]]
            out <- out@body[c("query", "query_environment", "display_settings")]
            out
        })
        payload <- wrapEntity(body = payload)
        crPOST(self(x), body = toJSON(payload))

        if (i < n_slides) {
            # You can't modify the contents of a slide by patching it
            # so we need to add the new slide, delete the original slide,
            # and reorder the slideCatalog.
            new_order <- moveLastElement(seq_len(n_slides + 1), i)
            reorderSlides(x, new_order)
            with_consent(delete(x[[length(x)]]))
        }
        invisible(refresh(x))
    }
)

#' Move and delete last element of a vector
#' This moves the last element of a vector to an index, then deletes the last
#' element, it is broken out for testing purposes
#'
#' @param v a vector
#' @param idx The index to move the last element to.
#'
#' @return a vector
#' @keywords internal
moveLastElement <- function(v, idx) {
    v[idx] <- v[length(v)]
    out <- v[1:(length(v) - 1)]
    return(out)
}

#' Reorder slides in a CrunchDeck
#'
#' @param x A SlideCatalog
#' @param order The numeric order for slides to be reordered to.
#'
#' @return A SlideCatalog
reorderSlides <- function(x, order) {
    url <- paste0(self(x), "flat")
    payload <- crGET(url)
    payload$graph <- payload$graph[order]
    crPATCH(url, body = toJSON(payload))
    return(refresh(x))
}

# CrunchSlide -------------------------------------------------------------------

# TODO: Find out what the mandatory display settings should be for the app then
# change this list to reflect those settings.
DEFAULT_DISPLAY_SETTINGS <- list(
    percentageDirection = "colPct",
    showEmpty = FALSE,
    showMean = FALSE,
    vizType = "table",
    countsOrPercents = "percent",
    decimalPlaces = 1L,
    showSignif = TRUE,
    currentTab = 0L
)

#' Append a new slide to a Crunch Deck
#'
#' @param deck A Crunch Deck
#' @param query A formula definition of a query to be used by the slide. This is
#' similar to CrunchCube query
#' @param display_settings (optional) A list of display settings, if om
#' @param title The slide's title
#' @param subtitle The slide's subtitle
#' @param ... Further options to be passed on to the API
#'
#' @return CrunchSlide object
#' @export
#'
#' @examples
#' \dontrun{
#' newSlide(
#'     main_deck,
#'     ~ cyl + wt,
#'     title = "Cyl and Weight",
#'     subtitle = "2017 Data"
#' )
#' }
newSlide <- function(deck,
                     query,
                     display_settings = list(),
                     title = "",
                     subtitle = "",
                     ...) {
    # TODO allow newSlide to accept list of formulas. In order for this to work
    # we need to send the analysis order. The only current use case for multiple
    # analyses is Profiles, and those probably shouldn't be set from R anyway.
    stopifnot(inherits(query, "formula"))
    ds <- loadDataset(datasetReference(deck))
    query <- list(query)

    settings <- modifyList(DEFAULT_DISPLAY_SETTINGS, display_settings)
    settings <- wrapDisplaySettings(settings)

    payload <- list(title = title, subtitle = subtitle, ...)
    payload[["analyses"]] <- lapply(query, function(x) {
        return(list(
            query = formulaToCubeQuery(x, ds),
            display_settings = settings
        ))
    })
    payload <- wrapEntity(body = payload)
    url <- crPOST(shojiURL(deck, "catalogs", "slides"), body = toJSON(payload))
    return(CrunchSlide(crGET(url)))
}

#' @rdname crunch-extract
#' @export
setMethod("[[", "CrunchSlide", function(x, i, ...) {
    an_cat <- analyses(x)
    return(an_cat[[i]])
})
#' @rdname crunch-extract
#' @export
setMethod("[[<-", "CrunchSlide", function(x, i, j, value) {
    an_cat <- analyses(x)
    an_cat[[i]] <- value
    invisible(refresh(x))
})

#' @rdname deck-titles
#' @export
setMethod("title", "CrunchSlide", function(x) {
    return(x@body$title)
})

#' @rdname deck-titles
#' @export
setMethod("title<-", "CrunchSlide", function(x, value) {
    setEntitySlot(x, "title", value)
})

#' @rdname deck-titles
#' @export
setMethod("subtitle", "CrunchSlide", function(x) {
    return(x@body$subtitle)
})

#' @rdname deck-titles
#' @export
setMethod("subtitle<-", "CrunchSlide", function(x, value) {
    setEntitySlot(x, "subtitle", value)
})

#' @rdname analysis-methods
#' @export
setMethod("analyses", "CrunchSlide", function(x) {
    AnalysisCatalog(crGET(shojiURL(x, "catalogs", "analyses")))
})

#' @rdname analysis-methods
#' @export
setMethod("analysis", "CrunchSlide", function(x) {
    out <- AnalysisCatalog(crGET(shojiURL(x, "catalogs", "analyses")))
    return(out[[1]])
})

#' @rdname analysis-methods
#' @export
setMethod("analysis<-", c("CrunchSlide", "formula"), function(x, value) {
    analyses[[1]] <- value
    return(invisible(x))
})

#' @rdname analysis-methods
#' @export
setMethod("query<-", "CrunchSlide", function(x, value) {
    analysis <- analyses(x)[[1]]
    query(analysis) <- value
    return(invisible(x))
})

#' @rdname analysis-methods
#' @export
setMethod("cubes", "CrunchSlide", function(x) cubes(analyses(x)))
#' @rdname analysis-methods
#' @export
setMethod("cube", "CrunchSlide", function(x) cube(analyses(x)[[1]]))
#' @rdname display-settings
#' @export
setMethod("displaySettings", "CrunchSlide", function(x) displaySettings(analyses(x)))
#' @rdname display-settings
#' @export
setMethod("displaySettings<-", "CrunchSlide", function(x, value) {
    an_cat <- analyses(x)
    displaySettings(an_cat) <- value
    return(invisible(x))
})

# AnalysisCatalog --------------------------------------------------------------

setMethod("initialize", "AnalysisCatalog", function(.Object, ...) {
    .Object <- callNextMethod()
    if (length(.Object@index) > 1) {
        order <- crGET(.Object@orders$order)
        order <- unlist(order$graph)
        .Object@index <- .Object@index[match(order, names(.Object@index))]
    }
    return(.Object)
})

#' @rdname crunch-extract
#' @export
setMethod("[[", "AnalysisCatalog", function(x, i, ...) {
    getEntity(x, i, Analysis)
})
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("AnalysisCatalog", "numeric", "missing", "formula"),
    function(x, i, j, value) {
        if (i > length(x)) {
            halt(
                "Index out of bounds, you can only assign a formula to an ",
                "existing analysis."
            )
        }
        analysis <- x[[i]]
        query(analysis) <- value
        invisible(refresh(x))
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("AnalysisCatalog", "numeric", "missing", "Analysis"),
    function(x, i, j, value) {
        if (length(i) > 1) {
            # TODO, recurse through i
        }

        if (i > length(x) + 1) {
            # TODO what to do with adding an analysis that's not the next one.
        }
        payload <- value@body[c("query", "display_settings", "query_environment")]
        payload <- wrapEntity(body = payload)
        if (i <= length(x)) {
            url <- names(x@index)[i]
            crPATCH(url, body = toJSON(payload))
        } else {
            crPOST(self(x), body = toJSON(payload))
        }
        invisible(refresh(x))
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("AnalysisCatalog", "numeric", "missing", "list"),
    function(x, i, j, value) {
        all_fmla <- vapply(value, function(x) inherits(x, "formula"), logical(1))
        if (any(!all_fmla)) {
            halt("Entry ", which(!all_fmla), " is not a formula")
        }
        if (length(i) != length(value)) {
            noun <- if (length(i) == 1) " analysis." else " analyses."
            halt(
                "Invalid assignment. You tried to assign ", length(value),
                " formulas to ", length(i), noun
            )
        }
        mapply(function(analysis, fmla) {
            analysis <- fmla
        }, analysis = x[[i]], fmla = value)
    }
)

#' @rdname analysis-methods
#' @export
setMethod("cubes", "AnalysisCatalog", function(x) {
    lapply(seq_along(x@index), function(i) cube(x[[i]]))
})

#' @rdname display-settings
#' @export
setMethod("displaySettings", "AnalysisCatalog", function(x) {
    settings_list <- lapply(seq_along(x), function(i) {
        displaySettings(x[[i]])
    })
    if (length(settings_list) == 1) {
        return(settings_list[[1]])
    } else {
        return(settings_list)
    }
})
#' @rdname display-settings
#' @export
setMethod("displaySettings<-", c("AnalysisCatalog", "list"), function(x, value) {
    analyses <- lapply(seq_along(x), function(i) x[[i]])
    lapply(analyses, function(x) displaySettings(x) <- value)
})


# Analysis ----------------------------------------------------------------
#' @rdname analysis-methods
#' @export
setMethod("query<-", c("Analysis", "formula"), function(x, value) {
    ds <- loadDataset(datasetReference(x))
    payload <- list(query = formulaToCubeQuery(value, data = ds))
    payload <- wrapEntity(body = payload)
    crPATCH(self(x), body = toJSON(payload))
    invisible(refresh(x))
})

#' @rdname analysis-methods
#' @export
setMethod("cube", "Analysis", function(x) {
    CrunchCube(crGET(
        cubeURL(x),
        query = list(query = toJSON(x@body$query)),
        filter = toJSON(x@body$query_environment$filter)
    ))
})
#' @rdname display-settings
#' @export
setMethod("displaySettings", "Analysis", function(x) {
    lapply(x@body$display_settings, function(x) x$value)
})
#' @rdname display-settings
#' @export
setMethod("displaySettings<-", "Analysis", function(x, value) {
    settings <- modifyList(displaySettings(x), value)
    settings <- wrapDisplaySettings(settings)
    payload <- list(display_settings = settings)
    payload <- wrapEntity(body = payload)
    crPATCH(self(x), body = toJSON(payload))
    invisible(refresh(x))
})

# This processes a names list of display setting values to the form that is
# required by the API.
wrapDisplaySettings <- function(settings) {
    return(lapply(settings, function(x) list(value = x)))
}
