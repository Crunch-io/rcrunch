# Generics ---------------------------------------------------------------
#nolint start
#' Get and set slide analyses
#'
#' Slides are composed of analyses, which are effectively `CrunchCubes` with some
#' additional metadata. You can get and set a slide's Analysis Catalog with the
#' `analyses` method, and access an individual analysis with `analysis`. There
#' are also helpers to get and set the components of the analysis such as `filter()`,
#' `weight()`, `transforms()`, `displaySettings()` and `vizSpecs()`. You can also
#' get the `CrunchCube` from an analysis using `cube()`.
#'
#' For more complex objects like `displaySettings()`, `vizSpecs()` and `transforms()`,
#' the [API documentation](
#' https://crunch.io/api/reference/#patch-/datasets/-dataset_id-/decks/-deck_id-/slides/-slide_id-/analyses/-analysis_id-/)
#' provides more details.
#'
#' Advanced users of the API can assign a list to  `analysis<-` to specify settings
#' on the analyses that are not otherwise available in `rcrunch`. The helpers
#' `formulaToSlideQuery()` and `slideQueryEnv()` help you create objects for the
#' `query` and `query_environment`.
#'
#' @param x a `CrunchSlide`, `AnalysisCatalog`, or `Analysis`
#' @param value for the setter, an object to set it
#' @param query For `formulaToSlideQuery()`, a formula that specifies the query, as in
#' `newSlide()`. See Details of [`crtabs()`] for more information.
#' @param dataset For `formulaToSlideQuery()`, a `CrunchDataset` that the variables in
#' `query` refer to.
#' @param weight For `slideQueryEnv()` a crunch variable to use as a weight or `NULL`
#' to indicate no weight should be used.
#' @param filter for `slideQueryEnv()`, a `CrunchFilter` or `CrunchExpression` to filter
#' the slide.
#' @param ... ignored
#'
#' @rdname analysis-methods
#' @export
#' @examples
#' \dontrun{
#' # Examples of setting analysis details (in general these setters work on
#' # the slide, analysis catalog and analysis, but for brevity the examples only
#' # show on the slide)
#'
#' # Change the filter
#' filter(slide) <- NULL # to remove a filter
#' filter(slide) <- filters(ds)[["My filter"]]
#' filter(deck) <- filters(ds)[["My filter"]] # Can set the same filter on a whole deck too
#'
#' # Change the weight
#' weight(slide) <- NULL # to remove
#' weight(slide) <- ds$weight
#' weight(deck) <- ds$weight # Can set the same weight on a whole deck too
#'
#' # Change the transforms
#' transforms(slide) <- list(rows_dimension = makeDimTransform(hide = "Neutral"))
#'
#' # Change the displaySettings
#' displaySettings(slide) <- list(vizType = "groupedBarPlot")
#'
#' # Change the vizSpecs
#' # viz_specs can get quite long, see
#' # https://crunch.io/api/reference/#post-/datasets/-dataset_id-/decks/-deck_id-/slides/
#' vizSpecs(slide) <- viz_specs
#'
#' # Change the query
#' #' query(slide) <- ~ cyl + wt
#' }
#nolint end
setGeneric("analyses", function(x) standardGeneric("analyses"))
#' @rdname analysis-methods
#' @export
setGeneric("analysis", function(x) standardGeneric("analysis"))
#' @rdname analysis-methods
#' @export
setGeneric("analysis<-", function(x, value) standardGeneric("analysis<-"))
#' @rdname analysis-methods
#' @export
setGeneric("query<-", function(x, value) standardGeneric("query<-"))
#' @rdname analysis-methods
#' @export
setGeneric("cube", function(x) standardGeneric("cube"))
#' @rdname analysis-methods
#' @export
setGeneric("cubes", function(x) standardGeneric("cubes"))

#' @rdname analysis-methods
#' @export
setGeneric("displaySettings", function(x) standardGeneric("displaySettings"))
#' @rdname analysis-methods
#' @export
setGeneric("displaySettings<-", function(x, value) standardGeneric("displaySettings<-"))

#' @rdname analysis-methods
#' @export
setGeneric("vizSpecs", function(x) standardGeneric("vizSpecs"))
#' @rdname analysis-methods
#' @export
setGeneric("vizSpecs<-", function(x, value) standardGeneric("vizSpecs<-"))


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
        if (i <= length(x)) {
            # we are replacing a slide, so return quickly modifying in place
            out <- modifyCatalogInPlace(x, i, j, value)
            return(out)
        }

        n_slides <- length(x)

        payload <- value@body[c("title", "subtitle")]
        anCat <- analyses(value)
        payload$analyses <- lapply(seq_along(anCat), function(i) {
            out <- anCat[[i]]
            # TODO: Make sure API always sends all parts (and no legacy pre-viz_specs)
            # and update fixtures
            out <- out@body[
                na.omit(match(
                    c("query", "query_environment", "display_settings", "transform", "viz_specs"),
                    names(out@body)
                ))
            ]
            out
        })
        payload <- wrapEntity(body = payload)
        crPOST(self(x), body = toJSON(payload))

        if (i < n_slides) {
            # You can't modify the contents of a slide by patching it
            # so we need to add the new slide, delete the original slide,
            # and reorder the slideCatalog.
            # Is ^^^ really true?
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

#' @rdname describe-catalog
#' @export
setMethod("types", "SlideCatalog", function(x) {
    getIndexSlot(x, "type")
})

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
#' @param query A formula definition of a query to be used by the slide. See
#' Details of [`crtabs()`] for more information about making queries.
#' @param display_settings (optional) A list of display settings. If omitted,
#' slide will be a table of column percentages with hypothesis test highlighting
#' enabled. The most common setting used is `vizType`, which can be:
#' `table`, `groupedBarPlot`, `stackedBarPlot`, `horizontalBarPlot`,
#' `horizontalStackedBarPlot`, `donut`, and (if the second variable in the
#' query formula is a wave variable) `timeplot`.
#' In addition, `showValueLabels` (logical) controls whether the web app and
#' exports show labels on bars or arcs of donuts.
#' @param title The slide's title
#' @param subtitle The slide's subtitle
#' @param filter a `CrunchLogicalExpression`, a crunch `filter` object or
#' a vector of names of \code{\link{filters}} defined in the dataset (defaults
#' to `NULL`, using all data).
#' @param weight A weight variable (defaults to NULL, meaning no weight)
#' @param viz_specs Another set of options for the display of the slide, see
#' the [API documentation](
#' https://crunch.io/api/reference/#post-/datasets/-dataset_id-/decks/-deck_id-/slides/)
#' for more information.
#' @param transform A list of slide transformations, usually created using the function
#' [`makeDimTransform()`].
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
#'
#' # Grouped bar plot
#' newSlide(
#'     main_deck,
#'     ~ approval + age4,
#'     title = "Approval by age group",
#'     display_settings = list(
#'         vizType = "groupedBarPlot",
#'         showValueLabels = TRUE
#'     ),
#'     subtitle = "2017 Data"
#' )
#'
#' # Horizontal stacked bars
#' newSlide(
#'     main_deck,
#'     ~ approval + age4,
#'     title = "Approval by age group",
#'     display_settings = list(
#'         vizType = "horizontalStackedBarPlot"
#'     ),
#'     subtitle = "2017 Data"
#' )
#'
#' # A donut is only suitable for a single variable
#' newSlide(
#'     main_deck,
#'     ~ approval,
#'     title = "Approval of new feature",
#'     display_settings = list(
#'         vizType = "donut",
#'         showValueLabels = FALSE
#'     ),
#'     subtitle = "2017 Data"
#' )
#'
#' # A Grouped bar plot with slide transformations to hide a category
#' newSlide(
#'     main_deck,
#'     ~ approval + age4,
#'     title = "Approval by age group",
#'     display_settings = list(
#'         vizType = "groupedBarPlot",
#'         showValueLabels = TRUE
#'     ),
#'     transform = list(rows_dimension = makeDimTransform(hide = "Neutral")),
#'     subtitle = "2017 Data"
#' )
#'
#' # Example of advanced options being set:
#' # viz_specs can get quite long, see
#' # https://crunch.io/api/reference/#post-/datasets/-dataset_id-/decks/-deck_id-/slides/
#' viz_specs <- list(
#'     default = list(
#'         format = list(
#'             decimal_places = list(percentages = 0L, other = 2L),
#'             show_empty = FALSE
#'         )
#'     ),
#'     table = list(
#'         measures = c("col_percent", "pairwise_t_test"),
#'         page_layout = list(
#'             rows = list(
#'                 top = list(),
#'                 bottom = c("base_unweighted", "scale_mean", "significant_columns")
#'             ),
#'             measure_layout = "long"
#'         ),
#'         pairwise_comparison = list(sig_threshold = c(0.05, 0.01)),
#'         format = list(pval_colors = FALSE)
#'     )
#' )
#'
#' newSlide(
#'     main_deck,
#'     ~categories(fav_array)+subvariables(fav_array),
#'     display_settings = list(viz_type = list(value = "table")),
#'     title = "custom slide",
#'     filter = filters(ds)[[1]],
#'     weight = ds$weight,
#'     viz_specs = viz_specs
#' )
#'
#' # Can also specify `analyses` directly, which allows for very advanced use.
#' # `formulaToSlideQuery()` and `slideQueryEnv()` help describe the API
#' newSlide(
#'     main_deck,
#'     title = "custom slide",
#'     analyses = list(list(
#'         query = formulaToSlideQuery(~categories(fav_array)+subvariables(fav_array), ds),
#'         query_environment = slideQueryEnv(filter = filters(ds)[[1]]),
#'         display_settings = list(viz_type = list(value = "table")),
#'         viz_specs = viz_specs
#'     ))
#' )
#' }
newSlide <- function(
    deck,
    query = NULL,
    display_settings = list(),
    title = "",
    subtitle = "",
    filter = NULL,
    weight = NULL,
    viz_specs = NULL,
    transform = NULL,
    ...
) {
    stopifnot(inherits(query, "formula") || is.null(query))
    settings <- modifyList(DEFAULT_DISPLAY_SETTINGS, display_settings)
    settings <- wrapDisplaySettings(settings)

    ds <- loadDataset(datasetReference(deck))
    filter <- standardize_tabbook_filter(ds, filter)

    payload <- list(title = title, subtitle = subtitle, ...)
    check_newslide_args(
        query, display_settings, payload, filter, weight, viz_specs, transform
    )

    if (any(dimTransformNeedsPrep(transform))) {
        transform <- prepareDimTransforms(transform, query, ds) #nolint
    }

    if (!is.null(query)) {
        query <- formulaToCubeQuery(query, ds)
        validateSlideQuery(query)
        analysis <- list(
            query = query,
            display_settings = settings
        )

        query_environment <- list()
        if (!is.null(filter)) query_environment$filter <- filter
        if (!is.null(weight)) {
            query_environment$weight <- self(weight)
            # Also add to the query to match webapp behavior
            analysis$query$weight <- self(weight)
        }
        if (length(query_environment) > 0) analysis$query_environment <- query_environment
        if (!is.null(viz_specs)) {
            analysis$viz_specs <- viz_specs
        }
        if (!is.null(transform)) analysis$transform <- transform

        payload[["analyses"]] <- list(analysis)
    }

    payload <- wrapEntity(body = payload)
    url <- crPOST(shojiURL(deck, "catalogs", "slides"), body = toJSON(payload))
    return(CrunchSlide(crGET(url)))
}


#nolint start
check_newslide_args <- function(
    query, display_settings, payload, filter, weight, viz_specs, transform
) {
    has_analyses <- "analyses" %in% names(payload)
    has_any_analysis_objects <- length(display_settings) != 0 ||
        !is.null(filter) ||
        !is.null(weight) ||
        !is.null(viz_specs) ||
        !is.null(transform)

    if (has_analyses && !is.null(query)) {
        halt("Cannot specify both a `query` and `analyses` for `newSlide()`")
    }
    if (!has_analyses && is.null(query)) {
        halt("Must specify either a `query` or `analyses` for `newSlide()`")
    }
    if (has_analyses && has_any_analysis_objects) {
        warning(paste0(
            "`display_settings`, `filter`, `weight`, `viz_specs` and `transform` are ",
            "ignored if `analyses` are defined directly for `newSlide()`"
        ))
    }
}
#nolint end

validateSlideQuery <- function(query) {
    dimensions <- query$dimensions

    if (length(dimensions) < 3) return()

    is_subvar_dim <- vapply(dimensions, function(x) {
        "function" %in% names(x) &&
            x[["function"]] == "dimension" &&
            "args" %in% names(x) &&
            identical(x[["args"]][[2]], zcl("subvariables"))
    }, logical(1))

    if (!any(is_subvar_dim)) return()

    subvar_args <- lapply(which(is_subvar_dim), function(x) dimensions[[x]][["args"]][[1]])

    first_dim_is_array_cat <- vapply(subvar_args, function(x) {
        identical(dimensions[[1]], x) ||
            identical(dimensions[[1]], zfunc("dimension", x, "categories"))
    }, logical(1))

    if (any(first_dim_is_array_cat)) {
        halt(
            "First dimension of 3+ dimension cube for slide analysis cannot be an array's",
            "categories. You probably want to use `selectCategories()` to collapse the",
            "categorical array's categories dimension."
        )
    }
}

#' @rdname crunch-extract
#' @export
setMethod("[[", "AnalysisCrunchSlide", function(x, i, ...) {
    an_cat <- analyses(x)
    return(an_cat[[i]])
})
#' @rdname crunch-extract
#' @export
setMethod("[[<-", "AnalysisCrunchSlide", function(x, i, j, value) {
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

#' @rdname newSlide
#' @export
setMethod("type", "CrunchSlide", function(x) {
    return(x@body$type)
})


#' @rdname analysis-methods
#' @export
setMethod("analyses", "AnalysisCrunchSlide", function(x) {
    AnalysisCatalog(crGET(shojiURL(x, "catalogs", "analyses")))
})


#' @rdname analysis-methods
#' @export
setMethod("analysis", "AnalysisCrunchSlide", function(x) {
    out <- AnalysisCatalog(crGET(shojiURL(x, "catalogs", "analyses")))
    return(out[[1]])
})

#' @rdname analysis-methods
#' @export
setMethod("analysis<-", c("AnalysisCrunchSlide", "formula"), function(x, value) {
    analysis <- analyses(x)[[1]]
    query(analysis) <- value
    return(invisible(x))
})

#' @rdname analysis-methods
#' @export
setMethod("analysis<-", c("AnalysisCrunchSlide", "Analysis"), function(x, value) {
    analysis_cat <- analyses(x)
    return(invisible(modifyCatalogInPlace(analysis_cat, 1, NULL, value)))
})

#' @rdname analysis-methods
#' @export
setMethod("analysis<-", c("AnalysisCrunchSlide", "list"), function(x, value) {
    payload <- wrapEntity(body = value)
    url <- self(analysis(x))
    crPATCH(url, body = toJSON(payload))
    invisible(refresh(x))
})

#' @rdname analysis-methods
#' @export
setMethod("filter", "AnalysisCrunchSlide", function(x, ...) {
    analysis <- analyses(x)[[1]]
    return(filter(analysis))
})

#' @rdname analysis-methods
#' @export
setMethod("filter<-", c("AnalysisCrunchSlide", "ANY"), function(x, value) {
    # check that there is only on analysis?
    first_analysis <- analyses(x)[[1]]
    filter(first_analysis) <- value
    return(invisible(x))
})

#' @rdname analysis-methods
#' @export
setMethod("query<-", "AnalysisCrunchSlide", function(x, value) {
    analysis <- analyses(x)[[1]]
    query(analysis) <- value
    return(invisible(x))
})

#' @rdname analysis-methods
#' @export
setMethod("cubes", "AnalysisCrunchSlide", function(x) cubes(analyses(x)))
#' @rdname analysis-methods
#' @export
setMethod("cube", "AnalysisCrunchSlide", function(x) cube(analyses(x)[[1]]))
#' @rdname analysis-methods
#' @export
setMethod("displaySettings", "AnalysisCrunchSlide", function(x) displaySettings(analyses(x)))
#' @rdname analysis-methods
#' @export
setMethod("displaySettings<-", "AnalysisCrunchSlide", function(x, value) {
    an_cat <- analyses(x)
    displaySettings(an_cat) <- value
    return(invisible(x))
})

#' @rdname analysis-methods
#' @export
setMethod("vizSpecs", "AnalysisCrunchSlide", function(x) vizSpecs(analyses(x)))
#' @rdname analysis-methods
#' @export
setMethod("vizSpecs<-", "AnalysisCrunchSlide", function(x, value) {
    an_cat <- analyses(x)
    vizSpecs(an_cat) <- value
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
        # TODO: Make sure API always sends all parts (and no legacy pre-viz_specs)
        # and update fixtures
        payload <- value@body[
            na.omit(match(
                c("query", "display_settings", "query_environment", "viz_specs", "transform"),
                names(value@body)
            ))
        ]
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

#' @rdname analysis-methods
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
#' @rdname analysis-methods
#' @export
setMethod("displaySettings<-", c("AnalysisCatalog", "list"), function(x, value) {
    analyses <- lapply(seq_along(x), function(i) x[[i]])
    lapply(analyses, function(x) displaySettings(x) <- value)
})

#' @rdname analysis-methods
#' @export
setMethod("vizSpecs", "AnalysisCatalog", function(x) {
    settings_list <- lapply(seq_along(x), function(i) {
        vizSpecs(x[[i]])
    })
    if (length(settings_list) == 1) {
        return(settings_list[[1]])
    } else {
        return(settings_list)
    }
})
#' @rdname analysis-methods
#' @export
setMethod("vizSpecs<-", c("AnalysisCatalog", "list"), function(x, value) {
    analyses <- lapply(seq_along(x), function(i) x[[i]])
    lapply(analyses, function(x) vizSpecs(x) <- value)
})


# Analysis ----------------------------------------------------------------
#' @rdname analysis-methods
#' @export
setMethod("query<-", c("Analysis", "formula"), function(x, value) {
    ds <- loadDataset(datasetReference(x))
    payload <- list(query = formulaToCubeQuery(value, data = ds))
    payload <- wrapEntity(body = payload)
    crPATCH(self(x), body = toJSON(payload))
    return(invisible(refresh(x)))
})

#' @rdname analysis-methods
#' @export
formulaToSlideQuery <- function(query, dataset) {
    formulaToCubeQuery(query, dataset)
}

#' @rdname analysis-methods
#' @export
setMethod("cube", "Analysis", function(x) {
    # Always use the weight from the query_environment (even if missing in qe, we want to
    # override with no weight in this case)
    cube_query <- x@body$query
    cube_query$weight <- NULL
    # Actually want weight=NULL to override the default dataset weight
    cube_query <- c(cube_query, list(weight = x@body$query_environment$weight))

    http_query <- list(query = toJSON(cube_query, for_query_string = TRUE))

    # Don't pass filter=NULL because API's probably grumpy about that. Also, rather than pass a
    # list of filters if there are multiple, the API expects multiple `filter=` URL query
    # parameters
    qe_filters <- x@body$query_environment$filter
    if (length(qe_filters) > 0) {
        qe_filters <- lapply(qe_filters, function(filt) toJSON(filt, for_query_string = TRUE))
        names(qe_filters) <- rep("filter", length(qe_filters))
        http_query <- c(http_query, qe_filters)
    }

    CrunchCube(crGET(
        cubeURL(x),
        query = http_query
    ))
})
#' @rdname analysis-methods
#' @export
setMethod("displaySettings", "Analysis", function(x) {
    lapply(x@body$display_settings, function(x) x$value)
})
#' @rdname analysis-methods
#' @export
setMethod("displaySettings<-", "Analysis", function(x, value) {
    settings <- modifyList(displaySettings(x), value)
    settings <- wrapDisplaySettings(settings)
    payload <- list(display_settings = settings)
    payload <- wrapEntity(body = payload)
    crPATCH(self(x), body = toJSON(payload))
    invisible(refresh(x))
})
#' @rdname analysis-methods
#' @export
setMethod("vizSpecs", "Analysis", function(x) {
    x@body$viz_specs
})
#' @rdname analysis-methods
#' @export
setMethod("vizSpecs<-", "Analysis", function(x, value) {
    payload <- wrapEntity(body = list(viz_specs = value))
    crPATCH(self(x), body = toJSON(payload))
    invisible(refresh(x))
})

# This processes a names list of display setting values to the form that is
# required by the API.
wrapDisplaySettings <- function(settings) {
    return(lapply(settings, function(x) list(value = x)))
}

#' @rdname analysis-methods
#' @export
setMethod("filter", "Analysis", function(x, ...) {
    filt <- x@body$query_environment$filter
    if (length(filt) == 0) {
        return(NULL)
    } else if (length(filt) == 1 && "filter" %in% names(filt[[1]])) {
        # a saved filter
        return(CrunchFilter(crGET(filt[[1]]$filter)))
    } else {
        # an adhoc filter
        ds_url <- datasetReference(x)
        adhoc_expr <- CrunchLogicalExpr(
            expression = idsToURLs(
                # 02/2021: Not sure if this is still needed anymore, server doesn't
                # currently seem to be sending the `dataset` attributes this takes out.
                # But mocks require it (/4/decks/8ad8/slides/72e8/analysies/52fb.json)
                fixAdhocFilterExpression(filt[[1]]),
                paste0(ds_url, "/variables/")
            ),
            dataset_url = ds_url
        )
        return(adhoc_expr)
    }
})

#' @rdname analysis-methods
#' @export
setMethod("filter", "ANY", function(x, ...) {
    for (searchpath in search()) {
        func <- get("filter", as.environment(searchpath))
        if (!identical(func, crunch::filter)) {
            return(func(x, ...))
        }
    }
    halt("No method found for filter for object of type ", dQuote(methods::getClass(x))) # nocov
})

#' @rdname analysis-methods
#' @export
setMethod("filter<-", "AnalysisCrunchSlide", function(x, value) {
    analysis <- analyses(x)[[1]]
    filter(analysis) <- value
    return(invisible(x))
})

#' @rdname analysis-methods
#' @export
setMethod("filter<-", c("Analysis", "CrunchLogicalExpr"), function(x, value) {
    return(set_analysis_filter_or_weight(x, filter = list(value@expression)))
})

#' @rdname analysis-methods
#' @export
setMethod("filter<-", c("Analysis", "CrunchFilter"), function(x, value) {
    # crPATCH(self(x), body = toJSON(frmt))
    return(set_analysis_filter_or_weight(x, filter = list(self(value))))
})

#' @rdname analysis-methods
#' @export
setMethod("filter<-", c("Analysis", "NULL"), function(x, value) {
    # crPATCH(self(x), body = toJSON(frmt))
    return(set_analysis_filter_or_weight(x, filter = list()))
})

#' @rdname analysis-methods
#' @export
slideQueryEnv <- function(weight, filter) {
    if (missing(weight) && missing(filter)) {
        halt("Must specify at least one of `weight` or `filter`")
    }
    out <- list()
    if (!missing(weight)) {
        out$weight <- if (is.null(weight)) list() else self(weight)
    }
    if (!missing(filter)) {
        if (is.null(filter)) {
            out$filter <- list()
        } else if (is.CrunchExpr(filter)) {
            out$filter <- list(filter@expression)
        } else {
            out$filter <- list(self(filter))
        }
    }
    out
}

#' @rdname analysis-methods
#' @export
setMethod("cubes", "CrunchDeck", function(x) {
    slide_types <- types(x)
    out <- lapply(seq_len(length(x)), function(i) {
        if (slide_types[i] != "analysis") return(NULL) # Markdown slides don't have cubes

        cubes <- cubes(x[[i]])
        # If a slide has several analyses we should return a sublist of
        # cubes, but most of the time they will have one analysis so not
        # including the sublist is preferable.
        if (length(cubes) == 1) {
            cubes <- cubes[[1]]
        }
        return(cubes)
    })
    names(out) <- titles(x)
    return(out)
})


#' @rdname analysis-methods
#' @export
setMethod("weight", "AnalysisCrunchSlide", function(x) {
    analysis <- analyses(x)[[1]]
    return(weight(analysis))
})

#' @rdname analysis-methods
#' @export
setMethod("weight<-", c("AnalysisCrunchSlide", "ANY"), function(x, value) {
    # check that there is only on analysis?
    first_analysis <- analyses(x)[[1]]
    weight(first_analysis) <- value
    return(invisible(x))
})

#' @rdname analysis-methods
#' @export
setMethod("weight", "Analysis", function(x) {
    wt <- x@body$query_environment$weight
    if (length(wt) == 0) {
        return(NULL)
    }
    full_ds <- loadDataset(datasetReference(VariableEntity(x)))
    wt_pos <- which(urls(allVariables(full_ds)) == wt)
    filt <- filter(x)
    if (inherits(filt, "CrunchFilter")) {
        filt <- CrunchLogicalExpr(
            dataset_url = self(full_ds),
            expression = filt@body[["expression"]]
        )
    }
    CrunchVariable(allVariables(full_ds)[[wt_pos]], filter = filt)
})

#' @rdname weight
#' @export
setMethod("weight<-", c("Analysis", "CrunchVariable"), function(x, value) {
    if (!is.weightVariable(value)) halt(paste0(
        "Variable '", alias(value), "' is not a weightVariable"
    ))
    return(set_analysis_filter_or_weight(x, weight = self(value)))
})

#' @rdname weight
#' @export
setMethod("weight<-", c("Analysis", "NULL"), function(x, value) {
    return(set_analysis_filter_or_weight(x, weight = NULL))
})

# TODO: setMethod("weight<-", c("Analysis", "CrunchVariable") method to use alias?

# Want to update filter/weight components separately so that we don't
# remove something accidentally.
set_analysis_filter_or_weight <- function(x, filter, weight) {
    query_env <- slot(x, "body")[["query_environment"]]
    # The single "[" <- list() notation allows NULLs in weight rather than just removing weight
    if (!missing(filter)) query_env["filter"] <- list(filter)
    if (!missing(weight)) query_env["weight"] <- list(weight)

    # Also need to set weight in the query to match webapp's behavior
    if (!missing(weight)) {
        query <- slot(x, "body")[["query"]]
        query$weight <- weight
        setMultiEntitySlots(x, query_environment = query_env, query = query)
    } else { # But if only updating filter, then leave query alone
        setEntitySlot(x, "query_environment", query_env)
    }

}

# Markdown  ---------------------------------------------------------------
