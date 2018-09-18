
# Generics ----------------------------------------------------------------

#' Manipulate deck titles
#'
#' Crunch slides have titles an subtitles. You can change these features at
#' either the Deck level by assigning a character vector which is
#' the same length as the Deck to the Crunch deck, or by assigning character strings to the
#' the slide.
#' @param x a `CrunchDeck` or `CrunchSlide`
#' @param value character, the new title or subtitle
#' @name deck-titles
#' @return `x`, modified
#' @export
#'
#' @examples
#' \dontrun{
#' titles(deck)
#' titles(deck) <- c(new_title1, new_title2)
#' slide <- deck[[1]]
#' title(slide) <- "new title"
#' subtitle(slide) <- "new subtitle"
#' subtitles(deck)
#' }
setGeneric("titles", function(x) standardGeneric("titles"))
#' @rdname deck-titles
#' @export
setGeneric("titles<-", function(x, value) standardGeneric("titles<-"))
#' @rdname deck-titles
setGeneric("title", function(x) standardGeneric("title"))
#' @rdname deck-titles
setGeneric("title<-", function(x, value) standardGeneric("title<-"))
#' @rdname deck-titles
#' @export
setGeneric("subtitles", function(x, value) standardGeneric("subtitles"))
#' @rdname deck-titles
#' @export
setGeneric("subtitles<-", function(x, value) standardGeneric("subtitles<-"))
#' @rdname deck-titles
#' @export
setGeneric("subtitle", function(x, value) standardGeneric("subtitle"))
#' @rdname deck-titles
#' @export
setGeneric("subtitle<-", function(x, value) standardGeneric("subtitle<-"))
#' @export
setGeneric("analyses", function(x) standardGeneric("analyses"))
#' @export
setGeneric("analyses<-", function(x, value) standardGeneric("analyses<-"))
#' @export
setGeneric("analysis", function(x) standardGeneric("analysis"))
#' @export
setGeneric("analysis<-", function(x, value) standardGeneric("analysis<-"))
#' @export
setGeneric("query", function(x) standardGeneric("query"))
#' @export
setGeneric("query<-", function(x, value) standardGeneric("query<-"))
#' @export
setGeneric("cube", function(x) standardGeneric("cube"))
#' @export
setGeneric("cubes", function(x) standardGeneric("cubes"))
#' @export
setGeneric("displaySettings", function(x) standardGeneric("displaySettings"))
#' @export
setGeneric("displaySettings<-", function(x, value) standardGeneric("displaySettings<-"))


#' Get a dataset's DeckCatalog
#'
#' Crunch decks are stored in catalogs. This function returns those catalogs so
#' that you can access and manipulate decks in R.
#'
#' @param dataset a Crunch Dataset
#' @return a DeckCatalog
#' @export
decks <- function(dataset){
    stopifnot(is.dataset(dataset))
    DeckCatalog(crGET(shojiURL(dataset, "catalogs", "decks")))
}

# Deck Catalog ------------------------------------------------------------

#' Create an empty Crunch Deck
#'
#' @param dataset A Crunch Dataset
#' @param name The name of the Deck
#' @param ... Further attributes of the deck such as the description, see API
#'   docs for options.
#' @return The `CrunchDeck` that was created.
#' @export
newDeck <- function(dataset, name, ...) {
    stopifnot(is.dataset(dataset))
    payload <- wrapEntity(name = name, ...)
    shoji <- shojiURL(dataset, "catalogs", "decks")
    url <- crPOST(shoji, body = toJSON(payload))
    return(CrunchDeck(crGET(url)))
}

setMethod("initialize", "DeckCatalog", init.sortCatalog)

setMethod("[[", "DeckCatalog",  function (x, i, ...) {
    getEntity(x, i, CrunchDeck, ...)
})

setMethod("[[", c("DeckCatalog", "character", "ANY"),  function (x, i, ...) {
    if (length(i) > 1) {
        halt("You can only select one deck at a time")
    }
    matches <- names(x) %in% i
    if (all(!matches)) {
        halt(dQuote(i), " is not present in deck catalog")
    }
    index <- which(matches)
    if (sum(matches) > 1) {
        warning(dQuote(i), " does not uniquely identify elements. Returning the first match")
        index <- index[1]
    }
    getEntity(x, index, CrunchDeck, ...)
})

setMethod("show", "DeckCatalog", function(object){
    out <- as.data.frame(object)
    print(out[, c("name", "team", "is_public", "owner_name")])
})

# CrunchDeck --------------------------------------------------------------

setMethod("[[", "CrunchDeck", function (x, i, ...) slides(x)[[i]])

setMethod("[[<-", "CrunchDeck", function(x, i, j, value) {
    slideCat <- slides(x)
    slideCat[[i]] <- value
    invisible(refresh(x))
})

#' @rdname delete
#' @export
setMethod("delete", "CrunchDeck", function (x, ...) {
    if (!askForPermission(paste0("Really delete deck ", dQuote(name(x)), "?"))) {
        halt("Must confirm deleting a deck")
    }
    out <- crDELETE(self(x))
    invisible(out)
})

#' Access the slides of a CrunchDeck
#'
#' Return a SlideCatalog from a CrunchDeck. All slide catalog methods should be
#' available for CrunchDecks, but this function is used internally to model the
#' API.
#'
#' @param x a CrunchDeck
#' @return a SliDe Catalog
#' @export
slides <- function(x){
    SlideCatalog(crGET(shojiURL(x, "catalogs", "slides")))
}

setMethod("name<-", "CrunchDeck", function(x, value) {
    stopifnot(is.singleCharacter(value))
    setEntitySlot(x, "name", value)
    invisible(x)
})

setMethod("description", "CrunchDeck", function(x) x@body$description)
setMethod("description<-", "CrunchDeck", function(x, value) {
    stopifnot(is.singleCharacter(value))
    setEntitySlot(x, "description", value)
    invisible(x)
})

setMethod("is.public", "CrunchDeck", function(x) x@body$is_public)
setMethod("is.public<-", "CrunchDeck", function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    setEntitySlot(x, "is_public", value)
})

setMethod("names", "CrunchDeck", function(x) titles(slides(x)))
setMethod("names<-", "CrunchDeck", function(x, value) {
    slide_cat <- slides(x)
    titles(slide_cat) <- value
})

setMethod("titles", "CrunchDeck", function(x) titles(slides(x)))
setMethod("titles<-", "CrunchDeck", function (x, value){
    titles(slides(x)) <- value
    invisible(refresh(x))
})

setMethod("subtitles", "CrunchDeck", function(x) subtitles(slides(x)))
setMethod("subtitles<-", "CrunchDeck", function(x, value){
    slides <- slides(x)
    subtitles(slides) <- value
    invisible(refresh(x))
})

#' Export a Crunch Deck
#'
#' Crunch decks can be exported as excel or json files.
#'
#' @param deck A CrunchDeck
#' @param file The file path to save the exported deck
#' @param format Either `xlsx` or `json`
#'
#' @return
#' @export
exportDeck <- function(deck, file, format = c("xlsx", "json")) {
    if (!inherits(deck, "CrunchDeck")) {
        halt("exportDeck is only available for CrunchDecks.")
    }
    url <- deck@views$export
    format <- match.arg(format)
    accept <- extToContentType(format)
    if (missing(file)) {
        file <- paste0(name(deck), ".", format)
    }
    dl_link <- crPOST(url, config = add_headers(`Accept` = accept))
    crDownload(dl_link, file)
}

setMethod("length", "CrunchDeck", function(x) return(length(slides(x))))

setMethod("cubes", "CrunchDeck", function(x){
    out <- lapply(seq_len(length(x)), function(i){
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

setMethod("show", "CrunchDeck", function(object){
    print(cubes(object))
})

# Slide Catalog -----------------------------------------------------------

setMethod("initialize", "SlideCatalog", function(.Object, ...){
    .Object <- callNextMethod()
    order <- crGET(.Object@orders$flat)
    order <- unlist(order$graph)
    .Object@index <- .Object@index[match(order, names(.Object@index))]
    return(.Object)
})

setMethod("names", "SlideCatalog", function(x) titles(x))
setMethod("names<-", "SlideCatalog", function(x, value) titles(x) <- value)

setMethod("titles", "SlideCatalog", function(x) getIndexSlot(x, "title"))
setMethod("titles<-", "SlideCatalog", function (x, value){
    setIndexSlot(x, "title", value)
})

setMethod("subtitles", "SlideCatalog", function (x) getIndexSlot(x, "subtitle"))
setMethod("subtitles<-", "SlideCatalog", function (x, value){
    setIndexSlot(x, "subtitle", value)
})

setMethod("[[", "SlideCatalog", function (x, i, ...) {
    getEntity(x, i, CrunchSlide)
})

setMethod("[[<-", c("SlideCatalog", "numeric", "missing", "CrunchSlide"),
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
        payload$analyses <- lapply(seq_len(anCat), function(i){
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
moveLastElement <- function(v, idx){
    v[idx] <- v[length(v)]
    out <- v[1:(length(v) -1)]
    return(out)
}

#' Reorder slides in a crunchdeck
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

setMethod("delete", "CrunchSlide", function (x, ...) {
    if (!askForPermission(paste0("Really delete slide ", dQuote(title(x)), "?"))) {
        halt("Must confirm deleting CrunchSlide")
    }
    out <- crDELETE(self(x), drop = dropCache(absoluteURL("../", self(x))))
    invisible(out)
})

# CrunchSlide -------------------------------------------------------------------

setMethod("show", "CrunchSlide", function(object){
    out <- cubes(object)
    names(out) <- title(object)
    print(out)
})

# TODO: Find out what the mandatory display settings should be for the app then
# change this list to reflect those settings.
DEFAULT_DISPLAY_SETTINGS <- list(
    percentageDirection = "colPct",
    showEmpty = FALSE,
    showMean = FALSE,
    vizType = "table",
    countsOrPercents = "percent",
    decimalPlaces = 1L,
    populationMagnitude = 3L,
    showSignif = TRUE,
    currentTab = 0L,
    uiView = "app.datasets.browse"
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
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' newSlide(
#'     main_deck,
#'     ~cyl + wt,
#'     title = "Cyl and Weight",
#'     subtitle = 2017 Data"
#'     )
#' }
newSlide <- function(deck,
                     query,
                     display_settings = list(),
                     title = "",
                     subtitle = "",
                     ...) {
    ds <- loadDataset(datasetReference(deck))
    if (inherits(query, "formula")) {
        query <- list(query)
    }

    settings <- modifyList(display_settings, DEFAULT_DISPLAY_SETTINGS)
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

setMethod("[[", "CrunchSlide", function (x, i, ...) {
    an_cat <- analyses(x)
    return(an_cat[[i]])
})

setMethod("[[<-", "CrunchSlide", function(x, i, j, value) {
    an_cat <- analyses(x)
    an_cat[[i]] <- value
    invisible(refresh(x))
})

setMethod("title", "CrunchSlide", function (x){
    return(x@body$title)
})

setMethod("title<-", "CrunchSlide", function(x, value){
    setEntitySlot(x, "title", value)
})

setMethod("subtitle", "CrunchSlide", function (x){
    return(x@body$subtitle)
})

setMethod("subtitle<-", "CrunchSlide", function(x, value){
    setEntitySlot(x, "subtitle", value)
})

setMethod("analyses", "CrunchSlide", function (x) {
    AnalysisCatalog(crGET(shojiURL(x, "catalogs", "analyses")))
})

setMethod("analysis", "CrunchSlide", function (x) {
    out <- AnalysisCatalog(crGET(shojiURL(x, "catalogs", "analyses")))
    return(out[[1]])
})

setMethod("analysis<-", c("CrunchSlide", "formula"), function(x, value){
    analyses[[1]] <- value
})

setMethod("query<-", "CrunchSlide", function(x, value){
    analysis <- analyses(x)[[1]]
    query(analysis) <- value
    return(invisible(x))
})

setMethod("cubes", "CrunchSlide", function(x) cubes(analyses(x)))
setMethod("cube", "CrunchSlide", function(x) cube(analyses(x)[[1]]))
setMethod("displaySettings", "CrunchSlide", function(x) displaySettings(analyses(x)))
setMethod("displaySettings<-", "CrunchSlide", function(x, value) {
    displaySettings(analyses(x)) <- value
})

# AnalysisCatalog --------------------------------------------------------------

setMethod("[[", "AnalysisCatalog", function (x, i, ...) {
  getEntity(x, i, Analysis)
})

setMethod("[[<-", c("AnalysisCatalog", "numeric", "missing", "formula"), function (x, i, j, value) {
    if (i > length(x)) {
        halt("Index out of bounds, you can only assign a formula to an existing analysis.")
    }
    analysis <- x[[i]]
    query(analysis) <- value
    invisible(refresh(x))
})

setMethod("[[<-", c("AnalysisCatalog", "numeric", "missing", "Analysis"),
    function(x, i, j, value){
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
            crPATCH(url, body=toJSON(payload))
        } else {
            crPOST(self(x), body = toJSON(payload))
        }
        invisible(refresh(x))
    }
)

setMethod("[[<-", c("AnalysisCatalog", "numeric", "missing", "list"),
    function (x, i, j, value) {
        all_fmla <- vapply(value, function(x) inherits(x, "formula"), logical(1))
        if (any(!all_fmla)) {
            halt("Entry ", which(!all_fmla), " is not a formula")
        }
        if (length(i) != length(value)) {
            noun <- if (length(i) == 1) " analysis." else " analyses."
            halt("Invalid assignment. You tried to assign ", length(value),
                " formulas to ", length(i), noun)
        }
        mapply(function(analysis, fmla) {
            analysis <- fmla
        }, analysis = x[[i]], fmla = value)
    }
)

setMethod("cubes", "AnalysisCatalog", function(x) {
    lapply(seq_along(x@index), function(i) cube(x[[i]]))
})

setMethod("displaySettings", "AnalysisCatalog", function(x){
    settings_list <- lapply(seq_len(length(x)), function(i) {
        displaySettings(x[[i]])
        })
    if (length(settings_list) == 1) {
        return(settings_list[[1]])
    } else {
        return(settings_list)
    }
})

setMethod("displaySettings<-", c("AnalysisCatalog", "list"), function(x, value){
    analyses <- lapply(seq_len(length(x)), function(i) x[[i]])
    lapply(analyses, function(x) displaySettings(x) <- value)
})


# Analysis ----------------------------------------------------------------
setMethod("query<-", c("Analysis", "formula"), function(x, value) {
    ds <- loadDataset(datasetReference(x))
    payload <- list(query = formulaToCubeQuery(value, data = ds))
    payload <- wrapEntity(body = payload)
    crPATCH(self(x), body = toJSON(payload))
    invisible(refresh(x))
})

setMethod("cube", "Analysis", function(x) {
    CrunchCube(crGET(
        cubeURL(x),
        query = list(query = toJSON(x@body$query)),
        filter = toJSON(x@body$query_environment$filter)
    ))
})

setMethod("displaySettings", "Analysis", function(x) {
    lapply(x@body$display_settings, function(x) x$value)
})

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
