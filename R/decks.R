setMethod("decks", "CrunchDataset", function (x) {
    DeckCatalog(crGET(shojiURL(x, "catalogs", "decks")))
})


# Deck Catalog ------------------------------------------------------------

#' Create an empty Crunch Deck
#'
#' @param dataset A Crunch Dataset
#' @param name The name of the Deck
#' @param ... Further attributes of the deck such as the description, see API
#'   docs for options.
#' @return
#' @export
#'
#' @examples
newDeck <- function(dataset, name, ...) {
    stopifnot(is.dataset(dataset))
    payload <- wrapEntity(name = name, ...)
    url <- shojiURL(dataset, "catalogs", "decks")
    url <- crPOST(url, body = toJSON(payload))
    return(CrunchDeck(crGET(url)))
}

setMethod("initialize", "DeckCatalog", init.sortCatalog)

setMethod("[[", "DeckCatalog",  function (x, i, ...) {
    getEntity(x, i, CrunchDeck, ...)
})

setMethod("[[<-", c("DeckCatalog", "character", "ANY", "CrunchDeck"),
          function (x, i, j, value) {
              payload <- value@body[c("name", "description", "is_public", "team")]
              payload$name <- i
              payload <- payload[vapply(payload, function(x)is.character(x) | is.logical(x), logical(1))]
              crPOST(self(x), body = toJSON(payload))
              return(invisible(refresh(x)))
          })

setMethod("show", "DeckCatalog", function(object){
    out <- as.data.frame(object)
    print(out[, c("name", "team", "is_public", "owner_name")])
})

# CrunchDeck --------------------------------------------------------------

setMethod("[[", "CrunchDeck", function (x, i, ...) {
    slideCat <- slides(x)
    return(slideCat[[i]])
})
setMethod("[[<-", "CrunchDeck", function(x, i, j, value) {
    slideCat <- slides(x)
    slideCat[[i]] <- value
    invisible(refresh(x))
})

#' @rdname delete
#' @export
setMethod("delete", "CrunchDeck", function (x, ...) {
    if (!askForPermission(paste0("Really delete deck ", dQuote(name(x)), "?"))) {
        halt("Must confirm deleting CrunchDeck")
    }
    out <- crDELETE(self(x))
    invisible(out)
})

setMethod("slides", "CrunchDeck", function (x) {
    SlideCatalog(crGET(shojiURL(x, "catalogs", "slides")))
})

setMethod("name<-", "CrunchDeck", function(x, value) {
    stopifnot(is.character(value))
    stopifnot(length(value) == 1)
    setEntitySlot(x, "name", value)
    invisible(refresh(x))
    })

setMethod("description", "CrunchDeck", function(x) x@body$description)
setMethod("description<-", "CrunchDeck", function(x, value) {
    stopifnot(is.character(value))
    stopifnot(length(value) == 1)
    setEntitySlot(x, "description", value)
    invisible(refresh(x))
    })

setMethod("is.public", "CrunchDeck", function(x) x@body$is_public)
setMethod("is.public<-", "CrunchDeck", function(x, value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1)
    setEntitySlot(x, "is_public", value)
})

setMethod("names", "CrunchDeck", function(x) titles(slides(x)))
setMethod("names<-", "CrunchDeck", function(x, value) titles(slides(x)) <- value)

setMethod("titles", "CrunchDeck", function (x){
    titles(slides(x))
})

setMethod("titles<-", "CrunchDeck", function (x, value){
    slides <- slides(x)
    titles(slides) <- value
    invisible(refresh(x))
})

setMethod("subtitles", "CrunchDeck", function (x){
     subtitles(slides(x))
})

setMethod("subtitles<-", "CrunchDeck", function (x, value){
    slides <- slides(x)
    subtitles(slides) <- value
    invisible(refresh(x))
})

exportDeck <- function(deck, file, type = c("xlsx", "json")) {
    if (!inherits(deck, "CrunchDeck")) {
        halt("exportDeck is only available for CrunchDecks.")
    }
    url <- deck@views$export
    type <- match.arg(type)
    if (type == "json") {
        dl_link <- crPOST(url, config = add_headers(`Accept`="application/json"))
        ext <- ".json"
    } else {
        dl_link <- crPOST(
            url,
            config = add_headers(`Accept` = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
            )
        ext <- ".xlsx"
    }
    browser()
    if (missing(file)) {
        file <-  paste0(name(deck), ext)
    }
    crDownload(dl_link, file)
}

setMethod("length", "CrunchDeck", function(x){
    return(length(slides(x)))
})

setMethod("cubes", "CrunchDeck", function(x){
    out <- lapply(seq_len(length(x)), function(i){
        cubes <- cubes(x[[i]])
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

setMethod("titles", "SlideCatalog", function (x){
    as.character(lapply(x@index, function(x) x$title))
})

setMethod("titles<-", "SlideCatalog", function (x, value){
    updateSlideCatTitles(x, value, "title")
})

setMethod("subtitles", "SlideCatalog", function (x){
    as.character(lapply(x@index, function(x) x$subtitle))
})

setMethod("subtitles<-", "SlideCatalog", function (x, value){
    updateSlideCatTitles(x, value, "subtitle")
})

updateSlideCatTitles <- function(x, value, type) {
    # TODO update a single title.
    stopifnot(is.character(value))
    payload <- mapply(function (item, new_title){
        out <- item[type]
        out[[type]] <- new_title
        return(out)
    }, x@index, value, SIMPLIFY = FALSE)
    crPATCH(self(x), body = toJSON(payload))
    invisible(refresh(x))
}

setMethod("[[", "SlideCatalog", function (x, i, ...) {
  getEntity(x, i, CrunchSlide)
})

setMethod("[[<-", c("SlideCatalog", "numeric", "missing", "CrunchSlide"),
          function(x, i, j, value) {
              if (length(i) > 1) {
                  #TODO, allow assignment of more than one slide
              }
              if (i > length(x) + 1) {
                  #TODO what to do with missing slide entries
                  i <- length(x) + 1
              }
              if (i > length(x)) {
                  # create new slide
                  # Replace with newSlide function when we have the
                  # analysesToFormula functions implemented.
                  payload <- value@body[c("title", "subtitle")]
                  analyses <- analyses(value)
                  payload$analyses <- analysesToQueryList(analyses(value))
                  payload <- wrapEntity(body = payload)
                  crPOST(self(x), body = toJSON(payload))
              } else {
                  slide <- x[[i]]
                  analyses(slide) <- analyses(value)
                  payload <- list(title = title(value), subtitle = subtitle(value))
                  payload <- wrapEntity(body = payload)
                  crPATCH(self(slide), body = toJSON(payload))
              }
              invisible(refresh(x))
          })

analysesToQueryList <- function(anCat) {
    lapply(seq_along(anCat), function(i){
        out <- anCat[[i]]
        out <- out@body[c("query", "query_environment", "display_settings")]
        out$display_settings <- out$display_settings[c(
            "decimalPlaces", "percentageDirection",
            "vizType", "countsOrPercents", "uiView")]
        out
    })
}

# CrunchSlide -------------------------------------------------------------------

setMethod("show", "CrunchSlide", function(object){
    out <- cubes(object)
    names(out) <- title(object)
    print(out)
})

# TODO: Find out what the mandatory display settings should be for the app then
# change this list to reflect those settings.
generateDefaultDisplays <- function(popMagnitude = 3) {
    out <- list(
        percentageDirection = "colPct",
        showEmpty = FALSE,
        showMean = FALSE,
        vizType = "table",
        countsOrPercents = "percent",
        decimalPlaces = 1L,
        populationMagnitude = popMagnitude,
        showSignif = TRUE,
        currentTab = 0L,
        uiView = "app.datasets.browse"
        )
    return(out)
}


spliceDisplaySettings <- function(new_settings, default = generateDefaultDisplays()) {
    in_default <- names(new_settings) %in% names(default)
    valid_names <- names(new_settings)[in_default]
    default[valid_names] <- new_settings[valid_names]
    if (any(!in_default)) {
        warning("Invalid display settings ommitted: ",
                serialPaste(dQuote(names(new_settings))[!in_default])
        )
    }
    return(default)
}

newSlide <- function(deck,
                     query,
                     display_settings = list(),
                     title = "",
                     subtitle = "",
                     ...) {
    ds <- loadDataset(datasetReference(deck))
    settings <- spliceDisplaySettings(
        generateDefaultDisplays(ds), display_settings
    )

    if (inherits(query, "formula")) {
        query <- list(query)
    }
    display_settings <- wrapDisplaySettings(display_settings)
    payload <- list(title = title, subtitle = subtitle, ...)
    queries <- lapply(query, function(x) {
        return(list(
            query = formulaToCubeQuery(x, ds),
            display_settings = display_settings
            ))
        })
    payload[["analyses"]] <- queries
    payload <- wrapEntity(body = payload)
    url <- crPOST(shojiURL(deck, "catalogs", "slides"), body = toJSON(payload))
    return(CrunchSlide(crGET(url)))
}

setMethod("[[", "CrunchSlide", function (x, i, ...) {
    an_cat <- analyses(x)
    return(an_cat[[i]])
})

setMethod("[[<-", "CrunchSlide", function(x, i, j, value) {
    anCat <- analyses(x)
    anCat[[i]] <- value
    invisible(refresh(x))
})

setMethod("title", "CrunchSlide", function (x){
    return(x@body$title)
})

setMethod("title<-", "CrunchSlide", function(x, value){
    payload <- list(title = value)
    crPATCH(self(x), body = toJSON(wrapEntity(body = payload)))
    invisible(refresh(x))
})

setMethod("subtitle", "CrunchSlide", function (x){
    return(x@body$subtitle)
})

setMethod("subtitle<-", "CrunchSlide", function(x, value){
    payload <- list(subtitle = value)
    crPATCH(self(x), body = toJSON(wrapEntity(body = payload)))
    invisible(refresh(x))
})

setMethod("analyses", "CrunchSlide", {
    function (x) {
        AnalysisCatalog(crGET(shojiURL(x, "catalogs", "analyses")))
    }
})

setMethod("analyses<-", c("CrunchSlide", "AnalysisCatalog"), function(x, value){
    x_analyses <- analyses(x)
    lapply(seq_along(value), function(i){
        x_analyses[[i]] <- value[[i]]
    })
    return(invisible(refresh(x)))
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
  if (i <= length(x)) {
      analysis <- x[[i]]
      analysis <- value
  } else {
      halt("Index out of bounds, you can only assign a formula to an existing analysis.")
  }
})

setMethod("[[<-", c("AnalysisCatalog", "numeric", "missing", "Analysis"),
          function(x, i, j, value){
              if (length(i) > 1) {
                  #TODO, recurse through i
              }

              if (i > length(x) + 1) {
                  #TODO what to do with adding an analysis that's not the next one.
              }

              display_fields <- names(default_display_settings)
              payload <- value@body[c("query", "display_settings", "query_environment")]
              payload$display_settings <- payload$display_settings[display_fields]
              payload <- wrapEntity(body = payload)
              if (i <= length(x)) {
                  url <- names(x@index)[i]
                  crPATCH(url, body=toJSON(payload))
              } else {
                  crPOST(self(x), body = toJSON(payload))
              }
              invisible(refresh(x))
          })

setMethod("[[<-", c("AnalysisCatalog", "numeric", "missing", "list"), function (x, i, j, value) {
  all_fmla <- vapply(value, function(x) inherits(x, "formula"), logical(1))
  if (any(!all_fmla)) {
      halt("Entry ", which(!all_fmla), " is not a formula")
  }
  if (length(i) != length(value)) {
      noun <- if (length(i) == 1) " analysis." else " analyses."
      halt("Invalid assignment. You tried to assign ", length(value),
           " formulas to ", length(i), noun)
  }
  mapply(function(analysis, fmla){
      analysis <- fmla
  }, analysis = x[[i]], fmla = value)
})

setMethod("cubes", "AnalysisCatalog", function(x) {
    lapply(seq_along(x@index), function(i) cube(x[[i]]))
})

setMethod("displaySettings", "AnalysisCatalog", function(x){
    analyses <- lapply(seq_along(length(x)), function(i) x[[i]])
    if (length(x) > 1) {
        warning("Slide has multiple analyses, returning display settings for the first analysis")
    }
    return(displaySettings(analyses[[1]]))
})

setMethod("displaySettings<-", "AnalysisCatalog", function(x, value){
    analyses <- lapply(seq_along(length(x)), function(i) x[[i]])
    if (length(x) > 1) {
        warning("Slide has multiple analyses, returning display settings for the first analysis")
    }
    lapply(analyses, function(x) displaySettings(x) <- value)
})


# Analysis ----------------------------------------------------------------

setMethod("query", c("Analysis"), function(x) {
    # This should return a formula, wait to implement QueryToFormula function
})

setMethod("query<-", c("Analysis", "formula"), function(x, value) {
    ds <- loadDataset(datasetReference(x))
    payload <- list(query = formulaToCubeQuery(value, data = ds))
    payload <- wrapEntity(body = payload)
    crPATCH(self(x), body = toJSON(payload))
    invisible(refresh(x))
})

setMethod("cube", "Analysis", function (x) {
CrunchCube(crGET(cubeURL(x),
    query = list(query = toJSON(x@body$query)),
    filter = toJSON(x@body$query_environment$filter))
    )
})

setMethod("displaySettings", "Analysis", function(x){
    out <- lapply(x@body$display_settings, function(x) x$value)
    return(out)
})

setMethod("displaySettings<-", "Analysis", function(x, value){
    settings <- spliceDisplaySettings(value, default = displaySettings(x))
    settings <- wrapDisplaySettings(settings)
    payload <- x@body
    payload$display_settings <- settings
    payload <- wrapEntity(body = payload)
    crPATCH(self(x), body = toJSON(payload))
    invisible(refresh(x))
})

wrapDisplaySettings <- function(settings) {
    return(lapply(settings, function(x) list(value = x)))
}