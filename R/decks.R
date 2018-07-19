
# Deck Catalog ------------------------------------------------------------

setMethod("decks", "CrunchDataset", function (x) {
    DeckCatalog(crGET(shojiURL(x, "catalogs", "decks")))
})

setMethod("initialize", "DeckCatalog", init.sortCatalog)

setMethod("[[", "DeckCatalog",  function (x, i, ...) {
    getEntity(x, i, CrunchDeck, ...)
})

setMethod("[[<-", c("DeckCatalog", "character", "ANY", "CrunchDeck"),
          function (x, i, j, value) {
              payload <- value@body[c("name", "description", "is_public", "team")]
              payload$name <- i
              payload <- payload[vapply(payload, function(x)is.character(x) | is.logical(x), logical(1))]
              new_deck <- crPOST(self(x), body = toJSON(payload))
              new_deck <- SlideCatalog(crGET(new_deck))
              invisible(refresh(x))
          })

setMethod("[[<-", c("DeckCatalog", "ANY", "missing", "NULL"),
          function (x, i, j, value) {
              stopifnot(length(i) == 1)
              if (is.character(i) && !i %in% names(x)) {
                  return()
              } else if (is.numeric(i) && !i %in% seq_along(urls(x))) {
                  return()
              }
              delete(x[[i]])
              invisible(NULL)
          })

# CrunchDeck --------------------------------------------------------------

#' @rdname delete
#' @export
setMethod("delete", "CrunchDeck", function (x, ...) {
    if (!askForPermission(paste0("Really delete deck ", dQuote(name(x)), "?"))) {
        halt("Must confirm deleting multitable")
    }
    out <- crDELETE(self(x))
    invisible(out)
})

setMethod("slides", "CrunchDeck", function (x) {
    SlideCatalog(crGET(shojiURL(x, "catalogs", "slides")))
})

setMethod("name", "CrunchDeck", function(x) x@body$name)
setMethod("name<-", "CrunchDeck", function(x, value) {
    stopifnot(is.character(value))
    stopifnot(length(value) == 1)
    setEntitySlot(x, "name", value)
    })

setMethod("is.public", "CrunchDeck", function(x) deck@body$is_public)
setMethod("is.public<-", "CrunchDeck", function(x, value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1)
    setEntitySlot(x, "is_public", value)
})

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
    if (missing(file)) {
        file <-  paste0(name(deck), ext)
    }
    crDownload(dl_link, file)
}

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
    updateSlideTitle(x, value, "title")
})

setMethod("subtitles", "SlideCatalog", function (x){
    as.character(lapply(x@index, function(x) x$subtitle))
})

setMethod("subtitles<-", "SlideCatalog", function (x, value){
    updateSlideTitle(x, value, "subtitle")
})

updateSlideTitle <- function(x, value, type) {
    # TODO update a single title.
    stopifnot(is.character(value))
    payload <- mapply(function (item, new_title){
        item[[type]] <- new_title
        return(item)
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

setMethod("title", "CrunchSlide", function (x){
    return(x@body$title)
})

setMethod("subtitle", "CrunchSlide", function (x){
    return(x@body$subtitle)
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


setMethod("cubes", "CrunchSlide", function(x) cubes(analyses(x)))


# Analyses ----------------------------------------------------------------

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

              display_fields <- c("decimalPlaces", "percentageDirection",
                                  "vizType", "countsOrPercents", "uiView")
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
  all_fmla <- vapply(value, function(x) inherits(x, "fmla"), logical(1))
  if (any(!all_fmla)) {
      halt("Entry", which(!all_fmla), "is not a formula")
  }
  if (length(i) != length(value)) {
      halt("Invalid assignment. You tried to assign ", length(value), " formulas to ", length(i), "entries.")
  }
  mapply(function(analysis, fmla){
      analysis <- fmls
  }, analysis = x[[i]], fmla = value)
})


setMethod("cubes", "AnalysisCatalog", function(x) {
    lapply(seq_along(x@index), function(i) cube(x[[i]]))
})

setMethod("analysis<-", c("Analysis", "formula"), function(x, value) {
    ds <- loadDataset(datasetReference(analysis))
    analysis@body$query <- formulaToCubeQuery(fmla, data = ds)
    crPATCH(self(analysis), body = toJSON(analysis@body))
})

setMethod("cube", "Analysis", function (x) {
CrunchCube(crGET(cubeURL(x),
    query = list(query = toJSON(x@body$query)),
    filter = toJSON(x@body$query_environment$filter))
    )
})
