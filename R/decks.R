
setMethod("initialize", "DeckCatalog", init.sortCatalog)

# Deck Catalog ------------------------------------------------------------


setMethod("decks", "CrunchDataset", function (x) {
    DeckCatalog(crGET(shojiURL(x, "catalogs", "decks")))
})

# CrunchDeck --------------------------------------------------------------

setMethod("[[", "DeckCatalog",  function (x, i, ...) {
    getEntity(x, i, CrunchDeck, ...)
})

setMethod("[[<-", c("DeckCatalog", "character", "ANY", "CrunchDeck"),
          function (x, i, j, value) {
              payload <- value@body[c("name", "description", "is_public", "team")]
              payload$name <- i
              payload <- payload[vapply(payload, function(x)is.character(x) | is.logical(x), logical(1))]
              browser()
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


# Slide -------------------------------------------------------------------

setMethod("analyses", "CrunchSlide", {
    function (x) {
        AnalysisCatalog(crGET(shojiURL(x, "catalogs", "analyses")))
    }
})
setMethod("analysis", "CrunchSlide", {
    function (x) {
        out <- AnalysisCatalog(crGET(shojiURL(x, "catalogs", "analyses")))
        out[[1]]
    }
})

setMethod("cubes", "CrunchSlide", function(x) cubes(analyses(x)))


# Analyses ----------------------------------------------------------------

setMethod("[[", "AnalysisCatalog", function (x, i, ...) {
  getEntity(x, i, Analysis)
})

setMethod("cubes", "AnalysisCatalog", function(x) {
    lapply(seq_along(x@index), function(i) cube(x[[i]]))
})

setMethod("cube", "Analysis", function (x) {
CrunchCube(crGET(cubeURL(x),
    query=list(query=toJSON(x@body$query)),
    filter=toJSON(x@body$query_environment$filter))
    )
})





