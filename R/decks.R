
setMethod("initialize", "DeckCatalog", init.sortCatalog)

setMethod("decks", "CrunchDataset", function (x) {
    DeckCatalog(crGET(shojiURL(x, "catalogs", "decks")))
})

setMethod("[[", "DeckCatalog",  function (x, i, ...) {
    getEntity(x, i, CrunchDeck, ...)
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

setMethod("export", "CrunchDeck", function (x, path) {
    # This is to avoid having a default argument in the generic
    if (missing(path)) {
        path <- ""
    }
    dl_link <- crPOST(x@urls$export_url, config = add_headers(`Accept`="application/json"))
    download.file(dl_link, paste0(path, name(x), ".json"))
})

setMethod("[[", "SlideCatalog", function (x, i, ...) {
  getEntity(x, i, CrunchSlide)
})

