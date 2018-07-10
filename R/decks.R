
setMethod("initialize", "DeckCatalog", init.sortCatalog)

setMethod("decks", "CrunchDataset", function (x) {
    DeckCatalog(crGET(shojiURL(x, "catalogs", "decks")))
})

setMethod("[[", "DeckCatalog",  function (x, i, ...) {
    getEntity(x, i, CrunchDeck, ...)
})

setMethod("slides", "CrunchDeck", function (x) {
    SlideCatalog(crGET(shojiURL(x, "catalogs", "slides")))
})

setMethod("name", "CrunchDeck", function(x) x@body$name)
setMethod("name<-", "CrunchDeck", function(x, value) {
    stopifnot(is.character(value))
    stopifnot(length(value) == 1)
    x@body$name <- value
    crPATCH(self(x), body = toJSON(x@body))
    invisible(refresh(x))
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

