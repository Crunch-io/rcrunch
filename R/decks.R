
setMethod("initialize", "DeckCatalog", init.sortCatalog)

setMethod("decks", "CrunchDataset", function (x) {
    DeckCatalog(crGET(shojiURL(x, "catalogs", "decks")))
})

setMethod("[[", "DeckCatalog",  function (x, i, ...) {
    getEntity(x, i, CrunchDeck, ...)
})

setMethod("slides", "CrunchDeck", function(x) {
    SlideCatalog(crGET(shojiURL(x, "catalogs", "slides")))
})

setMethod("export", "CrunchDeck", function(x, path = "test") {
browser()
    dl_link <- crPOST(x@urls$export_url, config = add_headers(`Accept`="application/json"))
    download.file(dl_link, paste0(path, name(x), ".json"))
    return(out)
})

setMethod("[[", "SlideCatalog", function(x, i, ...) {
  getEntity(x, i, CrunchSlide)
})

