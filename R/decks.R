#' @include shoji-catalog.R
NULL

# Generics ----------------------------------------------------------------

#' Manipulate deck titles
#'
#' Crunch slides have titles and subtitles. You can change these features at
#' either the deck level by assigning a character vector which is
#' the same length as the deck to the CrunchDeck, or by assigning character strings to the
#' the slide.
#' @param x a `CrunchDeck` or `CrunchSlide`
#' @param value character, the new title or subtitle
#' @rdname deck-titles
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

#' Get a dataset's DeckCatalog
#'
#' Crunch decks are stored in catalogs. This function returns those catalogs so
#' that you can access and manipulate decks in R.
#'
#' @param x a Crunch Dataset
#' @param value a `CrunchDeck` to add
#'
#' @return a DeckCatalog
#' @rdname decks
#' @export
setGeneric("decks", function(x) standardGeneric("decks"))
#' @rdname decks
#' @export
setGeneric("decks<-", function(x, value) standardGeneric("decks<-"))
#' @rdname decks
#' @export
setMethod("decks", "CrunchDataset", function(x) {
    return(DeckCatalog(crGET(shojiURL(x, "catalogs", "decks"))))
})

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

#' @rdname crunch-extract
#' @export
setMethod("[[", "DeckCatalog", function(x, i, ...) {
    getEntity(x, i, CrunchDeck, ...)
})

#' @rdname crunch-extract
#' @export
setMethod("[[", c("DeckCatalog", "character", "ANY"), function(x, i, ...) {
    if (length(i) > 1) {
        halt("You can only select one deck at a time")
    }
    matches <- names(x) %in% i
    if (all(!matches)) {
        halt(dQuote(i), " is not present in deck catalog")
    }
    index <- which(matches)
    if (sum(matches) > 1) {
        warning(
            dQuote(i),
            " does not uniquely identify elements. Returning the first match"
        )
        index <- index[1]
    }
    getEntity(x, index, CrunchDeck, ...)
})

#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("DeckCatalog", "ANY", "missing", "CrunchDeck"),
    modifyCatalogInPlace
)

#' This function is no-op because the items are already updated on the server
#' with other methods called prior to it.
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("CrunchDeck", "ANY", "missing", "AnalysisCatalog"),
    function(x, i, j, value) invisible(refresh(x))
)

#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("SlideCatalog", "ANY", "missing", "CrunchSlide"),
    modifyCatalogInPlace
)

#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("AnalysisCatalog", "ANY", "missing", "Analysis"),
    modifyCatalogInPlace
)


# CrunchDeck --------------------------------------------------------------

#' @rdname crunch-extract
#' @export
setMethod("[[", "CrunchDeck", function(x, i, ...) slides(x)[[i]])

#' @rdname crunch-extract
#' @export
setMethod("[[<-", "CrunchDeck", function(x, i, j, value) {
    slides(x)[[i]] <- value
    return(invisible(refresh(x)))
})

#' Access the slides of a CrunchDeck
#'
#' Return a `SlideCatalog` from a `CrunchDeck`. All slide catalog methods should be
#' available for `CrunchDecks`, but this function is used internally to model the
#' API.
#'
#' @param x a CrunchDeck
#' @param value a `SlideCatalog` or `CrunchSlide` to add
#'
#' @return a `SlideCatalog`
#' @rdname slides
#' @export
setGeneric("slides", function(x) standardGeneric("slides"))
#' The following function is no-op because the items are already updated on the
#' server with other methods called prior to it.
#' @rdname slides
#' @export
setGeneric("slides<-", function(x, value) standardGeneric("slides<-"))
#' @rdname slides
#' @export
setMethod("slides", "CrunchDeck", function(x) {
    return(SlideCatalog(crGET(shojiURL(x, "catalogs", "slides"))))
})
#' @rdname slides
#' @export
setMethod("slides<-", "CrunchDeck", function(x, value) invisible(refresh(x)))

#' @rdname describe-entity
#' @export
setMethod("name<-", "CrunchDeck", function(x, value) {
    stopifnot(is.singleCharacter(value))
    out <- setEntitySlot(x, "name", value)
    return(invisible(out))
})

#' @rdname describe-entity
#' @export
setMethod("description", "CrunchDeck", function(x) x@body$description)
#' @rdname describe-entity
#' @export
setMethod("description<-", "CrunchDeck", function(x, value) {
    stopifnot(is.singleCharacter(value))
    out <- setEntitySlot(x, "description", value)
    return(invisible(out))
})

#' @rdname describe-catalog
#' @export
setMethod("names", "CrunchDeck", function(x) titles(slides(x)))
#' @rdname describe-catalog
#' @export
setMethod("names<-", "CrunchDeck", function(x, value) {
    slide_cat <- slides(x)
    titles(slide_cat) <- value
})

#' @rdname deck-titles
#' @export
setMethod("titles", "CrunchDeck", function(x) titles(slides(x)))
#' @rdname deck-titles
#' @export
setMethod("titles<-", "CrunchDeck", function(x, value) {
    slide_cat <- slides(x)
    titles(slide_cat) <- value
    invisible(refresh(x))
})

#' @rdname deck-titles
#' @export
setMethod("subtitles", "CrunchDeck", function(x) subtitles(slides(x)))
#' @rdname deck-titles
#' @export
setMethod("subtitles<-", "CrunchDeck", function(x, value) {
    slide_cat <- slides(x)
    subtitles(slide_cat) <- value
    invisible(refresh(x))
})

#' Export a Crunch Deck
#'
#' Crunch decks can be exported as excel or json files.
#'
#' @param deck A CrunchDeck
#' @param file The file path to save the exported deck
#' @param format Either `"xlsx"`, `"pptx"`, or `"json"`
#'
#' @return  the filename (`file`, if specified, or the the autogenerated file
#' name).
#' @export
exportDeck <- function(deck, file, format = c("xlsx", "pptx", "json")) {
    if (!inherits(deck, "CrunchDeck")) {
        halt("exportDeck is only available for CrunchDecks.")
    }
    url <- deck@views$export
    format <- match.arg(format)
    accept <- extToContentType(format)
    if (missing(file)) {
        file <- paste0(name(deck), ".", format)
    }
    dl_link <- crPOST(
        url,
        body = toJSON(list(
            element = "shoji:entity",
            body = emptyObject()
            )),
        config = add_headers(`Accept` = accept)
    )
    crDownload(dl_link, file)
}


#' @rdname weight
#' @export
setMethod("weight<-", c("CrunchDeck", "ANY"), function(x, value) {
    lapply(seq_along(x), function(slide_num) {
        weight(x[[slide_num]]) <- value
    })

    return(refresh(x))
})

#' @rdname analysis-methods
#' @export
setMethod("filter<-", c("CrunchDeck", "ANY"), function(x, value) {
    lapply(seq_along(x), function(slide_num) {
        filter(x[[slide_num]]) <- value
    })

    return(refresh(x))
})
