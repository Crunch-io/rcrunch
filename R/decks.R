
# Generics ----------------------------------------------------------------

#' Manipulate deck titles
#'
#' Crunch slides have titles and subtitles. You can change these features at
#' either the deck level by assigning a character vector which is
#' the same length as the deck to the CrunchDeck, or by assigning character strings to the
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
decks <- function(dataset) {
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

setMethod("[[", "DeckCatalog", function(x, i, ...) {
    getEntity(x, i, CrunchDeck, ...)
})

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
        warning(dQuote(i), " does not uniquely identify elements. Returning the first match")
        index <- index[1]
    }
    getEntity(x, index, CrunchDeck, ...)
})

setMethod("show", "DeckCatalog", function(object) {
    out <- as.data.frame(object)
    print(out[, c("name", "team", "is_public", "owner_name")])
})

# CrunchDeck --------------------------------------------------------------

setMethod("[[", "CrunchDeck", function(x, i, ...) slides(x)[[i]])

setMethod("[[<-", "CrunchDeck", function(x, i, j, value) {
    slideCat <- slides(x)
    slideCat[[i]] <- value
    invisible(refresh(x))
})

#' @rdname delete
#' @export
setMethod("delete", "CrunchDeck", function(x, ...) {
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
slides <- function(x) {
    SlideCatalog(crGET(shojiURL(x, "catalogs", "slides")))
}

setMethod("name<-", "CrunchDeck", function(x, value) {
    stopifnot(is.singleCharacter(value))
    out <- setEntitySlot(x, "name", value)
    return(invisible(out))
})

setMethod("description", "CrunchDeck", function(x) x@body$description)
setMethod("description<-", "CrunchDeck", function(x, value) {
    stopifnot(is.singleCharacter(value))
    out <- setEntitySlot(x, "description", value)
    return(invisible(out))
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
setMethod("titles<-", "CrunchDeck", function(x, value) {
    slide_cat <- slides(x)
    titles(slide_cat) <- value
    invisible(refresh(x))
})

setMethod("subtitles", "CrunchDeck", function(x) subtitles(slides(x)))
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

setMethod("cubes", "CrunchDeck", function(x) {
    out <- lapply(seq_len(length(x)), function(i) {
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

setMethod("show", "CrunchDeck", function(object) {
    print(cubes(object))
})
