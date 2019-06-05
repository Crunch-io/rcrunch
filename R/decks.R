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

#' Get and set slide analyses
#'
#' Slides are composed of analyses, which are effectively `CrunchCubes` with some
#' additional metadata. You can get and set a slide's Analysis Catalog with the
#' `analyses` method, and access an individual analysis with `analysis`.
#'
#' You can get the CrunchCube from a slide or analysis with the `cube` method and
#' from a CrunchDeck with `cubes`. Analyses can be changed by assigning a formula
#' into the `query` function.
#' @param x A CrunchSlide, AnalysisCatalog, or Analysis
#' @param value for the setter, a query
#' @rdname analysis-methods
#' @export
#' @examples
#' \dontrun{
#' analysis(slide)
#' cube(slide)
#' cubes(deck)
#' query(slide) <- ~ cyl + wt
#' }
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

#' Get or set a slide's display settings
#'
#' A slide's display settings can be modified by assigning a named list
#' @param x a CrunchSlide, Analysis, or AnalysisCatalog
#' @param value a named list, for valid settings see docs.crunch.io
#' @rdname display-settings
#' @export
setGeneric("displaySettings", function(x) standardGeneric("displaySettings"))
#' @rdname display-settings
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

# CrunchDeck --------------------------------------------------------------

#' @rdname crunch-extract
#' @export
setMethod("[[", "CrunchDeck", function(x, i, ...) slides(x)[[i]])

#' @rdname crunch-extract
#' @export
setMethod("[[<-", "CrunchDeck", function(x, i, j, value) {
    slideCat <- slides(x)
    slideCat[[i]] <- value
    invisible(refresh(x))
})

#' Access the slides of a CrunchDeck
#'
#' Return a SlideCatalog from a CrunchDeck. All slide catalog methods should be
#' available for CrunchDecks, but this function is used internally to model the
#' API.
#'
#' @param x a CrunchDeck
#' @return a Slide Catalog
#' @export
slides <- function(x) {
    SlideCatalog(crGET(shojiURL(x, "catalogs", "slides")))
}

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
    dl_link <- crPOST(url, config = add_headers(`Accept` = accept))
    crDownload(dl_link, file)
}

#' @rdname display-settings
#' @export
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
