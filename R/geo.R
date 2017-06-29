#' Geography properties for crunch variables
#'
#' @param x a crunch variable
#' @param value value of the geography property to set
#'
#' @name geo
#' @aliases geo geo<- fetchGeoFile CrunchGeography, CrunchGeodata
NULL

#' @rdname geo
#' @export
setMethod("geo", "CrunchVariable", function (x) {
    var_geodata <- entity(x)@body$view$geodata[[1]]
    if (is.null(var_geodata)) {
        # if there's no geodata, return null.
        return()
    }

    geodatum <- CrunchGeodata(crGET(var_geodata$geodatum))
    geo_object <- CrunchGeography(
        geodatum = geodatum,
        feature_key = var_geodata$feature_key,
        match_field = var_geodata$match_field
    )

    return(geo_object)
})
#' @rdname geo
#' @export
setMethod("geo<-", c("CrunchVariable", "CrunchGeography"),
          function (x, value) {
              # if geodatum is of class CrunchGeodata, extact url
              if (is.Geodata(value$geodatum)) {
                  value$geodatum <- self(value$geodatum)
              } else {
                  value$geodatum <- value$geodatum
              }

              geodata <- list(geodata = list(value))

              dropCache(cubeURL(x))
              ent <- setEntitySlot(entity(x), "view", geodata)
              return(x)
          })

#' @rdname crunch-is
#' @export
is.Geodata <- function (x) inherits(x, "CrunchGeodata")

#' @rdname geo
#' @importFrom tools file_ext
#' @export
setMethod("fetchGeoFile", "CrunchGeography", function(x){
    if (!requireNamespace("geojsonio", quietly = TRUE)) {
        stop("The package geojsonio is needed for this function to work. Please install it.",
             call. = FALSE)
    }

    url <- x$geodatum$location
    fileext <- file_ext(url)
    if (fileext == "topojson") {
        geo_data <- geojsonio::topojson_read(url)
    } else if (fileext %in% c("geojson", "json")) {
        geo_data <- geojsonio::geojson_read(url)
    } else {
        halt("Unknown filetype ", dQuote(fileext), " in geodata url: ", url)
    }

    return(geo_data)
})

# TODO: make feature_key()<- match_field()<- geodatum()<- methods with more input checking
# TODO: checking intersection of category names to values of the specified feature_key
