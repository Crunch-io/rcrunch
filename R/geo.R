#' Geography properties for crunch variables
#'
#' Crunch stores geographic data as variable metadata. There are a number of
#' functions that help access and change this metadata.
#'
#' `geo` retrieves the geographic information associate with a variable. If there is geographic information it returns an object of class `CrunchGeography` otherwise it returns `NULL`.
#'
#' `CrunchGeography` objects store geography metadata from a variable. There are three slots:
#' * `geodatum` an object of class CrunchGeodata which stores references to the Crunch-hosted (geo|topo)json to use
#' * `feature_key` a character that is the feature inside of the (geo|topo)json to match `match_field` to (e.g. properties.name)
#' * `match_field` a character that is the the variable metadata information to match to `feature_key` to (e.g. name)
#'
#' @param x a crunch variable
#' @param value value of the geography property to set
#' @return geographic information of class `CrunchGeography` (`NULL` if there is none)
#'
#' @name geo
#'
#' @examples
#' \dontrun{
#' geo(ds$location)
#'
#' geo(ds$location)$feature_key <- "properties.name"
#' geo(ds$location)$match_field <- "name"
#' }
#' @aliases geo geo<- CrunchGeography
NULL

#' @rdname geo
#' @export
setMethod("geo", "CrunchVariable", function (x) {
    var_geodata <- entity(x)@body$view$geodata[[1]]
    if (is.null(var_geodata)) {
        # if there's no geodata, return null.
        return()
    }

    geodatum <- Geodata(crGET(var_geodata$geodatum))
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
is.Geodata <- function (x) inherits(x, "Geodata")


#' @rdname geo
#' @export
availableGeodata <- function(x = getAPIRoot()) {
    return(GeoCatalog(crGET(shojiURL(x, "catalogs", "geodata"))))
}

#' all available property features for all available geographies
#' 
#' @param x an API root address (default: the R-session default)
#' @param geodatum_fields character, what peices of information about each 
#' geodatum should be retained? (default: `c("name", "description", "location")``)
#' 
#' @return a (tidy) dataframe with all of the available features and 
#' geographies for matching
#' 
#' @export
availableFeatures <- function(x = getAPIRoot(), geodatum_fields=c("name", "description", "location")) {
    geo_cat <- availableGeodata(x)
    out <- lapply(geo_cat, function(geography) {
        meta <- geography$metadata$properties
        lapply(names(meta), function(x) {
            # remove nulls
            values <- unlist(meta[[x]])
            values[is.null(values)] <-  NA_character_
            data.frame(property=x, value=values)
        })
    })
    
    out <- lapply(names(out), function(x) {
        dfs <- do.call("rbind", out[[x]])
        dfs$geodatum <- x
        dfs[paste0("geodatum_", geodatum_fields)] <- geo_cat[[x]][geodatum_fields]
        return(dfs)
    })
    return(do.call("rbind", out))
}


#' Score similarity between a feature dataframe and categories
#' 
#' Implemented using the Jaccard index, where a number closer to 1 is 
#' more similar.
#' 
#' @param features a vector of features to match (usually from a subset of the
#' output `[availableFeatures]`) with a single property for a single geodatum.
#' @param categories a vector of categories to match
#' 
#' @return the Jaccard index for the values of the property given in 
#' feat_df and the vector of categories
#' 
#' @export
scoreCat2Feat <- function(features, categories) {
    feats <- unique(features)
    cats <- unique(categories)
    
    intersection <- length(cats[cats %in% feats])
    union <- length(union(cats, feats))
    
    return(intersection/union)
}

#' Match categories with features from geodata
#' 
#' @param categories a vector of categories to match
#' @param all_features a dataframe of all available geodata features. (default:
#' downloaded from Crunch servers)
#' 
#' @return geodatum to associate with the variable that produced categories
#' 
#' @export
matchCat2Feat <- function(categories, all_features = availableFeatures()) {
    scores <- aggregate(value~., data=all_features,
                        scoreCat2Feat, categories = categories)
    maxima <- which(scores$value == max(scores$value, na.rm = TRUE))
    return(scores[maxima,])
}

# TODO: show geodatums prettier for selection.
# TODO: make feature_key()<- match_field()<- geodatum()<- methods with more 
#       input checking
# TODO: geodatum()<- method should attempt some matching based on what's 
#       already in match_field
# TODO: availableGeodata() should be subsettable by name, and be asignable into 
#       geodatum(geo(ds$var)) or the like
# TODO: make sure the full resolution jsons are available
# TODO: GIS opertaions (contains, union, etc.)

