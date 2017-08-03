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
        return(NULL)
    }
    geo_object <- CrunchGeography(
        geodatum = var_geodata$geodatum,
        feature_key = var_geodata$feature_key,
        match_field = var_geodata$match_field
    )

    return(geo_object)
})
#' @rdname geo
#' @export
setMethod("geo<-", c("CrunchVariable", "CrunchGeography"),
          function (x, value) {
              geodata <- list(geodata = list(value))

              ent <- setEntitySlot(entity(x), "view", geodata)
              dropCache(cubeURL(x))
              return(x)
          })

#' Add geodata metadata to a crunch variable
#' 
#' @param variable a Crunch variable to use for matching. This must be either
#' a text or a categorical variable.
#' 
#' @return a CrunchGeography object that can be assigned into `geo(variable)`
#' 
#' @examples 
#' \dontrun{
#' geo(ds$state) <- addGeoMetadata("state", data=ds)
#' }
#' @export
addGeoMetadata <- function (variable) {
    match_field <- "name" ## TODO: should this be a param?
    # Validate
    if (!is.variable(variable)) {
        halt(dQuote(deparse(substitute(variable))), " must be a Crunch Variable.")
    }
    if (has.categories(variable)) {
        cats <- names(categories(variable))
    } else if (is.Text(variable)) {
        cats <- as.vector(variable)
    } else {
        halt("The variable ", dQuote(substitute(variable)),
             " is neither a categorical or text variable.")
    }
    
    match_scores <- matchCatToFeat(cats)
    
    if (max(match_scores$value, na.rm = TRUE) == 0) {
        halt("None of the geographies match at all. Either the variable is",
             " wrong, or Crunch doesn't yet have geodata for this variable.")
    }
    if (nrow(match_scores) > 1) {
        halt("There is more than one possible match. Please specify the geography manually:\n",
             paste0(capture.output(print(match_scores[,c("value", "geodatum_name", "geodatum")])), collapse = "\n"))
    }
    
    match_geo <- CrunchGeography(geodatum=match_scores$geodatum,
                          feature_key=paste0('properties.',match_scores$property),
                          match_field=match_field)
    return(match_geo)
}

#' @rdname crunch-is
#' @export
is.Geodata <- function (x) inherits(x, "Geodata")

#' @rdname geo
#' @export
availableGeodata <- function (x = getAPIRoot()) {
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
availableGeodataFeatures <- function (x = getAPIRoot(), geodatum_fields=c("name", "description", "location")) {
    geo_cat <- availableGeodata(x)
    
    # grab each geodatum in order to get metadata
    # TODO: this should probably not be done through a series of GETs, 
    # but the current API requires it.
    geo_metadatas <- lapply(urls(geo_cat), function(x) Geodata(crGET(x)) )
    names(geo_metadatas) <- urls(geo_cat)
    
    out <- lapply(geo_metadatas, function (geography) {
        meta <- geography$metadata$properties
        lapply(names(meta), function (x) {
            # remove nulls
            values <- unlist(meta[[x]])
            values[is.null(values)] <-  NA_character_
            data.frame(property=x, value=values)
        })
    })
    
    out <- lapply(names(out), function (x) {
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
#' output `[availableGeodataFeatures]`) with a single property for a single geodatum.
#' @param categories a vector of categories to match
#' 
#' @return the Jaccard index for the values of the property given in 
#' feat_df and the vector of categories
#' 
#' @export
scoreCatToFeat <- function (features, categories) {
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
#' @importFrom stats aggregate
#' 
#' @export
matchCatToFeat <- function (categories, all_features = availableGeodataFeatures()) {
    scores <- aggregate(value~., data=all_features,
                        scoreCatToFeat, categories = categories)
    return(scores[scores$value %in% max(scores$value, na.rm = TRUE),])
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
# TODO: Add geo(ds$var) <- NULL method

