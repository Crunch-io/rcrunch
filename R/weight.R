#' Dataset weights
#' @param x a Dataset
#' @param value a Variable to set as weight, or NULL to remove the existing
#' weight
#' @return For the getter, a Variable if there is a weight, else NULL. For the
#' setter, x, modified accordingly
#' @export
weight <- function (x) {
    stopifnot(is.dataset(x))
    ## Future API: on "preferences"
    prefs <- crGET(shojiURL(x, "fragments", "preferences"))
    if ("weight" %in% names(prefs$body)) {
        ## The future is here
        w <- prefs$body$weight
    } else {
        ## Old behavior
        w <- x@body$weight
    }
    if (!is.null(w)) {
        w <- CrunchVariable(allVariables(x)[[w]], filter=activeFilter(x))
    }
    return(w)
}

#' @rdname weight
#' @export
`weight<-` <- function (x, value) {
    stopifnot(is.dataset(x))
    if (is.variable(value)) {
        value <- self(value)
    } else if (!is.null(value)) {
        halt("Weight must be a Variable or NULL")
    }
    
    ## Future API: on "preferences"
    prefs <- crGET(shojiURL(x, "fragments", "preferences"))
    if ("weight" %in% names(prefs$body)) {
        ## The future is here
        crPATCH(shojiURL(x, "fragments", "preferences"), body=toJSON(list(weight=value)))
        x <- refresh(x)
    } else {
        ## Old behavior
        x <- setEntitySlot(x, "weight", value)
    }
    return(x)
}

weightVariables <- function(x) {
    stopifnot(is.dataset(x))
    
    return(tryCatch(VariableCatalog(crGET(shojiURL(x, "catalogs", "weight_variables"))),
        error=function(e){
            if (e$message == 'No URL “weight_variables” in collection “catalogs”') return(VariableCatalog(data.frame))
            else print(e)
        }))
}
