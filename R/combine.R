combine <- function (variable, categories, name, ...) {
    ds.entity <- ShojiObject(crGET(datasetReference(variable)))
    var.cat.url <- shojiURL(ds.entity, "catalogs", "variables")
    
    return(var.cat.url)
}