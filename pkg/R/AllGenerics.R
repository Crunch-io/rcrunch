setGeneric("values", function (x) standardGeneric("values"))
setGeneric("values<-", function (x, value) standardGeneric("values<-"))

setGeneric("id", function (x) standardGeneric("id"))
setGeneric("is.selected", function (x) standardGeneric("is.selected"))

setGeneric("ids", function (x) standardGeneric("ids"))
setGeneric("is.dichotomized", function (x) standardGeneric("is.dichotomized"))
setGeneric("dichotomize", function (x, i) standardGeneric("dichotomize"))
setGeneric("undichotomize", function (x) standardGeneric("undichotomize"))
setGeneric("value", function (x) standardGeneric("value"))
setGeneric("value<-", function (x, value) standardGeneric("value<-"))
setGeneric("name", function (x) standardGeneric("name"))
setGeneric("name<-", function (x, value) standardGeneric("name<-"),
    signature="x")
setGeneric("description", function (x) standardGeneric("description"))
setGeneric("description<-",
    function (x, value) standardGeneric("description<-"), signature="x")
setGeneric("alias", function (x) standardGeneric("alias"))
setGeneric("alias<-", function (x, value) standardGeneric("alias<-"),
    signature="x")
setGeneric("aliases", function (x) standardGeneric("aliases"))
setGeneric("aliases<-", function (x, value) standardGeneric("aliases<-"),
    signature="x")

setGeneric("type", function (x) standardGeneric("type"))
setGeneric("type<-", function (x, value) standardGeneric("type<-"))
setGeneric("preUpload", function (x) standardGeneric("preUpload"), signature="x")
setGeneric("postUpload",
    function (source.var, crunch.var) standardGeneric("postUpload"),
    signature="source.var")

setGeneric("categories", function (x) standardGeneric("categories"))
setGeneric("categories<-", function (x, value) standardGeneric("categories<-"))
setGeneric("variables", function (x) standardGeneric("variables"))
setGeneric("variables<-", function (x, value) standardGeneric("variables<-"))
setGeneric("subvariables", function (x) standardGeneric("subvariables"))
setGeneric("subvariables<-", 
    function (x, value) standardGeneric("subvariables<-"))
setGeneric("datasetReference", function (x) standardGeneric("datasetReference"))
setGeneric("hide", function (x) standardGeneric("hide"))
setGeneric("unhide", function (x) standardGeneric("unhide"))

setGeneric("self", function (x) standardGeneric("self"))
setGeneric("refresh", function (x) standardGeneric("refresh"))
setGeneric("delete", function (x, ...) standardGeneric("delete"),
    signature="x")
setGeneric("readonly<-", function (x, value) standardGeneric("readonly<-"))
setGeneric("entities", function (x) standardGeneric("entities"))
setGeneric("entities<-", function (x, value) standardGeneric("entities<-"))
setGeneric("tuple", function (x) standardGeneric("tuple"))
setGeneric("tuple<-", function (x, value) standardGeneric("tuple<-"))
setGeneric("ordering", function (x) standardGeneric("ordering"))
setGeneric("ordering<-", function (x, value) standardGeneric("ordering<-"))
setGeneric("entity", function (x) standardGeneric("entity"))
setGeneric("active", function (x) standardGeneric("active"))
setGeneric("hidden", function (x) standardGeneric("hidden"))
setGeneric("archived", function (x) standardGeneric("archived"))
setGeneric("imported", function (x) standardGeneric("imported"))
setGeneric("pending", function (x) standardGeneric("pending"))

setGeneric("dim")
setGeneric("ncol")
setGeneric("mean")
setGeneric("length")
setGeneric("sd")
setGeneric("median")
setGeneric("min")
setGeneric("max")
setGeneric("na.omit")

##' @export
setGeneric("toVariable", function (x, ...) standardGeneric("toVariable"))

setGeneric("lapply")
setGeneric("is.na")
setGeneric("is.na<-")
setGeneric("%in%")

setGeneric("zcl", function (x) standardGeneric("zcl"))
