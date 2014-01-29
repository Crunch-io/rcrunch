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

setGeneric("type", function (x) standardGeneric("type"))
setGeneric("type<-", function (x, value) standardGeneric("type<-"))
setGeneric("preUpload", function (x) standardGeneric("preUpload"), signature="x")
setGeneric("postUpload", 
    function (source.var, crunch.var) standardGeneric("postUpload"),
    signature="source.var")

setGeneric("categories", function (x) standardGeneric("categories"))
setGeneric("categories<-", function (x, value) standardGeneric("categories<-"))
setGeneric("datasetReference", function (x) standardGeneric("datasetReference"))
setGeneric("hide", function (x) standardGeneric("hide"))
setGeneric("unhide", function (x) standardGeneric("unhide"))

setGeneric("self", function (x) standardGeneric("self"))
setGeneric("refresh", function (x) standardGeneric("refresh"))
setGeneric("delete", function (x) standardGeneric("delete"))
setGeneric("readonly<-", function (x, value) standardGeneric("readonly<-"))

setGeneric("dim")
setGeneric("ncol")
setGeneric("mean")
setGeneric("sd")
setGeneric("median")
setGeneric("na.omit")

setGeneric("toVariable", function (x, ...) standardGeneric("toVariable"))