CrunchDataset <- setClass("CrunchDataset", contains="list", 
    representation(
        name="character",
        self.url="character",
        urls="list"
    ))

validCrunchDataset <- function (object) {
    oname <- object@name
    are.vars <- vapply(object, is.variable, logical(1))
    if (!all(are.vars)) {
        badcount <- sum(!are.vars)
        val <- paste0("Invalid dataset ", sQuote(oname), ": ", badcount, 
            ifelse(badcount>1, 
                " elements are not Crunch variable objects.", 
                " element is not a Crunch variable object."))
    } else {
        val <- TRUE  
    }
    return(val)
}
setValidity("CrunchDataset", validCrunchDataset)

.cr.dataset.shoji <- function (x) {
    bod <- c(x[["body"]], urls=x[["urls"]])
    out <- do.call("new", c(Class="CrunchDataset", 
        bod[names(bod) %in% slotNames("CrunchDataset")]))
    out@self.url <- x[["self"]]
    ## get variables
    return(out)
}

is.dataset <- function (x) inherits(x, "CrunchDataset")

setGeneric("name", function (x) standardGeneric("name"))
setMethod("name", "CrunchDataset", function (x) x@name)

setAs("shoji", "CrunchDataset", function (from) .cr.dataset.shoji(from))

as.dataset <- function (x) as(x, "CrunchDataset")