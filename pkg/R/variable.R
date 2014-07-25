init.CategoricalVariable <- function (.Object, ...) {
    .Object <- callNextMethod()
    .Object@body$categories <- Categories(.Object@body$categories)
    return(.Object)
}
setMethod("initialize", "CategoricalVariable", init.CategoricalVariable)
setMethod("initialize", "CategoricalArrayVariable", init.CategoricalVariable)

setMethod("tuple", "CrunchVariable", function (x) x@tuple)
setMethod("tuple<-", "CrunchVariable", function (x, value) {
    x@tuple <- value
    return(x)
})

##' @rdname crunch-is
##' @export
is.variable <- function (x) inherits(x, "CrunchVariable")

##' @rdname crunch-is
##' @export
is.Numeric <- function (x) inherits(x, "NumericVariable")

##' @rdname crunch-is
##' @export
is.Categorical <- function (x) inherits(x, "CategoricalVariable")

##' @rdname crunch-is
##' @export
is.Text <- function (x) inherits(x, "TextVariable")

##' @rdname crunch-is
##' @export
is.Datetime <- function (x) inherits(x, "DatetimeVariable")

##' @rdname crunch-is
##' @export
is.Multiple <- function (x) inherits(x, "MultipleResponseVariable")

##' @rdname crunch-is
##' @export
is.MR <- is.Multiple

##' @rdname crunch-is
##' @export
is.MultipleResponse <- is.Multiple

##' @rdname crunch-is
##' @export
is.CA <- function (x) class(x) %in% "CategoricalArrayVariable" ## so it doesn't return true for MultipleResponse

##' @rdname crunch-is
##' @export
is.Array <- function (x) inherits(x, "CategoricalArrayVariable")

##' @rdname crunch-is
##' @export
is.CategoricalArray <- is.CA
    
as.variable <- function (x, subtype=NULL, tuple=VariableTuple()) {
    x <- CrunchVariable(x)
    if (is.variable(x)) {
        x <- subclassVariable(x, to=subtype)
        tuple(x) <- tuple
    }
    return(x)
}

## In case variable type has been changed, need to instantiate off of new type
##' @export
setMethod("refresh", "CrunchVariable", function (x) {
    as.variable(GET(self(x)), tuple=refresh(tuple(x)))
})

as.Numeric <- function (x) as.variable(x, "numeric")
as.Categorical <- function (x) as.variable(x, "categorical")
as.Text <- function (x) as.variable(x, "text")

subclassVariable <- function (x, to=NULL) {
    if (is.null(to)) to <- type(x)
    Constructor <- pickSubclassConstructor(to)
    return(Constructor(x))
}

pickSubclassConstructor <- function (x=NULL) {
    constructors <- list(
            categorical=CategoricalVariable,
            numeric=NumericVariable,
            text=TextVariable,
            datetime=DatetimeVariable,
            multiple_response=MultipleResponseVariable,
            categorical_array=CategoricalArrayVariable
        )
    if (!is.null(x)) x <- constructors[[x]]
    if (is.null(x)) x <- CrunchVariable
    return(x)
}

##' @export
setMethod("name", "CrunchVariable", function (x) tuple(x)$name)
##' @export
setMethod("name<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "name", value))
##' @export
setMethod("description", "CrunchVariable", function (x) tuple(x)$description)
##' @export
setMethod("description<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "description", value))
##' @export
setMethod("alias", "CrunchVariable", function (x) tuple(x)$alias)

##' @export
setMethod("alias<-", "CrunchVariable", 
    function (x, value) setTupleSlot(x, "alias", value))

##' @export
setMethod("categories", "CrunchVariable", function (x) NULL)
setMethod("categories", "CategoricalVariable", function (x) x@body$categories)
setMethod("categories", "CategoricalArrayVariable",
    function (x) x@body$categories)

##' @export
setMethod("categories<-", "CategoricalVariable", 
    function (x, value) setCrunchSlot(x, "categories", value))
setMethod("categories<-", "CategoricalArrayVariable", 
    function (x, value) setCrunchSlot(x, "categories", value))

.dichotomize.var <- function (x, i) {
    categories(x) <- dichotomize(categories(x), i)
    invisible(refresh(x))
}
.undichotomize.var <- function (x) {
    categories(x) <- undichotomize(categories(x))
    invisible(refresh(x))
}
##' @export
setMethod("dichotomize", "CategoricalVariable", .dichotomize.var)
##' @export
setMethod("dichotomize", "CategoricalArrayVariable", .dichotomize.var)
##' @export
setMethod("undichotomize", "CategoricalVariable", .undichotomize.var)
##' @export
setMethod("undichotomize", "CategoricalArrayVariable", .undichotomize.var)

setMethod("datasetReference", "CrunchVariable", function (x) x@urls$dataset_url)
setMethod("datasetReference", "ANY", function (x) NULL)

unbind <- function (x) {
    stopifnot(inherits(x, "CategoricalArrayVariable"))
    invisible(DELETE(self(x)))
}

setMethod("delete", "CategoricalArrayVariable", function (x, ...) {
    subvars <- x@body$subvariables
    out <- DELETE(self(x))
    lapply(subvars, DELETE)
    invisible(out)
})

setMethod("[", c("CrunchVariable", "CrunchExpression"), function (x, i, ...) {
    CrunchExpression(dataset_url=datasetReference(x), expression=zcl(x),
        filter=zcl(i))
})

.updatePayload <- function (variable, value) {
    if (is.Array(variable)) {
        subvars <- subvariables(variable)@index
        subids <- unlist(lapply(subvars, function (x) x$id))
        out <- lapply(subids, function (x) zcl(typeof(value, structure(zfunc("typeof", structure(list(variable=x), class="zcl")), class="zcl"))))
        # out <- rep(list(zcl(typeof(value, "categorical"))), length(subvars))
        names(out) <- subids
    } else {
        out <- structure(list(zcl(typeof(value, variable))),
            .Names=tuple(variable)$id)
    }
    return(out)
}

.updateVariable <- function (variable, value, filter=NULL) {
    payload <- list(command="update", 
        variables=.updatePayload(variable, value))
    payload[["filter"]] <- zcl(filter)
    update_url <- paste0(datasetReference(variable), "table/")
    cat(toJSON(payload))
    invisible(POST(update_url, body=toJSON(payload)))
}

.dispatchFilter <- function (x) {
    ## How to turn numeric into filter?
    # fil <- list(`function`="contains", args=list(
    #     list(column=I(seq_len(20)), type=list(class="numeric")), 
    #     list(column=I(x), type=list(class="numeric"))))
    # cat(toJSON(fil))
    # class(fil) <- "zcl"
    # fil <- seq_len(20) %in% x
    return(x)
}

.sigs <- list(
    c("TextVariable", "character"),
    c("NumericVariable", "numeric"),
    c("DatetimeVariable", "Date"),
    c("DatetimeVariable", "POSIXt")
)

for (i in seq_along(.sigs)) {
    setMethod("[<-", c(.sigs[[i]][1], "ANY", "missing", .sigs[[i]][2]),
        function (x, i, j, value) {
            if (missing(i)) i <- NULL
            out <- .updateVariable(x, value, filter=.dispatchFilter(i))
            return(x)
        })
}

## Set of functions to use in multiple dispatches
.categorical.update <- list(
    numeric=function (x, i, j, value) {
        if (missing(i)) i <- NULL
        if (all(c(NA, -1) %in% value)) {
            stop("Cannot have both NA and -1 when specifying category ids",
                call.=FALSE)
        }
        value[is.na(value)] <- -1
        invalids <- setdiff(value, ids(categories(x)))
        add.no.data <- -1 %in% invalids
        invalids <- setdiff(invalids, -1)
        if (length(invalids)) {
            plural <- length(invalids) > 1
            stop(paste0("Input value", ifelse(plural, "s ", " "),
                serialPaste(invalids), ifelse(plural, " are ", " is "),
                "not present in the category ids of variable ", dQuote(name(x))),
                call.=FALSE)
        }
        # if (add.no.data) {
        #     newcats <- categories(x)
        #     newcats[[length(newcats) + 1]] <- Category(.no.data)
        #     print(class(newcats))
        #     print(newcats)
        #     categories(x) <- newcats
        # }
        out <- .updateVariable(x, value, filter=.dispatchFilter(i))
        return(x)
    },
    character=function (x, i, j, value) {
        if (missing(i)) i <- NULL
        value[is.na(value)] <- "No Data"
        invalids <- setdiff(value, names(categories(x)))
        if (length(invalids)) {
            plural <- length(invalids) > 1
            stop(paste0("Input value", ifelse(plural, "s ", " "),
                serialPaste(invalids), ifelse(plural, " are ", " is "),
                "not present in the category names of variable ",
                dQuote(name(x))),
                call.=FALSE)
        }
        value <- n2i(value, categories(x))
        out <- .updateVariable(x, value, filter=.dispatchFilter(i))
        return(x)
    },
    factor=function (x, i, j, value) {
        if (missing(i)) i <- NULL
        x[i] <- as.character(value) ## Inefficient, but probably fine
        return(x)
    }
)

for (i in c("CategoricalVariable", "CategoricalArrayVariable")) {
    for (j in c("numeric", "character", "factor")) {
        setMethod("[<-", c(i, "ANY", "missing", j), .categorical.update[[j]])
    }
}

# setMethod("[<-", c("CrunchVariable", "ANY", "missing", "logical"),
#     function (x, i, j, value) {
#         cal <- match.call()
#         print(cal)
#         if (all(is.na(value))) {
#             value <- ifelse(is.Text(x), NA_character_, NA_integer_)
#         } else {
#             stop("Cannot update CrunchVariable with logical", call.=FALSE)
#         }
#         if (missing(i)) i <- NULL
#         i <- zcl(.dispatchFilter(i))
#         
#         x@fragments$missing_rules
#         
#         x[i] <- value
#         return(x)
#     })

setMethod("is.na<-", "CrunchVariable", function (x, value) {
    lab <- gsub('"', "", deparse(substitute(value)))
    value <- zcl(.dispatchFilter(value))
    payload <- structure(list(value), .Names=lab)
    cat(toJSON(payload))
    out <- POST(x@fragments$missing_rules, body=toJSON(payload))
    return(x)
})