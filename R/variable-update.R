.updateVariable <- function(variable, value, filter = NULL) {
    ## if the value is an expression, check that the value is not a derivation,
    ## those can be dangerous if they change the variable type.
    ## only needed until https://www.pivotaltracker.com/story/show/154982045 is
    ## resolved
    if (is.Expr(value)) {
        castFuncs <- c(
            "cast", "format_datetime", "parse_datetime",
            "numeric_to_datetime", "datetime_to_numeric"
        )

        if (has.function(value@expression, castFuncs)) {
            halt(
                "A variable cannot be updated with a derivation that changes ",
                "its type. Are you trying to overwrite a variable with a ",
                "derivation of itself to change the type? If so, you might ",
                "want to use `type(ds$variable)<-` instead."
            )
        }
    }

    ## Construct a ZCL update payload, then POST it
    payload <- list(
        command = "update",
        variables = .updatePayload(variable, value)
    )
    payload[["filter"]] <- zcl(filter)
    dref <- datasetReference(variable)
    update_url <- paste0(dref, "table/")
    out <- crPOST(update_url, body = toJSON(payload))
    dropCache(dref)
    invisible(out)
}

.updatePayload <- function(variable, value) {
    ## Construct the "variables" key of a ZCL update payload
    if (is.Array(variable)) {
        out <- sapply(urls(subvariables(variable)), function(x) {
            zcl(value)
        }, simplify = FALSE)
    } else {
        out <- structure(list(zcl(value)),
            .Names = self(variable)
        )
    }

    ## Check for missingness and replace the NAs with special values
    out <- lapply(out, function(x) {
        if ("column" %in% names(x)) {
            missings <- is.na(x$column)
            if (any(missings)) {
                x$column <- as.list(x$column)
                x$column[missings] <- rep(
                    list(.no.data.value(variable)), sum(missings)
                )
            }
        }
        return(x)
    })
    return(out)
}

.dispatchFilter <- function(f) {
    ## Given a valid R index (numeric, logical) or CrunchExp, make a ZCL (?) filter
    ## TODO: f <- zcl(f) ?
    if (is.logical(f)) {
        ## TODO: Validate
        ## TODO: send 0/1
        # f <- as.integer(f)
        # And what about NA? set as 0? set to -1? let downstream set to -1?
        f <- which(f)
    } # TODO: if 0/1, else if
    if (is.numeric(f)) {
        ## TODO: Validate
        f <- .seqCrunch(zfunc("row"), f - 1)
    }
    return(f)
}

#' @rdname crunch-extract
#' @export
setMethod("[<-", c("CrunchVariable", "ANY", "missing", "ANY"), .backstopUpdate)

#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CrunchVariable", "ANY", "missing", "NULL"),
    function(x, i, j, value) return(NULL)
)

.var.updater <- function(x, i, j, value) {
    if (missing(i)) i <- NULL
    .updateVariable(x, value, filter = .dispatchFilter(i))
    return(x)
}

#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("TextVariable", "ANY", "missing", "character"),
    .var.updater
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("NumericVariable", "ANY", "missing", "numeric"),
    .var.updater
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("DatetimeVariable", "ANY", "missing", "Date"),
    .var.updater
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("DatetimeVariable", "ANY", "missing", "POSIXt"),
    .var.updater
)

.var.updater.crunchobj <- function(x, i, j, value) {
    if (missing(i)) i <- NULL
    if (!identical(zcl(i), zcl(activeFilter(value)))) {
        halt("Cannot update a variable with an expression that has a different filter")
    } else {
        .var.updater(x, i, j, value)
    }
}

#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CrunchVariable", "ANY", "missing", "CrunchExpr"),
    .var.updater.crunchobj
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CrunchVariable", "ANY", "missing", "CrunchVariable"),
    .var.updater.crunchobj
)

#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CrunchVariable", "ANY", "missing", "CrunchLogicalExpr"),
    .backstopUpdate
)

## Set of functions to use in multiple dispatches
.categorical.update <- list(
    numeric = function(x, i, j, value) {
        if (missing(i)) i <- NULL
        if (all(c(NA, -1) %in% value)) {
            halt("Cannot have both NA and -1 when specifying category ids")
        }
        value[is.na(value)] <- -1
        invalids <- setdiff(value, ids(categories(x)))
        add.no.data <- -1 %in% invalids
        invalids <- setdiff(invalids, -1)
        if (length(invalids)) {
            plural <- length(invalids) > 1
            halt(paste0(
                "Input value", ifelse(plural, "s ", " "),
                serialPaste(invalids), ifelse(plural, " are ", " is "),
                "not present in the category ids of variable ", dQuote(name(x))
            ))
        }
        if (add.no.data) {
            x <- addNoDataCategory(x)
        }
        out <- .updateVariable(x, value, filter = .dispatchFilter(i))
        return(x)
    },
    character = function(x, i, j, value) {
        if (missing(i)) i <- NULL
        value[is.na(value)] <- "No Data"
        invalids <- setdiff(value, names(categories(x)))
        add.no.data <- "No Data" %in% invalids
        invalids <- setdiff(invalids, "No Data")
        if (length(invalids)) {
            plural <- length(invalids) > 1
            halt(paste0(
                "Input value", ifelse(plural, "s ", " "),
                serialPaste(invalids), ifelse(plural, " are ", " is "),
                "not present in the category names of variable ",
                dQuote(name(x))
            ))
        }
        if (add.no.data) {
            x <- addNoDataCategory(x)
        }
        value <- n2i(value, categories(x))
        out <- .updateVariable(x, value, filter = .dispatchFilter(i))
        return(x)
    },
    factor = function(x, i, j, value) {
        if (missing(i)) i <- NULL
        x[i] <- as.character(value) ## Inefficient, but probably fine
        return(x)
    }
)

#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CategoricalVariable", "ANY", "missing", "numeric"),
    .categorical.update[["numeric"]]
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CategoricalVariable", "ANY", "missing", "character"),
    .categorical.update[["character"]]
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CategoricalVariable", "ANY", "missing", "factor"),
    .categorical.update[["factor"]]
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CategoricalArrayVariable", "ANY", "missing", "numeric"),
    .categorical.update[["numeric"]]
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CategoricalArrayVariable", "ANY", "missing", "character"),
    .categorical.update[["character"]]
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CategoricalArrayVariable", "ANY", "missing", "factor"),
    .categorical.update[["factor"]]
)

#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("CrunchVariable", "ANY", "missing", "logical"),
    function(x, i, j, value) {
        if (all(is.na(value))) {
            ## For assigning NA
            value <- .no.data.value(x, add.type = TRUE)
            if (has.categories(x)) {
                return(.categorical.update[["numeric"]](x, i, j, value))
            }
            if (missing(i)) i <- NULL
            out <- .updateVariable(x, value, filter = .dispatchFilter(i))
            return(x)
        } else if (has.categories(x) &&
            all(names(categories(x)) %in% c("True", "False", "No Data"))) {
            ## TODO: next, make this more 3VL-aware
            ## This is "logical-as-categorical", so we can take TRUE/FALSE values
            value <- c("True", "False")[2L - as.integer(value)]
            return(.categorical.update[["character"]](x, i, j, value))
        } else {
            ## halt()
            .backstopUpdate(x, i, j, value)
        }
    }
)

.no.data.value <- function(x, add.type = FALSE) {
    x_type <- type(x)
    if (has.categories(x_type)) {
        return(NA)
    } else {
        out <- NA
        if (add.type) {
            out <- list(value = NA, type = list(class = x_type))
            if (x_type == "datetime") out$type$resolution <- resolution(x)
        }
        return(out)
    }
}

#' @rdname crunch-extract
#' @export
setMethod("is.na<-", "CrunchVariable", function(x, value) {
    x[value] <- NA
})
