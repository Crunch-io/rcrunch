.updateVariable <- function (variable, value, filter=NULL) {
    ## Construct a ZCL update payload, then POST it
    payload <- list(command="update", 
        variables=.updatePayload(variable, value))
    payload[["filter"]] <- zcl(filter)
    update_url <- paste0(datasetReference(variable), "table/")
    # cat(toJSON(payload))
    invisible(POST(update_url, body=toJSON(payload)))
}

.updatePayload <- function (variable, value) {
    ## Construct the "variables" key of a ZCL update payload
    if (is.Array(variable)) {
        subvars <- index(subvariables(variable))
        subids <- unlist(lapply(subvars, function (x) x$id))
        out <- lapply(subids, function (x) zcl(typeof(value, structure(zfunc("typeof", structure(list(variable=x), class="zcl")), class="zcl"))))
        names(out) <- subids
    } else {
        out <- structure(list(zcl(typeof(value, variable))),
            .Names=tuple(variable)$id)
    }
    return(out)
}

.dispatchFilter <- function (f) {
    ## Given a valid R index (numeric, logical) or CrunchExp, make a ZCL (?) filter
    if (is.logical(f)) {
        ## Validate
        f <- which(f)
    }
    if (is.numeric(f)) {
        ## Validate
        
        fun <- ifelse(length(f) == 1, "==", "in")
        f <- zfunc(fun, zfunc("row"), f - 1)  ## 1-base to 0-base counting
    }
    return(f)
}

setMethod("[<-", c("CrunchVariable", "ANY", "missing", "ANY"),
    function (x, i, j, value) {
        ## Backstop error so you don't get "Object of class S4 is not subsettable"
        halt(paste("Cannot update", class(x), "with type", class(value)))
    })
    
.sigs <- list(
    c("TextVariable", "character"),
    c("NumericVariable", "numeric"),
    c("DatetimeVariable", "Date"),
    c("DatetimeVariable", "POSIXt"),
    c("CrunchVariable", "CrunchExpression")
)

for (i in seq_along(.sigs)) {
    setMethod("[<-", c(.sigs[[i]][1], "ANY", "missing", .sigs[[i]][2]),
        function (x, i, j, value) {
            if (missing(i)) i <- NULL
            out <- .updateVariable(x, value, filter=.dispatchFilter(i))
            return(x)
        })
}

setMethod("[<-", c("CrunchVariable", "CrunchExpression", "missing", "CrunchExpression"),
    function (x, i, j, value) {
        if (!identical(zcl(i), value@filter)) {
            halt("Cannot update a variable with a value that has a different filter")
        } else {
            callNextMethod()
        }
    })

## Set of functions to use in multiple dispatches
.categorical.update <- list(
    numeric=function (x, i, j, value) {
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
            halt(paste0("Input value", ifelse(plural, "s ", " "),
                serialPaste(invalids), ifelse(plural, " are ", " is "),
                "not present in the category ids of variable ", dQuote(name(x))))
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
            halt(paste0("Input value", ifelse(plural, "s ", " "),
                serialPaste(invalids), ifelse(plural, " are ", " is "),
                "not present in the category names of variable ",
                dQuote(name(x))))
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
#           ## For assigning NA
#         cal <- match.call()
#         print(cal)
#         if (all(is.na(value))) {
#             value <- ifelse(is.Text(x), NA_character_, NA_integer_)
#         } else {
#             halt("Cannot update CrunchVariable with logical")
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
    ## Temporarily kill this method until API supports correctly
    halt("is.na<- not yet supported for CrunchVariables")
    
    lab <- gsub('"', "", deparse(substitute(value)))
    value <- zcl(.dispatchFilter(value))
    payload <- structure(list(value), .Names=lab)
    # cat(toJSON(payload))
    out <- POST(x@fragments$missing_rules, body=toJSON(payload))
    return(x)
})
