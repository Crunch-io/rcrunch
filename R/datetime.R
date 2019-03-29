from8601 <- function(x) {
    ## Crunch timestamps look like "2015-02-12T10:28:05.632000+00:00"

    if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", na.omit(x)))) {
        ## return Date if resolution == D
        return(as.Date(x))
    }

    ## Check for timezone
    if (any(grepl("+", x, fixed = TRUE))) {
        ## First, strip out the : in the time zone
        x <- sub("^(.*[+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
        pattern <- "%Y-%m-%dT%H:%M:%OS%z"
    } else {
        pattern <- "%Y-%m-%dT%H:%M:%OS"
    }
    ## Then parse
    return(strptime(x, pattern, tz = "UTC"))
}

setGeneric("rollupResolution<-", function(x, value) standardGeneric("rollupResolution<-"))

#' @rdname expressions
#' @export
rollup <- function(x, resolution = rollupResolution(x)) {
    validateResolution(force(resolution))
    if (is.variable(x) && !is.Datetime(x)) {
        halt("Cannot rollup a variable of type ", dQuote(type(x)))
    }
    return(zfuncExpr("rollup", x, list(value = resolution)))
}

#' @rdname expressions
#' @export
rollupResolution <- function(x) {
    if (is.Datetime(x)) {
        return(tuple(x)$rollup_resolution)
    } else {
        return(NULL)
    }
}

#' @rdname expressions
#' @export
`rollupResolution<-` <- function(x, value) {
    stopifnot(is.Datetime(x))
    validateResolution(force(value))
    setEntitySlot(entity(x), "view", list(rollup_resolution = value))
    return(refresh(x))
}

# validate that rollup resolutions are what are allowed by Crunch
validateResolution <- function(resolution) {
    valid_res <- c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms")

    if (!is.null(resolution) && !(resolution %in% valid_res)) {
        halt(
            dQuote("resolution"), " is invalid. Valid values are ",
            serialPaste(valid_res, collapse = "or")
        )
    }
}

# default formats for various resolutions
datetimeFormater <- function(resolution) {
    validateResolution(resolution)
    formats <- list(
        "Y" = "%Y",
        # there is no %q in python strftime, so can't print quarters
        "Q" = "%Y-%m-%d",
        "M" = "%Y-%m",
        "W" = "%Y W%W",
        "D" = "%Y-%m-%d",
        "h" = "%Y-%m-%d %H:00",
        "m" = "%Y-%m-%d %H:%M",
        "s" = "%Y-%m-%d %H:%M:%S",
        "ms" = "%Y-%m-%d %H:%M:%S.%f"
    )
    # return format based on rollup or default of "s"
    return(formats[[resolution %||% "s"]])
}
