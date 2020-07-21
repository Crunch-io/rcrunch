from8601 <- function(x) {
    ## Crunch timestamps look like "2015-02-12T10:28:05.632000+00:00"

    if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", na.omit(x)))) {
        ## return Date if resolution == D
        return(as.Date(x))
    }

    if (all(grepl("^[0-9]{4}-[0-9]{2}$", na.omit(x)))) {
        ## return Date (of first of month) if resolution == M
        x[!is.na(x)] <- paste0(x[!is.na(x)], "-01")
        return(as.Date(x))
    }

    if (all(grepl("^[0-9]{4}$", na.omit(x)))) {
        ## return Date (of first of year) if resolution == Y
        x[!is.na(x)] <- paste0(x[!is.na(x)], "-01-01")
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

#' Methods for Datetime variable resolutions
#'
#' Datetime data has a "resolution", the units of the values.
#' `resolution()` exposes that property and `resolution<-` lets you set it.
#' "Rollups" are a way of binning datetime data into meaningful units.
#' `rollup()` lets you create an expression that you can query with. Datetime
#' variables also have a `rollupResolution()` attribute that is the default
#' resolution they will roll-up to, if not specified in `rollup()`;
#' `rollupResolution<-` lets you set that.
#'
#' Note that `resolution` is a property of the data while `rollupResolution` is
#' metadata. Setting `resolution` alters the column data, and if setting a more
#' coarse resolution (e.g. going from "s" to "m"), it cannot be reversed.
#' Setting `rollupResolution` is non-destructive.
#' @param x a Datetime variable
#' @param value a resolution string. Valid resolutions in Crunch are
#' `c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms")`. `NULL` is also valid for
#' `rollupResolution<-` but not for `resolution<-`.
#' @param resolution Same as `value`, in `rollup()`. This may be `NULL`, in
#' which case the server will determine an appropriate resolution based on the
#' range of the data.
#' @return `resolution()` and `rollupResolution()` return the resolution string
#' for datetime variables, `NULL` otherwise. The setters return the variable
#' entity after modifying the state on the server. `rollup()` returns a
#' `CrunchExpr` expression.
#' @export
#' @examples
#' \dontrun{
#' resolution(ds$starttime)
#' ## [1] "ms"
#' resolution(ds$starttime) <- "s"
#' rollup(ds$starttime)
#' rollup(ds$starttime, "D")
#' rollupResolution(ds$starttime) <- "D"
#' crtabs(~ rollup(starttime), data = ds)
#' }
resolution <- function(x) {
    if (is.Datetime(x)) {
        return(tuple(x)$resolution)
    } else {
        return(NULL)
    }
}

#' @rdname resolution
#' @export
`resolution<-` <- function(x, value) {
    stopifnot(is.Datetime(x))
    validateResolution(force(value))
    # and one more validation
    if (is.null(value)) {
        halt("resolution cannot be NULL")
    }
    setEntitySlot(entity(x), "resolution", value)
    return(refresh(x))
}

#' @rdname resolution
#' @export
rollup <- function(x, resolution = rollupResolution(x)) {
    validateResolution(force(resolution))
    if (is.variable(x) && !is.Datetime(x)) {
        halt("Cannot rollup a variable of type ", dQuote(type(x)))
    }
    return(zfuncExpr("rollup", x, list(value = resolution)))
}

#' @rdname resolution
#' @export
rollupResolution <- function(x) {
    if (is.Datetime(x)) {
        return(tuple(x)$rollup_resolution)
    } else {
        return(NULL)
    }
}

#' @rdname resolution
#' @export
`rollupResolution<-` <- function(x, value) {
    stopifnot(is.Datetime(x))
    validateResolution(force(value))
    setEntitySlot(entity(x), "view", list(rollup_resolution = value))
    return(refresh(x))
}

# validate that rollup resolutions are what are allowed by Crunch
validateResolution <- function(resolution) {
    valid_res <- c("Y", "Q", "M", "3M", "W", "D", "h", "m", "s", "ms")

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
