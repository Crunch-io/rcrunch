#' Create Crunch Datetime Variable
#'
#' Crunch Datetime variables are very similar to regular POSIXct vectors in R,
#' but have a few additional metadata fields that apply to the whole variable
#'
#' @param x A vector of values data
#' @param label The variable's label
#' @param description The variable's description
#' @param notes The variable's notes
#' @param path The variable's folder path
#' @param ... Ignored
#' @returns A `crunch_date_variable` object
#' @export
crunch_datetime_variable <- function(
        x,
        label,
        values,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    check_collect(
        "Invalid crunch_datetime_variable",
        check_single_string(label),
        check_single_string(description, null_ok = TRUE),
        check_single_string(notes, null_ok = TRUE),
        check_single_string(path, null_ok = TRUE),
        x <<- vec_cast(x, as.POSIXct(NA))
    )

    new_crunch_datetime_variable(
        x = x,
        label = label,
        description = description,
        notes = notes,
        path = path
    )
}

new_crunch_datetime_variable <- function(
        x,
        label,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    # vctrs doesn't really support subclassing any of the datetime types
    # Since we just want to add some variable level attributes on top of POSIXct
    # Try doing it this way and see how it works
    structure(
        x,
        label = label,
        description = description,
        notes = notes,
        path = path,
        class = c("crunch_datetime_variable", "crunch_variable", class(x))
    )
}

# ---- Attributes -----
# Don't have any extras

# ---- Coercion -----
# Defaults from crunch_variable are ok

# ---- Type system -----
#' @importFrom methods setOldClass
setOldClass(c("crunch_datetime_variable", "crunch_variable"))

#' @export
#' @rdname crunch_datetime_variable
is.crunch_datetime_variable <- function(x) inherits(x, "crunch_datetime_variable")


#' @export
vec_ptype2.crunch_datetime_variable.crunch_datetime_variable <- function(x, y, ...) {
    # TODO: Do we care about comparing crunch_variable metadata like label?
    x
}

#' @export
vec_ptype2.crunch_datetime_variable.POSIXct <- function(x, y, ...) {
    x
}
#' @export
vec_ptype2.POSIXct.crunch_datetime_variable <- function(x, y, ...) {
    y
}


#' @export
vec_cast.crunch_datetime_variable.crunch_datetime_variable <- function(x, to, ...) {
    # TODO: Should we be doing anything about x's metadata?
    new_crunch_datetime_variable(
        vctrs::vec_data(x),
        label = label(to),
        description = description(to),
        notes = notes(to),
        path = path(to)
    )
}

#' @export
vec_cast.crunch_datetime_variable.POSIXct <- function(x, to, ...) {
    vec_cast(vctrs::vec_data(x), to)
}

#' @export
vec_cast.POSIXct.crunch_datetime_variable <- vec_cast.crunch_datetime_variable.crunch_datetime_variable

# ---- Equality/Comparisons ----
# Defaults ok?


# ---- Printing -----
# TODO
# #' @export
# summary.crunch_datetime_variable <- function()

# ---- Formatting ----
#' @export
vec_ptype_abbr.crunch_datetime_variable <- function(x, ...) {
    "cr_dtm"
}

# ---- Arithmetic ----
# Intentionally not allowed
