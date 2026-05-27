#' Create Crunch Text Variable
#'
#' Crunch text variables are very similar to regular character vectors in R,
#' but have a few additional metadata fields that apply to the whole variable
#'
#' @param x A vector of values data
#' @param label The variable's label
#' @param description The variable's description
#' @param notes The variable's notes
#' @param path The variable's folder path
#' @param ... Ignored
#' @returns A `crunch_text_variable` object
#' @export
crunch_text_variable <- function(
        x,
        label,
        values,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    check_collect(
        "Invalid crunch_text_variable",
        check_single_string(label),
        check_single_string(description, null_ok = TRUE),
        check_single_string(notes, null_ok = TRUE),
        check_single_string(path, null_ok = TRUE),
        x <<- vec_cast(x, character(0))
    )

    new_crunch_text_variable(
        x = x,
        label = label,
        description = description,
        notes = notes,
        path = path
    )
}

new_crunch_text_variable <- function(
        x,
        label,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    out <- new_crunch_variable(
        x,
        label = label,
        description = description,
        notes = notes,
        path = path,
        inherit_base_type = TRUE
    )
    # Augment to numeric
    class(out) <- c("crunch_text_variable", class(out))

    out
}

# ---- Attributes -----
# Don't have any extras

# ---- Coercion -----
# Defaults from crunch_variable are ok

# ---- Type system -----
#' @importFrom methods setOldClass
setOldClass(c("crunch_text_variable", "crunch_variable", "vctrs_vctr"))

#' @export
#' @rdname crunch_text_variable
is.crunch_text_variable <- function(x) inherits(x, "crunch_text_variable")


#' @export
vec_ptype2.crunch_text_variable.crunch_text_variable <- function(x, y, ...) {
    # TODO: Do we care about comparing crunch_variable metadata like label?
    x
}

#' @export
vec_ptype2.crunch_text_variable.character <- function(x, y, ...) {
    x
}
#' @export
vec_ptype2.character.crunch_text_variable <- function(x, y, ...) {
    y
}


#' @export
vec_cast.crunch_text_variable.crunch_text_variable <- function(x, to, ...) {
    # TODO: Should we be doing anything about x's metadata?
    new_crunch_text_variable(
        vctrs::vec_data(x),
        label = label(to),
        description = description(to),
        notes = notes(to),
        path = path(to)
    )
}

#' @export
vec_cast.crunch_text_variable.character <- function(x, to, ...) {
    vec_cast(vctrs::vec_data(x), to)
}

#' @export
vec_cast.text.crunch_text_variable <- vec_cast.crunch_text_variable.crunch_text_variable

# ---- Equality/Comparisons ----
# Defaults ok?


# ---- Printing -----
# TODO
# #' @export
# summary.crunch_text_variable <- function()

# ---- Formatting ----
#' @export
vec_ptype_abbr.crunch_text_variable <- function(x, ...) {
    "cr_txt"
}

# ---- Arithmetic ----
# Intentionally not allowed
