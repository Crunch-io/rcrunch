#' Create Crunch Numeric Variable
#'
#' Crunch numeric variables are very similar to regular numeric vectors in R,
#' but have a few additional metadata fields that apply to the whole variable
#'
#' @param x A vector of values data
#' @param label The variable's label
#' @param description The variable's description
#' @param notes The variable's notes
#' @param path The variable's folder path
#' @param ... Ignored
#' @returns A `crunch_numeric_variable` object
#' @export
crunch_numeric_variable <- function(
        x,
        label,
        values,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    check_collect(
        "Invalid crunch_numeric_variable",
        check_single_string(label),
        check_single_string(description, null_ok = TRUE),
        check_single_string(notes, null_ok = TRUE),
        check_single_string(path, null_ok = TRUE),
        x <<- vec_cast(x, numeric(0))
    )

    new_crunch_numeric_variable(
        x = x,
        label = label,
        description = description,
        notes = notes,
        path = path
    )
}

new_crunch_numeric_variable <- function(
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
    class(out) <- c("crunch_numeric_variable", class(out))

    out
}

# ---- Attributes -----
# Don't have any extras

# ---- Coercion -----
# Defaults from crunch_variable are ok

# ---- Type system -----
#' @importFrom methods setOldClass
setOldClass(c("crunch_numeric_variable", "crunch_variable", "vctrs_vctr"))

#' @export
#' @rdname crunch_numeric_variable
is.crunch_numeric_variable <- function(x) inherits(x, "crunch_numeric_variable")


#' @export
vec_ptype2.crunch_numeric_variable.crunch_numeric_variable <- function(x, y, ...) {
    # TODO: Do we care about comparing crunch_variable metadata like label?
    x
}

#' @export
vec_ptype2.crunch_numeric_variable.double <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.crunch_numeric_variable.integer <- vec_ptype2.crunch_numeric_variable.double

#' @export
vec_ptype2.double.crunch_numeric_variable <- function(x, y, ...) vec_ptype2(y, x, ...)

#' @export
vec_ptype2.integer.crunch_numeric_variable <- vec_ptype2.double.crunch_numeric_variable

#' @export
vec_cast.crunch_numeric_variable.crunch_numeric_variable <- function(x, to, ...) {
    # TODO: Should we be doing anything about x's metadata?
    new_crunch_numeric_variable(
        vctrs::vec_data(x),
        label = label(to),
        description = description(to),
        notes = notes(to),
        path = path(to)
    )
}

#' @export
vec_cast.double.crunch_numeric_variable <- function(x, to, ...) {
    vec_cast(vctrs::vec_data(x), to)
}

#' @export
vec_cast.integer.crunch_numeric_variable <- vec_cast.double.crunch_numeric_variable

#' @export
vec_cast.integer.crunch_numeric_variable <- vec_cast.double.crunch_numeric_variable

#' @export
vec_cast.crunch_numeric_variable.double <- vec_cast.crunch_numeric_variable.crunch_numeric_variable

#' @export
vec_cast.crunch_numeric_variable.integer <- vec_cast.crunch_numeric_variable.crunch_numeric_variable


# ---- Equality/Comparisons ----
# Defaults ok?


# ---- Printing -----
# TODO
# #' @export
# summary.crunch_numeric_variable <- function()

# ---- Formatting ----
#' @export
vec_ptype_abbr.crunch_numeric_variable <- function(x, ...) {
    "cr_num"
}

# ---- Arithmetic ----

#' Internal vctrs methods
#'
#' @keywords internal
#' @export vec_arith.crunch_numeric_variable
#' @method vec_arith crunch_numeric_variable
#' @export
vec_arith.crunch_numeric_variable <- function(op, x, y, ...) {
    UseMethod("vec_arith.crunch_numeric_variable", y)
}
#' @export
#' @method vec_arith.crunch_numeric_variable default
vec_arith.crunch_numeric_variable.default <- function(op, x, y, ...) {
    stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.crunch_numeric_variable crunch_numeric_variable
vec_arith.crunch_numeric_variable.crunch_numeric_variable <- function(op, x, y, ...) {
    vec_arith_base(op, x, y)
}
#' @export
#' @method vec_arith.crunch_numeric_variable numeric
vec_arith.crunch_numeric_variable.numeric <- function(op, x, y, ...) {
    vec_arith_base(op, x, y)
}
#' @export
#' @method vec_arith.numeric crunch_numeric_variable
vec_arith.numeric.crunch_numeric_variable <- function(op, x, y, ...) {
    vec_arith_base(op, x, y)
}

#' @export
vec_math.crunch_numeric_variable <- function(.fn, .x, ...) {
    vec_math_base(.fn, .x, ...)
}

#' @importFrom stats median
#' @export
median.crunch_numeric_variable <- function(x, ...) {
    median(vctrs::vec_data(x), ...)
}

#' @importFrom stats quantile
#' @export
quantile.crunch_numeric_variable <- function(x, ...) {
    quantile(vctrs::vec_data(x), ...)
}
