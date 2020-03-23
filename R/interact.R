#' Create a variable by interacting categorical variables
#'
#' `interactVariables` takes two or more variables and creates a new one that
#' is the cartesian product expansion of their unique values. For example, if
#' we cross ethnicity (with 2 categories) and race (with 4 categories), the new
#' variable would have 8 valid categories (e.g. black:hispanic, white:hispanic,
#' black:non-hispanic, etc.) and 7 categories where at least one of the
#' variables is missing (e.g. white:No Data).
#'
#' @param ... a sequence of categorical variables to make an interaction from
#' as well as other properties to pass about the case variable (i.e. alias,
#'  description)
#' @param name a character to use as the name for the interaction variable
#' @param collapse_missings a logical indicating whether to combine all
#' new categories that are formed from existing missing categories into
#' a single one (defaults to `FALSE`).
#'
#' @return A [`VariableDefinition`] that creates the new interaction variable.
#' @examples
#' \dontrun{
#' ds$ethn_race <- interactVariables(
#'   ds$ethnicity, ds$race, name = "Interaction of ethnicity and race"
#' )
#' }
#' @export
interactVariables <- function(..., name, collapse_missings = FALSE) {
    dots <- list(...)
    vars <- Filter(is.variable, dots)
    other_dots <- Filter(Negate(is.variable), dots)

    if ("sep" %in% names(other_dots)) {
        warning("The `sep` argument is no longer supported.")
        other_dots$sep <- NULL
    }

    if (length(vars) < 2) {
        halt("must supply more than one variable to make an interaction")
    }

    other_dots$name <- name
    other_dots$derivation <- zfunc(
        "interact_categories",
        # unnaming prevents JSON request object getting numeric descriptors
        unname(lapply(vars, function(x) list(variable = urls(x)))),
        list(value = collapse_missings)
    )

    do.call(VariableDefinition, other_dots)
}

