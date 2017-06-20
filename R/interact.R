#' Make a variable that is the interaction of two or more other variables
#'
#' `interactVariables` takes two or more variables and creates a new one that
#' is the cartesian product expansion of their unique values. For example, if
#' we cross ethnicity (with 2 categories) and race (with 4 categories), the new
#' variable would have 8 valid categories (e.g. black:hispanic, white:hispanic,
#' black:non-hispanic, etc.) and 7 categories where at least one of the
#' variables is missing (e.g. white:No Data).
#'
#' @param ... a sequence of categorical variables to make an interaction from
#' @param name a character to use as the name for the interaction variable
#' @param sep a character to separate the values of the individual variables
#'  (default: `:`)
#'
#' @return A [`VariableDefinition`] that creates the new interaction variable.
#' @examples
#' \dontrun{
#' ds$ethn_race <- interactVariables(ds$ethnicity, ds$race, name="Interaction of ethnicity and race")
#' }
#' @export
interactVariables <- function(..., name, sep = ":") {
    dots <- list(...)
    vars <- Filter(is.variable, dots)
    other_dots <- Filter(Negate(is.variable), dots)

    if (length(vars) < 2) {
        halt("must supply more than one variable to make an interaction")
    }

    case_args <- c(vars, sep=sep)
    cases <- do.call(makeInteractions, case_args)

    int_args <- c(cases=list(cases), name=name, other_dots)
    interactionVar <- do.call(makeCaseVariable, int_args)
    return(interactionVar)
}

#' Make case statments that define the interaction of two or more other variables
#'
#' @param ... A sequence of categorical variables to make an interaction of
#' @param sep a character to separate the values of the individual variables
#'
#' @return A list of cases that can be passed to [`makeCaseVariable`] to make
#' the interaction variable.
#'
#' @keywords internal
makeInteractions <- function(..., sep) {
    vars <- list(...)
    names(vars) <- lapply(vars, alias)

    # check that vars are categorical
    if (any(!vapply(vars, is.Categorical, logical(1)))) {
        halt("makeInteractions can only take categorical variables")
    }

    cats <- lapply(vars, categories)
    all_combos <- expand.grid(cats)

    cases <- by(all_combos, seq(nrow(all_combos)), makeCase, vars=vars, sep=sep)

    # remove attributes/names
    attributes(cases) <- NULL

    return(cases=cases)
}

makeCase <- function(df, vars, sep) {
    exprs <- mapply(function (cat, var) var == names(cat), df, vars)
    nms <- lapply(df, names)
    name <- paste(nms, collapse = sep)
    msng <- any(vapply(df, is.na, logical(1)))

    list(expression = Reduce("&", exprs), name = name, missing = msng)
}
