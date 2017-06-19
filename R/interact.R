#' Make case statments that define the interaction of two or more other variables
#' 
#' @param ... A sequence of categorical variables to make an interaction of
#' @param drop should unused levels be dropped? (currently not implemented, so `FALSE`)
#' @param sep a character to separate the values of the individual variables (default: `:`)
#' 
#' @return A list of cases that can be passed to [`makeCaseVariable`] to make 
#' the interaction variable.
#' 
#' @export
makeInteractions <- function(..., drop = FALSE, sep = ":") {
    vars <- list(...)
    names(vars) <- lapply(vars, alias)
    
    # check that vars are categorical
    if (any(!vapply(vars, is.Categorical, logical(1)))) {
        halt("makeInteractions can only take categorical variables")
    }
    
    if (drop) halt(dQuote("drop"), "has not been implemented yet")
    cats <- lapply(vars, categories)
    all_combos <- expand.grid(cats)

    cases <- by(all_combos, seq(nrow(all_combos)), makeCase, vars=vars, sep=sep)
    
    # collpase and remove names
    cases <- c(cases)
    names(cases) <- NULL
    
    return(cases=cases)
}

#' Make a varaible that is the interaction of two or more other variables
#' 
#' @param ... A sequence of categorical variables to make an interaction of
#' @param name a character to use as the name for the interaction variable
#' 
#' @return A [`VariableDefinition`] that will create the new
#' interaction variable. 
#' 
#' @export
interactVariables <- function(..., name) {
    dots <- list(...)
    vars <- Filter(is.variable, dots)
    other_dots <- Filter(Negate(is.variable), dots)
    
    if (length(vars) < 2) {
        halt("must supply more than one variable to make an interaction")
    }
    
    case_args <- c(vars, drop=other_dots[["drop"]], sep=other_dots[["sep"]])
    cases <- do.call(makeInteractions, case_args)
    
    int_args <- c(cases=list(cases), name=name, other_dots)
    interactionVar <- do.call(makeCaseVariable, int_args)
    return(interactionVar)
}

makeCase <- function(df, vars, sep) {
    exprs <- mapply(function(cat, var) {var == names(cat)}, df, vars)
    nms <- lapply(df, names)
    name <- paste(nms, collapse = sep)
    msng <- any(vapply(df, is.na, logical(1)))
    
    list(expression = Reduce("&", exprs), name = name, missing = msng)
}

