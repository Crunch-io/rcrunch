#' Conditional transformation
#'
#' Create a new variable that has values when specific conditions are met. Conditions are specified using a series of formulas: the righthand side is the condition that must be true (a `CrunchLogicalExpr`) and the lefthand side is where to get the value if the condition on the righthand side is true. This is commonly a crunch variable. Imagine that we have two sets of questions, one about what pets one has (Pet1, Pet2, Pet3) and then a second that asks about ones opinion of those pets that one gave in the Pet* series of questions (Opinion1, Opinion2, opinion3). What we ultimately want is what people's opinion of Cats or Dogs is, but because the value of Opinion1, Opinion2, and Opinion3 is dependent on the answer to Pet1, Pet2, and Pet3 respectively, we can't easily get the overall opinions about Cats. `conditionalTransform`  allows you to specify conditions like the following: if variable Pet1 is equal to 'Cat' then use the value from variable Opinion1, if variable Pet2 is equal to 'Cat' then use the value from variable Opinion2, etc. See examples below for how to write this.
#'
#' `conditionalTransform` is similar to `makeCaseVariable` however `conditionalTransform` can use other crunch variables as a source of a variable, wehreas, whereas `makeCaseVariable` can only use characters. This additional power comes at a cost: `makeCaseVariable` can be executed entirely on Crunch servers, so no data needs to be downloaded or uploaded to/from the local R session. `conditionalTransform` on the other hand will download the data necesary to construct the new variable.
#'
#' @param ... a list of cases to evaluate
#' @param default_value a default value to use if none of the conditions are true (default: `NA`)
#' @param type a character that is either "categorical", "text", "numeric" what type of output should be returned?
#' @param categories if `type="categorical"`, these are all of the categories that should be in the resulting variable
#'
#' @return either a factor (if `type="categorical"`) or character (if `type="text"`)
#' @examples
#' \dontrun{
#' ds$cat_opinion <- conditionalTransform(ds$pet1 == 'Cat' ~ ds$Opinion1,
#'                                        ds$pet2 == 'Cat' ~ ds$Opinion2,
#'                                        ds$pet3 == 'Cat' ~ ds$Opinion3)
#' ds$dot_opinion <- conditionalTransform(ds$pet1 == 'Dog' ~ ds$Opinion1,
#'                                        ds$pet2 == 'Dog' ~ ds$Opinion2,
#'                                        ds$pet3 == 'Dog' ~ ds$Opinion3)
#' }
#'
#' @export
conditionalTransform <- function (..., default_value=NA, type="categorical", categories=NULL) {
    formulas <- list(...)
    n <- length(formulas)

    if (n == 0) {
        halt("no conditions have been supplied")
    }
    if (!type %in% c("categorical", "text", "numeric")){
        halt("type must be either ", dQuote("categorical"), dQuote("text"), " or ", dQuote("numeric"))
    }
    if (type != "categorical" & !is.null(categories)){
        halt("type is not ", dQuote("categorical"), " ignoring ", dQuote("categories"))
    }

    cases <- vector("list", n)
    values <- vector("list", n)
    for (i in seq_len(n)) {
        formula <- formulas[[i]]
        if (!inherits(formula, "formula") || length(formula) != 3) {
            halt("The case provided is not a formula: ", deparseAndFlatten(substitute(list(...))[[i + 1]]))
        }

        # TODO: switch ot the more lightweight rlang::bare_eval()?
        cases[[i]] <- eval(formula[[2]])
        if (!inherits(cases[[i]], "CrunchLogicalExpr")) {
            halt("The LHS provided is not a CrunchLogicalExpr: ", deparseAndFlatten(substitute(list(...))[[i + 1]][[2]]))
        }
        values[[i]] <- eval(formula[[3]])
    }

    n_rows <- nrow(CrunchDataset(crGET(datasetReference(cases[[1]]))))

    case_indices <- lapply(cases, which)

    # dedpulicate indices, favoring the first observation
    case_indices <- lapply(seq_along(case_indices), function(i) {
        setdiff(case_indices[[i]], unlist(case_indices[seq_len(i-1)]))
    })

    values_to_fill <- Map(function(ind, var) {
        if (inherits(var, "CrunchVariable")) {
            # grab the variable contents at inds
            return(as.vector(var[ind]))
        } else {
            # if var isn't a crunchvariable, just return var
            return(var)
        }
    }, ind = case_indices, var = values)

    # setup NAs for as default
    result <- rep(default_value, n_rows)

    for (i in seq_along(case_indices)) {
        if (type == "numeric") trans <- as.numeric
        else trans <- as.character
        vals <- trans(values_to_fill[[i]])
        result[case_indices[[i]]] <- vals
    }

    # check categories, return factor type="categorical"
    if (type == "categorical") {
        uni_results <- unique(result)
        results_not_categories <- !uni_results %in% categories
        # if categories are supplied and there are any
        if (is.null(categories)) {
            result <- factor(result)
        } else {
            if (any(results_not_categories)) {
                halt("there were categories in the results (",
                     serialPaste(uni_results[results_not_categories]),
                     ") that were not specified in categories")
            }
            result <- factor(result, levels = categories)
        }
    }

    return(result)
}