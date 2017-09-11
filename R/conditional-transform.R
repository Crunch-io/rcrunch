#' Conditional transformation
#'
#' Create a new variable that has values when specific conditions are met.
#' Conditions are specified using a series of formulas: the righthand side is
#' the condition that must be true (a `CrunchLogicalExpr`) and the lefthand
#' side is where to get the value if the condition on the righthand side is
#' true. This is commonly a Crunch variable but may be a string or numeric
#' value, depending on the type of variable you're constructing.
#'
#' `conditionalTransform` is similar to `makeCaseVariable`; however,
#' `conditionalTransform` can use other Crunch variables as a source of a
#' variable, whereas, `makeCaseVariable` can only use characters. This
#' additional power comes at a cost: `makeCaseVariable` can be executed
#' entirely on Crunch servers, so no data needs to be downloaded or uploaded
#' to/from the local R session. `conditionalTransform` on the other hand will
#' download the data necessary to construct the new variable.
#'
#' For more details see the \code{vignette("conditional-variables", package="crunch")} vignette.
#'
#' @param ... a list of cases to evaluate as well as other
#' properties to pass about the case variable (i.e. alias, description)
#' @param data a Crunch dataset object to use
#' @param else_condition a default value to use if none of the conditions are
#' true (default: `NA`)
#' @param type a character that is either "categorical", "text", "numeric" what
#'  type of output should be returned? The source variables will be converted
#'  to this type if necessary
#' @param categories a vector of characters if `type="categorical"`, these are
#' all of the categories that should be in the resulting variable, in the order
#' they should be in the resulting variable or a set of Crunch categories.
#'
#' @return a Crunch `VariableDefinition`
#' @examples
#' \dontrun{
#'
#' ds$cat_opinion <- conditionalTransform(pet1 == 'Cat' ~ Opinion1,
#'                                        pet2 == 'Cat' ~ Opinion2,
#'                                        pet3 == 'Cat' ~ Opinion3,
#'                                        data = ds,
#'                                        name = "Opinion of Cats")
#' }
#' @export
conditionalTransform <- function (..., data, else_condition=NA, type="categorical", categories=NULL) {
    dots <- list(...)
    is_formula <- function (x) inherits(x, "formula")
    formulas <- Filter(is_formula, dots)
    var_def <- Filter(Negate(is_formula), dots)

    if (length(formulas) == 0) {
        halt("no conditions have been supplied; please supply formulas as conditions.")
    }
    if (!type %in% c("categorical", "text", "numeric")){
        halt("type must be either ", dQuote("categorical"), ", ", dQuote("text"), ", or ", dQuote("numeric"))
    }
    if (type != "categorical" & !is.null(categories)){
        halt("type is not ", dQuote("categorical"), " ignoring ", dQuote("categories"))
    }

    result <- make_conditional_values(formulas, data, else_condition, type)

    var_def$type <- type

    if (type == "categorical") {
        # if categories are supplied and there are any
        if (missing(categories)) {
            result <- factor(result)
            # make categories from names
            categories <- Categories(data = categoriesFromLevels(levels(result)))
        } else {
            if (!is.categories(categories)) {
                # if categories aren't a Categories object,
                # make categories from names
                categories <- Categories(data = categoriesFromLevels(categories))
            }

            uni_results <- unique(result[!is.na(result)])
            results_not_categories <- !uni_results %in% names(categories)
            if (any(results_not_categories)) {
                halt("there were categories in the results (",
                     serialPaste(uni_results[results_not_categories]),
                     ") that were not specified in categories")
            }
            result <- factor(result, levels = names(categories))
        }
        categories <- ensureNoDataCategory(categories)
        # make a category list to send with VariableDefinition and then store that and convert values to ids values
        category_list <- categories
        var_def$categories <- category_list
        vals <- as.character(result)
        vals[is.na(vals)] <- "No Data" # na is system default
        var_def$values <- ids(categories[vals])
    } else {
        var_def$values <- result
    }

    class(var_def) <- "VariableDefinition"
    return(var_def)
}

make_conditional_values <- function (formulas, data, else_condition, type) {
    n <- length(formulas)
    cases <- vector("list", n)
    values <- vector("list", n)
    for (i in seq_len(n)) {
        formula <- formulas[[i]]
        if (length(formula) != 3) {
            halt("The case provided is not a proper formula: ", deparseAndFlatten(formula))
        }

        cases[[i]] <- evalLHS(formula, data)
        if (!inherits(cases[[i]], "CrunchLogicalExpr")) {
            halt("The LHS provided is not a CrunchLogicalExpr: ", LHS_string(formula))
        }
        values[[i]] <- evalRHS(formula, data)
    }

    # setup NAs for as default
    # check all datasets are the same and get the reference from the unique one
    ds_refs <- unlist(unique(lapply(cases, datasetReference)))
    if (length(ds_refs) > 1) {
        halt("There is more than one dataset referenced. Please ",
             "supply only one.")
    }
    n_rows <- nrow(CrunchDataset(crGET(ds_refs)))

    # grab booleans for cases
    case_indices <- lapply(cases, which)

    # deduplicate indices, favoring the first true condition
    case_indices <- lapply(seq_along(case_indices), function(i) {
        setdiff(case_indices[[i]], unlist(case_indices[seq_len(i-1)]))
    })

    if (type == "numeric") {
        trans <- as.numeric
    } else {
        trans <- as.character
    }
    # grab the values needed from source variables
    values_to_fill <- Map(function(ind, var) {
        if (inherits(var, c("CrunchVariable", "CrunchExpr"))) {
            # grab the variable contents at inds
            return(trans(as.vector(var[ind])))
        } else {
            # if var isn't a crunch variable or expression, just return var
            return(trans(var))
        }
    }, ind = case_indices, var = values)

    result <- rep(else_condition, n_rows)

    # fill values
    for (i in seq_along(case_indices)) {
        vals <- values_to_fill[[i]]
        result[case_indices[[i]]] <- vals
    }

    return(result)
}