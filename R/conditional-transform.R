#' Conditional transformation
#'
#' Create a new variable that has values when specific conditions are met. 
#' Conditions are specified using a series of formulas: the righthand side is 
#' the condition that must be true (a `CrunchLogicalExpr`) and the lefthand 
#' side is where to get the value if the condition on the righthand side is 
#' true. This is commonly a Crunch variable.
#'
#' `conditionalTransform` is similar to `makeCaseVariable` however 
#' `conditionalTransform` can use other Crunch variables as a source of a 
#' variable, whereas, whereas `makeCaseVariable` can only use characters. This 
#' additional power comes at a cost: `makeCaseVariable` can be executed 
#' entirely on Crunch servers, so no data needs to be downloaded or uploaded 
#' to/from the local R session. `conditionalTransform` on the other hand will 
#' download the data necessary to construct the new variable.
#'
#' @param ... a list of cases to evaluate
#' @param data a Crunch dataset object to use
#' @param else_condition a default value to use if none of the conditions are 
#' true (default: `NA`)
#' @param type a character that is either "categorical", "text", "numeric" what
#'  type of output should be returned?
#' @param categories if `type="categorical"`, these are all of the categories 
#' that should be in the resulting variable
#'
#' @return either a factor (if `type="categorical"`) or character (if `type="text"`)
#' @examples
#' \dontrun{
#' # Imagine that we have two sets of questions, one about what pets one has 
#' # (Pet1, Pet2, Pet3) and then a second that asks about ones opinion of those
#' #  pets that one gave in the Pet* series of questions (Opinion1, Opinion2, 
#' # opinion3). What we ultimately want is what people's opinion of Cats or 
#' # Dogs is, but because the value of Opinion1, Opinion2, and Opinion3 is 
#' # dependent on the answer to Pet1, Pet2, and Pet3 respectively, we can't 
#' # easily get the overall opinions about Cats. `conditionalTransform` allows 
#' # you to specify conditions like the following: if variable Pet1 is equal to
#' # 'Cat' then use the value from variable Opinion1, if variable Pet2 is 
#' # equal to 'Cat' then use the value from variable Opinion2, etc.
#' 
#' ds$cat_opinion <- conditionalTransform(ds$pet1 == 'Cat' ~ ds$Opinion1,
#'                                        ds$pet2 == 'Cat' ~ ds$Opinion2,
#'                                        ds$pet3 == 'Cat' ~ ds$Opinion3)
#'                                        
#' # We can also use `conditionalTransform` to return a string as well as the
#' # contents of other variables, if for example we want to separate out people
#' # who have only had pets for less than a year, we can specify that as the
#' # first condition (which will be used even if the subseqent conditions are
#' # also true).
#' 
#' ds$dog_opinion <- conditionalTransform(ds$days_have_pet < 365 ~ "too early",
#'                                        ds$pet1 == 'Dog' ~ ds$Opinion1,
#'                                        ds$pet2 == 'Dog' ~ ds$Opinion2,
#'                                        ds$pet3 == 'Dog' ~ ds$Opinion3)
#' # We can 
#' 
#' ds$dog_opinion <- conditionalTransform(ds$days_have_pet < 365 ~ "too early",
#'                                        ds$pet1 == 'Dog' ~ ds$Opinion1,
#'                                        ds$pet2 == 'Dog' ~ ds$Opinion2,
#'                                        ds$pet3 == 'Dog' ~ ds$Opinion3)                                        
#'                                        
#' }
#'
#' @export
conditionalTransform <- function (..., data, else_condition=NA, type="categorical", categories=NULL) {
    dots <- list(...)
    is_formula <- function (x) inherits(x, "formula")
    formulas <- Filter(is_formula, dots) 
    var_def <- Filter(Negate(is_formula), dots)
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
            halt("The case provided is not a formula: ", deparseAndFlatten(formula))
        }
        
        cases[[i]] <- evalLHS(formula, data)
        if (!inherits(cases[[i]], "CrunchLogicalExpr")) {
            halt("The LHS provided is not a CrunchLogicalExpr: ", formulaLHS(formula))
        }
        values[[i]] <- evalRHS(formula, data)
    }
    
    n_rows <- nrow(CrunchDataset(crGET(datasetReference(cases[[1]]))))
    
    case_indices <- lapply(cases, which)
    
    # dedpulicate indices, favoring the first observation
    case_indices <- lapply(seq_along(case_indices), function(i) {
        setdiff(case_indices[[i]], unlist(case_indices[seq_len(i-1)]))
    })
    
    values_to_fill <- Map(function(ind, var) {
        if (inherits(var, c("CrunchVariable", "CrunchExpr"))) {
            # grab the variable contents at inds
            return(as.vector(var[ind]))
        } else {
            # if var isn't a crunch variable or expression, just return var
            return(var)
        }
    }, ind = case_indices, var = values)
    
    # setup NAs for as default
    result <- rep(else_condition, n_rows)
    
    for (i in seq_along(case_indices)) {
        if (type == "numeric") {
            trans <- as.numeric
        } else {
            trans <- as.character
        }
        vals <- trans(values_to_fill[[i]])
        result[case_indices[[i]]] <- vals
    }
    
    # check categories, return factor type="categorical"
    if (type == "categorical") {
        # if categories are supplied and there are any
        if (is.null(categories)) {
            result <- factor(result)
        } else {
            uni_results <- unique(result)
            results_not_categories <- !uni_results %in% categories
            if (any(results_not_categories)) {
                halt("there were categories in the results (",
                     serialPaste(uni_results[results_not_categories]),
                     ") that were not specified in categories")
            }
            result <- factor(result, levels = categories)
        }
    }
    
    var_def$data <- result
    var_def <- do.call(VariableDefinition, var_def)
    
    return(var_def)
}