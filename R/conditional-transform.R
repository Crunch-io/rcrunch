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
#' @param ... a list of cases to evaluate as well as other
#' properties to pass about the case variable (i.e. alias, description)
#' @param data a Crunch dataset object to use
#' @param else_condition a default value to use if none of the conditions are 
#' true (default: `NA`)
#' @param type a character that is either "categorical", "text", "numeric" what
#'  type of output should be returned? The source variables will be converted 
#'  to this type if necesary
#' @param categories if `type="categorical"`, these are all of the categories 
#' that should be in the resulting variable, in the order they should be in the
#' resulting variable
#'
#' @return a Crunch `VariableDefinition`
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
#' ds$cat_opinion <- conditionalTransform(pet1 == 'Cat' ~ Opinion1,
#'                                        pet2 == 'Cat' ~ Opinion2,
#'                                        pet3 == 'Cat' ~ Opinion3, 
#'                                        data = ds)
#' as.vector(ds$cat_opinion)
#' # "Strongly Agree" "Disagree" "Agree" "Strongly Disagree"
#'                                        
#' # We can also use `conditionalTransform` to return a string as well as the
#' # contents of other variables, if for example we want to separate out people
#' # who have only had pets for less than a year, we can specify that as the
#' # first condition (which will be used even if the subseqent conditions are
#' # also true).
#' 
#' ds$dog_opinion <- conditionalTransform(days_having_pet < 365 ~ "too early",
#'                                        pet1 == 'Dog' ~ Opinion1,
#'                                        pet2 == 'Dog' ~ Opinion2,
#'                                        pet3 == 'Dog' ~ Opinion3,
#'                                        data = ds)
#' as.vector(ds$dog_opinion)
#' # "Strongly Agree" "Disagree" "too early" "Strongly Disagree"
#'                                     
#' # Further, we can also use conditional transform
#' 
#' ds$days_with_dog <- conditionalTransform(pet1 == 'Dog' ~ days_having_pet,
#'                                          pet2 == 'Dog' ~ days_having_pet,
#'                                          pet3 == 'Dog' ~ days_having_pet,
#'                                          data = ds, type = numeric)
#' as.vector(ds$days_with_dog)
#' # 300 57 70 5
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
        halt("no conditions have been supplied; please supply formulas as conditions.")
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
    case_indices <- lapply(cases, which)
    
    # dedpulicate indices, favoring the first true condition
    case_indices <- lapply(seq_along(case_indices), function(i) {
        setdiff(case_indices[[i]], unlist(case_indices[seq_len(i-1)]))
    })
    
    # grab the values needed from source variables
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
    n_rows <- nrow(CrunchDataset(crGET(datasetReference(cases[[1]]))))
    result <- rep(else_condition, n_rows)
    
    # fill values
    for (i in seq_along(case_indices)) {
        if (type == "numeric") {
            trans <- as.numeric
        } else {
            trans <- as.character
        }
        vals <- trans(values_to_fill[[i]])
        result[case_indices[[i]]] <- vals
    }
 
    if (type == "categorical") {
        # if categories are supplied and there are any
        if (missing(categories)) {
            result <- factor(result)
        } else {
            uni_results <- unique(result[!is.na(result)])
            results_not_categories <- !uni_results %in% categories
            if (any(results_not_categories)) {
                halt("there were categories in the results (",
                     serialPaste(uni_results[results_not_categories]),
                     ") that were not specified in categories")
            }
            result <- factor(result, levels = categories)
            var_def$categories <- categoriesFromLevels(categories)
        }
    }
    
    var_def$type <- type
    var_def$data <- result
    var_def <- do.call(VariableDefinition, var_def)
    
    return(var_def)
}