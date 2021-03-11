#' Crunch xtabs: Crosstab and otherwise aggregate variables in a Crunch Dataset
#'
#' Create a contingency table or other aggregation from cross-classifying
#' variables in a CrunchDataset, expanding on the notation allowed in
#' [`stats::xtabs()`] to tailor to the kinds of calculations available in crunch.
#'
#' There are 3 types of queries supported:
#'
#' - Crosstabs: Share the most in common with [`stats::xtabs()`], are defined by
#' a formula with only a right hand side, with each dimension specified on the
#' right-hand side, separated by a `+`. A dimension are generally variables, but
#' categorical array variables contribute 2 dimensions, \dQuote{categories} and
#' \dQuote{subvariables}.
#' If you just use the categorical array variable directly, the subvariables dimensions
#' will be added first and the categories second, but you can choose their order by
#' specifying both `categories(var)` and `subvariables(var)` (where `var` is a
#' Categorical Array CrunchVariable).
#'
#' - Aggregations: An extension to 'Crosstabs' where you can select one or more
#' measures by putting them in the left-hand side of the formula. Multiple measures
#' can be placed in a list to calculate them together. The currently supported
#' measures are `mean(var)`, `n()` (the same as a crosstab), `min(var)`, `max(var)`,
#'  `sd(var)`, `sum(var)` and `median(var)` (where `var` is a CrunchVariable).
#'
#' - Scorecards: When you want to compare multiple MR variables with the same
#' subvariables, you can use a scorecard to create a tabulation where they are
#' lined up. Scorecard queries cannot be combined with the other types. Use
#' the `scorecard(..., vars = NULL)` (where `...` is a set of MR variables or
#' `vars` is a list of them).
#'
#' @param formula a [stats::formula] object that specifies that query
#' to calculate. See Details for more information.
#' @param data an object of class `CrunchDataset`
#' @param weight a CrunchVariable that has been designated as a potential
#' weight variable for `data`, or `NULL` for unweighted results.
#' Default is the currently applied [`weight()`].
#' @param useNA whether to include missing values in tabular results. See
#' [base::table()].
#' @return an object of class `CrunchCube`
#' @importFrom stats as.formula terms
#' @seealso [weight()]
#' @export
#' @examples
#' \dontrun{
#' # Crosstab of people by `age_cat`:
#' crtabs(~age_cat, ds)
#'
#' # Aggregation of means of income by `age_cat`
#' crtabs(mean(income) ~ age_cat, ds)
#'
#' # Scorecard of multiple MRs with aligned subvariables
#' crtabs(~scorecard(trust_mr, value_mr, quality_mr), ds)
#' # Can also pre-define the variables in a scorecard with
#' mr_list <- list(ds$trust_mr, ds$value_mr, ds$quality_mr)
#' crtabs(~scorecard(vars = mr_list), ds)
#'
#' # Crosstab of people by `age_cat` and the reasons for enjoying a brand (cat array)
#' crtabs(~age_cat + enjoy_array, ds)
#'
#' # Crosstab of people by `age_cat` and the `enjoy_array` (cat array)
#' # But manually choosing the order of the dimensions
#' crtabs(~subvariables(enjoy_array) + age_cat + categories(enjoy_array), ds)
#'
#' # Aggregation of means & standard deviations of income by `age_cat`
#' crtabs(list(mean = mean(income), sd = sd(income)) ~ age_cat, ds)
#' }
crtabs <- function(formula, data, weight = crunch::weight(data),
                   useNA = c("no", "ifany", "always")) {
    ## Validate inputs
    if (missing(formula)) {
        halt("Must provide a formula")
    }
    if (missing(data) || !is.dataset(data)) {
        halt(dQuote("data"), " must be a Dataset")
    }
    useNA <- match.arg(useNA)

    query <- formulaToCubeQuery(formula, data)

    ## Handle "weight"
    force(weight)
    if (is.variable(weight)) {
        weight <- self(weight)
        ## Should confirm that weight is in weight_variables. Server 400s
        ## if it isn't.
    } else {
        weight <- NULL
    }
    query["weight"] <- list(weight)

    ## Get filter
    f <- zcl(activeFilter(data))

    ## GET it.
    resp <- crGET(cubeURL(data),
        query = list(
            query = toJSON(query, for_query_string = TRUE),
            filter = toJSON(f, for_query_string = TRUE)
        )
    )
    return(CrunchCube(resp, useNA = useNA))
}
