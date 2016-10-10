#' Summarize any characteristics of a dataset that could make for an awkward widget
#'
#' @param dataset CrunchDataset, potentially subsetted on variables
#' @return Invisbly, the dataset. Called for side-effect of printing things.
#' @export
preEmbedCheck <- function (dataset) {
    vm <- variableMetadata(dataset)[urls(variables(dataset))] ## [] for order of vars
    keeps <- aliases(vm)

    ## 1. Variable types
    suggested_types <- types(vm) %in% c("categorical", "multiple_response")
    if (!all(suggested_types)) {
        not_recommended_types <- !suggested_types
        num_types <- sum(not_recommended_types)
        cat("We recommend using only categorical and multiple_response",
            "variables.", demonstrativeCount(num_types), "are not:\n")
        print(data.frame(alias=keeps[!suggested_types],
            type=types(vm)[!suggested_types]))
    }

    ## 2. Shorter variable names will display in the menus better.
    ## Check threshold: 40 characters
    name_length <- nchar(names(vm))
    too_long_name <- name_length > 40
    if (any(too_long_name)) {
        num_too_long <- sum(too_long_name)
        cat("Shorter variable names will display in the menus better.",
            demonstrativeCount(num_too_long), "are longer than 40 characters:\n")
        print(data.frame(alias=keeps[too_long_name],
            length=name_length[too_long_name],
            name=names(vm)[too_long_name]))
    }

    ## 3. Categories
    ## a. Too many categories won't plot well as bars. (Unless they define
    ## regions on a map.)
    ## Check threshold: 7 categories
    num_cats <- vapply(vm, function (x) {
        cats <- x$categories
        if (is.null(cats)) return(0)
        ## We care only about non-missing categories
        return(sum(vapply(cats, function (ctg) !isTRUE(ctg$missing), logical(1))))
    }, numeric(1))
    too_many_cats <- num_cats > 7
    if (any(too_many_cats)) {
        num_too_many <- sum(too_many_cats)
        cat("Too many categories won't plot well.",
            demonstrativeCount(num_too_many),
            "have more than 7 non-missing categories:\n")
        print(data.frame(alias=keeps[too_many_cats],
            num_categories=num_cats[too_many_cats]))
    }

    ## b. Long category names won't fit well in the table headers, as bar/group
    ## labels, or in the graph legend.
    ## Check threshold: 40
    longest_cat <- vapply(vm, function (x) {
        cats <- x$categories
        if (is.null(cats)) return("")
        catnames <- vapply(cats, function (ctg) ctg$name, character(1))
        return(catnames[which.max(nchar(catnames))])
    }, character(1))
    cat_length <- nchar(longest_cat)
    too_long_cat <- cat_length > 40
    if (any(too_long_cat)) {
        num_too_long <- sum(too_long_cat)
        cat("Shorter category names will fit in the tables and graphs better.",
            demonstrativeCount(num_too_long),
            "have at least one category longer than 40 characters:\n")
        print(data.frame(alias=keeps[too_long_cat],
            length=cat_length[too_long_cat],
            category=longest_cat[too_long_cat]))
    }

    ## 4. Subvariables. Because multiple_response look like categorical when
    ## plotted, we'll use the same size/length check thresholds
    num_subvars <- vapply(vm, function (x) length(x$subvariables), numeric(1))
    too_many_subvars <- num_subvars > 7
    if (any(too_many_subvars)) {
        num_too_many <- sum(too_many_subvars)
        cat("Too many subvariables won't plot well. ",
            demonstrativeCount(num_too_many),
            " have more than 7 subvariables:\n")
        print(data.frame(alias=keeps[too_many_subvars],
            num_subvariables=num_subvars[too_many_subvars]))
    }

    longest_subvar <- vapply(vm, function (x) {
        cats <- x$subreferences
        if (is.null(cats)) return("")
        catnames <- vapply(cats, function (ctg) ctg$name, character(1))
        return(catnames[which.max(nchar(catnames))])
    }, character(1))
    subvar_length <- nchar(longest_subvar)
    too_long_subvar <- subvar_length > 40
    if (any(too_long_subvar)) {
        num_too_long <- sum(too_long_subvar)
        cat("Shorter subvariable names will fit in the tables and graphs better.",
            demonstrativeCount(num_too_long),
            "have at least one subvariable longer than 40 characters:\n")
        print(data.frame(alias=keeps[too_long_subvar],
            length=subvar_length[too_long_subvar],
            subvariable=longest_subvar[too_long_subvar]))
    }

    invisible(dataset)
}

demonstrativeCount <- function (n, noun="variable") {
    return(ifelse(n > 1, paste("These", n, "variables"), "This variable"))
}
