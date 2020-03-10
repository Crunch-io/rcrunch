#' Open dataset selector
#'
#' @inheritParams listDatasets
#' @return A `loadDataset()` call is pasted into your RStudio session
#' @keywords internal
listDatasetGadget <- function(kind = c("active", "all", "archived"),
                              refresh = FALSE) {
    call <- match.call()
    callFromOtherPackage(call, "crunchy")
}

#' Array builder
#'
#' Launch array builder gadget
#'
#' Categorical Array and Multiple Response variables can be difficult to
#' construct without being able to investigate the available variables, and
#' their categories. This shiny gadget lets you select subvariables from the
#' dataset list, and ensures that those variables have consistent categories. To
#' use the gadget you must have at least one CrunchDataset loaded into the global
#' environment.
#'
#' @return a valid call to `makeArray()` or `makeMR()`
#' @export
makeArrayGadget <- function() {
    call <- match.call()
    callFromOtherPackage(call, "crunchy")
}

callFromOtherPackage <- function(call, pkg) {
    # Find the function of the same name in the crunchy package and call it instead
    function_name <- as.character(call[[1]])
    # remove namespacing if included
    function_name <- function_name[!(function_name %in% c("crunch", "::", ":::"))]
    if (hasFunction(function_name, pkg)) {
        call[[1]] <- get(function_name, asNamespace(pkg))
    } else {
        halt(
            "Please install the latest version of ",
            pkg,
            " to access ",
            function_name
        )
    }
    eval.parent(call)
}
