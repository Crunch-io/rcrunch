#' Construct a variable definition
#'
#' Crunch variables are created by posting a `VariableDefinition` to the Crunch server.
#' The `VariableDefinition` contains the information the server requires to
#' calculate the variable. This can information can either be in the form
#' of the actual data which you would like to include in the variable, or a derivation
#' which tells the server how to derive the new variable from existing ones.
#' This function converts an R vector or set of attributes into a variable definition which
#' can be posted to the server.
#'
#' @param data an R vector of data to convert to the Crunch payload format.
#' See [toVariable] for how R data types are converted. This function can
#' also be used to construct a `VariableDefinition` directly by passing
#' attributes to `...`. This is only recommended for advanced users who are
#' familiar with the Crunch API.
#' @param ... additional attributes to be included in the `VariableDefinition`
#' @return a `VariableDefinition` object, ready to POST to Crunch.
#' @export
#' @examples
#' VariableDefinition(rnorm(5), name="Some numbers",
#'     description="Generated pseudorandomly from the normal distribution")
#' VarDef(name="Integers", values=1:5, type="numeric",
#'     description="When creating variable definitions with 'values', you must
#'     specify 'type', and categorical variables will require 'categories'.")
#' @seealso `toVariable`
VariableDefinition <- function(data, ...) {
    out <- list(...)
    if (!missing(data)) {
        out <- modifyList(toVariable(data), out)
    }
    class(out) <- "VariableDefinition"
    return(out)
}

#' @rdname VariableDefinition
#' @export
VarDef <- VariableDefinition

setOldClass("VariableDefinition")
