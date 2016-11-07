#' Permanently delete rows from a dataset
#'
#' @param dataset a \code{CrunchDataset}
#' @param expr a \code{CrunchLogicalExpr}
#' @return \code{dataset} without the rows indicated by \code{expr}
#' @seealso \code{\link{exclusion}} for a non-destructive way to suppress rows
#' @examples
#' \dontrun{
#' ds <- dropRows(ds, ds$gender == "Male")
#'}
#' @export
dropRows <- function (dataset, expr) {
    if (!length(expr)) {
        ## Could be NULL or like NULL == something, which is logical(0)
        halt("Invalid expression: ", deparse(match.call()$expr)[1])
    }
    payload <- list(command="delete", filter=zcl(expr))
    out <- crPOST(shojiURL(dataset, "fragments", "table"), body=toJSON(payload))
    dropCache(self(dataset)) ## Could do a more surgical cache drop, but this is safe
    return(dataset)
}
