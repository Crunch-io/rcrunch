CrunchDataFrame <- function (dataset) {
    ## S3 constructor method for CrunchDataFrame. terms.formula doesn't seem
    ## to like S4 subclass of environment
    stopifnot(is.dataset(dataset))
    out <- new.env()
    out$.crunchDataset <- dataset
    with(out, {
        ## Note the difference from as.environment: wrapped in as.vector
        for (a in aliases(allVariables(dataset))) {
            eval(substitute(delayedAssign(v, as.vector(.crunchDataset[[v]])),
                list(v=a)))
        }
    })
    class(out) <- "CrunchDataFrame"
    return(out)
}

setOldClass("CrunchDataFrame")

#' @export
dim.CrunchDataFrame <- function (x) dim(x$.crunchDataset)

#' @export
names.CrunchDataFrame <- function (x) names(x$.crunchDataset)

#' as.data.frame method for CrunchDataset
#'
#' This method is defined principally so that you can use a CrunchDataset as
#' a \code{data} argument to other R functions (such as
#' \code{\link[stats]{lm}}). Unless you give it the \code{force==TRUE}
#' argument, this function does not in fact return a \code{data.frame}: it
#' returns an object with an interface like a data.frame, such that you get
#' R vectors when you access its columns (unlike a CrunchDataset, which
#' returns CrunchVariable objects). This allows modeling functions that
#' require select columns of a dataset to retrieve only those variables from
#' the remote server, rather than pulling the entire dataset into local
#' memory.
#'
#' @param x a CrunchDataset
#' @param row.names part of as.data.frame signature. Ignored.
#' @param optional part of as.data.frame signature. Ignored.
#' @param force logical: actually coerce the dataset to \code{data.frame}, or
#' leave the columns as unevaluated promises. Default is \code{FALSE}.
#' @param ... additional arguments passed to as.data.frame.default
#' @return an object of class \code{CrunchDataFrame} unless \code{force}, in
#' which case the return is a \code{data.frame}.
#' @name dataset-to-R
NULL

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataset <- function (x, row.names = NULL, optional = FALSE,
                                        force=FALSE, ...) {
    out <- CrunchDataFrame(x)
    if (force) {
        out <- as.data.frame(out)
    }
    return(out)
}

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataFrame <- function (x, row.names = NULL, optional = FALSE, ...) {
    x <- x$.crunchDataset
    default.stringsAsFactors <- function () FALSE
    limit <- min(c(10000, getOption("crunch.data.frame.limit")))
    if (nrow(x) * ncol(x) > limit) {
        ## TODO: switch to downloading CSV and reading that?
        halt("Dataset too large to coerce to data.frame. ",
            "Consider subsetting it first")
    }
    out <- lapply(x, as.vector)
    names(out) <- names(x)
    return(structure(out, class="data.frame", row.names=c(NA, -nrow(x))))
}


#' as.data.frame method for VariableCatalog
#'
#' This method allows you to import the VariableCatalog into your R sessions in 
#' order to facilliate interactive use. Modifying the dataframe produced by this
#' method will not update the Crunch web app, for information about modifying 
#' variables in the app see: \code{vingette("variables",
#' package = "crunch")}  
#' 
#' @param varCat
#' A VariableCatalog produced by \code{variables(ds)}.
#' @param fields 
#' A character vector of the fields from the variable catalog which you would like 
#' included in the datafame. To include all fields or see which fields are available
#' set fields to "all". 
#' @return A data frame
#' @examples 
#' \dontrun{
#' ds <- loadDataset("iris")
#' vars <- variables(ds)
#' var_df <- as.data.frame(vars, fields = "all")
#' }
#' 
#' @rdname VariableCatalog-to-Data-Frame
#' @importFrom purrr map_df map_lgl
#' @importFrom dplyr bind_cols
#' @export
as.data.frame.VariableCatalog <- function(varCat, 
                                          fields = c("alias", "name", "type")) {
  out <- catalogToDataFrame(varCat)
  subvariable_names <- c("subvariables", "subvariables_catalog")
  sub_variables <- out[, subvariable_names]
  vectors <- out[, setdiff(names(out), subvariable_names)]
  ## Null values in the description field cause the description column to be
  ## shorter than the other columns. Recoding as a character value makes the data
  ## rectangular. 
  vectors$description[map_lgl(vectors$description, is.null)] <-  "NULL"
  vectors <- map_df(vectors, unlist)
  out <- bind_cols(vectors, sub_variables)

 if (all(fields == "all")) {
   return(out)
 }
 if (!all(fields %in% names(out))) {
   stop("Field name not present in variable catalog, use fields = 'all' to see available fields.")
 }
   return(out[, fields]) 
}


