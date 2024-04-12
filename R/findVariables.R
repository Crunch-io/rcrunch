#' Find variables and their paths in a Crunch dataset or folder
#'
#' @param x Crunch dataset or variable folder
#' @param deep FALSE (the default) or TRUE; should subfolders
#' @param include.hidden FALSE (default) or TRUE, should hidden be included in the result?
#'
#' @return Data.frame with one row per Crunch variable and columns \code{alias}, \code{path}, \code{hidden}
#' @export
findVariables <- function(x, deep = FALSE, include.hidden = FALSE) {
    if (is.dataset(x)) {
        x <- cd(x, ".")
        startpath <- ""
    } else if (is.folder(x)) {
        startpath <- name(x)
    } else {
        halt('`x` should be "CrunchDataset" or "VariableFolder", not "', paste(class(x), collapse = ", "), '"')
    }
    if (!isTRUE(deep) && !isFALSE(deep)) {
        halt("`deep` should be TRUE or FALSE")
    }
    if (!isTRUE(include.hidden) && !isFALSE(include.hidden)) {
        halt("`hidden` should be TRUE or FALSE")
    }
    if (!deep) {
        vars <- aliases(variables(x))
        nvars <- length(vars)
        res <- data.frame(alias = vars, path = rep(startpath, nvars), hidden = rep(FALSE, nvars))
        return(res)
    }
    res <- .findVariables(x, startpath)
    res$hidden <- rep(FALSE, nrow(res))
    if (include.hidden) {
        hidden <- .findVariables(hiddenFolder(x), startpath)
        hidden$hidden <- rep(TRUE, nrow(hidden))
        res <- rbind(res, hidden)
    }
    res
}

.findVariables <- function(x, path) {
    vars <- variables(x)
    res <- data.frame(alias = aliases(vars), path = rep(path, length(vars)))
    dirs <- x[types(x) %in% "folder"]
    if (length(dirs) == 0) {
        return(res)
    }
    dirnames <- names(dirs)
    res2 <- lapply(seq_along(dirnames), function(i) {
        if (identical(path, "")) {
            new_path <- dirnames[i]
        } else {
            new_path <- paste(path, dirnames[i], sep = " | ")
        }
        .findVariables(dirs[[i]], new_path)
    })
    rbind(res, do.call(rbind, res2))
}
