#' Find variables and their paths in a Crunch dataset or folder
#' 
#' Returns a data.frame whose rows correspond to Crunch variables found in \code{x}.
#' By default, only top-level, non-hidden, non-private variables in \code{x} are returned.
#' 
#' @param x Crunch dataset or variable folder
#' @param deep Defaults to \code{FALSE}, \code{TRUE} recursively examines any subfolders as well
#' @param include.hidden Defaults to \code{FALSE}, \code{TRUE} finds any hidden variables as well
#' @param include.private Defaults to \code{FALSE}, \code{TRUE} finds any private variables as well
#'
#' @return Data.frame with one row per Crunch variable and columns \code{alias} (Crunch variable alias),
#' \code{path} (location of the variable, with " | " indicating nesting,
#' e.g. "Foo | Bar" indicates that the variable can be found in the folder "Bar" and that "Bar" is located in folder "Foo"),
#' \code{hidden} (\code{TRUE} or \code{FALSE}), \code{private} (\code{TRUE} or \code{FALSE})
#' @export
findVariables <- function(x, deep = FALSE, include.hidden = FALSE, include.private = FALSE) {
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
        halt("`include.hidden` should be TRUE or FALSE")
    }
    if (!isTRUE(include.private) && !isFALSE(include.private)) {
        halt("`include.private` should be TRUE or FALSE")
    }
    if (!deep) {
        vars <- aliases(variables(x))
        nvars <- length(vars)
        res <- data.frame(alias = vars, path = rep(startpath, nvars), hidden = rep(FALSE, nvars), private = rep(FALSE, nvars))
        return(res)
    }
    res <- .findVariables(x, startpath)
    res <- do.call(rbind, res)
    res$hidden <- rep(FALSE, nrow(res))
    res$private <- rep(FALSE, nrow(res))
    if (include.hidden) {
        hidden <- .findVariables(hiddenFolder(x), startpath)
        hidden <- do.call(rbind, hidden)
        hidden$hidden <- rep(TRUE, nrow(hidden))
        hidden$private <- rep(FALSE, nrow(hidden))
        res <- rbind(res, hidden)
    }
    if (include.private) {
        private <- .findVariables(privateFolder(x), startpath)
        private <- do.call(rbind, private)
        private$hidden <- rep(FALSE, nrow(private))
        private$private <- rep(TRUE, nrow(private))
        res <- rbind(res, private)
    }
    res
}

.findVariables <- function(x, path) {
    vars <- variables(x)
    res <- list(data.frame(alias = aliases(vars), path = rep(path, length(vars))))
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
    c(res, unlist(res2, recursive = FALSE))
}
