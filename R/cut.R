#' Cut a numeric Crunch variable
#' 
#' crunch::cut() is equivalent to base::cut() except that it operates on 
#' crunch variables instead of in-memory variables. The function divides the range of 
#' x into intervals and codes the values in x according to which interval they fall. 
#' The leftmost interval corresponds to level one, the next leftmost to level two and 
#' so on.
#' @param x A crunch variable of the class NumericVariable
#' @param breaks Either a numeric vector of two or more unique cut points 
#' or a single number (greater than or equal to 2) giving the number of intervals 
#' into which x is to be cut.
#' @param variable.name The name of the resulting case variable as a character string. 
#' @param labels labels for the levels of the resulting category.
#' By default, labels are constructed using interval notation.
#' If labels = FALSE, simple integer codes are returned instead of a factor.
#' @param include.lowest logical, indicating if an `x[i]` equal to the lowest 
#' (or highest, for right = FALSE) `breaks` value should be included.
#' @param right logical, indicating if the intervals should be closed on the right 
#' (and open on the left) or vice versa.
#' @param dig.lab	integer which is used when labels are not given. 
#' It determines the number of digits used in formatting the break numbers.
#' @param ordered_result	Ignored.
#' @param ... further arguments passed to or from other methods.
#'
#'
#' @return a Crunch VariableDefinition
#' @export
#'
#' @examples
#' \dontrun{
#' ds <- loadDataset("mtcars")
#' ds$cat_var <- cut(ds$mpg, 3, variableName = "new_var")
#' }
#'  
setMethod("cut", "NumericVariable", function(x, 
                                             breaks, 
                                             variable.name, 
                                             labels = NULL,
                                             include.lowest = FALSE,
                                             right = TRUE,
                                             dig.lab = 3,
                                             ordered_result = FALSE, ...){
    env <- environment()
      if (missing(variable.name)) {
        halt("Must provide the name for the new variable")
    }
    if (length(breaks) == 1L) {
        if (is.na(breaks) || breaks < 2L) {
            halt("invalid number of intervals")
        }
        nb <- as.integer(breaks + 1) # one more than #{intervals}
        rx <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
        dx <- diff(rx)
        if (dx == 0) {
            dx <- abs(rx[1L])
            breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000,
                              length.out = nb)
        } else {
            breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
            breaks[c(1L, nb)] <- c(rx[1L] - dx/1000, rx[2L] + dx/1000)
        }
    }else {
        breaks <- sort.int(as.double(breaks))
        nb <- length(breaks)
    }
    if (anyDuplicated(breaks)) halt("'breaks' are not unique")
    if (is.null(labels)) { #Autogenerate labels if not supplied
      labels <- crunch:::generateCutLabels(dig.lab, breaks, nb, right, include.lowest)
    } else if (length(labels) != nb - 1L) {
        stop("lengths of 'breaks' and 'labels' differ")
    } 
    if (right) {
        comp_1 <- " <= "
        comp_2 <- " < "
    } else {
        comp_1 <- " < "
        comp_2 <- " <= "
    }
    cases <- vector("character", length = length(breaks) - 1)
    varname  <- deparse(substitute(x))
    for (i in 2:length(breaks)) {
        cases[i - 1] <- parse(
            text = paste0(breaks[i - 1], 
                          comp_1, 
                          varname, 
                          " & ",  
                          varname,
                          comp_2,
                          breaks[i])
        )
    }
    cases <- lapply(cases, function(x) eval(x, envir = env))
    case_list <- lapply(seq_along(cases), function(x) list(expression = cases[[x]], name = labels[x]))
    makeCaseVariable(cases = case_list, name = variable.name, ...)
}
)


#' Generate Labels for the cut function
#' 
#' A convenience function to generate labels for the cut function. This
#' function is extracted from base::cut() and is broken out to make it easier to 
#' test. It is not meant to be called on its own. 
#'
#' @param dig.lab see `cut()`
#' @param breaks see `cut()`
#' @param nb The number of breaks, equal to the small of 2 or the number of breaks
#' @param right  see `cut()`
#' @param include.lowest see`cut()`
#'
#' @return
#'
#' @examples
#' 
#' crunch:::generateCutLabels(2, c(2, 3, 4, 5), 4, FALSE, FALSE)
#' 
generateCutLabels <- function(dig.lab, breaks, nb, right, include.lowest) {
    for (dig in dig.lab:max(12L, dig.lab)) {
        ## 0+ avoids printing signed zeros as "-0"
        ch.br <- formatC(0 + breaks, digits = dig, width = 1L)
        ok <- all(ch.br[-1L] != ch.br[-nb])
        if (ok ) break
    }
    labels <- if (ok) {
            paste0(if (right) "(" else "[",
                   ch.br[-nb], ",", ch.br[-1L],
                   if (right) "]" else ")")
        } else {
            paste("Range", seq_len(nb - 1L), sep = "_")
        }
    if (ok && include.lowest) {
        if (right) {
            substr(labels[1L], 1L, 1L) <- "[" # was "("
        } else {
            substring(labels[nb - 1L],
                      nchar(labels[nb - 1L], "c")) <- "]" # was ")"
        }
    }
    labels
}
