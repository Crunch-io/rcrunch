##' Copy a variable
##'
##' Makes a copy of a Crunch variable on the server.
##'
##' Note that only shallow (linked) copying is supported, which has some
##' implications. When you append data to the original variable 
##' or otherwise alter its values, the values in the copy automatically update. 
##' This linking may be desirable, but it comes with some limitations. First,
##' you cannot edit the values of the copy independently of the original. 
##' Second, some attributes of the copy are immutable: of note, properties of
##' categories cannot be altered independely in the copy. Subvariable names and
##' ordering within arrays, however, can.
##'
##' @param x a CrunchVariable to copy
##' @param deep logical: should this be a deep copy, in which there is no
##' dependence on the original variable, or a shallow one, in which the copy
##' is more of a symbolic link? Default is \code{FALSE}, meaning symlink, and
##' in fact, deep copying is not yet supported.
##' @param ... Additional metadata to give to the new variable. If not given,
##' the new variable will have a name that is the same as the original but with
##' " (copy)" appended, and its alias will be the old alias with "_copy"
##' appended.
##' @return a VariableDefinition for the copy expression. Assign into a Dataset
##' to make the copy happen.
##' @export
copyVariable <- function (x, deep=FALSE, ...) {
    stopifnot(is.variable(x))
    if (deep) {
        halt("Deep copying not implemented.")
    }
    
    newbody <- list(...)
    oldbody <- updateList(copyVariableReferences(x), tuple(x)@body)
    oldbody$name <- paste0(oldbody$name, " (copy)")
    oldbody$alias <- paste0(oldbody$alias, "_copy")
    
    body <- updateList(oldbody, newbody)
    body$type <- NULL
    body$id <- NULL
    body$expr <- zfunc("copy_variable", x)
    
    class(body) <- "VariableDefinition"
    return(body)
}

##' @rdname copyVariable
##' @export
copy <- copyVariable

copyVariableReferences <- function (x, fields=c("name", "alias",
                                    "description", "discarded", "format",
                                    "view", "type")) {
    return(x@body[intersect(fields, names(x@body))])
}