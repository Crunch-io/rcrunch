#' Copy a variable
#'
#' Makes a copy of a Crunch variable on the server.
#'
#' Copies can be shallow (linked) or deep. Shallow copying is faster and is preferable
#' unless a true hard copy is required. Shallow copies are effectively pointers to the
#' original variable, and then you append data to the original
#' variable or otherwise alter its values, the values in the copy automatically
#' update. This linking may be desirable, but it comes with some limitations.
#' First, you cannot edit the values of the copy independently of the original.
#' Second, some attributes of the copy are immutable: of note, properties of
#' categories cannot be altered independently in the copy, but you can alter Subvariable names and
#' ordering within arrays.
#'
#' @param x a CrunchVariable to copy
#' @param deep logical: should this be a deep copy, in which there is no
#' dependence on the original variable, or a shallow one, in which the copy
#' is more of a symbolic link? Default is `FALSE`, meaning symlink.
#' @param ... Additional metadata to give to the new variable. If not given,
#' the new variable will have a name that is the same as the original but with
#' " (copy)" appended, and its alias will be the old alias with "_copy"
#' appended.
#' @return a VariableDefinition for the copied variable. Assign into a Dataset
#' to make the copy happen.
#' @export
copyVariable <- function (x, deep=FALSE, ...) {
    stopifnot(is.variable(x))

    newbody <- list(...)
    oldbody <- modifyList(copyVariableReferences(x), copyVariableReferences(tuple(x)))
    oldbody$name <- paste0(oldbody$name, " (copy)")
    oldbody$alias <- paste0(oldbody$alias, "_copy")

    body <- modifyList(oldbody, newbody)
    body$id <- NULL
    body$type <- NULL
    body$derivation <- zfunc("copy_variable", x)
    if (deep) {
        body$derived <- FALSE
    }
    class(body) <- "VariableDefinition"

    return(body)
}

#' @rdname copyVariable
#' @export
copy <- copyVariable

copyVariableReferences <- function (x, fields=c("name", "alias",
                                    "description", "discarded", "format", "notes",
                                    "view", "type")) {

    if (inherits(x, "CrunchVariable")) {
        return(modifyList(copyVariableReferences(tuple(x)),
            copyVariableReferences(entity(x))))
    } else {
        return(x@body[intersect(fields, names(x@body))])
    }
}
