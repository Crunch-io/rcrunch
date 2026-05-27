#' Get & Set crunch metadata
#'
#' Crunch Variables have metadata associated with them beyond what is typically
#' used by R vectors. All variables have
#' - label: A short description of the variable (suitable for labeling a chart).
#'   Labels must be unique across all variables in a dataset.
#' - description: A longer description of the variable
#' - notes: Other information about the variable
#' - path: Folder path in the CrunchDataset's folders
#'
#' @param x A crunch variable
#' @param value Replacement value
#' @param ... Saved for future expansion
#'
#' @name crunch-metadata
#' @export
label <- function(x, ...) {
    UseMethod("label", x)
}

#' @export
#' @rdname crunch-metadata
set_label <- function(x, value, ...) {
    UseMethod("set_label", x)
}

#' @export
#' @rdname crunch-metadata
`label<-` <- function(x, value) {
    set_label(x, value)
}

#' @export
#' @rdname crunch-metadata
description <- function(x, ...) {
    UseMethod("description", x)
}

#' @export
#' @rdname crunch-metadata
set_description <- function(x, value) {
    UseMethod("set_description", x)
}

#' @export
#' @rdname crunch-metadata
`description<-` <- function(x, value) {
    set_description(x, value)
}

#' @export
#' @rdname crunch-metadata
notes <- function(x, ...) {
    UseMethod("notes", x)
}

#' @export
#' @rdname crunch-metadata
set_notes <- function(x, value, ...) {
    UseMethod("set_notes", x)
}

#' @export
#' @rdname crunch-metadata
`notes<-` <- function(x, value) {
    set_notes(x, value)
}

#' @export
#' @rdname crunch-metadata
path <- function(x, ...) {
    UseMethod("path", x)
}

#' @export
#' @rdname crunch-metadata
set_path <- function(x, value, ...) {
    UseMethod("set_path", x)
}

#' @export
#' @rdname crunch-metadata
`path<-` <- function(x, value) {
    set_path(x, value)
}


#' @export
#' @rdname crunch-metadata
values <- function(x, ...) {
    UseMethod("values", x)
}

#' @export
#' @rdname crunch-metadata
set_values <- function(x, value, ...) {
    UseMethod("set_values", x)
}

#' @export
#' @rdname crunch-metadata
`values<-` <- function(x, value) {
    set_values(x, value)
}

#' Get Crunch Array axes
#'
#' Provide a summary of the structure of items inside a Crunch Array Variable.
#' Array variables ared stored in R as "packed" data.frames (eg data.frame columns
#' in another data.frame).
#' @param x A crunch array variable
#' @param ... Saved for future expansion
#'
#' @export
axes <- function(x, ...) {
    UseMethod("axes", x)
}

