setMethod("private", "CrunchDataset", function(x) private(folders(x)))

setMethod("private", "VariableCatalog", function(x) private(folders(x)))

setMethod("private", "VariableFolder", function(x) {
  return(VariableFolder(crGET(shojiURL(rootFolder(x), "catalogs", "secure"))))
})


#' @rdname hide
#' @export
setMethod("privatize", "CrunchVariable", function(x) {
  .moveToFolder(private(rootFolder(x)), x)
  # TODO: should these refresh?
  invisible(x)
})
#' @rdname hide
#' @export
setMethod("privatize", "VariableCatalog", function(x) {
  .moveToFolder(private(rootFolder(x)), x)
  invisible(x)
})

#' @rdname hide
#' @export
setMethod("deprivatize", "CrunchVariable", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})
#' @rdname hide
#' @export
setMethod("deprivatize", "VariableCatalog", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})

#' @rdname hide
#' @export
privatise <- function(...) {
  privatize(...)
}

#' @rdname hide
#' @export
deprivatise <- function(...) {
  deprivatize(...)
}

#' @rdname hideVariables
#' @export
privatizeVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, private(dataset))
  return(invisible(refresh(dataset)))
}

#' @rdname hideVariables
#' @export
`privateVariables<-` <- function(x, value) privatizeVariables(x, value)

#' @rdname hideVariables
#' @export
deprivatizeVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, folders(dataset))
  return(invisible(refresh(dataset)))
}

#' @rdname hiddenVariables
#' @export
privateVariables <- function(dataset, key = namekey(dataset)) {
  pv <- dataset@privateVariables
  if (length(pv)) {
    return(sort(vapply(index(pv), vget(key), character(1),
                       USE.NAMES = FALSE
    )))
  } else {
    return(c())
  }
}
