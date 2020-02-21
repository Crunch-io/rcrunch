setMethod("private", "CrunchDataset", function(x) private(folders(x)))

setMethod("private", "VariableCatalog", function(x) private(folders(x)))

setMethod("private", "VariableFolder", function(x) {
  return(VariableFolder(crGET(shojiURL(rootFolder(x), "catalogs", "secure", mustWork = FALSE))))
})


#' @rdname hide
#' @export
setMethod("privatize", "CrunchVariable", function(x) {
  private_dir <- private(rootFolder(x))
  if (is.null(private_dir)) {
    halt("Could not access private directory, are you an editor of this dataset?")
  }
  .moveToFolder(private_dir, x)
  # TODO: should these refresh?
  invisible(x)
})
#' @rdname hide
#' @export
setMethod("privatize", "VariableCatalog", function(x) {
  private_dir <- private(rootFolder(x))
  if (is.null(private_dir)) {
    halt("Could not access private directory, are you an editor of this dataset?")
  }
  .moveToFolder(private_dir, x)
  # TODO: should these refresh?
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
privatise <- function(x) {
  privatize(x)
}

#' @rdname hide
#' @export
deprivatise <- function(x) {
  deprivatize(x)
}

#' @rdname hideVariables
#' @export
privatizeVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, private(dataset))
  return(invisible(refresh(dataset)))
}

#' @rdname hide
#' @export
privatiseVariables <- function(dataset, variables) {
  privatizeVariables(dataset, variables)
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

#' @rdname hide
#' @export
deprivatiseVariables <- function(dataset, variables) {
  deprivatizeVariables(dataset, variables)
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
