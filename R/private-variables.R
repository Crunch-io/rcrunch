#' @rdname hide
#' @export
setMethod("privateFolder", "CrunchDataset", function(x) .firstLevelFolder(x, "private"))

#' @rdname hide
#' @export
setMethod("privateFolder", "VariableCatalog", function(x) .firstLevelFolder(x, "private"))

#' @rdname hide
#' @export
setMethod("privateFolder", "VariableFolder", function(x) .firstLevelFolder(x, "private"))

#' @rdname hide
#' @export
setMethod("privatize", "CrunchVariable", .firstLevelFolderMover("private"))

#' @rdname hide
#' @export
setMethod("privatize", "VariableCatalog", .firstLevelFolderMover("private"))

#' @rdname hide
#' @export
setMethod("deprivatize", "CrunchVariable", .firstLevelFolderMover("public"))
#' @rdname hide
#' @export
setMethod("deprivatize", "VariableCatalog", .firstLevelFolderMover("public"))

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

#' @rdname hide
#' @export
privatizeVariables <- function(dataset, variables) {
    dir <- privateFolder(dataset)
    if (is.null(dir)) .folderNotFoundError("hidden")
    dataset <- mv(dataset, variables, dir)
    return(invisible(refresh(dataset)))
}

#' @rdname hide
#' @export
privatiseVariables <- function(dataset, variables) {
  privatizeVariables(dataset, variables)
}

#' @rdname hide
#' @export
`privateVariables<-` <- function(x, value) privatizeVariables(x, value)

#' @rdname hide
#' @export
deprivatizeVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, publicFolder(dataset))
  return(invisible(refresh(dataset)))
}

#' @rdname hide
#' @export
deprivatiseVariables <- function(dataset, variables) {
  deprivatizeVariables(dataset, variables)
}


#' @rdname hide
#' @export
privateVariables <- function(dataset, key = namekey(dataset)) {
  if (is.unforcedVariableCatalog(dataset@privateVariables)) {
    pv <- getDatasetPrivateVariables(dataset)
  } else {
    pv <- dataset@privateVariables
  }

  if (length(pv)) {
    return(sort(vapply(index(pv), vget(key), character(1),
                       USE.NAMES = FALSE
    )))
  } else {
    return(c())
  }
}
