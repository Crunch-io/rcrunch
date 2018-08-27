#' @rdname tuple-methods
#' @export
setMethod("$", "ShojiEntity", function(x, name) x@body[[name]])
#' @rdname tuple-methods
#' @export
setMethod("$<-", "ShojiEntity", function(x, name, value) {
    x[[name]] <- value
    return(x)
})
#' @rdname tuple-methods
#' @export
setMethod("[[", "ShojiEntity", function(x, i) x@body[[i]])
#' @rdname tuple-methods
#' @export
setMethod("[[<-", "ShojiEntity", function(x, i, value) {
    if (is.variable(value)) {
        ## Setting a weight: get its URL
        ## Generalize this `if` whenever needed
        value <- self(value)
    }
    ## Assign in with [ and wrap value in list() so that NULL can be set without
    ## removing the attribute from x@body
    x@body[i] <- list(value)
    return(x)
})

updateEntity <- function(x, value) {
    old <- x@body
    new <- value@body

    ## Look for new (illegal) attributes
    newattrs <- setdiff(names(new), names(old))
    if (length(newattrs)) {
        halt("Invalid attribute", ifelse(length(newattrs) > 1, "s: ", ": "),
            serialPaste(newattrs),
            call. = FALSE
        )
    }
    ## Now look for updates
    common <- intersect(names(new), names(old))
    changed <- dirtyElements(old[common], new[common])
    if (any(changed)) {
        crPATCH(self(x), body = toJSON(new[common[changed]]))
        x <- refresh(x)
    }
    return(x)
}
