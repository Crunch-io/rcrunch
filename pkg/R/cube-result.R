.init.Cube <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@dims <- cubeDims(.Object)
    .Object@arrays <- lapply(.Object$result$measures, cToA, dims=.Object@dims)
    return(.Object)
}
setMethod("initialize", "CrunchCube", .init.Cube)

cubeToArray <- function (x, measure="count") {
    out <- x@arrays[[measure]]
    dimnames(out) <- dimnames(x@dims)
    out <- pruneCubeArray(out, x)
    return(out)
}

cToA <- function (x, dims) {
    ## Just make an array from the cube "measure's" data. Nothing else
    
    d <- unlist(x$data)
    d <- round(d) ## TODO digits should be an argument
    ## and rounding should also depend on whether you're looking at count or not
    
    dimsizes <- vapply(dims, length, integer(1), USE.NAMES=FALSE)
    ndims <- length(dims)
    if (ndims > 1) {
        ## Cube arrays come in row-col-etc. order, not column-major.
        ## Keep the labels right here, then aperm the array back to order
        dimsizes[1:2] <- dimsizes[c(2,1)]
    }
    out <- array(d, dim=dimsizes)
    if (ndims > 1) {
        ap <- seq_len(ndims)
        ap[1:2] <- 2:1
        out <- aperm(out, ap)
    }
    return(out)
}

pruneCubeArray <- function (x, cube) {
    ndims <- length(dim(x))
    if (ndims > 1) {
        marginals <- lapply(cube$result$margin[as.character(seq_len(ndims) - 1)],
            unlist)
        ## TODO: make marginals from the cube, not the server's
    } else {
        ## Kind of a hack. We just want to know whether there are 0 values
        ## for pruning
        marginals <- list(as.vector(x))
    }
    
    ## Evaluate which to drop
    keep.these <- mapply(pruneDimension,
        dimension=cube@dims, 
        marginal=marginals, 
        MoreArgs=list(useNA=cube@useNA),
        SIMPLIFY=FALSE,
        USE.NAMES=FALSE)
    names(keep.these) <- names(dimnames(x))
    x <- do.call("[", c(list(x=x, drop=FALSE), keep.these))
    return(x)
}

marginals <- function (array, dimensions) {
    lapply(seq_along(dim(array)), function (i) {

    })
}

cubeArrayMarginTable <- function (x, margin) {
    return(x)
    
    ## if mr, use __any__ | __none__
    ## if ca, is there that on the other slice?
    ## else, sweep
    attrs <- attr(x, "meta")
    if (!is.null(attrs)) {
        aon <- lapply(attrs, function (a) a[["any.or.none"]])
        args <- c(list(x = x, drop=FALSE), lapply(aon, function (a) {
            if (!length(a)) {
                ## If there isn't "any" or "none", keep all
                out <- TRUE
            }
            return(out)
        }))
        x <- do.call("[", args)
    }
    # return(margin.table(x, margin))
}

cmt <- function (x, dimension) {
    
}

##' @export
as.array.CrunchCube <- function (x, ...) cubeToArray(x, ...)

pruneDimension <- function (dimension, marginal, useNA) {
    ## Returns logicals of which rows/cols/etc. should be kept
    
    ## Always drop __any__ and __none__
    out <- !vapply(dimension, function (x) x$any.or.none, logical(1))
    
    if (useNA != "always") {
        ## Means drop missing always, or only keep if there are any
        valid.cats <- !vapply(dimension, function (x) x$missing, logical(1))
        if (useNA == "ifany") {
            valid.cats <- valid.cats | vapply(marginal, length, integer(1)) > 0
        }
        ## But still drop __any__ or __none__
        out <- valid.cats & out
    }

    return(out) 
}

setMethod("prop.table", "CrunchCube", function (x, margin=NULL) {
    out <- as.array(x)
    return(out/sum(out))
})