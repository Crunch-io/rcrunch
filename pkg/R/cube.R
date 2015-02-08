##' Crosstab and otherwise aggregate variables in a dataset
##'
##' Create a contingency table or other aggregation from cross-classifying
##' variables in a CrunchDataset.
##'
##' @param formula an object of class 'formula' object with the
##' cross-classifying variables, separated by '+', on the right hand side.
##' Compare to \code{\link[stats]{xtabs}}.
##' @param data an object of class \code{CrunchDataset}
##' @param weight a CrunchVariable that has been designated as a potential
##' weight variable for \code{data}, or \code{NULL} for unweighted results.
##' Default is the currently applied weight, \code{\link{weight}(data)}.
##' @param useNA whether to include missing values in tabular results. See
##' \code{\link[base]{table}}.
##' @return an object of class \code{CrunchCube}
##' @export
getCube <- function (formula, data, weight=rcrunch::weight(data), 
                     useNA=c("no", "ifany", "always")) {
    if (missing(formula)) {
        halt("Must provide a formula")
    }
    formula <- try(as.formula(formula), silent=TRUE)
    if (is.error(formula)) {
        halt(dQuote("formula"), " is not a valid formula")
    }
    
    f <- terms(as.formula(formula), allowDotAsName=TRUE) ## To catch "."
    f.vars <- attr(f, "variables")
    all.f.vars <- all.vars(f.vars)
    if ("." %in% all.f.vars) {
        halt("getCube does not support ", dQuote("."), " in formula")
    }
    if (!length(all.f.vars)) {
        halt("Must supply one or more variables")
    }
    
    if (missing(data) || !is.dataset(data)) {
        halt(dQuote("data"), " must be a Dataset")
    }
    
    where <- parent.frame()
    ## Find variables either in 'data' or in the calling environment
    vars <- lapply(all.f.vars, 
        function (x) data[[x]] %||% safeGet(x, envir=where))
    names(vars) <- all.f.vars
    notfound <- vapply(vars, is.null, logical(1))
    if (any(notfound)) {
        badvars <- all.f.vars[notfound]
        halt(serialPaste(dQuote(badvars)), 
            ifelse(length(badvars) > 1, " are", " is"),
            " not found in ", dQuote("data"))
    }
    vars <- registerCubeFunctions(vars)
    v.call <- do.call(substitute, list(expr=f.vars, env=vars))
    print(v.call)
    vars <- eval(v.call)
    
    resp <- attr(f, "response")
    if (resp) {
        measures <- lapply(vars[resp], absolute.zcl)
        names(measures) <- "count" ## HACK. Server provides margins iff "count"
        vars <- vars[-resp]
    } else {
        measures <- list(count=zfunc("cube_count"))
    }
    
    force(weight)
    if (is.variable(weight)) {
        weight <- self(weight)
        ## Should confirm that weight is in weight_variables. Server 400s
        ## if it isn't.
    } else {
        weight <- NULL
    }
    
    query <- list(dimensions=varsToCubeDimensions(vars),
        measures=measures, weight=weight)
    ## Final validations
    badmeasures <- vapply(query$measures, Negate(isCubeAggregation), logical(1))
    if (any(badmeasures)) {
        halt("Left side of formula must be a valid aggregation")
    }
    baddimensions <- vapply(query$dimensions, isCubeAggregation, logical(1))
    if (any(baddimensions)) {
        halt("Right side of formula cannot contain aggregation functions")
    }
    ## Go GET it!
    cube_url <- shojiURL(data, "views", "cube")
    return(CrunchCube(crGET(cube_url, query=list(query=toJSON(query))),
        useNA=match.arg(useNA)))
}

safeGet <- function (x, ..., ifnot=NULL) {
    out <- try(get(x, ...), silent=TRUE)
    if (is.error(out)) out <- ifnot
    return(out)
}

registerCubeFunctions <- function (vars) {
    ## Given a list of variables, add to it "cube functions", to substitute()
    ## in. A better approach, which would avoid potential name collisions, would
    ## probably be to have vars be an environment inside of another environment
    ## that has the cube functions.
    
    funcs <- list(
        mean=function (x) zfunc("cube_mean", x),
        min=function (x) zfunc("cube_min", x),
        max=function (x) zfunc("cube_max", x),
        # median=function (x) zfunc("cube_quantile", x, .5),
        # quantile=function (x, q) zfunc("cube_quantile", x, q),
        sd=function (x) zfunc("cube_stddev", x),
        sum=function (x) zfunc("cube_sum", x)
    )
    
    overlap <- intersect(names(vars), names(funcs))
    if (length(overlap)) {
        halt("Cannot evaluate a cube with reserved name", 
            ifelse(length(overlap) > 1, "s", ""), ": ",
            serialPaste(dQuote(overlap)))
    }
    return(c(vars, funcs))
}

isCubeAggregation <- function (x) {
    "function" %in% names(x) && grepl("^cube_", x[["function"]])
}

varsToCubeDimensions <- function (vars) {
    ## Given variables, construct the appropriate ZCL to get a cube with them
    ## as dimensions
    dimensions <- unlist(lapply(vars, function (x) {
        v <- absolute.zcl(x)
        if (is.MR(x)) {
            ## Multiple response gets "selected_array" and "each"
            return(list(zfunc("selected_array", v),
                list(each=self(x))))
        } else if (is.CA(x)) {
            ## Categorical array gets the var reference and "each"
            ## Put "each" first so that the rows, not columns, are subvars
            return(list(list(each=self(x)),
                v))
        } else {
            ## Just the var ref, but nest in a list so we can unlist below to
            ## flatten
            return(list(v))
        }
    }), recursive=FALSE)
    names(dimensions) <- NULL
    return(dimensions)
}

cubeToArray <- function (x, measure="count") {
    d <- unlist(x$result$measures[[measure]]$data)
    d <- round(d) ## TODO digits should be an argument
    ## and rounding should also depend on whether you're looking at count or not
    dimnames <- cubeDimnames(x)
    ndims <- length(dimnames)
    if (ndims > 1) {
        ## Cube arrays come in row-col-etc. order, not column-major.
        ## Keep the labels right here, then aperm the array back to order
        dimnames[1:2] <- dimnames[c(2,1)]
    }
    out <- array(d, dim=vapply(dimnames, length, integer(1)),
        dimnames=dimnames)
    if (ndims > 1) {
        ap <- seq_len(ndims)
        ap[1:2] <- 2:1
        out <- aperm(out, ap)
        names(dimnames(out))[1:2] <- names(dimnames(out))[2:1]
        marginals <- lapply(x$result$margin[as.character(seq_len(ndims) - 1)],
            unlist)
        ## TODO: make marginals from the cube, not the server's
    } else {
        ## Kind of a hack. We just want to know whether there are 0 values
        ## for pruning
        marginals <- list(as.vector(out))
    }
    
    ## Evaluate which to drop
    keep.these <- mapply(pruneDimension, dimension=x$result$dimensions, 
        marginal=marginals, 
        MoreArgs=list(useNA=x@useNA),
        SIMPLIFY=FALSE,
        USE.NAMES=FALSE)
    names(keep.these) <- names(dimnames(out))
    out <- do.call("[", c(list(x=out, drop=FALSE), keep.these))
    return(out)
}

cubeDimnames <- function (cube) {
    ## Grab the row/col/etc. labels from the cube
    dimnames <- lapply(cube$result$dimensions, function (a) {
        cats <- a$type$categories %||% a$type$elements
        ## If enumerated, will be "elements", not "categories"
        vapply(cats, elementName, character(1))
    })
    names(dimnames) <- vapply(cube$result$dimensions, 
        function (a) a$references$alias, character(1))
    return(dimnames)
}

elementName <- function (el) {
    ## Given a category or element in a cube dim, what's its name?
    out <- el$value
    if (is.null(out)) {
        ## This is probably categorical. Try "name" instead of "value".
        out <- el$name
    } else if (is.list(out)) {
        if (length(out) == 2 && is.null(names(out))) {
            ## el$value is bin boundaries, as in a binned numeric.
            out <- paste(unlist(out), collapse="-")
        } else {
            ## This is probably a subvariable. Look for its name.
            out <- out$references$name
        }
    }
    if (is.null(out)) {
        ## Damn. You may be here because you're hitting missing values in an
        ## array or multiple response, or the __any__ or __none__ values. 
        ## Bail out.
        out <- "<NA>"
    }
    return(as.character(out))
}

elementIsAnyOrNone <- function (el) {
    is.list(el$value) && ## Element has $value and value is a list
        "id" %in% names(el$value) && ## "value" has names (is not bin)
        el$value$id %in% c("__any__", "__none__") 
}

pruneDimension <- function (dimension, marginal, useNA) {
    ## Returns logicals of which rows/cols/etc. should be kept
    
    cats <- dimension$type$categories %||% dimension$type$elements
    ## Always drop __any__ and __none__
    out <- vapply(cats, Negate(elementIsAnyOrNone), logical(1))
    
    if (useNA != "always") {
        ## Means drop missing always, or only keep if there are any
        valid.cats <- !vapply(cats, function (x) isTRUE(x$missing), logical(1))
        if (useNA == "ifany") {
            valid.cats <- valid.cats | vapply(marginal, length, integer(1)) > 0
        }
        ## But still drop __any__ or __none__
        out <- valid.cats & out
    }

    return(out) 
}

