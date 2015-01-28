crCube <- function (dimensions, measures, data) {
    var_catalog <- allVariables(data)
    var_urls <- urls(var_catalog)[match(dimensions, aliases(var_catalog))]
    stopifnot(all(!is.na(var_urls)))
    dimensions <- lapply(var_urls, function (i) {
        list(variable=i)  ## This should be a zcl method
        ## Note also that different types require different params
    })
    names(dimensions) <- NULL
    query <- list(dimensions=dimensions,
        measures=list(count=rcrunch:::zfunc("cube_count")),
        weight=NULL) ## Weight should be an argument
    cube_url <- shojiURL(data, "views", "cube")
    crGET(cube_url, query=list(query=toJSON(query)))
}

cubeToArray <- function (x, measure="count") {
    d <- unlist(x$result$measures[[measure]]$data)
    d <- round(d) ## digits should be an argument
    dimnames <- lapply(x$result$dimensions, function (a) {
        cats <- a$type$categories ## If enumerated, won't be categories
        vapply(cats, function (ct) ct$name, character(1))
    })
    names(dimnames) <- vapply(x$result$dimensions, 
        function (a) a$references$name, character(1))
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
    }
    return(out)
}

getCube <- function (formula, dataset) {
    f <- terms(formula)
    f.vars <- attr(f, "variables")
    all.f.vars <- all.vars(f.vars)
    vars <- lapply(all.vars(f.vars), function (x) dataset[[x]])
    names(vars) <- all.f.vars
    v.call <- do.call(substitute, list(expr=f.vars, env=vars))
    vars <- eval(v.call)
    
    resp <- attr(f, "response")
    if (resp) {
        measures <- lapply(vars[resp], absolute.zcl)
        vars <- vars[-resp]
    } else {
        measures <- list(count=zfunc("cube_count"))
    }
    dimensions <- lapply(vars, absolute.zcl)
    names(dimensions) <- NULL
    
    query <- list(dimensions=dimensions,
        measures=measures,
        weight=NULL) ## Weight should be an argument
    cube_url <- shojiURL(dataset, "views", "cube")
    return(CrunchCube(crGET(cube_url, query=list(query=toJSON(query)))))
}
