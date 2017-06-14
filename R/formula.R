formulaToCubeQuery <- function (formula, data) {
    query <- formulaToQuery(formula, data)
    ## The formulaToQuery part is shared with newMultitable.
    ## What follows is needed to prepare for a cube query
    query$dimensions <- unlist(query$dimensions, recursive=FALSE)
    names(query$dimensions) <- NULL
    return(query)
}

formulaToQuery <- function (formula, data) {
    formula <- try(as.formula(formula), silent=TRUE)
    if (is.error(formula)) {
        halt(dQuote("formula"), " is not a valid formula")
    }

    ## Parse the formula
    f <- terms(formula, allowDotAsName=TRUE) ## To catch "."
    f.vars <- attr(f, "variables")
    all.f.vars <- all.vars(f.vars)

    ## More input validation
    if ("." %in% all.f.vars) {
        halt("crtabs does not support ", dQuote("."), " in formula")
    }
    if (!length(all.f.vars)) {
        halt("Must supply one or more variables")
    }

    ## Find variables either in 'data' or in the calling environment
    ## Evaluate the formula's terms in order to catch derived expressions
    v.call <- do.call(substitute,
        list(expr=f.vars, env=registerCubeFunctions(all.f.vars)))
    if (missing(data)) {
        vars <- eval(v.call, NULL, environment(formula))
    } else {
        vars <- eval(v.call, as.environment(data), environment(formula))
    }

    ## Validate that vars are non-null
    nullvars <- vapply(vars, is.null, logical(1))
    if (any(nullvars)) {
        ## Get the NULL expressions.
        ## Note the off-by-one problem:
        ## If f.vars == language list(CA$mr_1, CA$NOTAVAR),
        ## as.character(f.vars) == [1] "list"       "CA$mr_1"    "CA$NOTAVAR"
        varexprs <- as.character(f.vars)[-1]
        halt("Invalid cube dimension", ifelse(sum(nullvars) > 1, "s: ", ": "),
            serialPaste(varexprs[nullvars]), " cannot be NULL")
    }

    ## Construct the "measures", either from the formula or default "count"
    resp <- attr(f, "response")
    if (resp) {
        ## Pop them off
        measures <- vars[resp]
        vars <- vars[-resp]
        ## Look for multiple measures, as passed by `list(f(x), g(x) ~ a + b)`
        if (startsWith(as.character(f.vars[resp + 1]), "list(")) {
            measures <- unlist(measures, recursive=FALSE)
        }
        measures <- lapply(measures, zcl)
    } else {
        measures <- list(count=zfunc("cube_count"))
    }

    ## Make "dimensions".
    dimensions <- lapply(vars, varToDim)

    ## Final validations
    badmeasures <- vapply(measures, Negate(isCubeAggregation), logical(1))
    if (any(badmeasures)) {
        halt("Left side of formula must be a valid aggregation")
    }
    baddimensions <- vapply(unlist(dimensions, recursive=FALSE),
        isCubeAggregation, logical(1))
    if (any(baddimensions)) {
        halt("Right side of formula cannot contain aggregation functions")
    }

    ## One last munge
    if (is.null(names(measures))) {
        names(measures) <- vapply(measures, function (m) {
            sub("^cube_", "", m[["function"]])
        }, character(1))
    }

    return(list(dimensions=dimensions, measures=measures))
}

registerCubeFunctions <- function (varnames=c()) {
    ## Return a list of "cube functions" to substitute()
    ## in. A better approach, which would avoid potential name collisions, would
    ## probably be to have vars be an environment inside of another environment
    ## that has the cube functions. This version just checks for name collisions
    ## and errors if there is one.

    numfunc <- function (func, ...) {
        force(func)
        moreArgs <- list(...)
        return(function (x) {
            if (is.Categorical(x)) {
                ## "Cast" it on the fly
                x <- list(zfunc("cast", x, "numeric"))
            }
            do.call("zfunc", c(func, x, moreArgs))
            # zfunc(func, x)
        })
    }

    funcs <- list(
        mean=numfunc("cube_mean"),
        min=numfunc("cube_min"),
        max=numfunc("cube_max"),
        sd=numfunc("cube_stddev"),
        sum=numfunc("cube_sum"),
        median=numfunc("cube_quantile", list(value=I(.5))),
        as_array=function (x) {
            ## Kinda hacky way to do a query of an MR as CA
            if (!is.MR(x)) {
                halt("Cannot analyze a variable of type ", dQuote(type(x)),
                    " 'as_array'")
            }
            zfunc("as_array", x)
        },
        as_selected=function (x) {
            ## Another hacky solution
            if (!is.MR(x)) {
                halt("Cannot analyze a variable of type ", dQuote(type(x)),
                    " 'as_selected'")
            }
            zfunc("as_selected", x)
        },
        n=function (...) zfunc("cube_count")
    )

    overlap <- intersect(varnames, names(funcs))
    if (length(overlap)) {
        halt("Cannot evaluate a cube with reserved name",
            ifelse(length(overlap) > 1, "s", ""), ": ",
            serialPaste(dQuote(overlap)))
    }
    return(funcs)
}

isCubeAggregation <- function (x) {
    length(names(x)) == 2L &&
        setequal(names(x), c("function", "args")) &&
        grepl("^cube_", x[["function"]])
}

is.zfunc <- function (x, func) {
    out <- is.list(x) && "function" %in% names(x)
    if (out && !missing(func)) {
        out <- x[["function"]] == func
    }
    return(out)
}

varToDim <- function (x) {
    ## Given variable, construct the appropriate ZCL to get a cube with them
    ## as dimensions
    v <- zcl(x)
    if (is.MR(x)) {
        ## Multiple response gets "selected_array" and "each"
        return(list(zfunc("selected_array", v),
            list(each=self(x))))
    } else if (is.CA(x)) {
        ## Categorical array gets the var reference and "each"
        ## Put "each" first so that the rows, not columns, are subvars
        return(list(list(each=self(x)),
            v))
    } else if (is.zfunc(x, "as_array")) {
        ## Pseudo-ZCL from registerCubeFunctions, used to treat an MR like a CA
        ## x is thus list(`function`="as_array", args=list(list(variable=self)))
        ## Return instead list(list(each=self), list(variable=self))
        return(list(list(each=x$args[[1]]$variable), x$args[[1]]))
    } else if (is.zfunc(x, "as_selected")) {
        ## Pseudo-ZCL from registerCubeFunctions, used to compute MR by subvar
        ## x is thus list(`function`="as_selected", args=list(list(variable=self)))
        return(list(list(each=x$args[[1]]$variable), zfunc("as_selected", x$args[[1]])))
    } else {
        ## Just the var ref, but nest in a list so we can unlist to flatten
        return(list(v))
    }
}

formulaRHS <- function (f) {
    ## Return a string representation of the right-hand side of a formula
    ## (the stuff to the right of the ~)
    if (!is.character(f)) f <- paste(deparse(f), collapse=" ")
    sub("^ +", "", tail(unlist(strsplit(f, "~")), 1))
}
