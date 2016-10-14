#' Crunch xtabs: Crosstab and otherwise aggregate variables in a Crunch Dataset
#'
#' Create a contingency table or other aggregation from cross-classifying
#' variables in a CrunchDataset.
#'
#' @param formula an object of class 'formula' object with the
#' cross-classifying variables, separated by '+', on the right hand side.
#' Compare to \code{\link[stats]{xtabs}}.
#' @param data an object of class \code{CrunchDataset}
#' @param weight a CrunchVariable that has been designated as a potential
#' weight variable for \code{data}, or \code{NULL} for unweighted results.
#' Default is the currently applied weight, \code{\link{weight}(data)}.
#' @param useNA whether to include missing values in tabular results. See
#' \code{\link[base]{table}}.
#' @return an object of class \code{CrunchCube}
#' @importFrom stats as.formula terms
#' @export
crtabs <- function (formula, data, weight=crunch::weight(data),
                     useNA=c("no", "ifany", "always")) {
    ## Validate "formula"
    if (missing(formula)) {
        halt("Must provide a formula")
    }
    formula <- try(as.formula(formula), silent=TRUE)
    if (is.error(formula)) {
        halt(dQuote("formula"), " is not a valid formula")
    }

    ## Parse the formula
    f <- terms(as.formula(formula), allowDotAsName=TRUE) ## To catch "."
    f.vars <- attr(f, "variables")
    all.f.vars <- all.vars(f.vars)

    ## More input validation
    if ("." %in% all.f.vars) {
        halt("crtabs does not support ", dQuote("."), " in formula")
    }
    if (!length(all.f.vars)) {
        halt("Must supply one or more variables")
    }

    if (missing(data) || !is.dataset(data)) {
        halt(dQuote("data"), " must be a Dataset")
    }

    ## Find variables either in 'data' or in the calling environment
    ## Evaluate the formula's terms in order to catch derived expressions
    v.call <- do.call(substitute,
        list(expr=f.vars, env=registerCubeFunctions(all.f.vars)))
    where <- environment(formula) #parent.frame()
    vars <- eval(v.call, as.environment(data), environment(formula))

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
        measures <- lapply(vars[resp], zcl)
        vars <- vars[-resp]
    } else {
        measures <- list(count=zfunc("cube_count"))
    }

    ## Handle "weight"
    force(weight)
    if (is.variable(weight)) {
        weight <- self(weight)
        ## Should confirm that weight is in weight_variables. Server 400s
        ## if it isn't.
    } else {
        weight <- NULL
    }

    ## Construct the ZCL query
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

    ## One last munge
    names(query$measures) <- vapply(query$measures, function (m) {
        sub("^cube_", "", m[["function"]])
    }, character(1))
    ## Get filter
    f <- zcl(activeFilter(data))

    ## Convert to query params
    query <- list(
        query=toJSON(query),
        filter=toJSON(f)
    )

    ## Go GET it!
    return(CrunchCube(crGET(cubeURL(data), query=query),
        useNA=match.arg(useNA)))
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
        }
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
    "function" %in% names(x) && grepl("^cube_", x[["function"]])
}

varsToCubeDimensions <- function (vars) {
    ## Given variables, construct the appropriate ZCL to get a cube with them
    ## as dimensions
    dimensions <- unlist(lapply(vars, function (x) {
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
        } else if (is.list(x) && "function" %in% names(x) && x[["function"]] == "as_array") {
            ## Pseudo-ZCL from registerCubeFunctions, used to treat an MR like a CA
            ## x is thus list(`function`="as_array", args=list(list(variable=self)))
            ## Return instead list(list(each=self), list(variable=self))
            return(list(list(each=x$args[[1]]$variable), x$args[[1]]))
        } else {
            ## Just the var ref, but nest in a list so we can unlist below to
            ## flatten
            return(list(v))
        }
    }), recursive=FALSE)
    names(dimensions) <- NULL
    return(dimensions)
}
