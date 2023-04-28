formulaToCubeQuery <- function(formula, data) {
    query <- formulaToQuery(formula, data)
    ## The formulaToQuery part is shared with newMultitable.
    ## What follows is needed to prepare for a cube query
    if (length(query$dimensions) > 0) { # leave empty dimensions present in query
        query$dimensions <- unlist(query$dimensions, recursive = FALSE)
        names(query$dimensions) <- NULL
    }
    return(query)
}

formulaToQuery <- function(formula, data) {
    formula <- try(as.formula(formula), silent = TRUE)
    if (is.error(formula)) {
        halt(dQuote("formula"), " is not a valid formula")
    }

    parsed_terms <- parseTerms(formula, data)
    vars <- parsed_terms$RHS
    measures <- parsed_terms$LHS

    ## Construct the "measures", either from the formula or default "count"
    if (!length(measures)) {
        # if measures is an empty list, there are none.
        measures <- list(zfunc("cube_count"))
    } else {
        ## Look for multiple measures, as passed by `list(f(x), g(x) ~ a + b)`
        if (startsWith(as.character(formula)[2], "list(")) {
            measures <- unlist(measures, recursive = FALSE)
        }
        measures <- lapply(measures, zcl)
    }

    badmeasures <- vapply(measures, Negate(isCubeAggregation), logical(1))
    if (any(badmeasures)) {
        halt("Left side of formula must be a valid aggregation")
    }

    ## Name measures based on function (and append to end if already named)
    if (is.null(names(measures))) {
        names(measures) <- getCubeMeasureNames(measures)
    } else {
        names(measures) <- paste0(names(measures), "__", getCubeMeasureNames(measures))
    }

    ## Special case for scorecard queries
    if (any(is_scorecard(vars))) {
        return(scorecardQuery(vars, measures))
    }

    ## Make "dimensions".
    dimensions <- lapply(vars, varToDim)

    baddimensions <- vapply(
        unlist(dimensions, recursive = FALSE),
        isCubeAggregation, logical(1)
    )
    if (any(baddimensions)) {
        halt("Right side of formula cannot contain aggregation functions")
    }

    return(list(dimensions = dimensions, measures = measures))
}

parseTerms <- function(formula, data) {
    terms <- terms(formula, allowDotAsName = TRUE)
    f.vars <- attr(terms, "variables")
    all.f.vars <- all.vars(f.vars)

    ## More input validation
    if ("." %in% all.f.vars) {
        halt("Crunch formulae do not support ", dQuote("."), " in formula")
    }
    if (!length(all.f.vars)) {
        halt("Must supply one or more variables")
    }

    ## Find variables either in 'data' or in the calling environment
    ## Evaluate the formula's terms in order to catch derived expressions
    v.call <- do.call(
        substitute,
        list(expr = f.vars, env = registerCubeFunctions(all.f.vars))
    )
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
        halt(
            "Invalid cube dimension", ifelse(sum(nullvars) > 1, "s: ", ": "),
            serialPaste(varexprs[nullvars]), " cannot be NULL"
        )
    }

    resp <- attr(terms, "response")
    if (resp > 0) {
        # remove response vars only if there are any
        RHS <- vars[-resp]
    } else {
        RHS <- vars
    }
    LHS <- vars[resp]

    return(list(LHS = LHS, RHS = RHS))
}

registerCubeFunctions <- function(varnames = c()) {
    ## Return a list of "cube functions" to substitute()
    ## in. A better approach, which would avoid potential name collisions, would
    ## probably be to have vars be an environment inside of another environment
    ## that has the cube functions. This version just checks for name collisions
    ## and errors if there is one.

    numfunc <- function(func, ...) {
        force(func)
        moreArgs <- list(...)
        return(function(x) {
            if (is.Categorical(x)) {
                ## "Cast" it on the fly
                x <- list(zfunc("cast", x, "numeric"))
            }
            do.call("zfunc", c(func, x, moreArgs))
            # zfunc(func, x)
        })
    }

    funcs <- list(
        mean = numfunc("cube_mean"),
        min = numfunc("cube_min"),
        max = numfunc("cube_max"),
        sd = numfunc("cube_stddev"),
        sum = numfunc("cube_sum"),
        median = numfunc("cube_quantile", list(value = I(.5))),
        as_array = function(x) {
            ## Kinda hacky way to do a query of an MR as CA
            if (!(is.MR(x) | is.CrunchExpr(x))) {
                halt(
                    "Cannot analyze a variable of type ", dQuote(type(x)),
                    " 'as_array'"
                )
            }
            zfunc("as_array", x)
        },
        # Preserved for backwards compatibility here, but we don't allow changing
        # the default behavior of MRs anymore so this special case isn't needed
        # except for the error message
        as_selected = function(x) {
            if (!(is.MR(x) | is.CrunchExpr(x))) {
                halt(
                    "Cannot analyze a variable of type ", dQuote(type(x)),
                    " 'as_selected'"
                )
            }
            x
        },
        subvariables = function(x) {
            if (!(is.Array(x) | is.CrunchExpr(x))) {
                halt(
                    "Cannot analyze a variable of type ", dQuote(type(x)),
                    " using 'subvariables'"
                )
            }
            zfunc("dimension", x, "subvariables")
        },
        categories = function(x) {
            if (!(is.Array(x) | is.CrunchExpr(x))) {
                halt(
                    "Cannot analyze a variable of type ", dQuote(type(x)),
                    " using 'categories'"
                )
            }
            zcl(x)
        },
        n = function(...) zfunc("cube_count"),
        scorecard = function(..., vars = NULL) {
            vars <- c(list(...), vars)
            if (!all(vapply(vars, function(x) is.MR(x) | is.CrunchExpr(x), logical(1)))) {
                halt("Expected Multiple Response variables or expressions in a scorecard")
            }
            vars <- lapply(vars, function(x) {
                if (is.MR(x)) zfunc("as_selected", x) else zcl(x)
            })

            do.call(zfunc, c(list(func = "scorecard"), vars))
        }
    )

    overlap <- intersect(varnames, names(funcs))
    if (length(overlap)) {
        halt(
            "Cannot evaluate a cube with reserved ",
            pluralize("name", length(overlap)), ": ",
            serialPaste(dQuote(overlap))
        )
    }
    return(funcs)
}

getCubeMeasureNames <- function(measures) {
    vapply(measures, function(m) {
        sub("^cube_", "", m[["function"]])
    }, character(1))
}

isCubeAggregation <- function(x) {
    length(names(x)) == 2L &&
        setequal(names(x), c("function", "args")) &&
        grepl("^cube_", x[["function"]])
}

is.zfunc <- function(x, func) {
    out <- is.list(x) && "function" %in% names(x)
    if (out && !missing(func)) {
        out <- x[["function"]] == func
    }
    return(out)
}

varToDim <- function(x) {
    ## Given variable, construct the appropriate ZCL to get a cube with them
    ## as dimensions
    v <- zcl(x)
    if (is.MR(x)) {
        ## Multiple response gets "as_selected" by default and subvars dimension
        return(list(
            zfunc("dimension", zfunc("as_selected", v), list(value = "subvariables")),
            zfunc("as_selected", v)
        ))
    } else if ("function" %in% names(v) && identical(v[["function"]], "as_selected")) {
        # If user created an as_selected (eg with `asSelected` or `selectCategories()`) expand
        # out the dimensions for them like it's a MR
        return(list(
            zfunc("dimension", v, list(value = "subvariables")),
            v
        ))
    }    else if (is.CA(x) | is.NumericArray(x)) {
        ## Categorical array gets the subvariables dimension first
        ## and then itself so that the rows, not columns, are subvars
        ## We treat numeric arrays as categoricals when used bare
        ## in formulas like this too so that eg `table(ds$numarray)`
        ## matches expectations.
        return(list(
            zfunc("dimension", x, list(value = "subvariables")),
            v
        ))
    } else if (is.zfunc(x, "as_array")) {
        ## Pseudo-ZCL from registerCubeFunctions, used to treat an MR like a CA
        ## x is thus list(`function`="as_array", args=list(list(variable=self)))
        ## Return instead subvar dimension + variable
        return(list(
            zfunc("dimension", x$args[[1]]["variable"], list(value = "subvariables")),
            x$args[[1]]["variable"]
        ))
    } else if (is.CrunchExpr(x)) {
        ## Give a name and alias "references"
        ref <- formatExpression(x)
        v$references <- list(name = ref, alias = ref)
        return(list(v))
    } else {
        ## Just the var ref, but nest in a list so we can unlist to flatten
        return(list(v))
    }
}

evalLHS <- function(formula, data) {
    evalSide(formula[[2]], data, environment(formula))
}

evalRHS <- function(formula, data) {
    evalSide(formula[[3]], data, environment(formula))
}

evalSide <- function(formula_part, data, eval_env) {
    if (missing(data) || is.null(data)) {
        return(eval(formula_part, NULL, eval_env))
    } else {
        return(eval(formula_part, as.environment(data), eval_env))
    }
}

RHS_string <- function(f) {
    ## Return a string representation of the right-hand side of a formula
    ## (the stuff to the right of the ~)
    sub("^ +", "", tail(splitFormula(f), 1))
}

LHS_string <- function(f) {
    ## Return a string representation of the left-hand side of a formula
    ## (the stuff to the left of the ~)
    sub(" +$", "", head(splitFormula(f), 1))
}

splitFormula <- function(f) {
    if (!is.character(f)) f <- paste(deparse(f), collapse = " ")
    unlist(strsplit(f, "~"))
}

is_scorecard <- function(vars) {
    vapply(
        vars,
        function(x) is.list(x) && identical(x[["function"]], "scorecard"), logical(1)
    )
}

scorecardQuery <- function(vars, measures) {
    if (length(vars) > 1) halt("scorecard queries cannot be combined with other dimensions")
    if (length(measures) > 1 || !identical(measures[[1]][["function"]], "cube_count")) {
        halt("scorecard queries can only have count as the aggregation")
    }

    return(list(
        measures = measures,
        with = list(v1 = zfunc("fuse", vars[[1]]$args)),
        dimensions = list(list(
            list(local = "v1.subvariables"),
            list(local = "v1.categories"),
            list(local = "v1.variables")
        ))
    ))
}
