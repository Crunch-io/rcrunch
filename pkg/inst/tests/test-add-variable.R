context("Add a variable to a dataset")

test_that("toVariable parses R data types", {
    expect_identical(toVariable(2L:4L, name="Numbers!", alias="num"),
        list(values=2L:4L, type="numeric", name="Numbers!", alias="num"))
    expect_identical(toVariable(letters[1:3]),
        list(values=c("a", "b", "c"), type="text"))
    expect_equivalent(toVariable(as.factor(rep(LETTERS[2:3], 3))), 
        list(values=rep(1:2, 3), type="categorical", categories=list(
            list(id=1L, name="B", numeric_value=1L, missing=FALSE),
            list(id=2L, name="C", numeric_value=2L, missing=FALSE)
        ))) ## unclear why these aren't identical
    options(crunch.max.categories=4)
    expect_identical(getOption("crunch.max.categories"), 4)
    expect_identical(toVariable(as.factor(letters[1:5])), 
        list(values=c("a", "b", "c", "d", "e"), type="text"))
    expect_identical(toVariable(as.factor(letters[1:5]), name="v1"), 
        list(values=c("a", "b", "c", "d", "e"), type="text", name="v1"))
    options(crunch.max.categories=256)
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            testdf <- .setup
            test_that("addVariable creates a new remote numeric variable", {
                testdf <- addVariable(testdf, df$v3, name="New var", alias="newVar")
                expect_true("newVar" %in% names(testdf))
                nv <- testdf$newVar
                expect_true(is.Numeric(nv))
                expect_true(is.Numeric(testdf[['v3']]))
                expect_identical(as.vector(nv), as.vector(testdf$v3))
            })
            test_that("addVariable creates text variables from character", {
                testdf <- addVariable(testdf, df$v2, name="New var 2", alias="newVar2")
                expect_true("newVar2" %in% names(testdf))
                nv <- testdf$newVar2
                expect_true(is.Text(nv))
                expect_identical(as.vector(nv)[1:15], as.vector(testdf$v2)[1:15])
                    ## note that NAs aren't getting caught in the CSV importer
                    ## anymore, but they're right in the addVariable method
            })
            test_that("addVariable creates categorical from factor", {
                testdf <- addVariable(testdf, df$v4, name="New var 3", alias="newVar3")
                expect_true("newVar3" %in% names(testdf))
                nv <- testdf$newVar3
                expect_true(is.Categorical(nv))
                expect_identical(as.vector(nv), as.vector(testdf$v4))
            })
            test_that("addVariable creates datetime from Date", {
                testdf <- addVariable(testdf, df$v5, name="New var 4", alias="newVar4")
                expect_true("newVar4" %in% names(testdf))
                nv <- testdf$newVar4
                expect_true(is.Datetime(nv))
                expect_identical(as.vector(nv), as.vector(testdf$v5))
            })
            skip(test_that("addVariable creates datetime from POSIXct", {
                testdf <- addVariable(testdf, as.POSIXct(df$v5),
                    name="New var 5", alias="newVar5")
                expect_true("newVar5" %in% names(testdf))
                nv <- testdf$newVar5
                expect_true(is.Datetime(nv))
                expect_identical(as.vector(nv), as.vector(testdf$v5))
            }), reason="Can't support POSIXt until the app supports timezones")
            test_that("adding variable with duplicate name fails", {
                expect_error(addVariable(testdf, df$v5, name="New var 4", alias="newVar4"), "Variable with name: New var 4 already exists")
            })
        })
        
        with(test.dataset(df), {
            testdf <- .setup
            test_that("[[<- and $<- cannot overwrite existing variables", {
                expect_error(testdf[["v2"]] <- 1:20, 
                    "Cannot currently overwrite existing Variables")
                expect_error(testdf$v2 <- 1:20, 
                    "Cannot currently overwrite existing Variables")
                expect_error(testdf[[2]] <- 1:20, 
                    "Only character \\(name\\) indexing supported")
            })
            test_that("[[<- adds variables", {
                testdf$newvariable <- 20:1
                expect_true(is.Numeric(testdf$newvariable))
                expect_identical(mean(testdf$newvariable), 10.5)
            })
            test_that("Variable lengths must match, in an R way", {
                expect_error(testdf[['not valid']] <- 1:7, 
                    "replacement has 7 rows, data has 20")
                testdf[['ok']] <- 1
                expect_identical(as.vector(testdf$ok), rep(1, 20))
            })
        })
        
        ca.var <- list(
            name="Categorical array",
            alias="categoricalArray",
            description="Here are some variables. They go together.",
            type="categorical_array",
            subvariables=lapply(names(mrdf)[1:3],
                function (x) toVariable(as.factor(mrdf[[x]]), name=x))
        )
        test_that("addVariables that are categorical_array", {
            with(test.dataset(), {
                ds <- .setup
                POSTNewVariable(ds@urls$variables_url, ca.var,
                    bind_url=ds@urls$bind_url)
                ds <- refresh(ds)
                expect_true(is.CA(ds$categoricalArray))
                expect_identical(description(ds$categoricalArray), 
                    "Here are some variables. They go together.")
            })
        })
        test_that("adding an array cleans up after self if one subvar errors", {
            with(test.dataset(), {
                c2 <- ca.var
                c2$subvariables[[4]] <- list(this="is", not="a", valid="variable")
                ds <- .setup
                nvars.before <- ncol(ds)
                expect_identical(nvars.before, 0L)
                expect_error(POSTNewVariable(ds@urls$variables_url, c2,
                    bind_url=ds@urls$bind_url), 
                    "Subvariables errored on upload")
                ds <- refresh(ds)
                skip(expect_identical(ncol(ds), nvars.before), "DELETE still leaves variables in hierarchical order")
            })
        })
        test_that("addVariables that are multiple_response", {
            with(test.dataset(), {
                ds <- .setup
                newvar <- list(
                    name="Multiple response",
                    alias="multipleResponse",
                    description="Here are some variables. They go together.",
                    type="multiple_response",
                    subvariables=lapply(names(mrdf)[1:3],
                        function (x) toVariable(as.factor(mrdf[[x]]), name=x))
                )
                newvar$subvariables <- lapply(newvar$subvariables, function (x) {
                    x$categories[[1]]$selected <- TRUE
                    return(x)
                })
                POSTNewVariable(ds@urls$variables_url, newvar,
                    bind_url=ds@urls$bind_url)
                ds <- refresh(ds)
                expect_true(is.MR(ds$multipleResponse))
            })
        })
    })
}