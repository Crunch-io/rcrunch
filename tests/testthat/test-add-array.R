context("Add an array variable to a dataset")

test_that("POSTNewVariable rejects invalid categories", {
    expect_error(POSTNewVariable("",
        list(type="categorical", name="bad names",
            categories=list(
                list(id=1L, name="Name 1", numeric_value=1L, missing=FALSE),
                list(id=2L, name="Name 1", numeric_value=2L, missing=FALSE),
                list(id=-1L, name="No Data", numeric_value=NULL, missing=TRUE)
            ))),
        "Invalid category names: must be unique")
})

test_that("POSTNewVariable rejects invalid categories in subvariable", {
    expect_error(POSTNewVariable("",
        list(type="categorical_array", name="bad names",
            subvariables=list(list(
                values=1,
                categories=list(
                    list(id=1L, name="Name 1", numeric_value=1L, missing=FALSE),
                    list(id=2L, name="Name 1", numeric_value=2L, missing=FALSE),
                    list(id=-1L, name="No Data", numeric_value=NULL, missing=TRUE)
                )))
            )),
        "Invalid category names: must be unique")
})

test_that("POSTNewVariable validates that arrays have subvariables", {
    expect_error(POSTNewVariable("", list(type="categorical_array")),
        "Cannot create array variable without specifying subvariables")
})

with_test_authentication({
    ca.values <- mrdf[c(2,1,3)]
    ca.values[] <- lapply(ca.values, as.factor)
    ca.var <- list(
        name="Categorical array",
        alias="categoricalArray",
        description="Here are some variables. They go together.",
        type="categorical_array",
        subvariables=lapply(names(ca.values),
            function (x) toVariable(ca.values[[x]], name=x, alias=x))
    )
    test_that("addVariables that are categorical_array over subvariable defs", {
        with(test.dataset(), {
            POSTNewVariable(variableCatalogURL(ds), ca.var)
            ds <- refresh(ds)
            expect_true(is.CA(ds$categoricalArray))
            expect_identical(description(ds$categoricalArray),
                "Here are some variables. They go together.")
            expect_identical(as.vector(ds$categoricalArray), ca.values)
            expect_identical(names(subvariables(ds$categoricalArray)),
                c("mr_2", "mr_1", "mr_3"))
        })
    })
    test_that("Adding an array as a single definition", {
        c2 <- ca.var
        c2$categories <- c2$subvariables[[1]]$categories
        c2$values <- matrix(unlist(lapply(c2$subvariables,
            vget("values"))), ncol=3, nrow=4, byrow=FALSE)
        c2$subvariables <- lapply(c2$subvariables, function (x) {
            x[!(names(x) %in% c("type", "categories", "values"))]
        })
        with(test.dataset(), {
            try(POSTNewVariable(variableCatalogURL(ds), c2))
            ds <- refresh(ds)
            expect_true(is.CA(ds$categoricalArray))
            expect_identical(description(ds$categoricalArray),
                "Here are some variables. They go together.")
            expect_identical(as.vector(ds$categoricalArray), ca.values)
        })
    })
    test_that("addVariables that are multiple_response", {
        with(test.dataset(), {
            newvar <- ca.var
            newvar$type <- "multiple_response"
            newvar$alias <- "multipleResponse"
            newvar$subvariables <- lapply(newvar$subvariables, function (x) {
                x$categories[[1]]$selected <- TRUE
                return(x)
            })
            class(newvar) <- "VariableDefinition"
            ds <- addVariables(ds, newvar)
            expect_true(is.MR(ds$multipleResponse))
        })
    })
})
