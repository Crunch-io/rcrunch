context("Dataset object and methods") 


with(fake.HTTP, {
    session_store$datasets <- DatasetCatalog(GET("api/datasets.json"))
    test.ds <- loadDataset("test ds")
    # test.ds <- as.dataset(GET("api/datasets/dataset1.json"))
    
    test_that("Dataset can be created", {
        expect_true(is.dataset(test.ds))
    })
    
    test_that("Dataset VariableCatalog index is sorted", {
        expect_identical(urls(test.ds@variables), 
            c("api/datasets/dataset1/variables/birthyr.json",
            "api/datasets/dataset1/variables/gender.json",
            "api/datasets/dataset1/variables/mymrset.json",
            "api/datasets/dataset1/variables/starttime.json",
            "api/datasets/dataset1/variables/textVar.json"))
    })

    test_that("findVariables", {
        expect_identical(findVariables(test.ds, pattern="^gend", key="alias"), 2L)
        expect_identical(findVariables(test.ds, pattern="^bir", key="alias", value=TRUE), "birthyr")
    })

    test_that("useAlias exists and affects names()", {
        thisds <- test.ds
        expect_true(thisds@useAlias)
        expect_identical(names(thisds), 
            findVariables(thisds, key="alias", value=TRUE))
        thisds@useAlias <- FALSE
        expect_false(thisds@useAlias)
        expect_identical(names(thisds), 
            findVariables(thisds, key="name", value=TRUE))
    })

    test_that("useAlias is an argument to as.dataset", {
        expect_equal(as.dataset(GET("api/datasets/dataset1.json"),
            tuple=datasetCatalog()[["api/datasets/dataset1.json"]])@useAlias,
            default.useAlias())
        expect_false(as.dataset(GET("api/datasets/dataset1.json"),
            tuple=datasetCatalog()[["api/datasets/dataset1.json"]],
            useAlias=FALSE)@useAlias)
    })

    test_that("Dataset has names() and extract methods work", {
        expect_false(is.null(names(test.ds)))
        expect_identical(names(test.ds), c("birthyr", "gender", "mymrset", "textVar", "starttime"))
        expect_true(is.variable(test.ds[[1]]))
        expect_true("birthyr" %in% names(test.ds))
        expect_true(is.variable(test.ds$birthyr))
        expect_true(is.dataset(test.ds[1]))
        expect_true(is.dataset(test.ds["birthyr"]))
        expect_true(is.dataset(test.ds[names(test.ds)=="birthyr"]))
        expect_identical(names(test.ds[2]), c("gender"))
        expect_identical(test.ds$not.a.var.name, NULL)
        expect_error(test.ds[[999]], "subscript out of bounds")
    })

    test_that("Read only flag gets set appropriately", {
        expect_false(is.readonly(test.ds))
        expect_true(is.readonly(test.ds[1]))
    })

    test_that("Name and description setters in read only mode", {
        dataset <- test.ds
        readonly(dataset) <- TRUE
        name(dataset) <- "Bond. James Bond."
        expect_false(identical(name(dataset), name(test.ds)))
        expect_identical(name(dataset), "Bond. James Bond.")
        description(dataset) <- "007"
        expect_false(identical(description(dataset), description(test.ds)))
        expect_identical(description(dataset), "007")
    })
    
    test_that("show method", {
        expect_identical(describeDatasetVariables(test.ds), 
            c("$birthyr: Birth Year (numeric) \n",
            "$gender: Gender (categorical) \n",
            "$mymrset: mymrset (multiple_response) \n",
            "$textVar: Text variable ftw (text) \n",
            "$starttime: starttime (datetime) \n"     
        ))
    })
    
    test_that("dataset can refresh", {
        ds <- loadDataset("test ds")
        expect_identical(ds, refresh(ds))
    })
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df, "setter_test"), {
            test_that("Name and description setters push to server", {
                d2 <- setter_test <- .setup
                name(setter_test) <- "Bond. James Bond."
                expect_identical(name(refresh(d2)), name(setter_test))
            })
        })
        test_that("Name and description setters don't push to server if readonly", {

        })
        
        with(test.dataset(df), {
            testdf <- .setup
            test_that("dataset dim", {
                expect_identical(dim(testdf), dim(df))
                expect_identical(nrow(testdf), nrow(df))
                expect_identical(ncol(testdf), ncol(df))
            })
            
            test_that("refresh keeps useAlias setting", {
                expect_true(testdf@useAlias)
                expect_true(refresh(testdf)@useAlias)
                testdf@useAlias <- FALSE
                expect_false(testdf@useAlias)
                expect_false(refresh(testdf)@useAlias)
            })
            
            test_that("Dataset [[<-", {
                v1 <- testdf$v1
                name(v1) <- "Variable One"
                testdf$v1 <- v1
                expect_identical(variableNames(testdf)[1], "Variable One")
                expect_error(testdf$v2 <- v1, 
                    "Cannot overwrite one Variable")
            })
        })

        with(test.dataset(mrdf), {
            testdf <- .setup
            test_that("Dataset [<-", {
                cast.these <- grep("mr_", names(testdf))
                expect_true(all(vapply(active(testdf@variables)[cast.these], 
                    function (x) x$type == "numeric", logical(1))))
                expect_true(all(vapply(testdf[cast.these], 
                    function (x) is.Numeric(x), logical(1))))
                testdf[cast.these] <- lapply(testdf[cast.these],
                    castVariable, "categorical")
                expect_true(all(vapply(active(testdf@variables)[cast.these], 
                    function (x) x$type == "categorical", logical(1))))
                expect_true(all(vapply(testdf[cast.these], 
                    function (x) is.Categorical(x), logical(1))))
            })
        })
    })
}