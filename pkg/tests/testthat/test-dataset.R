context("Dataset object and methods") 

with(fake.HTTP, {
    test.ds <- loadDataset("test ds")
    
    test_that("Dataset can be created", {
        expect_true(is.dataset(test.ds))
    })
    
    test_that("Dataset VariableCatalog index is sorted", {
        expect_identical(urls(allVariables(test.ds)), 
            c("api/datasets/dataset1/variables/birthyr.json",
            "api/datasets/dataset1/variables/gender.json",
            "api/datasets/dataset1/variables/mymrset.json",
            "api/datasets/dataset1/variables/starttime.json",
            "api/datasets/dataset1/variables/subvar1.json",
            "api/datasets/dataset1/variables/subvar2.json",
            "api/datasets/dataset1/variables/subvar3.json",
            "api/datasets/dataset1/variables/textVar.json"))
    })

    test_that("findVariables", {
        expect_identical(findVariables(test.ds, pattern="^gend", key="alias"),
            2L)
        expect_identical(findVariables(test.ds, pattern="^bir", key="alias",
            value=TRUE), "birthyr")
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

    skip(test_that("Name and description setters in read only mode", {
        dataset <- test.ds
        readonly(dataset) <- TRUE
        name(dataset) <- "Bond. James Bond."
        expect_false(identical(name(dataset), name(test.ds)))
        expect_identical(name(dataset), "Bond. James Bond.")
        description(dataset) <- "007"
        expect_false(identical(description(dataset), description(test.ds)))
        expect_identical(description(dataset), "007")
    }), "Readonly mode for tuple updating not implemented")
    
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

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("Name and description setters push to server", {
                d2 <- ds
                name(ds) <- "Bond. James Bond."
                expect_identical(name(refresh(d2)), name(ds))
            })
        })
        test_that("Name and description setters don't push to server if readonly", {

        })
        
        with(test.dataset(df), {
            test_that("dataset dim", {
                expect_identical(dim(ds), dim(df))
                expect_identical(nrow(ds), nrow(df))
                expect_identical(ncol(ds), ncol(df))
            })
            
            test_that("refresh keeps useAlias setting", {
                expect_true(ds@useAlias)
                expect_true(refresh(ds)@useAlias)
                ds@useAlias <- FALSE
                expect_false(ds@useAlias)
                expect_false(refresh(ds)@useAlias)
            })
            
            test_that("Dataset [[<-", {
                v1 <- ds$v1
                name(v1) <- "Variable One"
                ds$v1 <- v1
                expect_identical(names(variables(ds))[1], "Variable One")
                expect_error(ds$v2 <- v1, 
                    "Cannot overwrite one Variable")
            })
        })

        with(test.dataset(mrdf), {
            cast.these <- grep("mr_", names(ds))
            test_that("Dataset [<-", {
                expect_true(all(vapply(variables(ds)[cast.these], 
                    function (x) x$type == "numeric", logical(1))))
                expect_true(all(vapply(ds[cast.these], 
                    function (x) is.Numeric(x), logical(1))))
                ds[cast.these] <- lapply(ds[cast.these],
                    castVariable, "categorical")
                expect_true(all(vapply(variables(ds)[cast.these], 
                    function (x) x$type == "categorical", logical(1))))
                expect_true(all(vapply(ds[cast.these], 
                    function (x) is.Categorical(x), logical(1))))
            })
            test_that("Dataset [[<- on new array variable", {
                try(ds$arrayVar <- makeArray(ds[cast.these], 
                    name="Array variable"))
                expect_true(is.CA(ds$arrayVar))
                expect_identical(name(ds$arrayVar), "Array variable")
            })
        })
        
        test_that("Dataset deleting is safe", {
            with(test.dataset(df), {
                expect_error(delete(ds, confirm=TRUE), 
                    "Must confirm deleting dataset")
                ds.sub <- ds[1]
                expect_true(is.dataset(ds.sub))
                expect_true(is.readonly(ds.sub))
                expect_error(delete(ds.sub), 
                    "Must confirm deleting dataset")
                ## Then can delete
                expect_false(is.error(delete(ds.sub, confirm=FALSE)))
            })
        })
    })
}