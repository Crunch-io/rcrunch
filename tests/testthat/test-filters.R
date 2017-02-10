context("Filters")

test_that("show method exists", {
    expect_true(is.character(get_output(CrunchFilter())))
})

with_mock_HTTP({
    ds <- loadDataset("test ds")
    ds3 <- loadDataset("ECON.sav")

    test_that("Test dataset has 2 filters", {
        expect_is(filters(ds), "FilterCatalog")
        expect_length(filters(ds), 2)
        expect_output(filters(ds),
            get_output(data.frame(
                    name=c("Occasional Political Interest", "Public filter"),
                    id=c("filter1", "filter2"),
                    is_public=c(FALSE, TRUE)
                )), fixed=TRUE)
    })

    test_that("Empty filter catalog", {
        expect_is(filters(ds3), "FilterCatalog")
        expect_length(filters(ds3), 0)
        expect_output(filters(ds3), get_output(data.frame()))
    })

    test_that("Filter catalog methods", {
        expect_identical(names(filters(ds)),
            c("Occasional Political Interest", "Public filter"))
        expect_identical(urls(filters(ds)),
            c("api/datasets/1/filters/filter1/",
              "api/datasets/1/filters/filter2/"))
        expect_identical(names(filters(ds)[c(2, 1)]),
           c("Public filter", "Occasional Political Interest"))
    })

    f <- filters(ds)[["Occasional Political Interest"]]
    test_that("Filter catalog extract", {
        expect_is(f, "CrunchFilter")
        expect_identical(f, filters(ds)[[1]])
        expect_identical(f, filters(ds)$`Occasional Political Interest`)
    })

    test_that("Filter entity is.public", {
        expect_false(is.public(f))
        expect_PATCH(is.public(f) <- TRUE,
            "api/datasets/1/filters/filter1/",
            '{"is_public":true}')
        expect_no_request(is.public(f) <- FALSE)
    })

    test_that("Assigning filters<- on a dataset doesn't itself modify anything", {
        expect_no_request(filters(ds) <- filters(ds)[c(2, 1)])
        expect_true(is.dataset(ds))
    })

    test_that("Create a filter by newFilter", {
        expect_POST(newFilter("A filter", ds$gender=="Male", catalog=filters(ds)),
            'api/datasets/1/filters/',
            '{"name":"A filter","expression":',
            '{"function":"==","args":[',
            '{"variable":"api/datasets/1/variables/gender/"},',
            '{"value":1}]}}')
        with_POST("api/datasets/1/filters/filter1/", {
            ## Mock the return of that creation
            f <- newFilter("A filter", ds$gender=="Male", catalog=filters(ds))
            expect_is(f, "CrunchFilter")
            expect_false(is.public(f))
        })
    })

    test_that("newFilter without explicitly setting 'catalog'", {
        expect_POST(newFilter("A filter", ds$gender=="Male", catalog=ds),
            'api/datasets/1/filters/',
            '{"name":"A filter","expression":',
            '{"function":"==","args":[',
            '{"variable":"api/datasets/1/variables/gender/"},',
            '{"value":1}]}}')
        expect_POST(newFilter("A filter", ds$gender=="Male"),
            'api/datasets/1/filters/',
            '{"name":"A filter","expression":',
            '{"function":"==","args":[',
            '{"variable":"api/datasets/1/variables/gender/"},',
            '{"value":1}]}}')
    })

    test_that("newFilter on an invalid 'catalog'", {
        expect_error(newFilter("A filter", ds$gender=="Male", catalog="Foo!"),
            "Cannot create a filter entity on an object of class character")
    })

    test_that("Create a filter by [[<-", {
        expect_POST(filters(ds)[["A filter"]] <- ds$gender=="Male",
            'api/datasets/1/filters/',
            '{"name":"A filter","expression":',
            '{"function":"==","args":[',
            '{"variable":"api/datasets/1/variables/gender/"},',
            '{"value":1}]}}')
    })
})

with_test_authentication({
    ds <- newDataset(df)
    test_that("We have an empty filter catalog", {
        expect_is(filters(ds), "FilterCatalog")
        expect_length(filters(ds), 0)
    })

    filters(ds)[["Test filter"]] <- ds$v4 == "B"
    test_that("We can create a filter", {
        expect_length(filters(ds), 1)
        expect_identical(names(filters(ds)), "Test filter")
        expect_identical(name(filters(ds)[[1]]), "Test filter")
    })
    test_that("Show methods for filter and filter catalog", {
        expect_output(filters(ds),
            get_output(data.frame(
                    name="Test filter",
                    id=filters(ds)[["Test filter"]]@body$id,
                    is_public=FALSE
                )), fixed=TRUE)
        expect_output(filters(ds)[["Test filter"]],
            paste0('Crunch filter ', dQuote("Test filter"),
                '\nExpression: v4 == "B"'))
    })

    test_that("We can make it public/private", {
        expect_false(is.public(filters(ds)[["Test filter"]]))
        is.public(filters(ds)[["Test filter"]]) <- TRUE
        expect_true(is.public(filters(ds)[["Test filter"]]))
        is.public(filters(ds)[["Test filter"]]) <- FALSE
        expect_false(is.public(filters(ds)[["Test filter"]]))
    })

    test_that("Setter/getter by index", {
        expect_false(is.public(filters(ds)[[1]]))
        is.public(filters(ds)[[1]]) <- TRUE
        expect_true(is.public(filters(ds)[[1]]))
        is.public(filters(ds)[[1]]) <- FALSE
        expect_false(is.public(filters(ds)[[1]]))
    })

    test_that("Can update a filter's expression by name", {
        expect_json_equivalent(zcl(expr(filters(ds)[[1]])),
            zcl(ds$v4 == "B"))
        filters(ds)[["Test filter"]] <- ds$v4 == "C"
        expect_json_equivalent(zcl(expr(filters(ds)[[1]])),
            zcl(ds$v4 == "C"))
    })
    test_that("Can update a filter's expression by index", {
        expect_json_equivalent(zcl(expr(filters(ds)[[1]])),
            zcl(ds$v4 == "C"))
        filters(ds)[[1]] <- ds$v4 == "B"
        expect_json_equivalent(zcl(expr(filters(ds)[[1]])),
            zcl(ds$v4 == "B"))
    })
    test_that("Error handling for [[<-", {
        expect_error(filters(ds)[[6]] <- ds$v4 == "B",
            "Subscript out of bounds: 6")
    })

    test_that("We have an applied filters view", {
        expect_length(appliedFilters(ds), 0)
    })

    test_that("We can 'apply' a filter", {
        appliedFilters(ds) <- filters(ds)[["Test filter"]]
        expect_length(appliedFilters(ds), 1)
    })

    test_that("'applied filters' for the UI don't affect R", {
        expect_length(appliedFilters(ds), 1)
        expect_valid_df_import(ds)
    })

    test_that("We also have 'active filter' for the R object", {
        expect_null(activeFilter(ds))
    })

    test_that("We can set 'active filter'", {
        activeFilter(ds) <- ds$v4 == "C"
        expect_identical(zcl(activeFilter(ds)), zcl(ds$v4 == "C"))
    })

    test_that("If we set an active filter, cubes will be filtered by it (and not UI filters)", {
        activeFilter(ds) <- ds$v4 == "C"
        expect_equivalent(as.array(crtabs(~ v4, data=ds)),
            array(c(0, 10), dim=2L, dimnames=list(v4=c("B", "C"))))
    })
})
