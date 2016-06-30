context("Merge/extend dataset")

with_mock_HTTP({
    ds1 <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")

    test_that("Input validation for merge/extend (plus method dispatch)", {
        expect_error(extendDataset(1),
            "x must be a Crunch Dataset")
        expect_error(merge(ds1, 1),
            "y must be a Crunch Dataset")
        expect_error(merge(ds1, ds2, by.x=1),
            "by.x must be a Crunch Variable")
        expect_error(merge(ds1, ds2, by.x=ds2[[1]]),
            "by.x must be a variable in x")
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=1),
            "by.y must be a Crunch Variable")
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=ds1[[1]]),
            "by.y must be a variable in y")
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=ds2[[1]], all=TRUE),
            'Option "all" not supported.')
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=ds2[[1]], all.x=FALSE),
            'Option "all.x=FALSE" not supported.')
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=ds2[[1]], all.y=TRUE),
            'Option "all.y" not supported.')
    })

    test_that("Correct payload without filtering", {
        expect_warning(
            expect_mock_request(merge(ds1, ds2, by.x=ds1$birthyr, ds2$birthyr),
                'POST /api/datasets/dataset1/variables.json ',
                '{"function":"adapt",',
                '"args":[{"dataset":"/api/datasets/dataset3.json","filter":null},',
                '{"variable":"/api/datasets/dataset3/variables/birthyr.json"},',
                '{"variable":"/api/datasets/dataset1/variables/birthyr.json"}]}'),
            "Variable birthyr is hidden")
    })
})
