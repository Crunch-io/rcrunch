

with_api_fixture <- function(fixture_path, expr) {
    httpcache::clearCache()
    with(
        crunch::temp.options(
            crunch.api = "https://app.crunch.io/api/",
            httptest.mock.paths = fixture_path,
            crunch.show.progress = FALSE
        ),
        httptest::with_mock_api(
            expr
        )
    )
}

with_api_fixture("../../tmp", {

    context('tabFramePrepare - weighted tests')
    
    ds <- loadDataset("Example dataset")
    mt <- newMultitable("~ `allpets`", ds)
    w <- list(weight1 = c('allpets', 'q1'), weight2 = 'q1')
    
    test_that("tabFramePrepare fails if given a dataset that does not include a specified complex weight", {
        expect_error(
            tabFramePrepare(ds[c("q1", "allpets", "weight2")], w),
            "One or more specified weights are not included in the dataset"
        )
    })

    
    test_that("tabFrame includes original weighted",{
        r <- structure(list(
            alias = c("q1", "q1", "wave", "allpets"), 
            weight = c("weight1","weight2", "weight1", "weight1"), 
            order = c(1L, 1L, 2L, 3L), 
            keep = c(TRUE, TRUE, FALSE, TRUE), 
            position = c(1L, 1L, 2L, 3L)), row.names = c(7L, 8L, 2L, 6L), 
            class = "data.frame"
        )
        
        res <- tabFramePrepare(ds[c("q1","wave", "allpets", "weight2", "weight1")], w)
        expect_equal(
            res, 
            r
        )
    })
    
    test_that("tabBook xlsx fails with ", {
        expect_error(
            tabBook(mt, ds, w, output_format = "xlsx"),
            "Excel TabBooks can only have one weight"
        )
    })
    
    test_that("Bad weights specifications fail", {
        expect_error(
            tabBook(mt, ds, weight = 1),
            "Weight can be NULL, a list, or a variable"
        )
    })
    
    test_that("Bad weights specifications fail", {
        expect_error(
            tabBook(mt, ds, weight = NA),
            "Weight can be NULL, a list, or a variable"
        )
    })
    


})


# httpcache::clearCache()

with_api_fixture("../../tmp", {
    context("tabBook multiweighted")
    test_that("tabBook returns multiple results in the expected order", {
        ds <- loadDataset("Example dataset")
        w <- list(weight1 = c("allpets", "q1"), weight2 = "q1")
        # This is how crunchtabs tricks tabBook into giving it a cube
        multitable <- newMultitable("~ `allpets`", ds)
        # debugonce(tabBook)
        tabFrame <- tabFramePrepare(ds, w)
        
        r <- with_multi_POST(
            c(
                "https://app.crunch.io/api/datasets/63b9d9/multitables/300608/tb-weight1/",
                "https://app.crunch.io/api/datasets/63b9d9/multitables/300608/tb-weight2/"
            ),
            tabBook(multitable, dataset = ds, w)
        )
        
        print(tabFrame)
        saveRDS(r, "test_object.rds")
        
        for (i in seq_len(nrow(r))) {
            # Test name
            
            # print(tabFrame[i, ])
            # print(r@.Data[[1]][[6]][[i]]$name)
            
            expect_equal(
                r@.Data[[1]][[6]][[i]]$name,
                name(ds[[tabFrame$alias[i]]])
            )
            
            # Test weight is null if na
            if (is.na(tabFrame$weight[i])) {
                expect_true(
                    is.null(r@.Data[[1]][[6]][[i]]$weight)
                )
                
            } else {
                expect_equal(
                    r@.Data[[1]][[6]][[i]]$weight,
                    tabFrame$weight[i]
                )
            }
        }
        
        expect_equal(length(aliases(r)), 11)
    })
})