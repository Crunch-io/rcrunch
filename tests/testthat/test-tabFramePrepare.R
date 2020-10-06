context('tabFramePrepare - weighted tests')

with_api_fixture <- function(fixture_path, expr) {
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
    ds <- loadDataset("Example dataset")
    w <- list(weight1 = c('allpets', 'q1'), weight2 = 'q1')
    
    test_that("tabFramePrepare fails if given a dataset that does not include a specified complex weight", {
        expect_error(
            tabFramePrepare(ds[c("q1", "allpets", "weight2")], w, TRUE),
            "One or more specified weights are not included in the dataset"
        )
    })

    test_that("tabFramePrepare drops unweighted dataset variables", {
        r <- structure(
            list(values = c("q1", "q1", "allpets"),
                 ind = c("weight1", "weight2", "weight1"),
                 ord = c(1L, 1L, 2L), keep = c(TRUE, TRUE, TRUE),
                 index = c(1L, 1L, 2L)),
            row.names = c(6L, 7L, 5L), class = "data.frame"
        )


        expect_equal(
            tabFramePrepare(
                ds[c("q1", "allpets", "weight2", "weight1")], w, FALSE),
            r
        )
    })

})

context('tabBook multiweight - unweighted tests')

with_api_fixture("../../tmp_unweighted",  {
    # needs to use unweighted dataset, which will be a separate mock ds
    ds <- loadDataset("Example dataset")

    test_that("tabFrame works with unweighted + complex weight specification", {
        r <- structure(
            list(
                values = c(
                    "allpets", "allpets", "q1", "q1", "q1", "petloc", "ndogs", 
                    "ndogs_a", "ndogs_b", "q3", "country", "wave"
                ), 
                ind = c(
                    NA, "weight1", NA, "weight1", "weight2", NA, NA, NA,NA, NA, NA, NA), 
                ord = c(1L, 1L, 2L, 2L, 2L, 3L, 4L, 5L, 6L, 
                                            7L, 8L, 9L), 
                keep = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, 
                                                                  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
                index = c(1L, 1L, 2L, 2L, 1L, 3L, 4L, 5L, 6L, 7L, 8L, 9L)), 
            row.names = c(1L, 12L, 2L, 13L, 14L, 3L, 4L, 5L, 6L, 7L, 8L, 9L), 
            class = "data.frame"
        )
        
        res <- tabFramePrepare(ds, w, TRUE)
        print(res)
        expect_equal(
            tabFramePrepare(ds, w, TRUE),
            r
        )
    })
    
#     test_that("tabBook runs appropriately with a list as a weight", {
#         # Needs to use unweighted dataset
#         # weight(ds) <- NULL
#         w <- list(weight1 = c("allpets", "q1"), weight2 = "q1")
#         # This is how crunchtabs tricks tabBook into giving it a cube
#         multitable <- newMultitable("~ `allpets`", ds)
#         # debugonce(tabBook)
#         tabFrame <- tabFramePrepare(ds, w, TRUE)
#         print("Made it to test!")
#         r <- with_multi_POST(
#             c(
#                 "https://app.crunch.io/api/datasets/a8aa2b/multitables/2c1266/",
#                 "https://app.crunch.io/api/datasets/a8aa2b/multitables/2c1266/"
#             ),
#             tabBook(multitable, dataset = ds, w)
#         )
# 
# 
#         for (i in seq_len(nrow(r))) {
#             # Test name
#             expect_equal(
#                 r@.Data[[1]][[6]][[i]]$name,
#                 name(ds[[tabFrame$values[i]]])
#             )
# 
#             if (is.na(tabFrame$ind[i])) {
#                 expect_true(
#                     is.null(r@.Data[[1]][[6]][[i]]$weight)
#                 )
#             } else {
#                 expect_equal(
#                     r@.Data[[1]][[6]][[i]]$weight,
#                     tabFrame$ind[i]
#                 )
#             }
#         }
# 
#         expect_equal(length(aliases(r)), 11)
#     })
})





