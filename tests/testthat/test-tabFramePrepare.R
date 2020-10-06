context('tabFramePrepare') 

with_mock_crunch({
    # ds <- newExampleDataset()
    ds <- loadDataset("Example dataset")
    
    # Dummy weights for testing
    ds$weight1 <- makeWeight(ds$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
    ds$weight2 <- makeWeight(ds$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight2')
    is.weight(ds$weight1) <- TRUE
    is.weight(ds$weight2) <- TRUE
    
    test_that("tabFramePrepare fails if given a dataset that does not include a specified complex weight", {
        
        w <- list(weigth1 = c('allpets', 'q1'), weight2 = 'q1')
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
    
    test_that("tabFrame works with unweighted + complex weight specification", {
        ds <- loadDataset("Example dataset")
        r <- structure(
            list(values = c("allpets", "allpets", "q1", "q1", "q1", "petloc",
                            "ndogs", "ndogs_a", "ndogs_b", "q3", "country",
                            "wave")
                 ,
                 ind = c(NA, "weight1", NA, "weight1", "weight2",NA, NA, NA,
                         NA, NA, NA, NA), 
                 ord = c(1L, 1L, 2L, 2L, 2L,3L, 4L, 5L, 6L, 7L, 8L, 9L),
                 keep = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
                          FALSE, FALSE, FALSE, FALSE), 
                 index = c(1L, 1L, 2L, 2L, 1L, 3L, 4L, 5L, 6L, 7L, 8L, 9L)),
            row.names = c(1L, 12L, 2L, 13L, 14L, 3L, 4L, 5L, 6L, 7L, 8L, 9L),
            class = "data.frame"
        )
        
        expect_equal(
            tabFramePrepare(ds, w, TRUE),
            r
        )
    })
    
    context("tabBook multiweight")
    
    test_that("tabBook runs appropriately with a list as a weight", {
        ds <- loadDataset("Example dataset")
        weight(ds) <- NULL
        w <- list(weight1 = c("allpets", "q1"), weight2 = "q1")
        # This is how crunchtabs tricks tabBook into giving it a cube
        multitable <- newMultitable("~ `allpets`", ds)
        # debugonce(tabBook)
        tabFrame <- tabFramePrepare(ds, w, TRUE)
        r <- tabBook(multitable, dataset = ds, w)
        
        for (i in seq_len(nrow(r))) {
            # Test name 
            expect_equal(
                r@.Data[[1]][[6]][[i]]$name, 
                name(ds[[tabFrame$values[i]]])
            )
            
            if (is.na(tabFrame$ind[i])) {
                expect_true(
                    is.null(r@.Data[[1]][[6]][[i]]$weight)
                )
            } else {
                expect_equal(
                    r@.Data[[1]][[6]][[i]]$weight,
                    tabFrame$ind[i]
                )
            }
        }
        
        expect_equal(length(aliases(r)), 12)
    })
})





