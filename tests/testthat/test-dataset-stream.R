context("Dataset stream")
# 
# mock_stream_rows <- data.frame(
#     birthyr = c(0.577530, 0.577530) 
#     gender = c(2, 1) # Female , Male
#     mymrset = c(list(2, 1, 1))
#     textVar = c("a", "b")
#     starttime = c("1955-12-28", "1955-12-29")
# )

with_mock_crunch({
    ds <- loadDataset("test ds")   ## has 2 rows waiting, 4 rows received
    ds2 <- loadDataset("ECON.sav") ## Has no streams
    
    test_that("getPendingMessages gets pending messages", {
        expect_equal(getPendingMessages(ds), 2)
        expect_GET(getPendingMessages(ds2), 'https://app.crunch.io/api/datasets/3/stream')        
    })
})


stream_rows <- data.frame(
    v1 = c(0.75331105, 0.75331105),
    v2 = c("a", "b"),
    v3 = c(28, 29),
    v4 = c(1, 1),
    v5 = c("1955-11-25T00:00:00.000000+00:00", "1955-11-26T00:00:00.000000+00:00"),
    v6 = c(1, 1)
)
with_test_authentication({
    ds <- newDataset(df)
    test_that("streamRows streams rows", {
        expect_equal(getPendingMessages(ds), 0)
        streamRows(ds, data=stream_rows)
        expect_equal(getPendingMessages(refresh(ds)), 2)      
    })
})