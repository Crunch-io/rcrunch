context("Dataset stream")
# 
mock_stream_rows <- data.frame(
    birthyr = c(0.577530, 0.577530),
    gender = c(2, 1),# Female , Male
    mymrset = c(list(2, 1, 1)),
    textVar = c("a", "b"),
    starttime = c("1955-12-28", "1955-12-29")
)

with_mock_crunch({
    ds <- loadDataset("test ds")   ## has 2 rows waiting, 4 rows received
    ds2 <- loadDataset("ECON.sav") ## Has no streams
    
    test_that("pendingMessages gets pending messages", {
        expect_equal(pendingMessages(ds), 2)
        expect_GET(pendingMessages(ds2), 'https://app.crunch.io/api/datasets/3/stream')        
    })
    
    test_that("pendingMessages gets pending messages", {
        expect_POST(streamRows(ds, data=mock_stream_rows),
                    'https://app.crunch.io/api/datasets/1/stream/',
                    '{"birthyr":0.5775,"gender":2,"mymrset.2":2,"mymrset.1":1,"mymrset.1.1":1,',
                    '"textVar":"a","starttime":"1955-12-28"}\n',
                    '{"birthyr":0.5775,"gender":1,"mymrset.2":2,"mymrset.1":1,"mymrset.1.1":1,',
                    '"textVar":"b","starttime":"1955-12-29"}')
        expect_POST(streamRows(ds[1,], data=mock_stream_rows),
                    'https://app.crunch.io/api/datasets/1/stream/',
                    '{"birthyr":0.5775,"gender":2,"mymrset.2":2,"mymrset.1":1,"mymrset.1.1":1,',
                    '"textVar":"a","starttime":"1955-12-28"}')
    })
    
    test_that("appendStreamedRows", {
        expect_POST(appendStreamedRows(ds),
                    'https://app.crunch.io/api/datasets/1/batches/',
                    '{"element":"shoji:entity","body":{',
                    '"stream":null,"type":"ldjson"}}')
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
        expect_equal(pendingMessages(ds), 0)
        streamRows(ds, data=stream_rows)
        expect_equal(pendingMessages(refresh(ds)), 1)
        appendStreamedRows(ds)
        expect_equal(nrow(refresh(ds)), 22)
    })
})