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
    ds <- loadDataset("test ds")   ## has 2 messages waiting, 4 rows received
    ds2 <- loadDataset("an archived dataset", kind="archived") ## Has no streams mock
    ds3 <- loadDataset("ECON.sav") ## has 0 messages waiting, 0 rows received
    test_that("pendingStream gets pending messages", {
        expect_equal(pendingStream(ds), 2)
        expect_GET(pendingStream(ds2), 'https://app.crunch.io/api/datasets/2/stream')        
    })
    
    test_that("streamRows streams rows", {
        expect_equal(streamRows(ds, data=data.frame()), ds)
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
    
    test_that("appendStream", {
        expect_POST(appendStream(ds),
                    'https://app.crunch.io/api/datasets/1/batches/',
                    '{"element":"shoji:entity","body":{',
                    '"type":"ldjson","stream":null},',
                    '"autorollback":true,"savepoint":true}')
        expect_message(appendStream(ds3), "There's no pending stream data to be appended.")
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
        expect_equal(pendingStream(ds), 0)
        ds <- streamRows(ds, data=stream_rows)
        expect_equal(pendingStream(ds), 1)
    })

    test_that("appendStream appends all pending rows", {
        ds <- appendStream(ds)
        expect_equal(nrow(ds), 22)
        expect_equal(pendingStream(ds), 0)
    })
})