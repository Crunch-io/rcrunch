context("Dataset stream")
#
mock_stream_rows <- data.frame(
    birthyr = c(0.577530, 0.577530),
    gender = c(2, 1), # Female , Male
    subvar1 = c(2, 2),
    subvar2 = c(1, 1),
    subvar3 = c(1, 1),
    textVar = c("a", "b"),
    starttime = c("1955-12-28", "1955-12-29")
)

with_mock_crunch({
    ds <- loadDataset("streaming test ds") ## has 2 messages waiting, 4 rows received
    ds2 <- loadDataset("an archived dataset", kind = "archived") ## Has no streams mock
    ds3 <- loadDataset("streaming no messages") ## has 0 messages waiting, 0 rows received
    test_that("pendingStream gets pending messages", {
        expect_equal(pendingStream(ds), 2)
        expect_GET(pendingStream(ds2), "https://app.crunch.io/api/datasets/2/stream")
    })

    test_that("streaming attribute setting", {
        expect_equal(streaming(ds), "streaming")
        expect_PATCH(
            streaming(ds) <- "finished",
            "https://app.crunch.io/api/datasets/1streaming/",
            '{"streaming":"finished"}'
        )
        expect_error(
            streaming(ds) <- "foo bar", 
            "'arg' should be one of .*no.*, .*streaming.*, .*finished.*"
        )
    })  
    
    test_that("streamRows streams rows", {
        expect_equal(streamRows(ds, data = data.frame()), ds)
        expect_POST(
            streamRows(ds, data = mock_stream_rows),
            "https://app.crunch.io/api/datasets/1streaming/stream/",
            '{"birthyr":0.5775,"gender":2,"subvar1":2,"subvar2":1,"subvar3":1,',
            '"textVar":"a","starttime":"1955-12-28"}\n',
            '{"birthyr":0.5775,"gender":1,"subvar1":2,"subvar2":1,"subvar3":1,',
            '"textVar":"b","starttime":"1955-12-29"}'
        )
        expect_POST(
            streamRows(ds, data = mock_stream_rows[2, ]),
            "https://app.crunch.io/api/datasets/1streaming/stream/",
            '{"birthyr":0.5775,"gender":1,"subvar1":2,"subvar2":1,"subvar3":1,',
            '"textVar":"b","starttime":"1955-12-29"}'
        )
    })

    test_that("appendStream", {
        expect_POST(
            appendStream(ds),
            "https://app.crunch.io/api/datasets/1streaming/batches/",
            '{"element":"shoji:entity","body":{',
            '"type":"ldjson","stream":null}'
        )
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
    metadata <- fromJSON(file.path("dataset-fixtures", "streaming.json"), simplifyVector = FALSE)
    ds <- suppressMessages(uploadMetadata(metadata))
    test_that("streamRows streams rows", {
        expect_equal(pendingStream(ds), 0)
        ds <- streamRows(ds, data = stream_rows)
        expect_equal(pendingStream(ds), 1)
    })

    test_that("appendStream appends all pending rows", {
        ds <- appendStream(ds)
        expect_equal(nrow(ds), 2)
        expect_equal(pendingStream(ds), 0)
    })
})
