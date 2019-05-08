context("Making a new dataset")

test_that("write.csv.gz gzips a csv", {
    df <- data.frame(a = 1:1000)
    f <- tempfile()
    f2 <- tempfile()
    write.csv.gz(df, file = f)
    write.csv(df, file = f2, row.names = FALSE)
    expect_true(file.exists(f))
    expect_true(file.exists(f2))
    expect_true(file.size(f) < file.size(f2)) ## bc compression
    expect_equal(read.csv(f), df) ## It sniffs the conn type
    expect_equal(read.csv(f2), df)
})

test_that("newDataset input validation", {
    expect_error(
        newDataset(NULL),
        "Can only make a Crunch dataset from a two-dimensional data"
    )
    expect_error(
        newDataset(1:5),
        "Can only make a Crunch dataset from a two-dimensional data"
    )
})

with_mock_crunch({
    test_that("Basic exercise of turning data.frame to Crunch payload", {
        expect_POST(
            newDataset(data.frame(a = 1), name = "Testing"),
            "https://app.crunch.io/api/datasets/",
            '{"element":"shoji:entity","body":{"name":"Testing",',
            '"table":{"element":"crunch:table",',
            '"metadata":{"a":{"type":"numeric","name":"a","alias":"a"}},',
            '"order":["a"]}}}'
        )
    })

    test_that("Turning data.frame to Crunch payload without a name", {
        expect_POST(
            newDataset(data.frame(a = 1)),
            "https://app.crunch.io/api/datasets/",
            '{"element":"shoji:entity","body":{"name":"data.frame(a = 1)",',
            '"table":{"element":"crunch:table",',
            '"metadata":{"a":{"type":"numeric","name":"a","alias":"a"}},',
            '"order":["a"]}}}'
        )
    })

    test_that("Turning data.frame to Crunch payload with a long name", {
        expect_POST(
            newDataset(data.frame(a = 1, really_really_long_name = 2)),
            "https://app.crunch.io/api/datasets/",
            '{"element":"shoji:entity","body":{"name":"data.frame(a = 1, really_really_long_nam",',
            '"table":{"element":"crunch:table",',
            '"metadata":{"a":{"type":"numeric","name":"a","alias":"a"},"really_really_long_name":{"type":"numeric","name":"really_really_long_name","alias":"really_really_long_name"}},',
            '"order":["a","really_really_long_name"]}}}'
        )
    })

    test_that("uploadData writes out a gzipped file", {
        ds <- loadDataset("test ds")
        with_DELETE(NULL, {
            ## with_DELETE to handle the cleanup so we can see the real error
            expect_POST(
                uploadData(ds, data.frame(a = 1)),
                "https://app.crunch.io/api/sources/",
                "list(uploaded_file"
            )
        })
    })

    test_that("createDataset with named args", {
        expect_POST(
            createDataset(name = "Foo", description = "Bar."),
            "https://app.crunch.io/api/datasets/",
            '{"element":"shoji:entity","body":{"name":"Foo",',
            '"description":"Bar."}}'
        )
    })
    test_that("createDataset returns a dataset", {
        with_POST(
            "https://app.crunch.io/api/datasets/1/",
            expect_true(is.dataset(createDataset(name = "Foo")))
        )
    })
    test_that("newDataset calls newDatasetFromFile if given a string", {
        expect_POST(
            newDataset("helper.R"),
            "https://app.crunch.io/api/datasets/",
            '{"element":"shoji:entity","body":{"name":"helper.R"}}'
        )
    })
    test_that("newDataset with a schema posts to sources", {
        expect_POST(
            newDataset(x = "helper.R", schema = "helper.R"),
            "https://app.crunch.io/api/sources/",
            'list\\(uploaded_file = list\\(path = .*helper.R',
            fixed = FALSE
        )
    })
    test_that("newDataset with schema and data posts, adds to batches and appends", {
        with_POST("https://app.crunch.io/api/datasets/1/", {
            # the batch had to be mocked in tests/testthat/app.crunch.io/... because
            # we supressMessages which makes detecting it harder
            ds <- newDataset(x = "teardown.R", schema = "setup.R")
        })
    })
    test_that("newDataset(FromFile) cleans up the dataset entity if the file is invalid", {
        with_POST("https://app.crunch.io/api/datasets/1/", {
            expect_DELETE(
                newDataset("NOTAFILE.exe"),
                "https://app.crunch.io/api/datasets/1/"
            )
            with_DELETE(NULL, {
                expect_error(
                    newDataset("NOTAFILE.exe"),
                    "File not found"
                )
            })
        })
    })
    test_that("newDataset(FromFile) can take an s3 URL", {
        with_DELETE(NULL, {
            expect_POST(
                newDataset("s3://httpbin.org/get"),
                "https://app.crunch.io/api/datasets/1/batches/",
                '{"element":"shoji:entity",',
                '"body":{"url":"s3://httpbin.org/get"}}'
            )
        })
    })
    test_that("newDataset(FromFile) can take an http(s) URL", {
        with_DELETE(NULL, {
            expect_POST(
                newDataset("https://httpbin.org/get"),
                "https://app.crunch.io/api/sources/",
                '{"element":"shoji:entity",',
                '"body":{"location":"https://httpbin.org/get"}}'
            )
        })
    })

    test_that("newDatasetByColumn", {
        expect_POST(
            newDatasetByColumn(data.frame(a = 1), name = "Bam!"),
            "https://app.crunch.io/api/datasets/",
            '{"element":"shoji:entity","body":{"name":"Bam!"}}'
        )
    })

    test_that("createSource validation", {
        expect_error(
            createSource("File not found"),
            "File not found"
        )
        expect_error(
            createSource(name = "x"),
            "Must provide a file or url to createSource"
        )
    })

    test_that("newExampleDataset", {
        expect_POST(newExampleDataset(),
            'https://app.crunch.io/api/datasets/',
            '{"element":"shoji:entity","body":{"name":"Example dataset",'
        )
    })
})

with_test_authentication({
    whereas("The two methods for sending data", {
        testfile.csv <- "fake.csv"
        testfile.df <- read.csv(testfile.csv)
        test_that("fake.csv is what we expect", {
            expect_identical(dim(testfile.df), c(20L, 6L))
        })

        test_that("newDataset creates a dataset if given a file", {
            ds <- newDataset(testfile.csv)
            expect_true(is.dataset(ds))
            expect_identical(nrow(ds), 20L)
            expect_identical(ncol(ds), 6L)
            expect_equivalent(mean(ds[[2]]), mean(testfile.df[[2]]))
        })

        test_that("Dataset-by-column variable types get set correctly", {
            ds <- newDatasetByColumn(df)
            expect_valid_df_import(ds)
            expect_equivalent(mean(ds$v3), mean(df$v3))
            expect_true(setequal(names(df), names(ds)))
            expect_identical(names(df), names(ds))
        })
    })

    m <- fromJSON(system.file("example-datasets", "pets.json", package="crunch"),
        simplifyVector = FALSE
    )

    whereas("Creating with metadata and csv", {
        test_that("createWithMetadataAndFile using docs example", {
            ds <- newDatasetFromFixture("apidocs")
            expect_valid_apidocs_import(ds)
        })

        test_that("data.frame with spaces in column names", {
            input <- data.frame(a = factor("A"), b = 4)
            names(input) <- c("var one", "var two")
            expect_identical(names(input), c("var one", "var two"))
            ds <- newDataset(input)
            expect_identical(names(ds), c("var one", "var two"))
        })

        test_that("Can create dataset with data in S3", {
            ds <- createWithMetadataAndFile(m,
                file = "s3://testing-crunch-io/example-dataset.csv"
            )
            expect_valid_apidocs_import(ds)
            ds2 <- newDatasetFromFixture("apidocs")
            ## Compare to dataset imported from local file upload
            expect_identical(dim(ds), dim(ds2))
            expect_identical(as.vector(ds$q1), as.vector(ds2$q1))
            ## Could add more assertions
        })
    })

    test_that("Duplicate subvariables are forbidden", {
        m2 <- m
        ## Add a duplicate subvariable
        m2$body$table$metadata$allpets$subvariables[[4]] <- list(name = "Another", alias = "allpets_1")
        expect_error(createWithMetadataAndFile(
            m2,
            system.file("example-datasets", "pets.csv", package="crunch")
        ))
    })

    test_that("newDataset without specifying name grabs object name", {
        dsz <- newDataset(df)
        expect_true(is.dataset(dsz))
        expect_identical(name(dsz), "df")
        expect_valid_df_import(dsz)
    })
})
