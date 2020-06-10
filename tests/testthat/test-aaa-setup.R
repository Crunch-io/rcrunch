context("rcrunch is ready to run tests")

test_that("cube fixtures are up to date", {
    skip_on_cran()
    potential_path <- file.path(testthat::test_path("..", "..", "mocks"))
    if (dir.exists(potential_path)) {
        current_files <- list.files(potential_path, recursive = TRUE)
        current_file_info <- data.frame(
            name = current_files,
            md5 = tools::md5sum(file.path(potential_path, current_files)),
            stringsAsFactors = FALSE,
            row.names = NULL
        )
        current_file_info <- current_file_info[order(current_file_info$name), ]

        temp_files <- list.files(file.path(tempdir(), "mocks"), recursive = TRUE)
        temp_file_info <- data.frame(
            name = temp_files,
            md5 = tools::md5sum(file.path(tempdir(), "mocks", temp_files)),
            stringsAsFactors = FALSE,
            row.names = NULL
        )
        temp_file_info <- temp_file_info[order(temp_file_info$name), ]

        if (!isTRUE(all.equal(current_file_info, temp_file_info, check.attributes = FALSE))) {
            warning(
                "mocks tarball looks out of date. Run command `make compress-fixtures` to update"
            )
        }

        expect_equivalent(current_file_info, temp_file_info)
    } else {
        warning(paste0(
            "Could not find cubes directory so could not check if mock fixures are up to date. ",
            "You can make sure they're updated by running `make compress-fixtures`."
        ))
    }
})