context("rcrunch is ready to run tests")

test_that("cube fixtures are up to date", {
    skip_on_cran()
    potential_path <- file.path(testthat::test_path("..", "..", "cubes"))
    if (dir.exists(potential_path)) {
        current_files <- list.files(potential_path)
        current_file_info <- data.frame(
            name = current_files,
            md5 = tools::md5sum(file.path(potential_path, current_files)),
            stringsAsFactors = FALSE,
            row.names = NULL
        )
        current_file_info <- current_file_info[order(current_file_info$name), ]

        temp_files <- list.files(file.path(tempdir(), "cubes/"))
        temp_file_info <- data.frame(
            name = temp_files,
            md5 = tools::md5sum(file.path(tempdir(), "cubes", temp_files)),
            stringsAsFactors = FALSE,
            row.names = NULL
        )
        temp_file_info <- temp_file_info[order(temp_file_info$name), ]

        if (!isTRUE(all.equal(current_file_info, temp_file_info, check.attributes = FALSE))) {
            warning(
                "cubes directory looks out of date. Run command `make compress-fixtures` to update"
            )
        }

        expect_equivalent(current_file_info, temp_file_info)
    } else {
        warning(paste0(
            "Could not find cubes directory so could not check if cube fixures are up to date. ",
            "You can make sure they're updated by running `make compress-fixtures`."
        ))
    }
})
