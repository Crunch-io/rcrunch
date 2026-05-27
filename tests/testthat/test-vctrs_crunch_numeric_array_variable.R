test_that("can create crunch_numeric_array_variable - From base types (1D)", {
    var <- crunch_numeric_array_variable(
        tibble::tibble(
            num1 = c(1, 2),
            num2 = c(NA, NA)
        ),
        label = "Num Array 1",
        axes = list(tibble::tibble(
            value = c("num1", "num2"),
            label = c("ARR 1", "ARR 2"),
            description = c("DESC 1", "DESC 2")
        )),
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_numeric_array_variable")
    expect_equal(ncol(var), 2)
    expect_equal(names(var), c("num1", "num2"))
    expect_equal(label(var), "Num Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_s3_class(var[[1]], "crunch_numeric_variable")
    expect_equal(typeof(var[[1]]), "double")
    expect_equal(var[[1]], crunch_numeric_variable(
        c(1, 2),
        label = "ARR 1",
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_numeric_variable")
    expect_equal(typeof(var[[2]]), "double")
    expect_equal(var[[2]], crunch_numeric_variable(
        c(NA_real_, NA_real_),
        label = "ARR 2",
        description = "DESC 2"
    ))

})

test_that("can create crunch_numeric_array_variable - From crunch_numeric_variables (1D)", {
    var <- crunch_numeric_array_variable(
        tibble::tibble(
            num1 = crunch_numeric_variable(
                1:2,
                label = "NUM 1",
                description = "ARR 1",
                notes = "NOTES",
                path = "/Public/d1"
            )),
        label = "Num Array 1"
    )
    expect_s3_class(var, "crunch_numeric_array_variable")
    expect_equal(ncol(var), 1)
    expect_equal(names(var), "num1")
    expect_s3_class(var[[1]], "crunch_numeric_variable")
    expect_equal(typeof(var[[1]]), "double")
    expect_equal(var[[1]], crunch_numeric_variable(
        c(1, 2),
        label = "NUM 1",
        description = "ARR 1"
    ))

    # From data.frame of base types (2D)
    var <- crunch_numeric_array_variable(
        tibble::tibble(
            num1 = tibble::tibble(x = c(1, 2)),
            num2 = tibble::tibble(x = c(NA, NA))
        ),
        label = "Num Array 1",
        axes = list(
            tibble::tibble(
                value = c("num1", "num2"),
                label = c("ARR 1", "ARR 2"),
                description = c("DESC 1", "DESC 2")
            ),
            tibble::tibble(
                value = c("x"),
                label = c("ARR X"),
                description = c("DESC X")
            )
        ),
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_numeric_array_variable")
    expect_equal(ncol(var), 2)
    expect_equal(names(var), c("num1", "num2"))
    expect_equal(label(var), "Num Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_s3_class(var[[1]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "double")
    expect_equal(var[[1]], crunch_numeric_array_variable(
        tibble::tibble(x = c(1, 2)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "double")
    expect_equal(var[[2]], crunch_numeric_array_variable(
        tibble::tibble(x = c(NA_real_, NA_real_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        description = "DESC 2"
    ))
})

test_that("can create crunch_numeric_array_variable - From data.frame of base types (2D)", {
    var <- crunch_numeric_array_variable(
        tibble::tibble(
            num1 = tibble::tibble(x = c(1, 2)),
            num2 = tibble::tibble(x = c(NA, NA))
        ),
        label = "Num Array 1",
        axes = list(
            tibble::tibble(
                value = c("num1", "num2"),
                label = c("ARR 1", "ARR 2"),
                description = c("DESC 1", "DESC 2")
            ),
            tibble::tibble(
                value = c("x"),
                label = c("ARR X"),
                description = c("DESC X")
            )
        ),
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_numeric_array_variable")
    expect_equal(ncol(var), 2)
    expect_equal(names(var), c("num1", "num2"))
    expect_equal(label(var), "Num Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_s3_class(var[[1]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "double")
    expect_equal(var[[1]], crunch_numeric_array_variable(
        tibble::tibble(x = c(1, 2)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "double")
    expect_equal(var[[2]], crunch_numeric_array_variable(
        tibble::tibble(x = c(NA_real_, NA_real_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        description = "DESC 2"
    ))
})

test_that("can create crunch_numeric_array_variable - From data.frame of crunch_numeric_variables (2D)", {
    var <- crunch_numeric_array_variable(
        tibble::tibble(
            num1 = tibble::tibble(x = crunch_numeric_variable(c(1, 2), label = "ARR X", description = "DESC X")),
            num2 = tibble::tibble(x = crunch_numeric_variable(c(NA, NA), label = "ARR X", description = "DESC X"))
        ),
        label = "Num Array 1",
        axes = list(
            tibble::tibble(
                value = c("num1", "num2"),
                label = c("ARR 1", "ARR 2"),
                description = c("DESC 1", "DESC 2")
            ),
            tibble::tibble(
                value = c("x"),
                label = c("ARR X"),
                description = c("DESC X")
            )
        ),
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_numeric_array_variable")
    expect_equal(ncol(var), 2)
    expect_equal(names(var), c("num1", "num2"))
    expect_equal(label(var), "Num Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_s3_class(var[[1]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "double")
    expect_equal(var[[1]], crunch_numeric_array_variable(
        tibble::tibble(x = c(1, 2)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "double")
    expect_equal(var[[2]], crunch_numeric_array_variable(
        tibble::tibble(x = c(NA_real_, NA_real_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        description = "DESC 2"
    ))
})

test_that("can create crunch_numeric_array_variable - From crunch_numeric_array_variables explicit axes (2D)", {
    var <- crunch_numeric_array_variable(
        tibble::tibble(
            num1 = crunch_numeric_array_variable(
                tibble::tibble(x = crunch_numeric_variable(c(1, 2), label = "ARR X", description = "DESC X")),
                label = "ARR 1",
                description = "DESC 1"
            ),
            num2 = crunch_numeric_array_variable(
                tibble::tibble(x = crunch_numeric_variable(c(NA, NA), label = "ARR X", description = "DESC X")),
                label = "ARR 2",
                description = "DESC 2"
            )
        ),
        label = "Num Array 1",
        axes = list(
            tibble::tibble(
                value = c("num1", "num2"),
                label = c("ARR 1", "ARR 2"),
                description = c("DESC 1", "DESC 2")
            ),
            tibble::tibble(
                value = c("x"),
                label = c("ARR X"),
                description = c("DESC X")
            )
        ),
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_numeric_array_variable")
    expect_equal(ncol(var), 2)
    expect_equal(names(var), c("num1", "num2"))
    expect_equal(label(var), "Num Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_s3_class(var[[1]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "double")
    expect_equal(var[[1]], crunch_numeric_array_variable(
        tibble::tibble(x = c(1, 2)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "double")
    expect_equal(var[[2]], crunch_numeric_array_variable(
        tibble::tibble(x = c(NA_real_, NA_real_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        description = "DESC 2"
    ))
})

test_that("can create crunch_numeric_array_variable - From crunch_numeric_array_variables implied axes (2D)", {
    var <- crunch_numeric_array_variable(
        tibble::tibble(
            num1 = crunch_numeric_array_variable(
                tibble::tibble(x = crunch_numeric_variable(c(1, 2), label = "ARR X", description = "DESC X")),
                label = "ARR 1",
                description = "DESC 1"
            ),
            num2 = crunch_numeric_array_variable(
                tibble::tibble(x = crunch_numeric_variable(c(NA, NA), label = "ARR X", description = "DESC X")),
                label = "ARR 2",
                description = "DESC 2"
            )
        ),
        label = "Num Array 1",
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_numeric_array_variable")
    expect_equal(ncol(var), 2)
    expect_equal(names(var), c("num1", "num2"))
    expect_equal(label(var), "Num Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_s3_class(var[[1]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "double")
    expect_equal(var[[1]], crunch_numeric_array_variable(
        tibble::tibble(x = c(1, 2)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_numeric_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "double")
    expect_equal(var[[2]], crunch_numeric_array_variable(
        tibble::tibble(x = c(NA_real_, NA_real_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        description = "DESC 2"
    ))
})

test_that("crunch_numeric_array_variable validations", {
    crunch_numeric_array_variable(
        tibble::tibble(num1 = crunch_numeric_variable(letters, label = "Num 1")),
        label = list(1),
        description = 35,
        notes = ~abc,
        path = identity
    ) |> expect_snapshot_error()

    crunch_numeric_array_variable(
        tibble::tibble(
            num1 = crunch_numeric_variable(1:5, label = "Num 1"),
            num2 = crunch_numeric_array_variable(
                tibble::tibble(x = 1:5,  y = 2:6),
                axes = list(data.frame(value = c("x", "y"), label = c("X", "Y"), description = NA)),
                label = "Num 2"
            )
        ),
        label = "Num Array"
    ) |> expect_snapshot_error()

    crunch_numeric_array_variable(
        tibble::tibble(
            num1 = crunch_numeric_array_variable(
                tibble::tibble(x = 1:5),
                axes = list(data.frame(value = c("x"), label = c("X"), description = NA)),
                label = "Num 1"
            ),
            num2 = crunch_numeric_array_variable(
                tibble::tibble(x = 1:5, y = 2:6),
                axes = list(data.frame(value = c("x", "y"), label = c("X", "Y"), description = NA)),
                label = "Num 2"
            )
        ),
        label = "Num Array"
    ) |> expect_snapshot_error()
})

test_that("is.crunch_numeric_array_variable works", {
    expect_true(is.crunch_numeric_array_variable(ex$num_arr1))
    expect_true(is.crunch_numeric_array_variable(ex$num_arr2))
    expect_false(is.crunch_numeric_array_variable(1:3))
    expect_false(is.crunch_numeric_array_variable(ex$cat_arr1))
})

test_that("list dimensional subsets work in `[[`", {
    var <- ex$num_arr2
    var1d <- ex$num_arr1

    expect_equal(var1d[[list("num1")]], var1d[["num1"]])
    expect_equal(var[[list("a", NULL)]], var[["a"]])
    expect_equal(var[[list("a", "x")]], var[["a"]][["x"]])
    expect_equal(var[[1, list("b", "y")]], var[["b"]][[1, "y"]])
    expect_equal(var[[list(NULL, "y")]], crunch_numeric_array_variable(
        as.data.frame(lapply(var, function(x) x[["y"]])),
        label = label(var[[1]][["y"]]),
        description = description(var[[1]][["y"]]),
        axes = axes(var)[1]
    ))
})

test_that("`[[<-` with list subscripts", {
    var <- ex$num_arr1
    var[[list("num1")]] <- NULL
    expect_equal(
        axes(var),
        list(tibble::tibble(value = "num2", label = "X 2", description = NA_character_))
    )

    var <- ex$num_arr2
    var[[list("a", NULL)]] <- NULL
    expect_equal(
        axes(var),
        list(
            tibble::tibble(
                value = c("b", "c"),
                label = c("AX B", "AX C"),
                description = NA_character_
            ),
            tibble::tibble(
                value = c("x", "y"),
                label = c("AX X", "AX Y"),
                description = NA_character_
            )
        )
    )

    var <- ex$num_arr2
    var[[list(NULL, "x")]] <- NULL
    expect_equal(
        axes(var),
        list(
            tibble::tibble(
                value = c("a", "b", "c"),
                label = c("AX A", "AX B", "AX C"),
                description = NA_character_
            ),
            tibble::tibble(
                value = c("y"),
                label = c("AX Y"),
                description = NA_character_
            )
        )
    )

    var <- ex$num_arr2
    var[[list("d", NULL)]] <- tibble::tibble(x = 1000, y = 1001)
    expect_equal(
        axes(var),
        list(
            tibble::tibble(
                value = c("a", "b", "c", "d"),
                label = c("AX A", "AX B", "AX C", "d"),
                description = NA_character_
            ),
            tibble::tibble(
                value = c("x", "y"),
                label = c("AX X", "AX Y"),
                description = NA_character_
            )
        )
    )

    var <- ex$num_arr2
    var[[list(NULL, "x")]] <- NULL
    expect_equal(
        axes(var),
        list(
            tibble::tibble(
                value = c("a", "b", "c"),
                label = c("AX A", "AX B", "AX C"),
                description = NA_character_
            ),
            tibble::tibble(
                value = c( "y"),
                label = c("AX Y"),
                description = NA_character_
            )
        )
    )


})

test_that("can do math on crunch_numeric_array variables", {
    expect_equal(mean(ex$num_arr1, na.rm = TRUE), tibble::tibble(num1 = 12.55, num2 = 54.5))
    expect_equal(
        mean(ex$num_arr2, na.rm = TRUE),
        tibble::tibble(
            a = tibble::tibble(x = 107/3, y = -2),
            b = tibble::tibble(x = 98, y = -2),
            c = tibble::tibble(x = 151/3, y = -44/3)
        )
    )
    expect_equal(median(ex$num_arr1, na.rm = TRUE), tibble::tibble(num1 = 1.4, num2 = 54.5))
    expect_equal(
        median(ex$num_arr2, na.rm = TRUE),
        tibble::tibble(
            a = tibble::tibble(x = 42, y = -2),
            b = tibble::tibble(x = 98, y = -2),
            c = tibble::tibble(x = 50, y = -19)
        )
    )
    expect_equal(
        quantile(ex$num_arr1, na.rm = TRUE),
        tibble::tibble(
            num1 = c(1.25, 1.325, 1.4, 18.2, 35) |> setNames(c('0%', '25%', '50%', '75%', '100%')),
            num2 = c(9, 31.75, 54.5, 77.25, 100) |> setNames(c('0%', '25%', '50%', '75%', '100%'))
        )
    )
    expect_equal(
        quantile(ex$num_arr2, probs = c(0.25, 0.75), na.rm = TRUE),
        tibble::tibble(
            a = tibble::tibble(
                x = c(32, 42.5) |> setNames(c("25%", "75%")),
                y = c(-2.5, -1.5) |> setNames(c("25%", "75%"))
            ),
            b = tibble::tibble(
                x = c(97.5, 98.5) |> setNames(c("25%", "75%")),
                y = c(-2, -2) |> setNames(c("25%", "75%"))
            ),
            c = tibble::tibble(
                x = c(50, 50.5) |> setNames(c("25%", "75%")),
                y = c(-19.5, -12) |> setNames(c("25%", "75%"))
            )
        )
    )
})
