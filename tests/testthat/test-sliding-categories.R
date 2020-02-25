context("Sliding Categories")

# Don't have great test fixtures for this so test helper functions hard
test_that("slide_over works in typical cases", {
  expect_equal(
    slide_over(letters[1:5], step = 1, width = 2, complete = TRUE),
    list(c("a", "b"), c("b", "c"), c("c", "d"), c("d", "e"))
  )
  
  expect_equal(
    slide_over(letters[1:5], step = 1, width = 2, complete = FALSE),
    list(c("a", "b"), c("b", "c"), c("c", "d"), c("d", "e"), "e")
  )
  
  expect_equal(
    slide_over(letters[1:5], step = 3, width = 1, complete = TRUE),
    list(c("a"), c("d"))
  )
  
  expect_equal(
    slide_over(letters[1:5], step = 2, width = 3, complete = TRUE),
    list(c("a", "b", "c"), c("c", "d", "e"))
  )
  
  expect_equal(
    slide_over(letters[1:5], step = 1, width = 100, complete = TRUE),
    list()
  )
})

test_that("slide_over fails correctly", {
  expect_error(
    slide_over(NULL, step = 1, width = 2, complete = TRUE),
    "No categories found to slide over"
  )
  
  expect_error(
    slide_over(letters[1:5], step = 0, width = 2, complete = TRUE),
    "'step' must be a positive number"
  )
  
  expect_error(
    slide_over(letters[1:5], step = 2, width = 0, complete = TRUE),
    "'width' must be a positive number"
  )
})

test_that("sliding_subvar_meta works correctly", {
  meta <- sliding_subvar_meta(
    list(
      name = c("a - c", "c - e"),
      description = function(x) paste(x, collapse = ", ")
    ), 
    list(c("a", "b", "c"), c("c", "d", "e"))
  )
  expect_equal(length(meta), 2)
  expect_equal(names(meta), c("name", "description"))
  expect_is(meta[[1]], "function")
  expect_is(meta[[2]], "function")
  expect_equal(meta[[1]](c("a", "b", "c")), "a - c")
  expect_equal(meta[[1]](c("c", "d", "e")), "c - e")
  expect_equal(meta[[2]](c("a", "b", "c")), "a, b, c")
  expect_equal(meta[[2]](c("c", "d", "e")), "c, d, e")
})

test_that("sliding_subvar_meta fails correctly", {
  expect_error(
    meta <- sliding_subvar_meta(
      list(name = 1), 
      list(c("a", "b", "c"), c("c", "d", "e"))
    ), 
    "Expected either function or character vector"
  )
  
  expect_error(
    meta <- sliding_subvar_meta(
      list(name = c("a - c", "d - e", "XXX")), 
      list(c("a", "b", "c"), c("c", "d", "e"))
    ), 
    "Expected subvariable meta object to be of length"
  )
})

with_mock_crunch({
  ds <- loadDataset("ECON.sav")
  
  test_that("slidingCategories creates expected VariableDefinition", {
    expect_equal(
      slideCategories(ds$gender, 1, 1, name = c("m", "f")),
      list(
        SubvarDef(ds$gender %in% "Male", name = "m"),
        SubvarDef(ds$gender %in% "Female", name = "f")
      )
    )
    
    expect_equal(
      slideCategories(ds$gender, 1, 2, name = "b"),
      list(
        SubvarDef(ds$gender %in% c("Male", "Female"), name = "b")
      )
    )
    
    expect_equal(
      slideCategories(ds$gender, 1, 2, name = function(x) paste(x, collapse = "-"), useNA = TRUE),
      list(
        SubvarDef(ds$gender %in% c("Male", "Female"), name = "Male-Female"),
        SubvarDef(ds$gender %in% c("Female", "No Data"), name = "Female-No Data")
      )
    )
  })
})