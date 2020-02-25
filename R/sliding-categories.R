#' Create sliding subvariable definitions 
#'
#' Create 
#'
#' @param variable A categorical crunch variable
#' @param step number of categories between starting points of groups
#' @param width number of categories wide the grouping should be
#' @param ... additional attributes to be included in the `SubvariableDefinition`,
#'     can be either functions that take the category names to be included in the 
#'     sliding group and returns a single string, or a character vector the same length 
#'     as the number of subvariables that will be created.
#' @param complete whether to only include category groupings that are as wide as width
#'     (defaults to `TRUE`)
#' @param useNA whether to use missing categories from the original variable (defaults
#'     to `FALSE`)
#'
#' @return A list of `SubvariableDefinition`s appropriate for use in `deriveArray()`
#' @export
#'
#' @examples
#' \dontrun{
#' login()
#' data <- data.frame(
#'     wave = factor(c("a", "b", "c", "d", "e"))
#' )
#' 
#' ds <- newDataset(data, "Sliding Categories")
#' 
#' # Make an MR variable where subvariable is 1 step apart, and with 3 categories wide
#' # and name subvariables with vector 
#' ds$wave_step1_wide3 <- deriveArray(
#'    slideCategories(ds$wave, step = 1, width = 3, name = c("a - c", "b - d", "c - e")),
#'    "Sliding example 1"
#' )
#' 
#' # You can also make names (and other subvariable metadata like alias or description) 
#' # with a function:
#' ds$wave_step2_wide2 <- deriveArray(
#'    slideCategories(
#'      ds$wave, 
#'      step = 2, 
#'      width = 2, 
#'      name = function(x) paste(x[1], "-", x[length(x)])
#'    ),
#'    "Sliding example 2"
#' )
#' }
slideCategories <- function(variable, step, width, ..., complete = TRUE, useNA = FALSE) {
  cats <- categories(variable)
  if (!useNA) cats <- cats[!is.na(cats)]
  
  cat_groups <- slide_over(names(cats), step, width, complete)
  
  subvar_meta <- sliding_subvar_meta(list(...), cat_groups)

  lapply(seq_along(cat_groups), function(iii) {
    call <- list(x = variable %in% cat_groups[[iii]])
    for (nnn in names(subvar_meta)) {
      call[[nnn]] <- subvar_meta[[nnn]](cat_groups[[iii]])
    }
    do.call(SubvariableDefinition, call)
  })
}

slide_over <- function(x, step, width, complete) {
  if (length(x) == 0) halt("No categories found to slide over")
  if (step < 1) halt("'step' must be a positive number")
  if (width < 1) halt("'width' must be a positive number")
  start <- seq(1, length(x), by = step)
  out <- lapply(start, function(iii) {
    sequence <- seq(iii, iii + width - 1)
    sequence <- sequence[sequence <= length(x)]
    x[sequence]
  })
  if (complete) out <- out[lengths(out) == width]
  out
}

# subvariable metadata can be either vectors the same length as the number of subvariables
# created or functions that take the categories and return a string.
# Convert them all to be functions so we can apply them
sliding_subvar_meta <- function(subvar_meta, cat_groups) {
  subvar_meta <- lapply(subvar_meta, function(meta) {
    if (is.function(meta)) return(meta)
    if (!is.character((meta))) {
      halt("Expected either function or character vector to describe sliding subariable's metadata.")
    }
    if (length(meta) != length(cat_groups)) {
      halt(
        "Expected subvariable meta object to be of length ", length(cat_groups), 
        " but found object of length ", length(meta), "."
      )
    }
    
    function(x) {
      meta[match(list(x), cat_groups)]
    }
  })
}