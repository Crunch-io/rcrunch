verify_file_path <- function(path, file) {
    file <- file.path(path, file)
    stopifnot(file.exists(file))
    file
}

str_trunc <- function(x, widths, subtle = FALSE) {
    str_width <- pillar::get_extent(x)
    too_wide <- which(!is.na(x) & str_width > widths)

    continue_symbol <- cli::symbol$continue
    if (subtle) continue_symbol <- pillar::style_subtle(continue_symbol)

    truncated <- Map(x[too_wide], widths[too_wide], f = function(item, wid) {
        paste0(crayon::col_substr(item, 1, wid - 1), continue_symbol)
    })
    truncated <- as.vector(truncated, "character")
    x[too_wide] <- truncated

    x
}

trim_ws_rhs <- function(x) {
    sub("[ \t\r\n]+$", "", x)
}

trim_ws_lhs <- function(x) {
    sub("^[ \t\r\n]+", "", x)
}

pad_space <- function(n) {
    vapply(n, function(x) paste(rep(" ", x), collapse = ""), "")
}

paste_with_align <- function(x, y, lhs_ws, rhs_ws) {
    y_wid <- pillar::get_extent(y)
    added_chars <- max(y_wid - rhs_ws)
    rhs_ws <- added_chars - (y_wid - rhs_ws)

    paste0(pad_space(lhs_ws), x, y, pad_space(rhs_ws))
}

pillar_print_pkgs_available <- function() {
    requireNamespace("crayon", quietly = TRUE) &&
        requireNamespace("cli", quietly = TRUE) &&
        requireNamespace("utf8", quietly = TRUE)
}

#' Get credentials from clipboard as environmental variables
#'
#' Some S3 buckets offer to add environmental variables to your clipboard,
#' this function allows directly adding those to the current R session as
#' environment variables.
#' @export
cr_clip_to_env <- function() {
    env_vars <- clipr::read_clip()
    env_vars <- strsplit(env_vars, "\n")
    env_vars <- gsub("^((export )|(SET )|(\\$Env:))", "", env_vars)
    env_vars <- regmatches(env_vars, regexec("^(.+?)=\"?(.+?)\"?$", env_vars))

    if (!all(lengths(env_vars) == 3)) cli::cli_abort("Invalid environment variables format found in clipboard")
    args <- setNames(
        purrr::map(env_vars, 3),
        purrr::map_chr(env_vars, 2)
    )
    cli::cli_alert_info("Setting environment variables found in clipboard: {names(args)}")

    do.call(Sys.setenv, args)
}
