#' @importFrom crayon col_nchar has_style col_align
prettyPrint2d <- function (ary, row_styles = NULL, col_styles = NULL) {
    # TODO: warn if bold is used, since it messes up alignment?

    # if the array is [n,1], enforce that explicitly:
    if (length(dim(ary)) == 1) {
        ary <- array(ary, dim = c(length(ary), 1),
                     dimnames = c(dimnames(ary), NULL))
    }

    if (!is.null(row_styles) && length(row_styles) != nrow(ary)) {
        halt("The number of row styles doesn't match the number of rows")
    }
    if (!is.null(col_styles) && length(col_styles) != ncol(ary)) {
        halt("The number of column styles doesn't match the number of columns")
    }

    # calculate column widths including headers, calculate column var title
    col_var_name <- names(dimnames(ary))[2] %||% ""
    col_var_name_width <- col_nchar(col_var_name)

    col_widths <- apply(ary, 2, function(x) max(col_nchar(x), na.rm = TRUE))
    col_names <- col_nchar(colnames(ary) %||% "")
    col_header_width <- max(col_names, na.rm = TRUE)
    col_widths <- pmax(col_widths, col_header_width,
                       c(col_var_name_width, rep(0, length(col_header_width)-1)),
                         na.rm = TRUE)
    col_var_name_padded <- col_align(col_var_name, col_widths[1], align = "left")


    # calculate row name widths,  calculate row var title
    row_var_name <- names(dimnames(ary))[1] %||% ""
    row_header_width <- max(col_nchar(c(rownames(ary), row_var_name)))
    row_var_name_padded <- col_align(row_var_name, row_header_width, align = "left")

    # format rows (before styling)
    cell_widths <- matrix(col_widths, nrow = nrow(ary),
                          ncol = ncol(ary), byrow = TRUE)
    padded_ary <- array(col_align(ary, cell_widths, align = "right"),
                        dim = dim(ary), dimnames = dimnames(ary))



    ### style columns
    # make headers
    col_heads <- col_align(colnames(ary), col_widths, align = "right")
    for (i in seq_len(ncol(padded_ary))) {
        if (!is.null(col_styles[[i]])) {
            # style column
            padded_ary[,i] <- applyStyles(padded_ary[,i], col_styles[[i]])
            # style header
            col_heads[i] <- applyStyles(col_heads[i], col_styles[[i]])
        }
    }
    # paste headers together
    col_var <- paste0(pad(row_header_width), col_var_name_padded, collapse = "")
    col_heads <- paste0(c(row_var_name_padded, col_heads), collapse = " ")


    ### style rows
    # TODO: left align headers?
    row_heads <- col_align(rownames(ary), row_header_width, align = "right")
    rows <- lapply(seq_len(nrow(padded_ary)), function (i) {
            rw <- c(row_heads[i], padded_ary[i,])
            rw <- paste0(rw, collapse = " ")
            if (!is.null(row_styles[i])) {
                rw <- applyStyles(rw, row_styles[[i]])
            }
            return(rw)
        })

    return(c(col_var, col_heads, rows))
}

pad <- function (n, char = " ") strrep(char, n)

# Remove NAs from crayon styled strings
nonas <- function (string, to_remove = c("NA")) {
    for (rm in to_remove) {
        # respect ANSI codes?
        string <- gsub(rm, strrep(" ", nchar(rm)), string)
    }

    return(string)
}

# apply multiple crayon styles (or other functions in sequence)
applyStyles <- function(string, styles = NULL) {
    if (is.null(styles) || length(styles) == 0) {
        return(string)
    }

    if (!is.list(styles)) {
        return(styles(string))
    }

    # apply the first style
    first_style <- styles[1]
    string <- first_style[[1]](string)

    rest_styles <- styles[-1]

    return(applyStyles(string, rest_styles))
}

# pretty print for Crunch cube that translates transform into styles, and applies them
