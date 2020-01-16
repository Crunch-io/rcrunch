#' @include show.R
NULL

#' @importFrom crayon col_nchar has_style col_align
prettyPrint2d <- function(array, row_styles = NULL, col_styles = NULL) {
    # TODO: warn if bold is used, since it messes up alignment?

    # if the array is [n,1], enforce that explicitly:
    if (length(dim(array)) == 1) {
        array <- array(array,
            dim = c(length(array), 1),
            dimnames = c(dimnames(array), NULL)
        )
    }

    if (!is.null(row_styles) && length(row_styles) != nrow(array)) {
        row_styles <- NULL
        warning("The number of row styles doesn't match the number of rows")
    }
    if (!is.null(col_styles) && length(col_styles) != ncol(array)) {
        col_styles <- NULL
        warning("The number of column styles doesn't match the number of columns")
    }

    # calculate column widths including headers, calculate column var title
    col_var_name <- names(dimnames(array))[2] %||% ""
    col_var_name_width <- col_nchar(col_var_name)

    col_widths <- apply(array, 2, function(x) max(col_nchar(x), na.rm = TRUE))
    col_names <- col_nchar(colnames(array) %||% "")
    col_header_width <- max(col_names, na.rm = TRUE)
    col_widths <- pmax(col_widths, col_header_width,
        c(col_var_name_width, rep(0, length(col_header_width) - 1)),
        na.rm = TRUE
    )
    col_var_name_padded <- col_align(col_var_name, col_widths[1], align = "left")


    # calculate row name widths,  calculate row var title
    row_var_name <- names(dimnames(array))[1] %||% ""
    row_header_width <- max(col_nchar(c(rownames(array), row_var_name)))
    row_var_name_padded <- col_align(row_var_name, row_header_width, align = "left")

    # format rows (before styling)
    cell_widths <- matrix(col_widths,
        nrow = nrow(array),
        ncol = ncol(array), byrow = TRUE
    )
    padded_array <- array(col_align(array, cell_widths, align = "right"),
        dim = dim(array), dimnames = dimnames(array)
    )

    ### style columns
    # make headers
    col_heads <- col_align(colnames(array), col_widths, align = "right")
    for (i in seq_len(ncol(padded_array))) {
        if (!is.null(col_styles[[i]])) {
            # style column
            padded_array[, i] <- applyStyles(padded_array[, i], col_styles[[i]])
            # style header
            col_heads[i] <- applyStyles(col_heads[i], col_styles[[i]])
        }
    }
    # paste headers together
    col_var <- paste0(pad(row_header_width), col_var_name_padded, collapse = "")
    col_heads <- paste0(c(row_var_name_padded, col_heads), collapse = " ")

    ### style rows
    # TODO: left align headers?
    row_heads <- col_align(rownames(array), row_header_width, align = "right")
    rows <- lapply(seq_len(nrow(padded_array)), function(i) {
        rw <- c(row_heads[i], padded_array[i, ])
        rw <- paste0(rw, collapse = " ")
        if (!is.null(row_styles[i])) {
            rw <- applyStyles(rw, row_styles[[i]])
        }
        return(rw)
    })

    return(c(col_var, col_heads, rows))
}

pad <- function(n, char = " ") strrep(char, n)

# available in a more efficient implementation, with type checking in R>=3.3.0
# TODO: when it's reasonable / other requirements require R>=3.3.0 switch to R's
# implementation
strrep <- function(char, n) paste0(rep(char, n), collapse = "")

# Remove NAs from crayon styled strings
nonas <- function(string, to_remove = c("NA")) {
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


# make styles based on transforms and categories
transformStyles <- function(trans, cats) {
    # collate categories and insertions
    all_labs <- collateCats(trans$insertions, cats)

    # make a list of styles to apply
    styles <- lapply(all_labs, function(lab) {
        if (is.Subtotal(lab) | is.SummaryStat(lab)) {
            # if subtotal or a summary stat, use the subtotal style
            return(subtotalStyle)
        } else if (is.Heading(lab)) {
            return(headingStyle)
        } else {
            return(NULL)
        }
    })
    return(styles)
}

#' @importFrom crayon make_style italic underline
headingStyle <- c(nonas, make_style("#546499"), underline) # blue with underline
subtotalStyle <- c(italic, make_style("#005e46"))

print_tree <- function(x, prefix = "", depth = 100, current_depth = 0) {
    len <- length(x)
    what <- types(x)
    these <- colorize_folder_contents(names(x), what)
    tree <- lapply(seq_len(len), function(i) {
        ## Iterate over the indices so we can distinguish the last one, which
        ## gets different treatment
        last <- i == len
        ## Alternate ASCII representation in comments, for readability (fallback?)
        # node <- ifelse(last, "`-- ", "|-- ")
        node <- ifelse(last, "\u2514\u2500\u2500 ", "\u251C\u2500\u2500 ")
        # pass <- ifelse(last, "    ", "|   ")
        pass <- ifelse(last, "    ", "\u2502   ")
        ## Prepare the current node
        out <- paste0(prefix, node, these[i])
        if (what[i] %in% c("folder", "project")) {
            ## If this one is a folder, indicate that it is
            out <- paste0(out, folderDelimiter())
            if (depth > current_depth) {
                ## And if we're not at our max depth, recurse into the folder,
                ## adding "pass" as our "prefix", and bumping our "current_depth"
                out <- c(
                    out,
                    print_tree(
                        x[[i]],
                        paste0(prefix, pass),
                        depth = depth,
                        current_depth + 1
                    )
                )
            }
        }
        return(out)
    })
    return(unlist(tree))
}

#' @rdname show
#' @export
setMethod("show", "ShojiFolder", function(object) {
    cat(formatFolderTitle(object), "\n")
    colored_print(
        names(object),
        styler = function(x) colorize_folder_contents(x, types(object)),
        empty = ifelse(is.project(object), "project(0)", "folder(0)")
    )
})

colored_print <- function(x, styler = force, empty = "") {
    ## Simulate the print.default method, which doesn't handle crayon right
    len <- length(x)
    if (len == 0) {
        ## Special case length-0 because the arithmetic below won't work
        cat(empty)
        cat("\n")
    } else {
        ## The header is like "[12345] ". Find its theoretical max width based
        ## on the max value (len) + 3 for "[] "
        header_width <- nchar(len) + 3L
        ## Each "cell" is as wide as the widest one, plus 1 for padding and 2
        ## for ""
        w <- max(nchar(x)) + 3L
        ## Find out how many we can fit on a line, allowing for the header
        n <- (getOption("width") - header_width) %/% w
        if (n == 0) {
            ## Handle if our widest element is wider than the width:
            ## make it 1 per line.
            n <- 1
        }
        ## And thus how many rows we need
        rows <- ceiling(len / n)

        ## Quote and pad
        x <- col_align(paste0('"', x, '"'), w)
        ## Color
        x <- styler(x)
        ## Print
        for (i in seq_len(rows)) {
            start <- 1L + n * (i - 1)
            cat(col_align(paste0("[", start, "] "), header_width, "right"))
            cat(x[start:min(n * i, len)], sep = "")
            cat("\n")
        }
    }
}

#' @export
print.ShojiFolder <- function(x, depth = 0, verbose = FALSE,
                              pretty = !verbose & depth > 0, ...) {
    if (pretty) {
        out <- c(formatFolderTitle(x), print_tree(x, depth = depth))
        cat(out, sep = "\n")
    } else {
        ## Default: like a vector of names, but with color
        show(x)
    }
}

formatFolderTitle <- function(folder) {
    sep <- folderDelimiter()
    p <- path(folder)
    if (identical(p, sep)) {
        ## Root. Return (uncolored) "/" (or whatever delimiter)
        return(sep)
    }
    return(paste0(bold$red(p), sep))
}

#' @importFrom crayon bold cyan red
colorize_folder_contents <- function(contents, types) {
    folders <- types %in% c("folder", "project")
    contents[folders] <- bold$red(contents[folders])
    arrays <- types %in% c("multiple_response", "categorical_array")
    contents[arrays] <- cyan(contents[arrays])
    return(contents)
}
