#' Clean up an array variable imported from a lossy file format
#'
#' Array and multiple-response variables coming in from SPSS or other file
#' formats generally need some work to reconstruct the "right" metadata because
#' they have to shove both parent and subvariable metadata into the "varlabels"
#' of the subvariables. This often follows a pattern of having varlabels with a
#' prefix containing the parent question wording (description) and a suffix that
#' is the actual response label.
#'
#' This function detects this prefix and reconstructs what may have been the
#' original array definition.
#'
#' @param variable An array Variable
#' @param min.prefix.length Integer: how many characters long does the common
#' string need to be in order to consider it significant enough to use? Default
#' is 20.
#' @return `variable` with edits pushed to the API. A common prefix on
#' subvariable names is extracted and set as the variable's description.
#' @export
cleanImportedArray <- function (variable, min.prefix.length=20) {
    if (length(subvariables(variable)) > 1) {
        prefix <- findCommonPrefix(names(subvariables(variable)))
        # If length of the common stem is enough, extract it,
        # remove it from the subvar names,
        # remove trailing whitespace/punctuation,
        # and set it as variable description.
        if (nchar(prefix) >= min.prefix.length) {
            # Use wildcard regexp with length just in case there are special chars in prefix.
            # We already know that the prefix matches.
            re <- paste0("^.{", nchar(prefix), "}")
            names(subvariables(variable)) <- sub(re, "", names(subvariables(variable)))
            # Now, remove whitespace and some punctuation from end of prefix, but
            # don't remove a question mark or other reasonable punctuation
            prefix <- sub("[[:space:]\\-\\:;|]*$", "", prefix)
            description(variable) <- prefix
        }
    }
    return(variable)
}

findCommonPrefix <- function (x) {
    # Find the shortest one and start with that
    step_size <- prefix_length <- min(nchar(x))
    out <- ""
    while (step_size > 0 && prefix_length > 0) {
        # Bisect to find the common stem
        step_size <- round(step_size / 2)
        stems <- unique(substr(x, 1, prefix_length))
        if (length(stems) == 1) {
            # Keep this one
            out <- stems
            # Try longer
            prefix_length <- prefix_length + step_size
        } else {
            # Try shorter
            prefix_length <- prefix_length - step_size
        }
    }
    return(out)
}
