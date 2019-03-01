cleanImportedArray <- function (variable) {
    if (length(subvariables(variable)) > 1) {
        MIN_PREFIX_LENGTH <- 20 # TODO: tune/make configurable
        prefix <- findCommonPrefix(names(subvariables(variable)))
        # If length of the common stem is enough, extract it,
        # remove it from the subvar names,
        # remove trailing whitespace/punctuation,
        # and set it as variable description.
        if (nchar(prefix) >= MIN_PREFIX_LENGTH) {
            # Use wildcard regexp with length just in case there are special chars in prefix.
            # We already know that the prefix matches.
            re <- paste0("^.{", nchar(prefix), "}")
            names(subvariables(variable)) <- sub(re, "", names(subvariables(variable)))
            # Now, remove whitespace and some punctuation from end of prefix, but
            # don't remove a question mark or other reasonable punctuation
            prefix <- sub("[[:space:]\\-\\:;]*$", "", prefix)
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
