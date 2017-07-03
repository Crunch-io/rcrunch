#' Conditional transformation
#'
conditionalTransform <- function (cases, values, name) {
    # TODO: change to case formula ~ value format

    if (length(cases) != length(values)) {
        halt(dQuote("cases"), " and ", dQuote("values"), "must be the same length")
    }

    n_rows <- nrow(CrunchDataset(crGET(datasetReference(cases[[1]]))))

    case_indices <- lapply(cases, which)

    # dedpulicate indices, favoring the first observation
    case_indices <- lapply(seq_along(case_indices), function(i) {
        setdiff(case_indices[[i]], unlist(case_indices[seq_len(i-1)]))
    })

    values_to_fill <- Map(function(ind, var) {
        as.vector(var[ind])
    }, ind = case_indices, var = values)

    # setup NAs for as default
    result <- rep(NA, n_rows)

    for (i in seq_along(case_indices)) {
        vals <- as.character(values_to_fill[[i]])
        result[case_indices[[i]]] <- vals
    }

    return(result)
}