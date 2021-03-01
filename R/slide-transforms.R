#' Helper for creating slide dimension transformations for dashboards and exports
#'
#' When displayed in a Crunch Dashboard or exported, crunch slides can have transformations
#' that customize their display. This is a helper to form the correct data structure for
#' the functions [`newSlide()`] pr setting the transformation directly. For more details see the
#' [API documentation](
#' https://crunch.io/api/reference/#post-/datasets/-dataset_id-/decks/-deck_id-/slides/)
#'
#' @param palette A vector of color RGB hex color codes that will be used for the color of
#' graphs in the dashboard.
#' @param hide A vector of category names/ids or subvariable names/aliases to hide from display
#' @param renames A named vector of category names/ids or subvariable names/aliases to override
#' their default values
#' @param order A vector of category names/ids or subvariable names/aliases to override the
#' default ordering of the dimension.
#' @param name A name for the dimension, overrides the variable's name
#' @param description A description for the dimension, overrides the variable's description
#' @param ... Other arguments, passed directly to the API for future expansion
#'
#' @export
slideTransform <- function(
    palette = NULL,
    hide = NULL,
    renames = NULL,
    order = NULL,
    name = NULL,
    description = NULL,
    # insertions = NULL, # TODO: catch R up to API insertions
    ...
) {
    # Will need to use slide's query to convert to structure API needs
    # (eg get ids from names) so just store as list for now
    out <- list(
        palette = palette,
        renames = renames,
        hide = hide,
        order = order,
        name = name,
        description = description,
        ...
    )
    class(out) <- c("SlideTransform", class(out))
    out
}

prepareSlideTransform <- function(transform, dim, cube) {
    # User specified transform without helper (and so send along unmodified)
    if (!inherits(transform, "SlideTransform")) return(transform)

    needs_element <- !is.null(transform$palette) ||
        !is.null(transform$renames) ||
        !is.null(transform$hide)

    if ("elements" %in% names(transform) && needs_element) {
        halt("Cannot specify `palette`, `renames`, or `hide` if `elements` is provided")
    }

    dim_types <- c("rows_dimension", "columns_dimension", "tabs_dimension")
    if (!dim %in% dim_types) {
        halt(
            "Expected slideTransform dim to be one of ",
            paste0(dQuote(dim_types), collapse = ", "),
            "but got ", dQuote(dim)
        )
    }

    # Convert renames/hide/order to dimension id (eg cat id / subvar)
    dim_num <- match(dim, dim_types)

    crosswalk <- getDimIDCrosswalk(cube, dim_num)
    transform$renames <- standardizeTransformIDs(transform$renames, crosswalk, "renames")
    transform$hide <- standardizeTransformIDs(transform$hide, crosswalk, "hide")
    transform$order <- standardizeTransformIDs(transform$order, crosswalk, "order")

    # Prepare palette
    if (!is.null(transform$palette)) {
        ids <- transform$order %||% crosswalk[[1]]

        len <- seq_len(min(length(ids), length(transform$palette)))
        transform$palette <- setNames(ids[len], transform$palette[len])
    }

    # Form elements
    elements <- lapply(crosswalk$id, function(id) {
        out <- list()
        if (any(transform$renames == id)) {
            out$name <- names(transform$renames)[transform$renames == id]
        }
        if (any(transform$hide == id)) {
            out$hide <- TRUE
        }
        if (any(transform$palette == id)) {
            out$fill <- names(transform$palette)[transform$palette == id]
        }
        out
    })
    names(elements) <- crosswalk$id
    elements <- elements[lengths(elements) > 0]

    # Form return object
    out <- Filter(function(x) !is.null(x), transform)
    # And remove components of element + add element if formed
    out <- out[setdiff(names(out), c("palette", "renames", "hide"))]
    if (length(elements) > 0) out$elements <- elements
    out
}

getDimIDCrosswalk <- function(cube, dim_num) {
    cube_dim <- cubeDims(cube)[[dim_num]]
    cube_dim_type <- getDimTypes(cube)[dim_num]
    if (cube_dim_type %in% c("categorical", "ca_categories")) {
        data.frame(
            id = vapply(cube_dim$references$categories, function(x) x$id, numeric(1)),
            name = vapply(cube_dim$references$categories, function(x) x$name, character(1)),
            stringsAsFactors = FALSE
        )
    } else if (cube_dim_type %in% c("ca_items", "mr_items")) {
        data.frame(
            id = vapply(cube_dim$references$subreferences, function(x) x$alias, character(1)),
            alias = vapply(cube_dim$references$subreferences, function(x) x$alias, character(1)),
            name = vapply(cube_dim$references$subreferences, function(x) x$name, character(1)),
            stringsAsFactors = FALSE
        )
    }
}

standardizeTransformIDs <- function(x, crosswalk, type) {
    dups <- duplicated(x)
    if (any(dups)) halt("Found duplicated transform ids for", type, ": ", paste0(unique(x[dups]), collapse = ", "))

    if (!is.character(x)) return(x)

    missing_matches <- lapply(names(crosswalk)[-1], function(id_type) {
        setdiff(x, crosswalk[[id_type]])
    })
    names(missing_matches) <- names(crosswalk)[-1]

    if (!any(lengths(missing_matches) == 0)) {
        error_text <- vapply(names(missing_matches), function(name) {
            bad_vals <- if (length(missing_matches[[name]]) == length(x)) "All" else  paste(missing_matches[[name]], collapse = ", ")
            paste0("  - ", name, ": ", bad_vals, collapse = ", ")
        }, character(1))
        halt(
            "Could not match transform ids for ", type, " to a set of expected values:\n",
            paste0(error_text, collapse = "\n")
        )
    }

    match_type <- crosswalk[[min(which(lengths(missing_matches) == 0)) + 1]]
    out <- crosswalk[[1]][match(x, match_type)]
    names(out) <- names(x)
    out
}
