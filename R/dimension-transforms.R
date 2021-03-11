#' Helper for creating slide dimension transformations for dashboards and exports
#'
#' When displayed in a Crunch Dashboard or exported, crunch slides can have transformations
#' that customize their display. This is a helper to form the correct data structure for
#' the functions [`newSlide()`] for setting the transformation directly. For more details see the
#' [API documentation](
#' https://crunch.io/api/reference/#post-/datasets/-dataset_id-/decks/-deck_id-/slides/)
#'
#' @param colors A vector of color RGB hex color codes that will be used for the color
#' of graphs in the dashboard (used in the order of appearance of categories/subvariables).
#' @param hide A vector of category names/ids or subvariable names/aliases to hide from display
#' @param rename A named vector of category names/ids or subvariable names/aliases to override
#' their default values
#' @param order A vector of category names/ids or subvariable names/aliases to override the
#' default ordering of the dimension.
#' @param name A name for the dimension, overrides the variable's name
#' @param description A description for the dimension, overrides the variable's description
#' @param ... Other arguments, passed directly to the API for future expansion
#'
#' @examples
#' \dontrun{
#' # Hiding an element
#' transform(slide) <- list(rows_dimension = makeDimTransform(hide = "Neutral"))
#'
#' # Setting pre-specified colors
#' transform(slide) <- list(rows_dimension = makeDimTransform(
#'      colors = c("#af8dc3", "#f7f7f7", "#7fbf7b")
#' ))
#'
#' # Reordering & renaming elements
#' transform(slide) <- list(
#'      rows_dimension = makeDimTransform(
#'          rename = c("V. Good" = "Very Good", "V. Bad" = "Very Bad"),
#'          order = 5:1
#'      ),
#'      columns_dimension = makeDimTransform(order = c("Brand X", "Brand A", "Brand B"))
#' ))
#' }
#'
#' @export
makeDimTransform <- function(
    colors = NULL,
    hide = NULL,
    rename = NULL,
    order = NULL,
    name = NULL,
    description = NULL,
    # insertions = NULL, # TODO: catch R up to API insertions
    ...
) {
    # Will need to use slide's query to convert to structure API needs
    # (eg get ids from names) so just store as list for now
    out <- list(
        colors = colors,
        rename = rename,
        hide = hide,
        order = order,
        name = name,
        description = description,
        ...
    )
    class(out) <- c("DimensionTransform", class(out))
    out
}


dimTransformNeedsPrep <- function(transform) {
    vapply(transform, is.DimTransform, logical(1))
}

is.DimTransform <- function(x) {
    inherits(x, "DimensionTransform")
}

prepareDimTransforms <- function(transforms, query = NULL, ds = NULL, cube = NULL) {
    if (is.null(cube)) cube <- crtabs(query, ds)
    old_names <- names(transforms)
    transforms <- lapply(old_names, function(tname) {
        if (!is.DimTransform(transforms[[tname]])) return(transforms[[tname]])
        prepareDimTransform(transforms[[tname]], tname, cube)
    })
    names(transforms) <- old_names
    if (length(transforms) > 0 && !"version" %in% names(transforms)) transforms$version <- "1.0"

    transforms
}

prepareDimTransform <- function(transform, dim, cube) {
    # User specified transform without helper (and so send along unmodified)
    if (!is.DimTransform(transform)) return(transform)

    needs_element <- !is.null(transform$colors) ||
        !is.null(transform$rename) ||
        !is.null(transform$hide)

    if ("elements" %in% names(transform) && needs_element) {
        halt("Cannot specify `colors`, `rename`, or `hide` if `elements` is provided")
    }

    dim_types <- c("rows_dimension", "columns_dimension", "tabs_dimension")
    if (!dim %in% dim_types) {
        halt(
            "Expected dimTransform dim to be one of ",
            paste0(dQuote(dim_types), collapse = ", "),
            "but got ", dQuote(dim)
        )
    }

    # Convert rename/hide/order to dimension id (eg cat id / subvar)
    dim_num <- match(dim, dim_types)

    crosswalk <- getDimIDCrosswalk(cube, dim_num)
    transform$rename <- standardizeTransformIDs(transform$rename, crosswalk, "rename")
    transform$hide <- standardizeTransformIDs(transform$hide, crosswalk, "hide")
    transform$order <- standardizeTransformIDs(transform$order, crosswalk, "order")

    # Prepare colors
    if (!is.null(transform$colors)) {
        ids <- transform$order %||% crosswalk[[1]]
        ids <- setdiff(ids, transform$hide)
        len <- seq_len(min(length(ids), length(transform$colors)))
        transform$colors <- setNames(ids[len], transform$colors[len])
    }

    # Form elements
    elements <- lapply(crosswalk$id, function(id) {
        out <- list()
        if (any(transform$rename == id)) {
            out$name <- names(transform$rename)[transform$rename == id]
        }
        if (any(transform$hide == id)) {
            out$hide <- TRUE
        }
        if (any(transform$colors == id)) {
            out$fill <- names(transform$colors)[transform$colors == id]
        }
        out
    })
    names(elements) <- crosswalk$id
    elements <- elements[lengths(elements) > 0]

    # Form return object
    out <- Filter(function(x) !is.null(x), transform)
    # And remove components of element + add element if formed
    out <- out[setdiff(names(out), c("colors", "rename", "hide"))]
    if (length(elements) > 0) out$elements <- elements
    out
}

getDimIDCrosswalk <- function(cube, dim_num) {
    elements <- getDimElements(cube, dim_num)
    if (is.categories(elements)) {
        data.frame(
            id = ids(elements),
            name = names(elements),
            stringsAsFactors = FALSE
        )
    } else {
        data.frame(
            id = aliases(elements),
            alias = aliases(elements),
            name = names(elements),
            stringsAsFactors = FALSE
        )
    }
}

getDimElements <- function(cube, dim_num) {
    cube_dim_alias <- cubeDims(cube)[[dim_num]]$references$alias
    cube_dim_type <- getDimTypes(cube)[dim_num]
    cube_vars <- variables(cube)
    dim_var_pos <- which(aliases(cube_vars) == cube_dim_alias)

    if (length(dim_var_pos) > 1) {
        # prefer categorical because even for CA it has both cats & subvars
        # but nothing's stopping you from having mr by itself, so if no cats, just grab
        # the first and hope for the best
        if (any(types(cube_vars) == "categorical")) {
            dim_var_pos <- which(
                aliases(cube_vars) == cube_dim_alias & types(cube_vars) == "categorical"
            )[1]
        } else {
            dim_var_pos <- dim_var_pos[1]
        }
    }

    var <- cube_vars[[dim_var_pos]]
    if (cube_dim_type %in% c("categorical", "ca_categories")) {
        cats <- categories(var)
        return(cats[!is.na(cats)])
    } else if (cube_dim_type %in% c("ca_items", "mr_items")) {
        return(subvariables(var))
    } else {
        halt("Unexpected cube dimension type when getting elements: ", cube_dim_type)
    }
}

standardizeTransformIDs <- function(x, crosswalk, type) {
    dups <- duplicated(x)
    if (any(dups)) {
        halt(
            "Found duplicated transform ids for", type, ": ",
            paste0(unique(x[dups]), collapse = ", ")
        )
    }

    if (!is.character(x)) return(x)

    missing_matches <- lapply(names(crosswalk)[-1], function(id_type) {
        setdiff(x, crosswalk[[id_type]])
    })
    names(missing_matches) <- names(crosswalk)[-1]

    if (!any(lengths(missing_matches) == 0)) {
        error_text <- vapply(names(missing_matches), function(name) {
            if (length(missing_matches[[name]]) == length(x)) {
                bad_vals <- "All"
            } else {
                bad_vals <- paste(missing_matches[[name]], collapse = ", ")
            }
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


# NB: A super annoying thing
# The API refers to these as `transform` even though it's a list of multiple
# The R package already has `transform**s**` generic so we use that (plus there's
# already base R function named `transform`), but this inconsistency will probably
# trip you up some day

#' @rdname analysis-methods
#' @export
setMethod("transforms", "CrunchSlide", function(x) {
    transforms(analyses(x))
})
#' @rdname analysis-methods
#' @export
setMethod("transforms", "AnalysisCatalog", function(x) {
    transforms_list <- lapply(seq_along(x), function(i) {
        transforms(x[[i]])
    })
    if (length(transforms_list) == 1) {
        return(transforms_list[[1]])
    } else {
        return(transforms_list)
    }
})
#' @rdname analysis-methods
#' @export
setMethod("transforms", "Analysis", function(x) {
    x@body$transform
})


#' @rdname analysis-methods
#' @export
setMethod("transforms<-", "CrunchSlide", function(x, value) {
    all_analyses <- analyses(x)
    transforms(all_analyses) <- value
    invisible(x)
})
#' @rdname analysis-methods
#' @export
setMethod("transforms<-", "AnalysisCatalog", function(x, value) {
    all_analyses <- lapply(seq_along(x), function(i) x[[i]])
    lapply(all_analyses, function(analysis) transforms(analysis) <- value)
    invisible(x)
})
#' @rdname analysis-methods
#' @export
setMethod("transforms<-", "Analysis", function(x, value) {
    if (any(dimTransformNeedsPrep(value))) {
        value <- prepareDimTransforms(value, cube = cube(x))
    }
    # API doesn't accept totally empty lists
    if (length(value) == 0) value <- list(version = "1.0")
    payload <- wrapEntity(body = list(transform = value))
    crPATCH(self(x), body = toJSON(payload))
    invisible(refresh(x))
})
