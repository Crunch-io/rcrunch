#' Make a CrunchBox
#'
#' CrunchBoxes allow you to publish results to the world.
#'
#' @param dataset CrunchDataset
#' @param filters FilterCatalog, or \code{NULL} for no filters. Default all
#' filters in your catalog, \code{filters(dataset)}.
#' @param brand_colors an optional vector with colors (in hex format) (or a named list with names 'primary', 'secondary', and 'message')
#' @param static_colors an optional vector of static colors (in hex format) to use for categoricals (taken in order)
#' @param category_color_lookup an optional list of category names to colors (in hex format) to use for that category (for fine grained tuning of category colors.)
#' @param ... additional metadata for the box, such as "title", "header", etc.
#' @return The URL to the newly created box.
#'
#' @examples
#'
#' \dontrun{
#' # Creating a CrunchBox with three variables
#' crunchBox(ds[c("var1", "var2", "var3")], title="New CrunchBox")
#'
#' # Creating a CrunchBox changing primary, secondary, and message brand colors
#' crunchBox(ds[c("var1", "var2", "var3")],
#'           title="Branded CrunchBox",
#'           brand_colors = c("#ff0aa4", "#af17ff", "#260aff"))
#'
#' # Creating a CrunchBox changing category-specific colors
#' crunchBox(ds[c("var1", "var2", "var3")],
#'           title="CrunchBox with category colors",
#'           category_color_lookup = list("agree" = "#ff0aa4",
#'                                        "disagree" = "#af17ff",
#'                                        "don't know" = "#260aff"))
#' }
#'
#' @seealso \code{\link{preCrunchBoxCheck}} to provide guidance on what you're including in the CrunchBox
#' @export
crunchBox <- function (dataset, filters=crunch::filters(dataset),
                       brand_colors, static_colors,
                       category_color_lookup, ...) {
    ## Validate inputs
    if (missing(dataset) || !is.dataset(dataset)) {
        halt("'dataset' must be a CrunchDataset, potentially subsetted on variables")
    }
    if (is.null(filters)) {
        ## Make it an empty filter catalog so that it has methods we want below
        filters <- FilterCatalog()
    }
    if (!inherits(filters, "FilterCatalog")) {
        halt("'filters' should be a FilterCatalog or NULL")
    }

    ## Subset on non-hidden variables only
    dataset <- dataset[names(dataset)]

    ## Check that we can compute everything without exploding the server
    nvars <- length(variables(dataset))
    nfilt <- length(filters)
    if (boxTooBig(nvars, nfilt)) {
        halt(nvars, " variable", ifelse(nvars == 1, "", "s"),
            " and ", nfilt, " filter", ifelse(nfilt == 1, "", "s"),
            " results in too many cubes to fit in the box. ",
            "Please try again with fewer of either.")
    }

    ## Construct the payload
    payload <- list(filters=lapply(urls(filters), function (x) list(filter=x)),
        ...)
    ## Add "where" after so that it no-ops if variablesFilter returns NULL (i.e. no filter)
    payload$where <- variablesFilter(dataset)

    ## Add colors if they exist to the payload
    brand_labels <- c("primary", "secondary", "message")
    if (!missing(brand_colors)) {
        if (!vectorOrList(brand_colors, "character")) {
            halt(sQuote("brand_colors"), " must be character vector or list",
                 " of characters")
        }
        if (length(brand_colors) > 3) {
            halt(sQuote("brand_colors"), " must be at most 3 elements long")
        }

        # ensure is a list of valid colors
        brand_colors <- lapply(brand_colors, validHexColor)

        # if there are no names, name them according to position
        # if there are names, check that they are the right ones.
        if (is.null(names(brand_colors))) {
            names(brand_colors) <- brand_labels[seq_along(brand_colors)]
        } else if (any(!names(brand_colors) %in% brand_labels)) {
            halt("If ", sQuote("brand_colors"), " is a named list, it must",
                 " contain only ",
                 serialPaste(dQuote(brand_labels), collapse = "and"))
        }

        payload$display_settings$palette$brand_colors <- brand_colors
    }
    if (!missing(static_colors)) {
        if (!vectorOrList(static_colors, "character")) {
            halt(sQuote("static_colors"), " must be a vector or list of characters")
        }
        static_colors <- lapply(static_colors, validHexColor)
        payload$display_settings$palette$static_colors <- static_colors
    }
    if (!missing(category_color_lookup)) {
        if (!is.list(category_color_lookup) || is.null(names(category_color_lookup))) {
            halt(sQuote("category_color_lookup"), " must be a named list")
        }
        category_color_lookup <- lapply(category_color_lookup, validHexColor)
        payload$display_settings$palette$category_lookup <- category_color_lookup
    }

    ## Send it
    out <- crPOST(shojiURL(dataset, "catalogs", "boxdata"),
        body=toJSON(do.call("wrapEntity", payload)))
    return(out)
}

## Make this a function so tests can mock it
.boxlimit <- function () 60000L

boxTooBig <- function (nvars, nfilters) {
    ## Make sure that the number of cubes the box will contain is below a threshold
    nvars * (nvars - 1) * (nfilters + 1) > .boxlimit()
}

#' Summarize any characteristics of a dataset that could make for an awkward CrunchBox
#'
#' @param dataset CrunchDataset, potentially subsetted on variables
#' @return Invisbly, the dataset. Called for side-effect of printing things.
#' @seealso \code{\link{crunchBox}}
#' @export
preCrunchBoxCheck <- function (dataset) {
    vm <- variableMetadata(dataset)[urls(variables(dataset))] ## [] for order of vars
    keeps <- aliases(vm)

    ## 1. Variable types
    suggested_types <- types(vm) %in% c("categorical", "multiple_response")
    if (!all(suggested_types)) {
        not_recommended_types <- !suggested_types
        num_types <- sum(not_recommended_types)
        cat("We recommend using only categorical and multiple_response",
            "variables.", demonstrativeCount(num_types), "an unsupported type:\n")
        print(data.frame(alias=keeps[!suggested_types],
            type=types(vm)[!suggested_types]))
    }

    ## 2. Shorter variable names will display in the menus better.
    ## Check threshold: 40 characters
    name_length <- nchar(names(vm))
    too_long_name <- name_length > 40
    if (any(too_long_name)) {
        num_too_long <- sum(too_long_name)
        cat("Shorter variable names will display in the menus better.",
            demonstrativeCount(num_too_long), "a name longer than 40 characters:\n")
        print(data.frame(alias=keeps[too_long_name],
            length=name_length[too_long_name],
            name=names(vm)[too_long_name]))
    }

    ## 3. Categories
    ## a. Too many categories won't plot well as bars. (Unless they define
    ## regions on a map.)
    ## Check threshold: 7 categories
    ## TODO: add checking for category color specifications?
    num_cats <- vapply(vm, function (x) {
        cats <- x$categories
        if (is.null(cats)) return(0)
        ## We care only about non-missing categories
        return(sum(vapply(cats, function (ctg) !isTRUE(ctg$missing), logical(1))))
    }, numeric(1))
    too_many_cats <- num_cats > 7
    if (any(too_many_cats)) {
        num_too_many <- sum(too_many_cats)
        cat("Too many categories won't plot well.",
            demonstrativeCount(num_too_many),
            "more than 7 non-missing categories:\n")
        print(data.frame(alias=keeps[too_many_cats],
            num_categories=num_cats[too_many_cats]))
    }

    ## b. Long category names won't fit well in the table headers, as bar/group
    ## labels, or in the graph legend.
    ## Check threshold: 40
    longest_cat <- vapply(vm, function (x) {
        cats <- x$categories
        if (is.null(cats)) return("")
        catnames <- vapply(cats, function (ctg) ctg$name, character(1))
        return(catnames[which.max(nchar(catnames))])
    }, character(1))
    cat_length <- nchar(longest_cat)
    too_long_cat <- cat_length > 40
    if (any(too_long_cat)) {
        num_too_long <- sum(too_long_cat)
        cat("Shorter category names will fit in the tables and graphs better.",
            demonstrativeCount(num_too_long),
            "at least one category longer than 40 characters:\n")
        print(data.frame(alias=keeps[too_long_cat],
            length=cat_length[too_long_cat],
            category=longest_cat[too_long_cat]))
    }

    ## 4. Subvariables. Because multiple_response look like categorical when
    ## plotted, we'll use the same size/length check thresholds
    num_subvars <- vapply(vm, function (x) length(x$subvariables), numeric(1))
    too_many_subvars <- num_subvars > 7
    if (any(too_many_subvars)) {
        num_too_many <- sum(too_many_subvars)
        cat("Too many subvariables won't plot well. ",
            demonstrativeCount(num_too_many),
            "more than 7 subvariables:\n")
        print(data.frame(alias=keeps[too_many_subvars],
            num_subvariables=num_subvars[too_many_subvars]))
    }

    longest_subvar <- vapply(vm, function (x) {
        cats <- x$subreferences
        if (is.null(cats)) return("")
        catnames <- vapply(cats, function (ctg) ctg$name, character(1))
        return(catnames[which.max(nchar(catnames))])
    }, character(1))
    subvar_length <- nchar(longest_subvar)
    too_long_subvar <- subvar_length > 40
    if (any(too_long_subvar)) {
        num_too_long <- sum(too_long_subvar)
        cat("Shorter subvariable names will fit in the tables and graphs better.",
            demonstrativeCount(num_too_long),
            "at least one subvariable longer than 40 characters:\n")
        print(data.frame(alias=keeps[too_long_subvar],
            length=subvar_length[too_long_subvar],
            subvariable=longest_subvar[too_long_subvar]))
    }

    invisible(dataset)
}

demonstrativeCount <- function (n, noun="variable") {
    return(ifelse(n > 1, paste("These", n, "variables have"), "This variable has"))
}

#' Get HTML for embedding a CrunchBox
#'
#' \code{\link{crunchBox}} returns a URL to the box data that it generates, but
#' in order to view it in a CrunchBox or to embed it on a website, you'll need
#' to translate that to the Box's public URL and wrap it in some HTML.
#'
#' @param box character URL of the box data, as returned by
#' \code{crunchBox}
#' @param title character title for the Box, to appear above the iframe. Default
#' is \code{NULL}, meaning no title shown
#' @param logo character URL of a logo to show instead of a title. Default is
#' \code{NULL}, meaning no logo shown. If both logo and title are provided, only
#' logo will be shown. Note also that logo must be a URL of an image hosted
#' somewhere--it cannot be a path to a local file.
#' @param ... Additional arguments, not currently used.
#' @return Prints the HTML markup to the screen and also returns it invisibly.
#' @seealso \code{\link{crunchBox}}
#' @examples
#' \dontrun{
#' box <- crunchBox(ds)
#' embedCrunchBox(box, logo="//myco.example/img/logo_200px.png")
#' }
#' @export
embedCrunchBox <- function (box, title=NULL, logo=NULL, ...) {
    iframe <- paste0('<iframe src="',
        boxdataToWidgetURL(box),
        '" width="600" height="480" style="border: 1px solid #d3d3d3;"></iframe>')
    if (!is.null(logo)) {
        iframe <- boxfig(paste0('<img src="', logo,
            '" style="height:auto; width:200px; margin-left:-4px"></img>'),
            iframe)
    } else if (!is.null(title)) {
        iframe <- boxfig(
            '<div style="padding-bottom: 12px">',
            paste0('    <span style="font-size: 18px; color: #444444; line-height: 1;">',
                title, '</span>'),
            '</div>',
            iframe)
    }
    cat(iframe, "\n")
    invisible(iframe)
}

boxdataToWidgetURL <- function (box) {
    ## Grab the box id hash from one URL and plug it into the public widget URL
    sub(".*([0-9a-f]{32}).*", "//s.crunch.io/widget/index.html#/ds/\\1/", box)
}

boxfig <- function (...) {
    ## Wrap HTML in more HTML, a <figure> tag
    paste0(
        '<figure style="text-align: left;" class="content-list-component image">\n',
        paste0("    ", c(...), "\n", collapse=""),
        '</figure>'
    )
}

validHexColor <- function (color) {
    ## Given a color string, possibly an R color name, validate it and
    ## standardize its formatting like `#AABBCC`
    if (!is.character(color)) {
        halt("A color must be a character, got ", class(color)," instead")
    }

    # if color is an R color name like "aliceblue" convert to hex
    if (color %in% colors()) {
        color <- rgb(t(col2rgb(color)/255))
    } else {
        if (!startsWith(color, "#")) {
            color <- paste0("#", color)
        }
        if (!grepl("^#[[:xdigit:]]{6,8}$", color)) {
            halt(dQuote(color), " is not a valid hex color.")
        }
    }
    # remove the last two chars if given 8 digit version
    return(substr(color, 1, 7))
}
