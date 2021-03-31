# Helpers for redacting IDs with the usually randomly generated
# IDs changed to be stable

#' Redact an rcrunch dataset fixtures with stable IDs
#'
#' When using httptest to capture requests, it can be hard to
#' get reproducible results because many IDs are generated
#' on the fly without user control. These redactors allow us to
#' set them to be constant. The items must already have been
#' created on the server before the capture for the redaction
#' to work.
#'
#' Currently supported items are:
#' - user
#' - dataset
#' - variable
#' - variable folders
#' - multitables
#' - (TODO: deck/slide/analysis/filter/version/forks & more?)
#' @param ds A Crunch Dataset
#' @param desired_ds_id A string that is the desired ID for the dataset
#'
#' @examples
#' # Response redactor:
#' set_redactor(response_redactor(ds, "veg"))
#' # Also request redactor
#' set_requester(request_redactor(ds, "veg"))
response_redactor <- function(ds, desired_ds_id) {
    id_redactor <- id_response_redactor(ds, desired_ds_id)

    # Combine ID redaction with the redaction done in
    # inst/httptest/redact.R
    function(response) {
        ## Remove multipart form fields because POST sources/ sends a tmpfile path
        ## that's different every time, so the request will never match.
        response$request$fields <- NULL
        ## So that the login request isn't tied to one user's creds, ignore it in mocks
        if (response$url == "https://app.crunch.io/api/public/login/") {
            response$request$options[["postfields"]] <- NULL
        }
        response %>%
            redact_auth() %>%
            id_redactor() %>%
            gsub_response(
                "https.//app.crunch.io/api/progress/[^\"].*?/",
                "https://app.crunch.io/api/progress/"
            ) %>%
            gsub_response("([0-9a-f]{6})[0-9a-f]{26}", "\\1") ## Prune UUIDs if they slip through
    }
}

#' @rdname response_redactor
request_redactor <- function(ds, desired_ds_id) {
    id_request_redactor(ds, desired_ds_id)
}


#' Stabilize json files
#'
#' Helper function so that json files can be stable over multiple capture
#' runs. For all .json files, it sorts the keys alphabetically. Additionally,
#' can specify some items to set as constant. This function requires that the
#' json be read into R and then saved, so it is possible that it is altering
#' parts of the json not mentioned, though in testing this does not happen.
#'
#' @param capture_dir The path to the directory of captured tests.
#' @param ... One or more lists, where the first item is a filepath, and
#' the other items are lists, where the first item is a list indicating
#' a path to the item in the json using [`purrr::pluck()`] notation and
#' the second item is what the replacement value should be.
#'
#' @return Ignored, used for side effects of changing the files on disk
#' @examples
#' \dontrun{
#' # before:
#' json <- jsonlite::read_json("test1.json")
#' json$index[[1]]$current_editor_name
#' #> "Greg"
#'
#' redact_json_files(
#'     temp_dir,
#'     list(
#'         "test1.json",
#'         list(list("index", 1, "current_editor_name"), "User"),
#'     )
#' )
#'
#' # after:
#' json <- jsonlite::read_json("test1.json")
#' json$index[[1]]$current_editor_name
#' #> "User"
#' }
stabilize_json_files <- function(capture_dir, ...) {
    purrr::walk(list(...), redact_json_file, capture_dir = capture_dir)

    purrr::walk(
        fs::dir_ls(capture_dir, glob = "*.json", recurse = TRUE),
        sort_json_keys
    )
}



redact_json_file <- function(capture_dir, redaction) {
    path <- fs::path(capture_dir, redaction[[1]])
    json <- jsonlite::read_json(path, simplifyVector = FALSE)

    purrr::walk(redaction[-1], function(x) {
        purrr::pluck(json, !!!x[[1]]) <<- x[[2]]
    })

    jsonlite::write_json(json, path, auto_unbox = TRUE, null = "null", pretty = 4, digits = NA)
}

sort_json_keys <- function(path) {
    json <- jsonlite::read_json(path, simplifyVector = FALSE)
    json <- object_sort(json)
    jsonlite::write_json(json, path, auto_unbox = TRUE, null = "null", pretty = 4, digits = NA)
}

# Adapated from httptest:::object_sort
object_sort <- function(x) {
    if (is.list(x)) {
        if (!is.null(names(x))) {
            x <- x[sort(names(x))]
        }
        return(lapply(x, object_sort))
    }
    x
}

ids_from_ds <- function(ds, desired_ds_id) {
    ids <- c(
        # User ID
        "user_id" = me()@body$id,
        # Dataset ID
        setNames(crunch::id(ds), desired_ds_id),
        # Variable IDs
        setNames(
            ids(allVariables(ds))[order(aliases(allVariables(ds)))],
            sprintf("var_%02d", seq_along(allVariables(ds)))
        ),
        # Multitable IDs
        setNames(
            lapply(multitables(ds), function(mt) mt@body$id),
            sprintf("mt_%02d", seq_along(multitables(ds)))
        ),
        # Folder IDs
        ids_from_folders(ds),
        # Deck/Slide/Analyses IDs
        ids_from_decks(ds)
        # TODO: versions, filters, scripts, etc.
    )

    # rather than named list, make it 2 item list so that
    # it's easier to work with `reduce`
    purrr::imap(ids, list)
}

ids_from_folders <- function(ds) {
    out <- c(
        ids_below(cd(ds, "/")),
        ids_below(hiddenFolder(ds)),
        ids_below(privateFolder(ds))
    )

    out <- unlist(out)
    setNames(out, sprintf("vdir_%02d", seq_along(out)))
}

ids_below <- function(folder) {
    id <- folder@body$id
    subdirs <- folder[types(folder) %in% "folder"]
    out <- lapply(seq_along(subdirs), function(iii) ids_below(subdirs[[iii]]))
    c(id, out)
}

ids_from_decks <- function(ds) {
    if (length(decks(ds)) == 0) return
    deck_ids <- lapply(seq_along(decks(ds)), function(deck_num) {
        deck <- refresh(decks(ds)[[deck_num]])
        slide_ids <- lapply(seq_along(refresh(slides(deck))), function(slide_num) {
            slide <- refresh(slides(deck)[[slide_num]])
            analyses_ids <- lapply(
                seq_along(refresh(analyses(slide))),
                function(a_num) refresh(slide[[a_num]])@body$id
            )
            names(analyses_ids) <- sprintf("a%02d", seq_along(analyses_ids))
            c(slide@body$id, analyses_ids)
        })
        names(slide_ids) <- sprintf("s%02d", seq_along(slide_ids))
        c(deck@body$id, slide_ids)
    })
    names(deck_ids) <- sprintf("dk%02d", seq_along(deck_ids))

    out <- unlist(deck_ids)
    names(out) <- gsub("\\.", "", names(out))
    out
}

id_response_redactor <- function(ds, desired_ds_id) {
    ids <- ids_from_ds(ds, desired_ds_id)


    function(x) {
        reduce(ids, ~gsub_response(.x, .y[[1]], .y[[2]]), .init = x)
    }
}

id_request_redactor <- function(ds, desired_ds_id) {
    ids <- ids_from_ds(ds, desired_ds_id)


    function(x) {
        reduce(ids, ~gsub_request(.x, .y[[1]], .y[[2]]), .init = x)
    }
}
