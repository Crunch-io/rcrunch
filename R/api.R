#' Main Crunch API handling function
#' @param http.verb character in GET, PUT, POST, PATCH, DELETE
#' @param url character URL to do the verb on
#' @param ... additional arguments passed to `GET`, `PUT`,
#' `POST`, `PATCH`, or `DELETE`
#' @param config list of config parameters. See httr documentation.
#' @param status.handlers named list of specific HTTP statuses and a response
#' function to call in the case where that status is returned. Passed to the
#' [handleAPIresponse()] function.
#' @keywords internal
crunchAPI <- function(http.verb, url, config = list(), status.handlers = list(), ...) {
    url ## force lazy eval of url
    if (isTRUE(getOption("crunch.debug"))) {
        ## TODO: work this into httpcache.log
        payload <- list(...)$body
        if (!is.null(payload)) try(cat("\n", payload, "\n"), silent = TRUE)
    }
    FUN <- get(http.verb, envir = asNamespace("httpcache"))
    x <- FUN(url, ..., config = c(get_crunch_config(), config, strip_token_if_outside(url)))
    out <- handleAPIresponse(x, special.statuses = status.handlers)
    return(out)
}

#' HTTP methods for communicating with the Crunch API
#'
#' These methods let you communicate with the Crunch API, for more background
#' see [Crunch Internals](http://crunch.io/r/crunch/articles/crunch-internals.html).
#'
#' @param ... see [`crunchAPI`] for details. `url` is the first
#' named argument and is required; `body` is also required for PUT,
#' PATCH, and POST.
#' @return Depends on the response status of the HTTP request and any custom
#' handlers.
#' @importFrom httpcache GET PUT PATCH POST DELETE
#' @name http-methods
#' @export
crGET <- function(...) crunchAPI("GET", ...)
#' @rdname http-methods
#' @export
crPUT <- function(...) crunchAPI("PUT", ...)
#' @rdname http-methods
#' @export
crPATCH <- function(...) crunchAPI("PATCH", ...)
#' @rdname http-methods
#' @export
crPOST <- function(...) crunchAPI("POST", ...)
#' @rdname http-methods
#' @export
crDELETE <- function(...) crunchAPI("DELETE", ...)

#' Do the right thing with the HTTP response
#' @param response an httr response object
#' @param special.statuses an optional named list of functions by status code.
#' @return The full HTTP response object, just the content, or any other
#' status-specific action
#' @importFrom httr content http_status
#' @keywords internal
handleAPIresponse <- function(response, special.statuses = list()) {
    warning <- get_header("Warning", response$headers)
    if (!is.null(warning)) {
        if (startsWith(warning, "299")) {
            msg <- c(
                "The API resource at ",
                response$url,
                " returned a deprecation warning. Updating to the latest version ",
                "of the package is recommended and may resolve the issue."
            )
        } else {
            msg <- c("The API resource at ", response$url, " returned a warning.")
        }

        warning(
            msg,
            " Details: ",
            warning,
            call. = FALSE
        )
    }
    code <- response$status_code
    handler <- special.statuses[[as.character(code)]]
    if (is.function(handler)) {
        invisible(handler(response))
    } else if (tolower(http_status(response)$category) == "success") {
        handleAPIsuccess(code, response)
    } else {
        handleAPIfailure(code, response)
    }
}

handleAPIsuccess <- function(code, response) {
    if (code == 202) {
        ## 202 Continue: a few cases:
        ## 1) Legacy: POST /batches/ returns Batch entity in Location, no
        ##    response content
        ## 2) Progress body with Location
        ## 3) Progress body without Location
        ## So, if there's a shoji:value response, it's a Progress, so poll it.
        ## Otherwise, return the location.
        loc <- locationHeader(response)
        if (length(response$content) > 0) {
            ## Progress URL comes in a shoji:value
            progress_url <- handleShoji(content(response))
            ## Quick validation
            if (is.character(progress_url) && length(progress_url) == 1) {
                tryCatch(pollProgress(progress_url),
                         error = function(e) {
                             ## Handle the error here so we can message the
                             ## Location header, if present
                             if (!is.null(loc)) {
                                 message("Result URL: ", loc)
                             }
                             stop(e)
                         }
                )
            }
        }
        ## Return the location header, if it exists
        invisible(loc)
    } else if (code == 201) {
        ## 201 Location: return the Location header
        return(locationHeader(response))
    } else if (code == 204 || length(response$content) == 0) {
        ## If No Content, invisibly return NULL
        invisible(NULL)
    } else {
        ## Parse the content
        return(handleShoji(content(response)))
    }
}

handleAPIfailure <- function(code, response) {
    if (code == 401) {
        halt("You are not authenticated. Please `login()` and try again.")
    } else if (code == 410) {
        halt(
            "The API resource at ",
            response$url,
            " has moved permanently. Please upgrade crunch to the ",
            "latest version."
        )
    } else if (code == 503 && response$request$method == "GET" &&
               "retry-after" %in% tolower(names(response$headers))) {
        ## Server is busy and telling us to retry the request again after
        ## some period.
        wait <- get_header("Retry-After", response$headers)
        message("This request is taking longer than expected. Please stand by...")
        Sys.sleep(as.numeric(wait))
        ## TODO: resend request headers? Or, include the request to evaluate
        ## inside this function, do match.call at the beginning, and re-eval?
        return(crGET(response$url))
    }
    msg <- http_status(response)$message
    if (code == 404) {
        # Add the URL that was "not found" (there isn't going to be any
        # useful response content message)
        msg2 <- response$url
    } else {
        msg2 <- try(content(response)$message, silent = TRUE)
    }
    if (!is.error(msg2)) {
        msg <- paste(msg, msg2, sep = ": ")
    }
    if (code == 409 && grepl("current editor", msg)) {
        halt(
            "You are not the current editor of this dataset. `unlock()` ",
            "it and try again."
        )
    }
    halt(msg)
}

get_header <- function(x, headers, default = NULL) {
    m <- tolower(names(headers)) == tolower(x)
    if (any(m)) {
        return(headers[[which(m)[1]]])
    } else {
        return(default)
    }
}

locationHeader <- function(response) {
    loc <- response$headers$location
    return(loc)
}

get_crunch_config <- function() getOption("crunch.httr_config")

#' Set or modify general Crunch API request configuration
#'
#' @param cfg A [httr::config()] object
#' @return A list of length one containing the configuration that was set; this
#' function is called primarily for its side effects.
#' @keywords internal
#' @export
set_crunch_config <- function(cfg = c(
                                  config(postredir = 3),
                                  add_headers(`user-agent` = crunch_user_agent())
                              ),
                              update = FALSE) {
    if (update) {
        cfg <- c(get_crunch_config(), cfg)
    }
    options(crunch.httr_config = cfg)
}

#' Generate or extend the User-Agent string
#'
#' By default, the names and versions of curl, httr, and any attached Crunch
#' packages are included in the User-Agent request header. You can add to this
#' using this function.
#' @param ... Additional character terms to add to the User-Agent string
#' @return The User-Agent string. Provide this appropriately in requests or set
#' globally with [set_crunch_config()].
#' @export
#' @keywords internal
#' @importFrom curl curl_version
crunch_user_agent <- function(...) {
    ## Cf. httr:::default_ua
    ## Include versions of any of these packages, if attached
    pkgs <- ua_packages[ua_packages %in% loadedNamespaces()]
    ua <- c(
        # Also include the libcurl version
        paste0("libcurl/", curl_version()$version),
        mapply(packageUA, pkgs, names(pkgs)),
        # And any extra bits provided
        ...
    )
    return(paste(ua, collapse = " "))
}

ua_packages <- c(
    # This is a named vector so that we can provide an alternate name in the
    # user-agent string ("rcrunch" instead of "crunch", for example)
    curl = "curl",
    httr = "httr",
    rcrunch = "crunch",
    crplyr = "crplyr",
    crunchy = "crunchy"
)

#' @importFrom utils packageVersion
packageUA <- function(pkg, name = pkg) {
    # Return a string like "rcrunch/3.4.2" for a package
    paste0(name, "/", as.character(packageVersion(pkg)))
}

handleShoji <- function(x) {
    if (is.shoji.like(x)) {
        class(x) <- c("shoji", x$element)
    }
    if ("shoji:view" %in% class(x)) {
        x <- x$value %||% x$views ## Special-casing the dataset export :(
    }
    return(x)
}

getAPIRoot <- function(x = getOption("crunch.api")) {
    ShojiObject(crGET(x))
}

sessionURL <- function(key, collection = "catalogs") {
    return(shojiURL(getAPIRoot(), collection, key))
}

rootURL <- function(x, obj = getAPIRoot()) {
    ## DEPRECATE ME
    if (is.shojiObject(obj)) {
        return(obj@urls[[paste0(x, "_url")]])
    } else {
        return(NULL)
    }
}

retry <- function(expr, wait = .1, max.tries = 10) {
    ## Retry (e.g. a request)
    e <- substitute(expr)
    tries <- 0
    while (tries < max.tries) {
        out <- try(eval.parent(e), silent = TRUE)
        if (inherits(out, "try-error")) {
            tries <- tries + 1
            Sys.sleep(wait)
        } else {
            tries <- max.tries
        }
    }
    if (is.error(out)) {
        stop(out)
    }
    return(out)
}

#' @importFrom httr write_disk
crDownload <- function(url, file, ...) {
    ## Retry is for delay in propagating the file to the CDN
    ## TODO: consider only "retry" if `url` is in CDN (don't want to retry
    ## necessarily on every url/server response)
    retry(crGET(url, config = write_disk(file, overwrite = TRUE)))
    return(file)
}

featureFlag <- function(flag) {
    url <- sessionURL("feature_flag", "views")
    f <- crGET(url, query = list(feature_name = flag))
    return(isTRUE(f$active))
}

# Don't want Authorization header when going outside crunch domain
#' @importFrom httr parse_url
strip_token_if_outside <- function(url) {
    api_hostname <- parse_url(getOption("crunch.api"))$hostname
    url_hostname <- parse_url(url)$hostname
    if (!identical(api_hostname, url_hostname)) add_headers(Authorization = "")
}
