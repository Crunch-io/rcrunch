#' Kill the active Crunch session
#' @export
#' @importFrom httpcache clearCache
logout <- function() {
    try(crGET(rootURL("logout")), silent = TRUE)
    clearCache()
    old.prompt <- getOption("crunch.old.prompt")
    if (!is.null(old.prompt)) {
        options(prompt = old.prompt, crunch.old.prompt = NULL)
    }
}

#' Authenticate with the Crunch API
#'
#' Note that you can store your Crunch account info in your .Rprofile under
#' `crunch.email` and `crunch.pw` for convenience. If you do so, you can simply
#' `login()` to authenticate. For running batch jobs, this could be
#' particularly useful. However, be warned that storing your
#' password in a plain text file such as .Rprofile is a security risk (though
#' perhaps less so than in every .R script you write), and we
#' cannot officially recommend that you do so.
#'
#' Additionally, your email and password can be stored in and read from the
#' environmental variables `R_CRUNCH_EMAIL` and `R_CRUNCH_PW` respectively.
#'
#' If a password is not supplied (or, if no arguments are supplied and only
#' the `crunch.email` is specified in .Rprofile), and you are in an
#' interactive session, you will be prompted to enter your password. At
#' present, this is the most secure practice as your password is not stored
#' locally.
#'
#' @param email the email address associated with the user's Crunch account
#' @param password the password associated with the user's Crunch account
#' @param ... additional parameters passed in the authentication. Not
#' currently supported by the Crunch API.
#' @export
login <- function(email = envOrOption("crunch.email"),
                  password = envOrOption("crunch.pw"), ...) {
    old.prompt <- getOption("crunch.old.prompt")
    if (!is.null(old.prompt)) {
        ## We may already be logged in. Log out first.
        logout()
    }
    crunchAuth(email = email, password = password, ...)
    options(
        prompt = paste("[crunch]", getOption("prompt")),
        crunch.old.prompt = getOption("prompt")
    )
    message("Logged into crunch.io as ", email)
    ## Return a Session object
    invisible(session())
}

#' Get various catalogs for your Crunch session
#' @return A Session object. Access dataset and project catalogs from it.
#' @keywords internal
#' @examples
#' \dontrun{
#' cr <- session()
#' cr$datasets
#' cr$projects
#' }
#' @export
session <- function() new("Session")

#' @importFrom utils installed.packages
crunchAuth <- function(email, password = NULL, ...) {
    ## Validate authentication inputs and then POST to the API
    if (is.null(email)) {
        halt("Must supply the email address associated with your crunch.io account")
    }
    if (is.null(password)) {
        if (is.interactive()) {
            prompt <- paste0("Crunch.io password for ", email, ": ")
            if ("rstudioapi" %in% rownames(installed.packages()) &&
                rstudioapi::hasFun("askForPassword")) {
                password <- rstudioapi::askForPassword(prompt)
            } else {
                cat(prompt)
                without_echo(password <- read_input())
            }
        } else {
            halt("Must supply a password")
        }
    }

    out <- crPOST(absoluteURL("public/login/", getOption("crunch.api")),
        body = toJSON(list(email = email, password = password, token = TRUE, ...)),
        status.handlers = list(`401` = function(response, user = email) {
            halt(paste("Unable to authenticate", user))
        })
    )
    tokenAuth(out$access_token)
}

without_echo <- function(expr) {
    if (.Platform$OS.type == "unix") {
        ## Don't print the password being typed
        try(system("stty -echo", ignore.stdout = TRUE, ignore.stderr = TRUE),
            silent = TRUE
        )
        on.exit({
            try(system("stty echo", ignore.stdout = TRUE, ignore.stderr = TRUE),
                silent = TRUE
            )
            cat("\n")
        })
    }
    eval.parent(expr)
}

## Pass through for test mocking
read_input <- function(...) readline(...)

#' Add an auth token as a cookie manually
#'
#' Set the auth token rather than from a Set-Cookie response header. Also modify
#' the user-agent to include the service this is coming from.
#' @param token character auth token
#' @param ua character optional string to add to the User-Agent request header
#' @return Nothing; called for its side effects.
#' @export
#' @keywords internal
#' @importFrom httr set_cookies
tokenAuth <- function(token, ua = "token") {
    set_crunch_config(
        c(
            add_headers(Authorization = paste0("Bearer ", token)),
            add_headers(`user-agent` = crunch_user_agent(ua))
        ),
        update = TRUE
    )
}

jupyterLogin <- function(token) tokenAuth(token, ua = "jupyter.crunch.io")
