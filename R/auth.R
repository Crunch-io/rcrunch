#' Crunch API Keys
#'
#' The rcrunch package recommends using API keys for authentication.
#'
#' To get an API key for your account, follow the instructions in the
#' [crunch help desk](https://help.crunch.io/hc/en-us/articles/4415963337869-API-Keys)
#'
#' The rcrunch package looks for the key in the environmental variable
#' "R_CRUNCH_API_KEY" or the option "crunch.api.key" (see [`envOrOption()`]
#' for details).
#'
#' One way to establish your key is to add it to your ".Renviron"
#' file. This file is located in your home directory (you can
#' use `usethis::edit_r_environ()` to open the file if you have the
#' `usethis` package installed). The .Renviron file has the name of the
#' environment varia ble, followed by an equal sign and then the value. It
#' is good practice to set the API host too, (usually equal to
#' "https://app.crunch.io/api/").
#'
#' ```
#' R_CRUNCH_API=https://app.crunch.io/api/
#' R_CRUNCH_API_KEY=YOUR_SECRET_KEY
#' ```
#'
#' You can either restart your session, or run `readRenviron("~/.Renviron")`
#' and then rcrunch will know to use your key going forward.
#'
#' @name crunch-api-key
NULL

get_api_key <- function() {
  key <- envOrOption("crunch.api.key")
  # Treat empty string equal to NULL so we have a way to explicitly
  # turn off keys
  if (length(key) == 0 || identical(key, "")) key <- NULL
  key
}


#' Crunch situation report
#'
#' Get a  situation report on how R will connect to crunch.io
#'
#' @param redact Whether to redact the API key found (default TRUE)
#' @param verbose Whether to print information to the console (default TRUE)
#'
#' @return Invisibly, a list with information about the API
#' @export
#'
#' @examples
#' \dontrun{
#' crunch_sitrep()
#' }
crunch_sitrep <- function(redact = TRUE, verbose = TRUE) {
    key <- get_api_key()
    if (redact) key <- redact_key(key)

    out <- list(
        api = envOrOption("crunch.api") %||% "NOT FOUND!",
        api_source = envOrOptionSource("crunch.api"),
        key = key,
        key_source = envOrOptionSource("crunch.api.key")
    )

    key_text <- out$key %||% "NOT FOUND!"
    if (verbose) {
        if (!grepl("https://.+\\.crunch\\.io/api/", out$api)) {
            domain_warning <- "     WARNING! API not in expected form: https://xyz.crunch.io/api/\n"
        } else {
            domain_warning <- NULL
        }
        message(
            "crunch API situation report\n",
            "API: ", out$api, "\n",
            domain_warning,
            "     (", out$api_source, ")\n",
            "key: ", key_text, "\n",
            "     (", out$key_source, ")"
        )
    }

    invisible(out)
}

redact_key <- function(x) {
  if (is.null(x)) return(x)
  if (nchar(x) > 15) {
      substr(x, 10, nchar(x)) <- paste(rep("*", nchar(x) - 9), collapse = "")
  } else {
      substr(x, 1, nchar(x)) <- paste(rep("*", nchar(x)), collapse = "")
  }
  x
}

#' Helper for switching between API keys and urls
#'
#' Credentials can be stored in the options or environment variables with the following
#' structure (option = `crunch.api.<ID>` or environment variable `R_CRUNCH_API_<ID>`) where
#' `<ID>` is a string. Then you can use this function to choose which credentials you want to use.
#'
#' @param id A string indicating the id of the credentials
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using crunch options:
#' set_crunch_opts(
#'     crunch.api.account1 = "https://company1.crunch.io/api/",
#'     crunch.api.key.account1 = "MY KEY"
#' )
#'
#' # Or with environment variables
#' Sys.setenv(
#'     "R_CRUNCH_API_ACCOUNT2" = "https://company2.crunch.io/api/",
#'     "R_CRUNCH_API_KEY_ACCOUNT2" = "ANOTHER KEY"
#' )
#'
#' # Can now switch between accounts
#' setupCrunchAuth("account1")
#' crunch_sitrep()
#'
#' setupCrunchAuth("account2")
#' crunch_sitrep()
#' }
#'
setupCrunchAuth <- function(id) {
    api <- envOrOption(paste0("crunch.api.", id))
    if (is.null(api)) {
        halt("Could not find api in `envOrOption('", paste0("crunch.api.", id), "')`")
    }
    key <- envOrOption(paste0("crunch.api.key.", id))
    if (is.null(key)) {
        halt("Could not find key in `envOrOption('", paste0("crunch.api.key.", id), "')`")
    }

    set_crunch_opts(
        crunch.api = api,
        crunch.api.key = key,
        .source = paste0("setupCrunchAuth('", id, "')")
    )
}



deprecate_password <- function() {
    halt(
        "Using the crunch API with a username and password is no longer supported. ",
        "See `help('crunch-api-key')` to learn how to authenticate using an API key."
    )
}

#' @export
#' @rdname login
logout <- function() {
    deprecate_password()
}

#' DEPRECATED! Authenticate with the Crunch API
#'
#' A deprecated method to authenticate to the crunch.io API. See [`crunch-api-key`]
#' for the currently supported method, as `login()`, `logout()` and `resetPassword`()
#' no longer work.
#' @param ... Ignored
#' @export
login <- function(...) {
    deprecate_password()
}

#' @export
#' @rdname login
resetPassword <- function(...) {
    deprecate_password()
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

#' Add an auth token manually
#'
#' Set the auth token with a token you provide. Also modify
#' the user-agent to include the service this is coming from.
#' @param token character auth token
#' @param ua character optional string to add to the User-Agent request header
#' @return Nothing; called for its side effects.
#' @export
#' @keywords internal
tokenAuth <- function(token, ua = NULL, source = "tokenAuth") {
    set_crunch_opts("crunch.api.key" = token, .source = source)
    if (!is.null(ua)) {
        set_crunch_config(
            c(add_headers(`user-agent` = crunch_user_agent(ua))),
            update = TRUE
        )
    }
}

jupyterLogin <- function(token) tokenAuth(token, ua = "jupyter.crunch.io", source = "jupyterLogin")
