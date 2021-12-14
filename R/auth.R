#' Crunch API Keys
#'
#' The rcrunch package recommends using API keys for authentication.
#' Using a username and password with the [`login()`] function is
#' deprecated and may be removed in a future release.
#'
#' To get an API key for your account, (EXISTING DOCUMENTATION??).
#'
#' The rcrunch package looks for the key in the environmental variable
#' "R_CRUNCH_API_KEY" or the option "crunch.api.key" (see [`envOrOption()`]
#' for details).
#'
#' One way to establish your key is to add it to your ".Renviron"
#' file. This file is located in your home directory (you can
#' use `usethis::edit_r_environ()` to open the file if you have the
#' `usethis` package installed). The .Renviron file has the name of the
#' environment variable, followed by an equal sign and then the value. It
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
  if (identical(key, "")) key <- NULL
  key
}

api_key_source <- function() {
  if (isTRUE(get_crunch_opt("used.session.token"))) {
    return("from `login()`")
  } else if (!is.null(get_crunch_opt("crunch.api.key"))) {
    return("set using `set_crunch_opt(crunch.api.key = ...)`")
  } else if (Sys.getenv("R_CRUNCH_API_KEY") != "") {
    return("found in environment variable `R_CRUNCH_API_KEY`")
  } else if (!is.null(getOption("crunch.api.key"))) {
    return("found in `options(crunch.api.key = ...)`")
  } else {
    return("unknown source")
  }
}

deprecate_password <- function(source) {
    .Deprecated(
        msg = paste0(
            "Using the crunch API with a username and password with `",
            source, "()` is deprecated and may be removed from future releases.\n",
            "See `help('crunch-api-key')` to learn how to authenticate using an API key."
        ),
        package = "crunch",
        old = source
    )
}

setupCrunchAuth <- function(id) {
    api <- envOrOption(paste0("crunch.api.", id))
    if (is.null(api)) {
        halt("Could not find api in `envOrOption('", paste0("crunch.api.", id), "')`")
    }
    key <- envOrOption(paste0("crunch.api.key.", id))
    if (is.null(api)) {
        halt("Could not find key in `envOrOption('", paste0("crunch.api.key.", id), "')`")
    }

    set_crunch_opts(crunch.api = api, crunch.api.key = key)
}

#' Kill the active Crunch session
#' @export
#' @importFrom httpcache clearCache
logout <- function() {
    deprecate_password("logout")
    # If we didn't login with a session token, there's no real concept of logging out
    if (!isTRUE(get_crunch_opt("used.session.token"))) return(invisible())

    try(crGET(rootURL("logout")), silent = TRUE)
    set_crunch_opts(crunch.api.key = NULL, used.session.token = NULL)
    clearCache()
    old.prompt <- getOption("crunch.old.prompt")
    if (!is.null(old.prompt)) {
        options(prompt = old.prompt, crunch.old.prompt = NULL)
    }
}

#' Authenticate with the Crunch API
#'
#' Note that you can store your Crunch account info in encrypted format via
#' the keyring package, with `key_set(service = "crunch", "<USERNAME>", ...)`
#' If you do so, you can simply `login()` to authenticate. For running batch jobs,
#' this could be particularly useful.
#'
#' Your email and password can also be stored in and read from the
#' environmental variables `R_CRUNCH_EMAIL` and `R_CRUNCH_PW`,
#' or from your .Rprofile under `crunch.email` and `crunch.pw`.
#' However, environmental variables and `.RProfile` files are not encrypted, so
#' this practice is no longer recommended. If an email or password is found in
#' multiple locations, priority is given to 1) environmental variables, 2)
#' `.RProfile`, and 3) keyring. This order of priority is for backwards compatibility,
#' and methods 1) and 2) are no longer recommended.
#'
#' If a password is not stored in any of these locations, and you are in an
#' interactive session, you will be prompted to enter your password.
#'
#' @param email the email address associated with the user's Crunch account
#' @param password the password associated with the user's Crunch account
#' @param ... additional parameters passed in the authentication. Not
#' currently supported by the Crunch API.
#' @export
login <- function(email = NULL,
                  password = NULL, ...) {
    deprecate_password("login")
    old.prompt <- getOption("crunch.old.prompt")
    if (!is.null(old.prompt)) {
        ## We may already be logged in. Log out first.
        logout()
    }

    #Get username and password combo
    login_info <- get_user_pass_combo(email = email, password = password)

    crunchAuth(email = login_info$email, password = login_info$password, ...)
    options(
        prompt = paste("[crunch]", getOption("prompt")),
        crunch.old.prompt = getOption("prompt")
    )
    message("Logged into crunch.io as ", login_info$email)
    ## Return a Session object
    invisible(session())
}

get_user_pass_combo <- function(email, password) {
  #Check for email/username stored in global environment, .RProfile, or keyring
  if (is.null(email)) {
    email <- envOrOption("crunch.email")  #First check envOrOption to find password and username

    #If nothing is found via envOrOption, check keyring
    if (is.null(email) & requireNamespace("keyring", quietly = TRUE)) {
      keyring.username <- keyring::key_list("crunch")$username
      if (length(keyring.username) == 1 & keyring.username[1] != "") {
        email <- keyring.username
      } else if(length(keyring.username) > 1) {
        halt(
          "More than one saved Crunch username/email address found in keyring. Try specifying ",
          "login(email = ...)"
        )
      }
    }
  }

  #Check for password stored in global environment, .RProfile, or keyring
  if(is.null(password)) {
    password <- envOrOption("crunch.pw")  #First check envOrOption to find password and username

    # If nothing is found via envOrOption, check keyring (keyring is a better behaviour,
    # but has not been set to default for continuity)
    if (is.null(password) & requireNamespace("keyring", quietly = TRUE)) {
      # Check how many saved passwords there are for the "crunch" service
      n.passes <- nrow(keyring::key_list("crunch"))
      # If there is a saved password, try matching to given email address;
      # if no email address is associated with password then try it
      if (n.passes >= 1) {
        if (!is.null(email) & any(keyring::key_list("crunch")$username == email)) {
          password <- keyring::key_get("crunch", username = email)
        } else if (n.passes == 1 & keyring::key_list("crunch")$username[1] == "") {
          password <- keyring::key_get("crunch")
        } else if (interactive() == TRUE) {
          warning("Saved Crunch passwords in keyring do not match specified email")
        }
      }
    } else {
      # Check with customer success team before enabling this warning
      # warning(paste0(
      #   "Retrieving saved Crunch email saved in global environment or .RProfile. This may not ",
      #   "be secure. Consider using they \"keyring\" package and key_set(\"crunch\") to save ",
      #   "password in encrypted format."
      # ))
    }
  }

  return(list(email = email, password = password))
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
            if (requireNamespace("rstudioapi", quietly = TRUE) &&
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
    # Don't use existing keys
    set_crunch_opt("crunch.api.key", "")
    out <- crPOST(absoluteURL("public/login/", envOrOption("crunch.api")),
        body = toJSON(list(email = email, password = password, token = TRUE, ...)),
        status.handlers = list(`401` = function(response, user = email) {
            halt(paste("Unable to authenticate", user))
        })
    )
    tokenAuth(out$access_token, ua = "login")
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

#' Add an auth token manually
#'
#' Set the auth token with a token you provide. Also modify
#' the user-agent to include the service this is coming from.
#' @param token character auth token
#' @param ua character optional string to add to the User-Agent request header
#' @return Nothing; called for its side effects.
#' @export
#' @keywords internal
tokenAuth <- function(token, ua = NULL) {
    set_crunch_opts("crunch.api.key" = token, "used.session.token" = TRUE)
    if (!is.null(ua)) {
        set_crunch_config(
            c(add_headers(`user-agent` = crunch_user_agent(ua))),
            update = TRUE
        )
    }
}

jupyterLogin <- function(token) tokenAuth(token, ua = "jupyter.crunch.io")
