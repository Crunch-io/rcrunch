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
#' Note that you can store your Crunch account info in encrypted format via 
#' the keyring package, with `key_set(service = "crunch",...)` 
#' If you do so, you can simply `login()` to authenticate. For running batch jobs,
#' this could be particularly useful. 
#' 
#' Your email and password can also be stored in and read from the
#' environmental variables `R_CRUNCH_EMAIL` and `R_CRUNCH_PW`,
#' or from your .Rprofile under `crunch.email` and `crunch.pw`.
#' However, environmental variables and .RProfiles files are not encrpyed, so
#' this practice is no longer recommended. If an email or password is found in
#' multiple locations, priority is given to 1) environmental variables, 2)
#' .RProfile, and 3) keyring. This order of priority is for backwards compatibility,
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
    old.prompt <- getOption("crunch.old.prompt")
    if (!is.null(old.prompt)) {
        ## We may already be logged in. Log out first.
        logout()
    }
    
    #Check for email/username stored in global environment, .RProfile, or keyring
    if(missing(email)){
      email <- envOrOption("crunch.email")  #First check envOrOption to find password and username
      
      if(is.null(email) & requireNamespace("keyring", quietly = TRUE)){ #If nothing is found via envOrOption, check keyring
        keyring.username <- keyring::key_list("crunch")$username
        if(length(keyring.username) == 1 & keyring.username[1] != ""){ email <- keyring.username
        } else if(length(keyring.username) > 1) stop("More than one saved Crunch username/email address found in keyring. Try specifying login(email = ...)")

      }
    }

    #Check for password stored in global environment, .RProfile, or keyring
    if(missing(password)){
      password <- envOrOption("crunch.pw")  #First check envOrOption to find password and username
      
      if(is.null(password) & requireNamespace("keyring", quietly = TRUE)){ #If nothing is found via envOrOption, check keyring (keyring is a better behaviour, but has not been set to default for continuity)
        n.passes <- nrow(keyring::key_list("crunch")) #Check how many saved passwords there are for the "crunch" service
        
        if(n.passes >= 1){ #If there is a saved password, try matching to given email address; if no email address is associated with password then try it
          if(!is.null(email) & any(keyring::key_list("crunch")$username == email)){ password <- keyring::key_get("crunch", username = email)
          } else if(n.passes == 1 & keyring::key_list("crunch")$username == ""){ password <- keyring::key_get("crunch")
          } else warning("Saved Crunch passwords in keyring do not match specified email") #NB - not sure if we want this warning message - could be helpful or spammy
        }
      } else(warning(
        "Retrieving saved Crunch email saved in global environment or .RProfile. This may not be secure. 
        Consider using they \"keyring\" package and key_set(\"crunch\") to save password in encrypted format."
      ))
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

    crPOST(absoluteURL("public/login/", getOption("crunch.api")),
        body = toJSON(list(email = email, password = password, ...)),
        status.handlers = list(`401` = function(response, user = email) {
            halt(paste("Unable to authenticate", user))
        })
    )
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
            set_cookies(token = token),
            add_headers(`user-agent` = crunch_user_agent(ua))
        ),
        update = TRUE
    )
}

jupyterLogin <- function(token) tokenAuth(token, ua = "jupyter.crunch.io")
