options(
    ## For real auth, pass in creds via env vars. This makes login() not error
    ## when mocking
    crunch.email = "fake",
    crunch.pw = "fake",
    crunch.show.progress = FALSE
)
library(magrittr)
set_redactor(function(response) {
    ## Remove multipart form fields because POST sources/ sends a tmpfile path
    ## that's different every time, so the request will never match.
    response$request$fields <- NULL
    ## So that the login request isn't tied to one user's creds, ignore it in mocks
    if (response$url == "https://app.crunch.io/api/public/login/") {
        response$request$options[["postfields"]] <- NULL
    }
    response %>%
        redact_auth() %>%
        gsub_response("([0-9a-f]{6})[0-9a-f]{26}", "\\1") %>% ## Prune UUIDs
        httptest::gsub_response("[0-9A-Za-z]{22}([0-9]{6})", "\\1") %>% # UUIDs in variables now too
        gsub_response(
            "https.//app.crunch.io/api/progress/[^\"].*?/",
            "https://app.crunch.io/api/progress/"
        ) %>% ## Progress is meaningless in mocks
        gsub_response("https.//app.crunch.io", "") %>% ## Shorten URL
        gsub_response("https%3A%2F%2Fapp.crunch.io", "") ## Shorten encoded URL
})
set_requester(function(request) {
    request$fields <- NULL
    if (request$url == "https://app.crunch.io/api/public/login/") {
        request$options[["postfields"]] <- NULL
    }
    request %>%
        gsub_request("https.//app.crunch.io", "") ## Shorten URL
})
