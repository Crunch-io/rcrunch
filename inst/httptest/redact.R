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
        gsub_response("([0-9a-f]{6})[0-9a-f]{26}", "\\1") %>% ## Prune UUIDs
        httptest::gsub_response("[0-9A-Za-z]{22}([0-9]{6})", "\\1") %>% # UUIDs in variables now too
        gsub_response(
            "https.//app.crunch.io/api/progress/[^\"].*?/",
            "https://app.crunch.io/api/progress/"
        )
}
