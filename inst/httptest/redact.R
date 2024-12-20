function(response) {
    ## Remove multipart form fields because POST sources/ sends a tmpfile path
    ## that's different every time, so the request will never match.
    response$request$fields <- NULL

    response %>%
        redact_auth() %>%
        gsub_response("([0-9a-f]{6})[0-9a-f]{26}", "\\1") %>% ## Prune UUIDs
        gsub_response("[0-9A-Za-z]{22}([0-9]{6})", "\\1") %>% # UUIDs in variables now too
        gsub_response(
            "https.//(app|team).crunch.io/api/progress/[^\"].*?/",
            "https://\\1.crunch.io/api/progress/"
        )
}
