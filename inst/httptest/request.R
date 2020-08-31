function(request) {
    request %>%
        redact_auth() %>%
        gsub_request("([0-9a-f]{6})[0-9a-f]{26}", "\\1") %>% ## Prune UUIDs
        gsub_request("[0-9A-Za-z]{22}([0-9]{6})", "\\1") # UUIDs in variables now too
}
