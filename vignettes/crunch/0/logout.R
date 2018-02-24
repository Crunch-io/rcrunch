structure(list(url = "/logout/", status_code = 401L, headers = structure(list(
    allow = "GET, HEAD, OPTIONS", `content-encoding` = "gzip", 
    `content-type` = "application/json", date = "Sat, 24 Feb 2018 04:33:05 GMT", 
    server = "nginx", vary = "Accept-Encoding", `content-length` = "168", 
    connection = "keep-alive"), .Names = c("allow", "content-encoding", 
"content-type", "date", "server", "vary", "content-length", "connection"
), class = c("insensitive", "list")), all_headers = list(structure(list(
    status = 401L, version = "HTTP/1.1", headers = structure(list(
        allow = "GET, HEAD, OPTIONS", `content-encoding` = "gzip", 
        `content-type` = "application/json", date = "Sat, 24 Feb 2018 04:33:05 GMT", 
        server = "nginx", vary = "Accept-Encoding", `content-length` = "168", 
        connection = "keep-alive"), .Names = c("allow", "content-encoding", 
    "content-type", "date", "server", "vary", "content-length", 
    "connection"), class = c("insensitive", "list"))), .Names = c("status", 
"version", "headers"))), cookies = structure(list(domain = logical(0), 
    flag = logical(0), path = logical(0), secure = logical(0), 
    expiration = structure(numeric(0), class = c("POSIXct", "POSIXt"
    )), name = logical(0), value = logical(0)), .Names = c("domain", 
"flag", "path", "secure", "expiration", "name", "value"), row.names = integer(0), class = "data.frame"), 
    content = charToRaw("{\"status\": \"401 Unauthorized\", \"urls\": {\"login_url\": \"/public/login/\", \"password_reset_url\": \"/public/password_reset/\", \"public_url\": \"/public/\", \"email_change_url\": \"/public/change_email/\", \"password_change_url\": \"/public/password_change/\", \"collect_inquires\": \"/public/inquire/\"}}"), 
    date = structure(1519446785, class = c("POSIXct", "POSIXt"
    ), tzone = "GMT"), times = structure(c(0, 0.036212, 0.30803, 
    0.755321, 0.982481, 0.982543), .Names = c("redirect", "namelookup", 
    "connect", "pretransfer", "starttransfer", "total"))), .Names = c("url", 
"status_code", "headers", "all_headers", "cookies", "content", 
"date", "times"), class = "response")
