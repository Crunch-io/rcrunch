# nolint start
structure(list(url = "https://app.crunch.io/api/datasets/1/scripts/",
    status_code = 400L, headers = structure(list(date = "Wed, 06 May 2020 17:36:37 GMT",
    `content-type` = "application/json", `content-length` = "513",
    server = "nginx", allow = "GET, HEAD, OPTIONS, POST",
    `set-cookie` = "REDACTED"), class = c("insensitive",
    "list")), all_headers = list(list(status = 400L, version = "HTTP/2",
    headers = structure(list(date = "Wed, 06 May 2020 17:36:37 GMT",
    `content-type` = "application/json", `content-length` = "513",
    server = "nginx", allow = "GET, HEAD, OPTIONS, POST",
    `set-cookie` = "REDACTED"), class = c("insensitive",
    "list")))), cookies = structure(list(domain = ".crunch.io",
    flag = TRUE, path = "/", secure = FALSE, expiration = structure(1620322597, class = c("POSIXct",
    "POSIXt")), name = "token", value = "REDACTED"), row.names = c(NA,
    -1L), class = "data.frame"), content = charToRaw("{\n    \"resolutions\": [\n        {\n            \"message\": \"Variables wrong_var_name don't exist in the specified source\",\n            \"command\": 1,\n            \"line\": 1\n        },\n{\n            \"message\": \"Variables wrong_var_name2 don't exist in the specified source\",\n            \"command\": 2,\n            \"line\": 2\n        }\n    ],\n    \"self\": \"https://app.crunch.io/api/datasets/43d745/scripts/\",\n    \"type\": \"script:validation\",\n    \"description\": \"Errors processing the script\",\n    \"element\": \"crunch:error\"\n}                                                                                                                         "),
    date = structure(1588786597, class = c("POSIXct", "POSIXt"
    ), tzone = "GMT"), times = c(redirect = 0, namelookup = 3.6e-05,
    connect = 3.8e-05, pretransfer = 0.000122, starttransfer = 0.000127,
    total = 0.152848)), class = "response")
# nolint end