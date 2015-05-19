# share <- function (dataset, emails) {
#     share_url <- shojiURL(dataset, "catalogs", "permissions")
#     
#     list(edit=FALSE, view=TRUE)
# }

shareDataset <- function (x, emails, send_notification=TRUE) {
    dscat <- active(datasetCatalog())
    if (!is.numeric(x)) {
        x <- selectDatasetFromCatalog(x, dscat, strict=TRUE)
    }
    dsurls <- urls(dscat)[x]
    perm_urls <- vapply(dsurls, function (x) absoluteURL("permissions/", x),
        character(1))
    
    payload <- sapply(emails, 
            function (x) list(dataset_permissions=list(edit=FALSE, view=TRUE)),
            simplify=FALSE)
    payload$send_notification <- send_notification
    payload <- toJSON(payload)
    
    out <- lapply(perm_urls, function (x) {
        message("Sharing ", x)
        crPATCH(x, body=payload)
    })
    invisible(out)
}
