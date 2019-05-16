#' Setup authentication with Decipher
#'
#' You'll have to get your own API key
#'
#' @param key the API key you retrieved from Decipher
#' @param company_name the company name associated with your account in Decipher
#'
#' @return `NULL` if successful
#'
#' @export
setupDecipherAuth <- function(key, company_name) {
    # this doens't work because integrations isn't a catalog (or any sort of collection)
    # url <- shojiURL(getAPIRoot(), "catalog", key = "integrations/decipher/auth/")
    url <- paste0(self(getAPIRoot()), "integrations/decipher/auth/")
    payload <- wrapEntity(apikey = key, company_name = company_name)
    return(crPOST(url, body = toJSON(payload)))
}

#' List Decipher surveys available
#'
#' @return ShojiCatalog of surveys available
#'
#' @export
listDecipherSurveys <- function() {
    # get the Decipher integration item
    integrations_catalog <- ShojiCatalog(
        crGET(shojiURL(me(), "catalog", key = "integrations"))
    )
    tryCatch(
        decipher <- integrations_catalog["decipher_apikey"],
        error = function(e) {
            halt("There is no Decipher integration available for this user, ",
                 "please use `setupDecipherAuth` first.")
        }
    )

    return(ShojiCatalog(crGET(paste0(urls(decipher), "/surveys"))))
}