
#' @rdname geo
#' @export
setMethod("geo", "CrunchVariable", function (x) {
    var_geodata <- entity(x)@body$view$geodata[[1]]
    geodatum <- CrunchGeodata(crGET(var_geodata$geodatum))
    geo_object <- CrunchGeography(
        geodatum = geodatum,
        feature_key = var_geodata$feature_key,
        match_field = var_geodata$match_field
    )

    return(geo_object)
})
#' @rdname geo
#' @export
setMethod("geo<-", "CrunchVariable", function (x, value) {
    # if (!is.numeric(value) || !is.whole(value)) {
    #     halt("digit specifications should be an integer")
    # }
    # if (value < 0 | value > 16) {
    #     halt("digit specifications should be between 0 and 16")
    # }
    #
    # frmt <- wrapEntity("format" = list("data" = list("digits" = value)))
    # crPATCH(self(x), body=toJSON(frmt))
    # invisible(x)
})

# [{"geodatum": <uri>, "feature_key": "properties.postal-code"}]