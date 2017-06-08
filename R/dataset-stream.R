#' Tools for working with streaming data
#'
#' Crunch allows you to stream data to a dataset. Streaming data is useful for
#' datasets which have frequent updates (see 
#' [the Crunch documentation](http://docs.crunch.io/#streaming-rows) for more)
#' information. 
#'
#' @param x CrunchDataset
#' @param rows stuff
#' @param data a dataframe with data to send as a stream, The given data values
#'  must be in the Crunch I/O format (for example, category ids instead of 
#'  names or numeric_values)
#' @return nothing?
#' @name streaming
#' @examples
#' \dontrun{
#' # need examples!
#' }
NULL


#' @rdname streaming
#' @export
setMethod("getPendingMessages", "CrunchDataset", function (x) {
    stream_cat <- ShojiEntity(crGET(shojiURL(x, "fragments", "stream")))
    stream_cat$pending_messages
})

#' @rdname streaming
#' @export
setMethod("streamRows", "CrunchDataset", function (x, data) {
    payload <- by(data, 1:nrow(data), function(row) toJSON(row) )
    payload <- paste0(payload, collapse = "\n") 
    out <- crPOST(shojiURL(x, "fragments", "stream"), body=payload)
})

