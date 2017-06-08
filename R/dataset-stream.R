#' Tools for working with streaming data
#'
#' Crunch allows you to stream data to a dataset. Streaming data is useful for
#' datasets which have frequent updates (see 
#' [the Crunch documentation](http://docs.crunch.io/#streaming-rows) for more)
#' information. 
#'
#' @param x CrunchDataset
#' @param messages the number of messages that are pending to be appended to 
#'  the dataset (default: NULL, which will append all pending messages)
#' @param data a dataframe with data to send as a stream, The given data values
#'  must be in the Crunch I/O format (for example, category ids instead of 
#'  names or numeric_values)
#' @return nothing?
#' @name streaming
#' @aliases streaming getPendingMessages streamRows appendStreamedRows
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

#' @rdname streaming
#' @export
setMethod("appendStreamedRows", "CrunchDataset", function (x, messages) {
    n_msg <- getPendingMessages(x)
    
    # out <- crPOST(shojiURL(x, "fragments", "stream"), body=payload)
})