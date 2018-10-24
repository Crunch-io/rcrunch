#' Get the pending streams for a dataset
#'
#' Retrieves the number of pending messages. Use [appendStream()] to
#' append all pending streamed rows to the dataset.
#'
#' @param ds a CrunchDataset
#' @return number of pending messages in the stream for the dataset
#' @export
pendingStream <- function(ds) {
    stopifnot(is.dataset(ds))
    stream_cat <- ShojiEntity(crGET(shojiURL(ds, "fragments", "stream")))
    stream_cat$pending_messages
}

#' Stream data to a Crunch dataset
#'
#' @param ds a CrunchDataset
#' @param data a data.frame with data to send as a stream, The given data values
#'  must be in the Crunch I/O format (for example, category ids instead of
#'  names or numeric_values)
#' @keywords internal
streamRows <- function(ds, data) {
    if (nrow(data)) {
        payload <- by(data, seq_len(nrow(data)), function(row) toJSON(row))
        payload <- paste0(payload, collapse = "\n")
        crPOST(shojiURL(ds, "fragments", "stream"), body = payload)
    }
    invisible(refresh(ds))
}

#' Manually trigger a pending append to a dataset
#'
#' Crunch allows you to stream data to a dataset. Streaming data is useful for
#' datasets which have frequent updates (see the
#' [Crunch API documentation](http://docs.crunch.io/#streaming-rows) for more
#' information). Crunch automatically appends streamed data periodically;
#' however, if you would like to trigger appending pending streamed data to a
#' dataset, you can call `appendStream()`.
#'
#' @param ds a CrunchDataset
#' @return the dataset with pending stream data appended.
#' @export
appendStream <- function(ds) {
    stopifnot(is.dataset(ds))
    if (pendingStream(ds) < 1) {
        message("There's no pending stream data to be appended.")
        return(ds)
    }

    ds <- addBatch(ds, type = "ldjson", stream = NULL)

    return(ds)
}


#' Set the streaming property of a dataset
#'
#' Only datasets that have their streaming property set to "streaming" can
#' have rows streamed to them. Before attempting to streaming rows (with
#' [streamRows] for example), the dataset has to be set up to stream rows. Use
#' `streaming(ds)` to get the streaming status, and `streaming(ds) <-
#' "streaming"` to set the streaming status.
#' 
#' @param x a CrunchDataset
#' @param value for setting only (values can be: `"no"`, `"streaming"`, or
#' `"finished"`)
#' 
#' @return the streaming status
#' @rdname streaming
setGeneric("streaming", function(x) standardGeneric("streaming"))

#' @rdname streaming
setGeneric("streaming<-", function(x, value) standardGeneric("streaming<-"))

#' @rdname streaming
#' @export
setMethod("streaming", "CrunchDataset", function(x) x@body$streaming)


#' @rdname streaming
#' @export
setMethod("streaming<-", "CrunchDataset", function(x, value) {
    if (!(value %in% c("no", "streaming", "finished"))) {
        halt("Streaming can only be set to no, streaming, or finished.")
    }
    return(setEntitySlot(x, "streaming", value))
})