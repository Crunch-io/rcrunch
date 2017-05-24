#' Upload a data.frame to Crunch to make a new dataset
#'
#' This function uses the CSV+JSON import format, which is generally faster and
#' more effective than \code{\link{newDatasetByColumn}}.
#'
#' @param x a data.frame or other rectangular R object
#' @param name character, the name to give the new Crunch dataset. Default is
#' the name of the R object passed in \code{x}
#' @param ... additional arguments passed to \code{ \link{createDataset}}
#' @return If successful, an object of class CrunchDataset.
#' @export
newDataset <- function (x, name=deparseAndTruncate(substitute(x)), ...) {
    is.2D <- !is.null(dim(x)) && length(dim(x)) %in% 2
    if (!is.2D) {
        halt("Can only make a Crunch dataset from a two-dimensional data ",
            "structure")
    }

    ## TODO: something with paginating the CSV batching if lots of data
    force(name)
    d <- prepareDataForCrunch(x, name=name, ...)
    ds <- createWithPreparedData(d)
    invisible(ds)
}

#' Create an empty dataset
#'
#' Use only if you're writing a function to create a Crunch dataset from a
#' custom data structure. If you have a data.frame, just call
#' \code{\link{newDataset}} on it.
#'
#' @param name character, the name to give the new Crunch dataset. This is
#' required.
#' @param body list correctly formatted metadata definition for a dataset. See
#' docs.crunch.io.
#' @param ... additional arguments for the POST to create the dataset, such as
#' "description".
#' @return An object of class CrunchDataset.
#' @seealso \code{\link{newDataset}}
#' @keywords internal
#' @export
createDataset <- function (name, body, ...) {
    if (missing(body)) {
        body <- wrapEntity(name=name, ...)
    }
    dataset_url <- crPOST(sessionURL("datasets"), body=toJSON(body))
    ds <- entity(datasets()[[dataset_url]])
    invisible(ds)
}

#' Translate a data.frame to Crunch format
#'
#' This is called within \code{newDataset(ByCsv)} to extract the Crunch metadata
#' from the data and to transform the data to match the extracted metadata. You
#' can call this directly in order to tailor the data import flow more finely.
#'
#' @param data A data.frame or other rectangular R object
#' @param ... additional arguments passed to \code{ \link{createDataset}}.
#' "name" will be required by the Crunch server but is not required by this
#' function.
#' @return A data.frame that is a transformation of \code{data} suitable for
#' uploading to Crunch, also containing a "metadata" attribute that is
#' the associated Crunch metadata.
#' @seealso createWithPreparedData writePreparedData
#' @export
prepareDataForCrunch <- function (data, ...) {
    ## Get all the things
    message("Processing the data")
    vars <- lapply(names(data),
        function (i) toVariable(data[[i]], name=i, alias=i))
    names(vars) <- names(data)

    ## Extract the data
    ## Do data.frame here because write.csv will internally if we don't, and
    ## we need check.names=FALSE so that the names don't get mangled and changed
    ## away from what the metadata has
    cols <- data.frame(lapply(vars, vget("values")), check.names=FALSE)

    ## Drop the columns from the metadata and compose the payload
    vars <- lapply(vars, function (v) {
        v[["values"]] <- NULL
        return(v)
    })
    meta <- shojifyDatasetMetadata(vars, ...)

    ## Return the data frame to write to csv to upload, with the metadata to
    ## POST as an attribute on it.
    return(structure(cols, metadata=meta))
}

#' Upload a prepared data.frame with metadata to Crunch
#'
#' @param data a data.frame that meets the Crunch API specification, as returned
#' by \code{\link{prepareDataForCrunch}}, or a character path to a file or URL
#' where such data has been written as CSV.
#' @param metadata list of Crunch metadata that corresponds to \code{data}.
#' Default is the "metadata" attribute of \code{data}, as returned by
#' \code{prepareDataForCrunch}, or a character path to a file where such
#' metadata has been written as JSON.
#' @return A CrunchDataset.
#' @export
createWithPreparedData <- function (data, metadata=attr(data, "metadata")) {
    createWithMetadataAndFile(metadata, data)
}

#' Persist to disk a prepared data.frame and metadata
#'
#' @param data a data.frame that meets the Crunch API specification, as returned
#' by \code{\link{prepareDataForCrunch}}.
#' @param metadata list of Crunch metadata that corresponds to \code{data}.
#' Default is the "metadata" attribute of \code{data}, as returned by
#' \code{prepareDataForCrunch}.
#' @param file character file path, without extension, to write to.
#' @return A character vector of length 2: given a value of
#' \code{file="example"}, it would return c("example.csv.gz", "example.json").
#' The function, of course, is called for its side effects of writing a gzipped
#' CSV and a JSON file to those locations.
#' @export
writePreparedData <- function (data, metadata=attr(data, "metadata"), file) {
    filenames <- paste(file, c("csv.gz", "json"), sep=".")
    write.csv.gz(data, filenames[1])
    cat(toJSON(metadata), file=filenames[2])
    return(filenames)
}

#' Write CSV to a compressed file
#'
#' @param x A data.frame or similar CSV-writeable object
#' @param file character destination to write the gzipped CSV to
#' @param na See \code{\link[utils]{write.csv}}. This just changes the default
#' to a Crunch-friendly empty string.
#' @param row.names logical: write out row names? See \code{write.csv}. This
#' just sets a default of \code{FALSE}.
#' @param ... Additional arguments passed to \code{write.csv}.
#' @return Whatever \code{write.csv} returns.
#' @importFrom utils write.csv
#' @export
write.csv.gz <- function (x, file, na="", row.names=FALSE, ...) {
    gf <- gzfile(file, "w")
    on.exit(close(gf))
    invisible(write.csv(x, file=gf, na=na, row.names=row.names, ...))
}

## TODO:
# * Test these new features
# * Examples

#' Make a dataset with metadata and a CSV
#'
#' This function just takes what you give it and POSTs to Crunch. No
#' validation, no automatic wrapping in the Shoji envelope, etc.
#'
#' @param metadata a list representation of the dataset's metadata, which
#' will be JSON-serialized and POSTed.
#' @param file a path to a CSV file, optionally zipped, that corresponds to
#' the above metadata.
#' @param strict logical: must the metadata exactly match the data? Default is
#' TRUE.
#' @return On success, a new dataset.
#' @export
#' @importFrom jsonlite fromJSON
#' @keywords internal
createWithMetadataAndFile <- function (metadata, file, strict=TRUE) {
    ds <- uploadMetadata(metadata)
    tryCatch({
        out <- uploadData(ds, file, strict)
    }, error=function (e) {
        ## We failed to add the batch successfully, so we don't really have
        ## a useful dataset. So delete the entity that was created initially.
        with_consent(delete(ds))
        stop(e)
    })

    message("Done!")
    return(out)
}

uploadMetadata <- function (metadata) {
    message("Uploading metadata")
    if (is.character(metadata)) {
        ## File name. Read it in.
        metadata <- fromJSON(metadata, simplifyVector=FALSE)
    }
    return(createDataset(body=metadata))
}

uploadData <- function (dataset, data, strict=TRUE) {
    message("Uploading data")
    if (!is.character(data)) {
        ## It's a data.frame. Write it out to a file.
        f <- tempfile()
        write.csv.gz(data, f)
        data <- f
    }
    return(addBatchFile(dataset, data, savepoint=FALSE, strict=strict))
}

#' Wrap variable metadata inside a dataset entity
#'
#' @param metadata list of variable metadata
#' @param order a valid "order" payload: list containing either aliases or
#' list(group, entities)
#' @param ... dataset entity metadata. "name" is required.
#' @param return list suitiable for JSONing and POSTing to create a dataset
#' @export
#' @keywords internal
shojifyDatasetMetadata <- function (metadata, order=I(names(metadata)), ...) {
    tbl <- list(element="crunch:table", metadata=metadata, order=order)
    return(wrapEntity(..., table=tbl))
}

#' @rdname newDataset
#' @export
newDatasetByCSV <- function (...) {
    Call <- match.call()
    Call[[1]] <- as.name("newDataset")
    warning("newDatasetByCSV is deprecated. Use 'newDataset' (it's the same thing).")
    ds <- eval.parent(Call)
    invisible(ds)
}

#' Upload a data.frame column-by-column to make a new dataset
#'
#' Use this version if you have lots of variables, under 1M rows, perhaps
#' backed by ff or other memory-mapped files, and time to kill.
#'
#' @param x a data.frame or other rectangular R object
#' @param name character, the name to give the new Crunch dataset. Default is
#' the name of the R object passed in \code{x}
#' @param ... additional arguments passed to \code{ \link{createDataset}}
#' @return If successful, an object of class CrunchDataset.
#' @seealso \code{\link{newDataset}}
#' @export
newDatasetByColumn <- function (x, name=deparseAndTruncate(substitute(x)), ...) {
    vardefs <- lapply(names(x),
        function (v) toVariable(x[[v]], name=v, alias=v))
    ds <- createDataset(name=name, ...)
    ds <- addVariables(ds, vardefs)
    saveVersion(ds, "initial import")
    invisible(ds)
}

#' Upload a file to Crunch to make a new dataset
#'
#' Use this import method if you have an SPSS data file. Reading such a file
#' into R as a data.frame will result in lost metadata. You can just send it
#' directly to Crunch and let the server process it.
#'
#' @param file character, the path to a file to upload. This should either be
#' a .csv or .sav (SPSS) file.
#' @param name character, the name to give the new Crunch dataset. Default is
#' the file name
#' @param ... additional arguments passed to \code{ \link{createDataset}}
#' @return On success, an object of class \code{CrunchDataset}.
#' @export
newDatasetFromFile <- function (file, name=basename(file), ...) {
    if (!file.exists(file)) {
        halt("File not found")
    }
    ds <- createDataset(name=name, ...)
    ds <- addBatchFile(ds, file, savepoint=FALSE)
    invisible(ds)
}
