#' Upload data to Crunch to make a new dataset
#'
#' This function creates a new dataset on the Crunch server with either a
#' data.frame or similar object in your R session, a file, or a URL to a file.
#' It captures available metadata from your R object and translates it into
#' Crunch types.
#'
#' If you have an SPSS file, it is better specify the file name directly rather
#' than first reading it into R. Uploading SPSS files directly to Crunch will
#' preserve metadata that is stripped by the R import, regardless of the library
#' used to read it into R.
#' 
#' If you have Triple-S files, you can import those directly to Crunch like you
#' can with SPSS files. You should use the filename to the data file (ending in
#' `.asc` or `.dat`) as the `x` argument and use the metadata file (ending in
#' `.sss` or `.xml`) as the `schema` argument.
#'
#' @param x a `data.frame` or other rectangular R data object, or a string
#' file name or URL to upload to create a dataset. The file may be a compressed
#' Zip file containing a single file in CSV or SPSS format.
#' @param name character name to give the new Crunch dataset. By default the
#' function uses the name of the R object, or, if passing a file, the file name.
#' @param ... additional arguments passed to [createDataset()], or `schema` if 
#'  you're upload Triple-S
#' @return If successful, an object of class CrunchDataset.
#' @examples
#' \dontrun{
#' ds <- newDataset(mtcars, "cars")
#' ds <- newDataset("mysurvey.sav")
#' }
#' @export
#' @seealso [newDatasetFromFile()]; [newDatasetByColumn()] for an alternate
#' upload method.
newDataset <- function(x, name = NULL, ...) {
    Call <- match.call()
    if (is.character(x)) {
        ## Assume we have a file/URL
        Call[[1]] <- as.name("newDatasetFromFile")
        return(eval.parent(Call))
    }

    is.2D <- !is.null(dim(x)) && length(dim(x)) %in% 2
    if (!is.2D) {
        halt(
            "Can only make a Crunch dataset from a two-dimensional data ",
            "structure"
        )
    }

    if (is.null(name)) {
        name <- deparseAndFlatten(substitute(x), max_length = 40)
    }
    ## TODO: something with paginating the CSV batching if lots of data
    d <- prepareDataForCrunch(x, name = name, ...)
    ds <- createWithPreparedData(d)
    invisible(ds)
}

#' Create an empty dataset
#'
#' Use only if you're writing a function to create a Crunch dataset from a
#' custom data structure. If you have a `data.frame`, just call
#' [newDataset()] on it.
#'
#' @param name character, the name to give the new Crunch dataset. This is
#' required.
#' @param body list correctly formatted metadata definition for a dataset. See
#' the [Crunch API documentation](http://docs.crunch.io).
#' @param ... additional arguments for the POST to create the dataset, such as
#' "description".
#' @return An object of class CrunchDataset.
#' @seealso [newDataset()]
#' @keywords internal
#' @export
createDataset <- function(name, body, ...) {
    if (missing(body)) {
        body <- wrapEntity(name = name, ...)
    }
    dataset_url <- crPOST(sessionURL("datasets"), body = toJSON(body))
    dropDatasetsCache()
    return(invisible(loadDatasetFromURL(dataset_url)))
}

#' Translate a data.frame to Crunch format
#'
#' This is called within `newDataset` to extract the Crunch metadata
#' from the data and to transform the data to match the extracted metadata. You
#' can call this directly in order to tailor the data import flow more finely.
#'
#' @param data A `data.frame` or other rectangular R object
#' @param ... additional arguments passed to [createDataset].
#' "name" will be required by the Crunch server but is not required by this
#' function.
#' @return A data.frame that is a transformation of \code{data} suitable for
#' uploading to Crunch, also containing a "metadata" attribute that is
#' the associated Crunch metadata.
#' @seealso createWithPreparedData writePreparedData
#' @export
prepareDataForCrunch <- function(data, ...) {
    ## Get all the things
    progressMessage("Processing the data")
    vars <- lapply(
        names(data),
        function(i) toVariable(data[[i]], name = i, alias = i)
    )
    names(vars) <- names(data)

    ## Extract the data
    ## Do data.frame here because write.csv will internally if we don't, and
    ## we need check.names=FALSE so that the names don't get mangled and changed
    ## away from what the metadata has
    cols <- data.frame(lapply(vars, vget("values")), check.names = FALSE)

    ## Drop the columns from the metadata and compose the payload
    vars <- lapply(vars, function(v) {
        v[["values"]] <- NULL
        return(v)
    })
    meta <- shojifyDatasetMetadata(vars, ...)

    ## Return the data frame to write to csv to upload, with the metadata to
    ## POST as an attribute on it.
    return(structure(cols, metadata = meta))
}

#' Upload a prepared data.frame with metadata to Crunch
#'
#' If you have manually created a Crunch dataset object with [prepareDataForCrunch()]
#' this function allows you to upload it to the app.
#'
#' @param data a data.frame that meets the Crunch API specification, as returned
#' by [prepareDataForCrunch()], or a character path to a file or URL
#' where such data has been written as CSV.
#' @param metadata list of Crunch metadata that corresponds to `data`.
#' Default is the "metadata" attribute of `data`, as returned by
#' `prepareDataForCrunch`, or a character path to a file where such
#' metadata has been written as JSON.
#' @return A CrunchDataset.
#' @export
createWithPreparedData <- function(data, metadata = attr(data, "metadata")) {
    createWithMetadataAndFile(metadata, data)
}

#' Persist to disk a prepared data.frame and metadata
#'
#' @param data a data.frame that meets the Crunch API specification, as returned
#' by [prepareDataForCrunch()].
#' @param metadata list of Crunch metadata that corresponds to \code{data}.
#' Default is the "metadata" attribute of \code{data}, as returned by
#' [prepareDataForCrunch()].
#' @param file character file path, without extension, to write to.
#' @return A character vector of length 2: given a value of
#' `file="example"`, it would return c("example.csv.gz", "example.json").
#' The function, of course, is called for its side effects of writing a gzipped
#' CSV and a JSON file to those locations.
#' @keywords internal
#' @export
writePreparedData <- function(data, metadata = attr(data, "metadata"), file) {
    filenames <- paste(file, c("csv.gz", "json"), sep = ".")
    write.csv.gz(data, filenames[1])
    cat(toJSON(metadata), file = filenames[2])
    return(filenames)
}

#' Write CSV to a compressed file
#'
#' @param x A data.frame or similar CSV-writable object
#' @param file character destination to write the gzipped CSV to
#' @param na See [`utils::write.csv`]. This just changes the default
#' to a Crunch-friendly empty string.
#' @param row.names logical: write out row names? See [`utils::write.csv`].
#' @param ... Additional arguments passed to `write.csv`.
#' @return A csv file written to dist
#' @importFrom utils write.csv
#' @export
write.csv.gz <- function(x, file, na = "", row.names = FALSE, ...) {
    gf <- gzfile(file, "w")
    on.exit(close(gf))
    invisible(write.csv(x, file = gf, na = na, row.names = row.names, ...))
}

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
#' @keywords internal
createWithMetadataAndFile <- function(metadata, file, strict = TRUE) {
    ds <- uploadMetadata(metadata)
    ds <- uploadData(ds, file, strict, first_batch = TRUE)
    progressMessage("Done!")
    return(ds)
}

#' @importFrom jsonlite fromJSON
uploadMetadata <- function(metadata) {
    progressMessage("Uploading metadata")
    if (is.character(metadata)) {
        ## File name. Read it in.
        metadata <- fromJSON(metadata, simplifyVector = FALSE)
    }
    return(createDataset(body = metadata))
}

uploadData <- function(dataset, data, strict = TRUE, first_batch = TRUE) {
    progressMessage("Uploading data")
    if (!is.character(data)) {
        ## It's a data.frame. Write it out to a file.
        f <- tempfile()
        write.csv.gz(data, f)
        data <- f
    }
    return(addBatchFile(dataset, data, strict = strict, first_batch = TRUE))
}

#' Wrap variable metadata inside a dataset entity
#'
#' @param metadata list of variable metadata
#' @param order a valid "order" payload: list containing either aliases or
#' list(group, entities)
#' @param ... dataset entity metadata. "name" is required.
#' @param return list suitable for JSONing and POSTing to create a dataset
#' @export
#' @keywords internal
shojifyDatasetMetadata <- function(metadata, order = I(names(metadata)), ...) {
    tbl <- list(element = "crunch:table", metadata = metadata, order = order)
    return(wrapEntity(..., table = tbl))
}

#' Upload a data.frame column-by-column to make a new dataset
#'
#' Use this version if you have lots of variables, under 1M rows, perhaps
#' backed by `ff` or other memory-mapped files, and time to kill. You really
#' probably want [newDataset()] instead.
#'
#' @param x a data.frame or other rectangular R object
#' @param name character, the name to give the new Crunch dataset. Default is
#' the name of the R object passed in `x`
#' @param ... additional arguments passed to [createDataset()]
#' @return If successful, an object of class CrunchDataset.
#' @seealso [newDataset()]
#' @export
#' @keywords internal
newDatasetByColumn <- function(x, name = deparseAndFlatten(substitute(x), max_length = 40), ...) {
    vardefs <- lapply(
        names(x),
        function(v) toVariable(x[[v]], name = v, alias = v)
    )
    ds <- createDataset(name = name, ...)
    ds <- addVariables(ds, vardefs)
    ds <- saveVersion(ds, "initial import")
    invisible(ds)
}

#' Upload a file to Crunch to make a new dataset
#'
#' This function allows you to upload a `.csv` or `.sav` file directly to Crunch
#' without first reading it into R. This is useful both because it preserves SPSS
#' metadata that is lost when reading `.sav` files into R and because it is more
#' efficient just to upload the file to the server.
#'
#' You no longer need to call this function directly: you can call
#' [newDataset()] and pass the filename or URL, and it will handle it for you,
#' thereby saving you eight keystrokes.
#'
#' @param file character, the path to a local file to upload, or a URL. This
#'   should either be a `.csv`, `.sav` (SPSS), `.asc` (Triple-S data), or `.dat`
#'   (Triple-S data) file
#' @param name character, the name to give the new Crunch dataset. By default
#'   the name of the dataset will be the filename
#' @param schema character, the path to a local file to upload, or a URL. The
#'   file specifies the dataset schema (variable types, categories, etc.) to
#'   use. Currently only implemented for Triple-S files. This should either be a
#'   `.xml` or `.sss` file (both are possible for Triple-S metadata files)
#' @param ... additional arguments passed to [createDataset]
#' @return On success, an object of class [CrunchDataset]
#' @export
#' @seealso [newDataset()]
#' @keywords internal
newDatasetFromFile <- function(x, name = basename(x), schema, ...) {
    # TODO: check file extensions of the files to not return "Error: An error 
    # occurred processing your request. We have been notified"
    if (!missing(schema)) {
        schema_source = createSource(schema)
        body_payload = list(name = name, table = list(source = schema_source))
        ds <- createDataset(body = body_payload, ...)
        ds <- addBatchFile(ds, x, first_batch = TRUE, schema = schema_source)
    } else {
        ds <- createDataset(name = name, ...)
        ds <- addBatchFile(ds, x, first_batch = TRUE)
    }
    
    return(invisible(ds))
}
