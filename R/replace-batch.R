replaceBatch <- function (dataset1, dataset2, batch_id=max(ids(batches(dataset1)))) {
    
    stopifnot(is.dataset(dataset1))
    
    ## Find the batch to PUT to
    batch_url <- urls(batches(dataset1))[ids(batches(dataset1)) == batch_id]
    
    ## Prepare the replacement
    if (!is.dataset(dataset2)) {
        ## TODO: compose batch directly, not as dataset?
        temp.ds.name <- paste("Replacement data for", name(dataset1), now())
        message("Creating ", sQuote(temp.ds.name), " as temporary dataset")
        dataset2 <- newDataset(dataset2, name=temp.ds.name)
    }
    
    ## Validate
    if (identical(self(dataset1), self(dataset2))) {
        halt("Cannot append dataset to itself")
    }
    
    body <- list(
        element="shoji:entity",
        body=list(
            dataset=self(dataset2),
            status="idle"
        )
    )
    rep <- crPUT(batch_url, body=toJSON(body))
    return(refresh(dataset1))
}