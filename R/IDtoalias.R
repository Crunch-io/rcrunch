IDtoalias <- function(id, ds) {
    aliases(variables(ds))[urls(variables(ds)) == paste0(self(ds), "variables/", id, "/")]
}

IDtoalias(id, ds=ds2)
