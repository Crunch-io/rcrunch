    
is.variable <- function (x) inherits(x, "CrunchVariable")

## do a setAs("character", "CrunchVariable") which GETs (assuming url) and then does as() using the shoji method
## do a setAs("shoji", "CrunchVariable")

.cr.variable.shojiObject <- function (x, ...) {
    out <- CrunchVariable(x, ...)
    return(out)
}

setAs("ShojiObject", "CrunchVariable", 
    function (from) .cr.variable.shojiObject(from))
setAs("shoji", "CrunchVariable", 
    function (from) do.call("CrunchVariable", from))
    
as.variable <- function (x) as(x, "CrunchVariable")