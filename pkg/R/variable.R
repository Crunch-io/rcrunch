    
is.variable <- function (x) inherits(x, "CrunchVariable")

## do a setAs("character", "CrunchVariable") which GETs (assuming url) and then does as() using the shoji method
## do a setAs("shoji", "CrunchVariable")

