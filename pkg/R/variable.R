CrunchVariable <- setClass("CrunchVariable", 
    representation(
        name="character",
        alias="character", 
        description="character",
        self.url="character"
    ))
    
is.variable <- function (x) inherits(x, "CrunchVariable")