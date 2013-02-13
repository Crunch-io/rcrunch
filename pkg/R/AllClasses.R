##' Mix-in class for multiple inheritance of variables and datasets.
##' 
##' Exists for common methods in interacting with Crunch API only. Has no
##' Extract methods declared so as not to conflict with the
##' vector/list/data.frame methods jointly inherited in CrunchVariable and
##' CrunchDataset.
ShojiObject <- setClass("ShojiObject",
    representation(
        element="ANY",
        self="ANY",
        description="ANY",
        body="ANY",
        urls="ANY",
        entities="ANY",
        specification="ANY",
        template="ANY"
    ))

CrunchVariable <- setClass("CrunchVariable", contains="ShojiObject")
    
CrunchDataset <- setClass("CrunchDataset", contains=c("list", "ShojiObject"))
