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

CrunchVariable <- setClass("CrunchVariable", contains="ShojiObject",
    representation= representation(
        readonly="logical"
    ), 
    prototype=prototype(readonly=FALSE))
NumericVariable <- setClass("NumericVariable", contains="CrunchVariable")
CategoricalVariable <- setClass("CategoricalVariable",
    contains="CrunchVariable")
TextVariable <- setClass("TextVariable", contains="CrunchVariable")
DatetimeVariable <- setClass("DatetimeVariable", contains="CrunchVariable")
CategoricalArrayVariable <- setClass("CategoricalArrayVariable", contains="CrunchVariable")
MultipleResponseVariable <-setClass("MultipleResponseVariable", contains="CategoricalArrayVariable")

CrunchDataset <- setClass("CrunchDataset", contains=c("list", "ShojiObject"),
    representation= representation(
        readonly="logical"
    ), 
    prototype=prototype(readonly=FALSE))

Categories <- setClass("Categories", contains="list")
Category <- setClass("Category", contains="namedList")