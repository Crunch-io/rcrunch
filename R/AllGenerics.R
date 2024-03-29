# Well, not exactly _all_ generics. Some are moved into files with their methods
# because pkgdown prefers documentation that way.


#' Get and set names, aliases on Catalog-type objects
#'
#' These methods let you get and set names and aliases for variables in a
#' Dataset's catalog, or within [`Subvariables`] in an array
#' variable. They work like the base R names methods.
#'
#' Note that the Dataset `names` method returns the aliases of its
#' variables by default. This behavior is controlled by
#' `envOrOption("crunch.namekey.dataset")`.
#' Set `options(crunch.namekey.dataset="name")` if you wish to use
#' variable names. See the variables vignette for more information.
#'
#' @param x a `VariableCatalog`, `Subvariables`, or similar object
#' @param value For the setters, an appropriate-length character vector to
#' assign
#' @return Getters return the character object in the specified slot; setters
#' return `x` duly modified.
#' @aliases describe-catalog aliases aliases<- descriptions descriptions<-
#' types emails timestamps scriptBody names names<-
#' @seealso [`Subvariables`] [`Categories`] [base::names()]
#' `vignette("variables", package="crunch")`
#' @rdname describe-catalog
setGeneric("aliases", function(x) standardGeneric("aliases"))
#' @rdname describe-catalog
setGeneric("aliases<-", function(x, value) standardGeneric("aliases<-"),
    signature = "x"
)
#' @rdname describe-catalog
setGeneric("descriptions", function(x) standardGeneric("descriptions"))
#' @rdname describe-catalog
setGeneric("descriptions<-",
    function(x, value) standardGeneric("descriptions<-"),
    signature = "x"
)
#' @rdname describe-catalog
setGeneric("emails", function(x) standardGeneric("emails"))
#' @rdname describe-catalog
setGeneric("types", function(x) standardGeneric("types"))
#' @rdname describe-catalog
setGeneric("timestamps", function(x) standardGeneric("timestamps"))
#' @rdname describe-catalog
setGeneric("ids", function(x) standardGeneric("ids"))
#' @rdname describe-catalog
setGeneric("ids<-", function(x, value) standardGeneric("ids<-"))
#' @rdname describe-catalog
#' @export
setGeneric("values", function(x) standardGeneric("values"))
#' @rdname describe-catalog
setGeneric("values<-", function(x, value) standardGeneric("values<-"))
#' @rdname describe-catalog
setGeneric("scriptBody", function(x) standardGeneric("scriptBody"))

#' @rdname describe-catalog
#' @export
setGeneric("dates", function(x) standardGeneric("dates"))
#' @rdname describe-catalog
setGeneric("dates<-", function(x, value) standardGeneric("dates<-"), signature = "x")

setGeneric("names")
setGeneric("names<-")

#' Name, alias, and description for Crunch objects
#'
#' @param x a Dataset or Variable.
#' @param object Same as `x` but for the `alias` method, in order to
#' match the generic from another package.
#' @param value For the setters, a length-1 character vector to assign
#' @param ... additional arguments in the `alias` generic, ignored.
#' @return Getters return the character object in the specified slot; setters
#' return `x` duly modified.
#' @name describe-entity
#' @aliases describe name name<- description description<- alias alias<- startDate
#' startDate<- endDate endDate<- notes notes<- digits digits<- uniformBasis uniformBasis<-
#' @seealso [`Categories`] [`describe-catalog`]
setGeneric("name", function(x) standardGeneric("name"))
#' @rdname describe-entity
setGeneric("name<-", function(x, value) standardGeneric("name<-"))
#' @rdname describe-entity
setGeneric("id", function(x) standardGeneric("id"))
#' @rdname describe-entity
setGeneric("value", function(x) standardGeneric("value"))
#' @rdname describe-entity
setGeneric("value<-", function(x, value) standardGeneric("value<-"))
#' @rdname describe-entity
setGeneric("description", function(x) standardGeneric("description"))
#' @rdname describe-entity
setGeneric("description<-",
    function(x, value) standardGeneric("description<-"),
    signature = "x"
)
#' @rdname describe-entity
setGeneric("startDate", function(x) standardGeneric("startDate"))
#' @rdname describe-entity
setGeneric("startDate<-",
    function(x, value) standardGeneric("startDate<-"),
    signature = "x"
)
#' @rdname describe-entity
setGeneric("endDate", function(x) standardGeneric("endDate"))
#' @rdname describe-entity
setGeneric("endDate<-",
    function(x, value) standardGeneric("endDate<-"),
    signature = "x"
)
#' @rdname describe-entity
setGeneric("alias")
#' @rdname describe-entity
setGeneric("alias<-", function(x, value) standardGeneric("alias<-"),
    signature = "x"
)
#' @rdname describe-entity
setGeneric("digits", function(x) standardGeneric("digits"))
#' @rdname describe-entity
setGeneric("digits<-", function(x, value) standardGeneric("digits<-"))
#' @rdname describe-entity
setGeneric("uniformBasis", function(x) standardGeneric("uniformBasis"))
#' @rdname describe-entity
setGeneric("uniformBasis<-", function(x, value) standardGeneric("uniformBasis<-"))
#' @rdname describe-entity
setGeneric("notes", function(x) standardGeneric("notes"))
#' @rdname describe-entity
setGeneric("notes<-", function(x, value) standardGeneric("notes<-"),
    signature = "x"
)

#' @rdname population
setGeneric("popSize", function(x) standardGeneric("popSize"))
#' @rdname population
setGeneric("popMagnitude", function(x) standardGeneric("popMagnitude"))
#' @rdname population
setGeneric("popSize<-", function(x, value) standardGeneric("popSize<-"))
#' @rdname population
setGeneric("popMagnitude<-", function(x, value) standardGeneric("popMagnitude<-"))
#' @rdname population
setGeneric("setPopulation", function(x, size, magnitude) standardGeneric("setPopulation"))

setGeneric("expr", function(x) standardGeneric("expr"))
setGeneric("expr<-", function(x, value) standardGeneric("expr<-"),
    signature = "x"
)

#' Extract the email from a User Entity
#'
#' @param x a UserEntity returned from `me()`
#' @return a character string of the user's email
#' @rdname user-email
setGeneric("email", function(x) standardGeneric("email"))
#' @rdname geo
setGeneric("geo", function(x) standardGeneric("geo"))
#' @rdname geo
setGeneric("geo<-", function(x, value) standardGeneric("geo<-"))
setGeneric("fetchGeoFile", function(x) standardGeneric("fetchGeoFile"))
#' @rdname Insertions
setGeneric("anchor", function(x, ...) standardGeneric("anchor"))
#' @rdname Insertions
setGeneric("anchors", function(x) standardGeneric("anchors"))
#' @rdname Insertions
setGeneric("anchor<-", function(x, value) standardGeneric("anchor<-"))
#' @rdname Insertions
setGeneric("arguments", function(x, ...) standardGeneric("arguments"))
#' @rdname Insertions
setGeneric("arguments<-", function(x, value) standardGeneric("arguments<-"))
# not exported
setGeneric("subtotalTerms", function(x, ...) standardGeneric("subtotalTerms"))
#' @rdname Insertions
setGeneric("func", function(x) standardGeneric("func"))
#' @rdname Insertions
setGeneric("funcs", function(x) standardGeneric("funcs"))
#' @rdname SubtotalsHeadings
setGeneric("subtotals", function(x) standardGeneric("subtotals"))
#' @rdname SubtotalsHeadings
setGeneric("subtotals<-", function(x, value) standardGeneric("subtotals<-"))
setGeneric("makeInsertion", function(x, var_items, alias) standardGeneric("makeInsertion"))
#' @rdname applyTransforms
setGeneric("subtotalArray", function(x, ...) standardGeneric("subtotalArray"))

#' @rdname type
setGeneric("type", function(x) standardGeneric("type"))
#' @rdname type
setGeneric("type<-", function(x, value) standardGeneric("type<-"))

#' @rdname hide
setGeneric("publicFolder", function(x) standardGeneric("publicFolder"))
#' @rdname hide
setGeneric("hiddenFolder", function(x) standardGeneric("hiddenFolder"))
#' @rdname hide
setGeneric("privateFolder", function(x) standardGeneric("privateFolder"))
#' @rdname hide
setGeneric("hide", function(x) standardGeneric("hide"))
#' @rdname hide
setGeneric("unhide", function(x) standardGeneric("unhide"))
#' @rdname hide
setGeneric("privatize", function(x) standardGeneric("privatize"))
#' @rdname hide
setGeneric("deprivatize", function(x) standardGeneric("deprivatize"))

#' @rdname derivations
setGeneric("derivation", function(x) standardGeneric("derivation"))
#' @rdname derivations
setGeneric("derivation<-", function(x, value) standardGeneric("derivation<-"))


setGeneric("urls", function(x) standardGeneric("urls"))
#' @rdname self
setGeneric("self", function(x) standardGeneric("self"))

#' Get a fresh copy from the server
#'
#' Crunch objects generally keep themselves in sync with the server when you
#' manipulate them, but some operations cause the local version to diverge from
#' the version on the server. For instance, someone else may have
#' modified the dataset you're working on, or maybe
#' you have modified a variable outside of the context of its dataset.
#' `refresh()` allows you to get back in sync.
#'
#' @param x pretty much any Crunch object
#' @return a new version of `x`
#' @name refresh
#' @aliases refresh
#' @importFrom httpcache dropCache dropOnly
setGeneric("refresh", function(x) standardGeneric("refresh"))
setGeneric("entities", function(x, ...) standardGeneric("entities"))
setGeneric("entities<-", function(x, value) standardGeneric("entities<-"))
setGeneric("tuple", function(x) standardGeneric("tuple"))
setGeneric("tuple<-", function(x, value) standardGeneric("tuple<-"))
#' @rdname tuple-methods
setGeneric("entity", function(x) standardGeneric("entity"))
#' @rdname shoji-index
setGeneric("index", function(x) standardGeneric("index"))
#' @rdname shoji-index
setGeneric("index<-", function(x, value) standardGeneric("index<-"))
setGeneric("active", function(x) standardGeneric("active"))
setGeneric("archived", function(x) standardGeneric("archived"))
setGeneric("imported", function(x) standardGeneric("imported"))
setGeneric("pending", function(x) standardGeneric("pending"))

#' @rdname multitable-catalog
setGeneric("multitables", function(x) standardGeneric("multitables"))
#' @rdname multitable-catalog
setGeneric("multitables<-", function(x, value) standardGeneric("multitables<-"))
#' @rdname filter-catalog
setGeneric("filters", function(x) standardGeneric("filters"))
#' @rdname filter-catalog
setGeneric("filters<-", function(x, value) standardGeneric("filters<-"))
setGeneric("appliedFilters", function(x) standardGeneric("appliedFilters"))
setGeneric(
    "appliedFilters<-",
    function(x, value) standardGeneric("appliedFilters<-")
)
setGeneric("activeFilter", function(x) standardGeneric("activeFilter"))
setGeneric(
    "activeFilter<-",
    function(x, value) standardGeneric("activeFilter<-")
)
#' @rdname derivations
setGeneric("is.derived", function(x) standardGeneric("is.derived"))
#' @rdname derivations
setGeneric("is.derived<-", function(x, value) standardGeneric("is.derived<-"))
#' @rdname variable-as-methods
setGeneric("as.Text", function(x, ...) standardGeneric("as.Text"))
#' @rdname variable-as-methods
setGeneric("as.Numeric", function(x) standardGeneric("as.Numeric"))
#' @rdname variable-as-methods
setGeneric("as.Categorical", function(x, ...) standardGeneric("as.Categorical"))
#' @rdname variable-as-methods
setGeneric("as.Datetime", function(x,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   resolution,
                                   offset) {
    standardGeneric("as.Datetime")
})
setGeneric("groupClass", function(x) standardGeneric("groupClass"))
setGeneric("entityClass", function(x) standardGeneric("entityClass"))
setGeneric("entitiesInitializer", function(x) standardGeneric("entitiesInitializer"))
setGeneric("folderExtraction", function(x, tuple) standardGeneric("folderExtraction"))
setGeneric("personalFolder", function(x) standardGeneric("personalFolder"))
setGeneric("rootFolder", function(x) standardGeneric("rootFolder"))

#' Get a dataset's weightVariables
#'
#' @param x a CrunchDataset
#' @return `weightVariables` returns a character vector of the aliases of the
#' variables that are eligible to be used as weights.
#' @seealso [weight()] [makeWeight()] [modifyWeightVariables()]
#'
#' @export
setGeneric("weightVariables", function(x) standardGeneric("weightVariables"))

#' @rdname modifyWeightVariables
setGeneric("weightVariables<-", function(x, value) standardGeneric("weightVariables<-"))
#' @rdname modifyWeightVariables
setGeneric("is.weightVariable<-", function(x, value) standardGeneric("is.weightVariable<-"))

#' @rdname weight
setGeneric("is.weight<-", function(x, value) standardGeneric("is.weight<-"))

setGeneric("whichCatalogEntry", function(x, i, ...) standardGeneric("whichCatalogEntry"))
#' @rdname script-catalog
setGeneric("scripts", function(x) standardGeneric("scripts"))

#' Undo behavior of a Crunch Automation Script
#'
#' There are two ways to revert the output of a script:
#' - `undoScript()` - A "softer" delete of a script's created artifacts and variables, or
#' - `revertScript()` - A "harder" revert that returns the dataset to the state it was before
#'   running such script.
#'
#' The difference between both is that a hard revert restores the dataset, as it drops all
#' ensuing scripts and their output (artifacts and variables), while an undo only deletes the
#' artifacts and variables created by this script, but changes made by other scripts and this
#' script's record will remain in place.
#'
#' The function `scriptSavepoint()` gets the version object
#'
#' @param dataset A `CrunchDataset`
#' @param x A `Script` or index for a `ScriptCatalog` (generally a number)
#' @return For `undoScript()` and `revertSctipt()`, invisibly return the updated dataset.
#' For `scriptSavePoint()` a version list object that can be used in [`restoreVersion()`].
#' @name automation-undo
#' @aliases undoScript
#' @seealso [`runCrunchAutomation()`] & [`script-catalog`]
#' @export
setGeneric("undoScript", function(dataset, x) standardGeneric("undoScript"))
#' @rdname automation-undo
setGeneric("revertScript", function(dataset, x) standardGeneric("revertScript"))
#' @rdname automation-undo
setGeneric("scriptSavepoint", function(x) standardGeneric("scriptSavepoint"))

#' @rdname dataset-owner
setGeneric("owner", function(x) standardGeneric("owner"))
#' @rdname dataset-owner
setGeneric("owner<-", function(x, value) standardGeneric("owner<-"))

#' @rdname cube-missingness
setGeneric("showMissing", function(cube) standardGeneric("showMissing"))
#' @rdname cube-missingness
setGeneric("hideMissing", function(cube) standardGeneric("hideMissing"))
#' @rdname cube-missingness
setGeneric("showIfAny", function(cube) standardGeneric("showIfAny"))

setGeneric("dim")
#' @rdname dim-dataset
setGeneric("ncol")
#' @rdname na-omit-categories
setGeneric("na.omit")
setGeneric("as.environment")
setGeneric("dimnames")

#' @rdname cube-methods
setGeneric("dimensions", function(x) standardGeneric("dimensions"))
#' @rdname cube-methods
setGeneric("dimensions<-", function(x, value) standardGeneric("dimensions<-"))
#' @rdname cube-methods
setGeneric("measures", function(x) standardGeneric("measures"))

setGeneric("subset")
setGeneric("which", signature = "x")


#' Extract and modify Crunch objects
#'
#'
#'
#' @param x a Crunch object (Dataset, Variable, `CrunchExpr`, Catalog,
#' `CrunchCube`, etc.)
#' @param i The elements to extract; as with R, this can generally be (1) a
#' logical vector of length matching `x`; (2) a character vector of appropriate
#' length, which can generally be URLs or also names, aliases, or other
#' identifier; (3) integer indices, potentially negative, to select. Datasets
#' and Variables can also be subset by `CrunchExpr`. Note that when subsetting
#' Datasets and variables by R vectors, the input `i` is turned into a
#' `CrunchExpr` so that it can be used in API queries.
#' @param name for `$`, the same as `i` for `[[`
#' @param j For two-dimensional objects, elements to take from the columnar
#' dimension. Note that Datasets work like `data.frame`s in that you can extract
#' columns either by `ds[, j]` or by the list-like `ds[i]` (with no comma).
#' @param drop Ignored and always assumed `FALSE`
#' @param ... additional arguments supported by some methods. For example, some
#' catalogs support a `secondary` vector of elements to match
#' against like `emails(x)` or `owners(x)`; by default this is `names(x)`. This
#' lets (for example) you to look up variables by URL but fall back to name.
#' @param value For updating, an object of the appropriate class and size to
#' insert. This is generally the same class of object you would get from the
#' extract method; i.e. for `x[[i]] <- value`, `value` must be the same class
#' as `x[[i]]`. Exceptions include `ds$var <- VariableDefinition(...)` to create
#' a new variable, and `ds$var[i] <- value`, which can take R vectors and
#' `CrunchExpr`.
#' @return `[` generally returns a subset of `x`, same class and "self" URL;
#' when subsetting with a `CrunchExpr`, the expression is recorded in an
#' attribute of the object. `[[` and `$`
#' return appropriate extractions from `x`, wherever possible without making an
#' additional API request. The assignment methods return `x` appropriately
#' updated. Most assignment methods do not make an API request except where
#' they clearly are used to create a new entity (as in assigning a variable
#' definition to a Dataset); for editing metadata attributes, these methods
#' generally assume that the API request to set the changes on the server
#' happens in a different method (e.g. in `names(variables(ds)[1:5]) <- value`,
#' assignment happens in the `names<-` method).
#' @name crunch-extract
#' @keywords internal
NULL

setGeneric("lapply")
setGeneric("is.na")
setGeneric("is.na<-")
setGeneric("write.csv", function(x, ...) utils::write.csv(x, ...))
#' @rdname duplicated
setGeneric("duplicated")

setGeneric("zcl", function(x) standardGeneric("zcl"))

#' toJSON methods for Crunch objects
#'
#' `crunch` uses the `jsonlite` package for JSON serialization and
#'  deserialization. Unfortunately, \code{\link[jsonlite:fromJSON]{jsonlite::toJSON()}}
#' does not allow for defining S4 methods for other object types. So,
#' `crunch::toJSON` wraps `jsonprep`, which exists to translate
#' objects to base R objects, which `jsonlite::toJSON` can handle.
#' `jsonprep` is defined as an S4 generic, and it is exported, so you can define
#' methods for it if you have other
#' objects that you want to successfully serialize to JSON.
#'
#' @param x the object
#' @param ... additional arguments
#' @param for_query_string If `TRUE`, and `crunch.stabilize.query` option
#' is also set to `TRUE`, then dictionary items in the JSON are
#' sorted alphabetically, which can be useful when capturing
#' mocks using "httptest".
#' @return `jsonprep` returns a base R object that `jsonlite::toJSON`
#' can handle. `toJSON` returns the JSON-serialized character object.
#' @name tojson-crunch
#' @seealso \code{\link[jsonlite:fromJSON]{jsonlite::toJSON()}}
#' @aliases jsonprep
#' @export
setGeneric("jsonprep", function(x, ...) standardGeneric("jsonprep"))

setGeneric(
    "getShowContent",
    function(x, ...) standardGeneric("getShowContent")
)

.backstopUpdate <- function(x, i, j, value) {
    ## Backstop error so you don't get "Object of class S4 is not subsettable"
    halt(paste("Cannot update", class(x), "with type", class(value)))
}

#' @rdname analysis-methods
#' @export
setGeneric("filter", function(x, ...) standardGeneric("filter"))
#' @rdname analysis-methods
#' @export
setGeneric("filter<-", function(x, value) standardGeneric("filter<-"))
#' @rdname weight
#' @export
setGeneric("weight", function(x) standardGeneric("weight"))
#' @rdname weight
#' @export
setGeneric("weight<-", function(x, value) standardGeneric("weight<-"))


#' Get the palettes from a crunch object
#'
#' `CrunchDataset`s have color palettes associated with them
#' that can be used as default colors for dashboard tiles. One of
#' them can be assigned the "default".
#'
#' @param x A crunch object, like a `CrunchDataset`
#' @param ... ignored (reserved for future expansion)
#' @name palettes
#' @export
setGeneric("palettes", function(x) standardGeneric("palettes"))

#' @rdname palettes
#' @export
setGeneric("defaultPalette", function(x, ...) standardGeneric("defaultPalette"))

# for ggplot to not complain when given crunchdata
#' Fortify crunch objects for use with ggplot
#'
#' @param model model or other R object to convert to data frame
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @name fortify
#' @export fortify.CrunchDataFrame
#' @keywords internal
fortify.CrunchDataFrame <- function(model, data, ...) model

#' @rdname fortify
#' @export fortify.CrunchDataset
fortify.CrunchDataset <- function(model, data, ...) model

setGeneric(
    "sendCrunchAutomationScript",
    function(x, ...) standardGeneric("sendCrunchAutomationScript")
)
