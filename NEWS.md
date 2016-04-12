### crunch 1.9.5 (Under development)
* Add warning that the `pattern` argument for functions including `makeArray`, `makeMR`, `deleteVariables`, and `hideVariables` is being deprecated. The help pages for those functions advise you to grep for or otherwise identify your variables outside of these functions.
* Do more validation that `type<-` assignment is safe.

### crunch 1.9.4
* Add a "session" object, retrievable by either `session()` or returned from `login()`, containing the various catalog resources (Datasets, etc.).
* Additional methods on the dataset catalog, such as `names<-`.
* Extract from most catalogs either by URL or name.
* Initial implementation of Projects API.
* `loadDataset` with a dataset catalog tuple, allowing some degree of tab completion by dataset name. (Example: `cr <- login(...); ds <- loadDataset(cr$datasets$My_Dataset_Name)`)
* Update tests to pass with forthcoming release of `testthat`.
* Remove `useAlias` attribute of datasets and move it to a global option, "crunch.namekey.dataset", defaulted to "alias". Implement the same for array variables, "crunch.namekey.array", and default to "name" for consistency with previous versions. This default will change in a future release.
* New Progress API for checking status of pending, long-running server jobs.
* Switch `as.vector` for `CrunchExpr` to GET rather than POST.

### crunch 1.9.2
* `forkDataset` to make a fork (copy) of a dataset; `mergeFork` to merge changes from a fork back to its parent (or vice versa)
* Remove a duplicate request made when setting variable order
* Update to new API to get a datetime variable's rollup resolution and save a request

## crunch 1.9.0

#### Major changes
* Pull HTTP query cache out to the [httpcache](https://github.com/nealrichardson/httpcache) package and take dependency on that. Remove dependency on `digest` package (httpcache depends on it instead).
* New vignette on [filters and exclusions](inst/doc/filters.md)
* `combine` categories of categorical and categorical-array variables, and responses of multiple-response variables, into new derived variables
* `startDate` and `endDate` attributes and setters for dataset entities (#10, #11)
* Allow editing of filter expressions in UI filter objects (`CrunchFilter`)

#### Other changes
* Improved validation for "name" setting, especially for categories
* Speed up `ncol(ds)` by removing a server request
* Speed up variable catalog editing by avoiding unnecessary updates to the variable order
* Fix cache invalidation when reordering subvariables
* Improve error message for subscript out of bounds in catalog objects
* Include active filter in print method for datasets and variables, if applicable

## crunch 1.8.0
* More formal support for creating and managing UI filters
* Better print method for Crunch expressions (`CrunchExpr`): prints an R formula-like expression
* Fix error in reading/writing query cache with a very long querystring. Requires new dependency on the `digest` package.
* Fix bug in assigning `name(ds$var$subvar) <- value`
* Fix overly rigid validation in `share`
* Update API usage to always send full variable URLs in queries

### crunch 1.7.12
* Add method for R logical &/| Crunch expression
* Upgrade for compatibility with httr 1.1

### crunch 1.7.10
* `addSubvariable` function to add to array and multiple response variables (#7)
* Make paginated requests to GET /values/ for greater reliability

### crunch 1.7.8
* Update to match changes in filter API

### crunch 1.7.6
* `dropRows` to permanently delete rows from a dataset.
* Better print method for catalog resources, using the new `catalogToDataFrame` function.
* Export a few more functions (`shojiURL`, `batches`)

### crunch 1.7.4
* Catch `NULL` in cube dimension when referencing subvariable that does not exist (as when using alias instead of name) and return a useful message.
* Fix for unintended substring matching in `%in%` expression translation.
* Internal change to match user catalog API update

### crunch 1.7.3
* Update docs to conform to R-devel changes to `as.vector`'s signature.

### crunch 1.7.2
* `addVariables` function to add multiple variables to a dataset efficiently
* Support aggregating with `CrunchExpr`s and filtered variables in `table`
* Save a variable catalog refresh on (un)dichotomize. Slight speedup as a result.
* Fix bug in creating VariableOrder with a named list.

## crunch 1.7.0
* Improve performance of many operations by more lazily loading variable entities from the server. Changes to several internal package APIs to make that happen, but the public package interface should be unchanged.
* Also speed up loading of variable catalogs by deferring resolution of relative subvariable URLs until requested. Eliminates significant load time for datasets with lots of array variables.
* Fix bug in results from `crtabs` when requesting a crosstab of three or more dimensions.

### crunch 1.6.1
* `VariableDefinition` (or `VarDef`) function and class for creating variable definitions with more metadata (rather than assigning R vectors into a dataset and having to add metadata after).
* Reworked various new variable functions, including `copy`, `makeArray`, and `makeMR`, to return `VariableDefinition`s rather than creating the new variables themselves. Creation happens on assignment into the dataset.
* Support adding No Data (`NA` for categoricals) even if No Data doesn't already exist
* Tools for logging and profiling HTTP requests and cache performance. See `?startLog` and `?logMessage`.
* Support deep copying of non-array variables.

## crunch 1.6.0
* Check for new version of the package on GitHub when the package is loaded.
* Make a shallow `copy` of a variable. See `?copyVariable`.
* Fix error in updating the values of a subvariable in an array.
* Handle case of assigning `NULL` into a dataset when the referenced variable (alias) does not exist.
* More support for `NA` assignment into variables.

### crunch 1.5.4
* Gradually slow the polling of `/batches/` while waiting for an append to complete. Improves the performance of the append operation.
* New `c` method for Categories, plus support for creating and adding new categories to variables. See `?Categories` and `?"c-categories"`
* Get category ids or numeric values from `as.vector` by specifying a "mode" of "id" or "numeric", respectively. See `?"variable-to-R"`
* Set values as missing by assigning `NA` into variables.

### crunch 1.5.3
* Always send No Data category when creating Categorical Variables.
* Fixed minor bugs in `margin.table` on `CrunchCube` objects.
* Better validation of category subsetting.

### crunch 1.5.2
* Add Python-esque context manager for use in `with` statements. Use it to give `consent()` to delete things.
* Delete variables by `<- NULL` into a dataset (like removing a column from a data.frame). Requires consent. Also create `deleteVariable(s)` functions that also return the dataset object. Use either method to prevent your dataset from getting out of sync with the server when you delete variables.
* Delete subvariables from within array variables with `deleteSubvariable(s)`.
* Better evaluation of formulas within `crtabs` to allow you to crosstab array subvariables.
* Update to new exclusion API.

### crunch 1.5.1
* Validate inputs on making filter expressions with categorical variables
* Very basic print methods for all Crunch objects

## crunch 1.5.0
* Subset rows of datasets and variables for analysis, using either `[` or `subset`
* Access and set `exclusion` filters on datasets to drop certain rows
* Fix some inconsistent handling in R of filters that are set on the server (i.e. for persistent viewing in the web application)
* `(un)lock` datasets for editing when there are multiple editors

### crunch 1.4.3
* Send better emails when sharing datasets

### crunch 1.4.2
* Support for auto-login in Jupyter notebooks
* One more CRANdated import

### crunch 1.4.1
* Import functions from methods, stats, and utils, per change in CRAN policy.

## crunch 1.4.0
* Functions `saveVersion` and `restoreVersion` for dataset versioning
* Update requirement to `httr` 1.0; remove dependency on `RCurl` in favor of `curl`
* Minor API updates
* Fix for some issues authenticating on Windows
* Fix bug in editing array variables with a single subvariable

### crunch 1.3.3
* More tools (not yet exported) for managing users

### crunch 1.3.2
* Adapt to minor updates in append API: new intermediate "appended" state for append operations.

### crunch 1.3.1
* More methods for managing teams
* Prepare for httr 1.0

## crunch 1.3.0
* Provisional interface for managing users and teams.
* Improved messaging for failure modes in `appendDataset`.
* Adapt to minor updates in append API
* Fix bug in updating an array with only one subvariable.

### crunch 1.2.2
* Add `types` method to VariableCatalog.

### crunch 1.2.1
* Additional methods for working with VariableOrder and VariableGroup. You can create new Groups by assigning into an Order or Group with a new name. And, with the new `duplicates` parameter, which is `FALSE` by default, adding new Groups to an Order "moves" the variable references to the new Group, rather than creating copies. See the [variable order vignette](inst/doc/variable-order.md) for more details.
* Add `share` function for sharing a dataset with other users.

## crunch 1.2.0
* New vignettes for [deriving variables](inst/doc/derive.md) and [analyzing datasets](inst/doc/analyze.md).

* Update appending workflow to support new API.

### crunch 1.1.1
* Remove all non-ASCII from test files so that tests will run on Solaris.

## crunch 1.1.0
* Add query cache, on by default.

* `as.data.frame` now does not return an actual `data.frame` unless given the argument `force=TRUE`. Instead, it returns a `CrunchDataFrame`, and environment containing unevaluated promises. This allows R functions, particularly those of the form `function(formula, data)` to work with CrunchDatasets without copying the entire dataset from the server to local memory. Only the variables referenced in the formula fetch data when their promises evaluated.

* Remove `RJSONIO` dependency in favor of `jsonlite` for `toJSON`.

# crunch 1.0.0
* Rename package to `crunch`. Update all docs to reflect that. Make amendments to pass CRAN checks.

## rcrunch 0.11.1
* `newDataset2` renamed to `newDatasetByCSV` and made to be the default strategy in `newDataset`. The old `newDataset` has been moved to `newDatasetByColumn`.

* Support for NA and NaN in `crtabs` response.

## rcrunch 0.11.0
* `getCube` is now `crtabs`. Ready for more extensive beta testing. Has prop.table and margin.table methods. Vignette forthcoming.

* `newDataset2` that uses the CSV+JSON import method, rather than the columm-by-column strategy that `newDataset` uses.

## rcrunch 0.10.0

* Support for shoji:order document for hierarchical variable order. HTTP API change.

* Initial, limited support for `xtabs`-like crosstabbing with a formula with the `getCube` function.
