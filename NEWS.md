### crunch 1.6.1

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
