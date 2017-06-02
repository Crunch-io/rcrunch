### crunch 1.16.1 (under development)

* `dashboard` and `dashboard<-` methods to view and set a dashboard URL on a dataset
* `changeCategoryID` function to map categorical data to a new "id" and value in the data (#38, #47)
* Fix issue in printing filter expressions with long value columns
* Progress bars now clean up after themselves and do not leave the prompt hanging out at the end of the line
* Fix issue where long expressions were truncated. By default, long expressions are not truncated, although they can be when it is appropriate (ie when creating a new dataset without a name). (#39, #45)
* Added `importMultitable()` to copy a multitable form one dataset to another. Additionally, `Multitable`s now have a show method showing its name and column variables.
* Can now extract variables from a dataset by the variable URL
* `appendDataset()` now truly appends a dataset and no longer upserts if there is a primary key set. This is accomplished by removing the primary key before appending. (#35)
* Primary keys can now be viewed with `pk(dataset)` and set with `pk(dataset) <- variable`.

## crunch 1.16.0

### Cube and tab book improvements

* Reshape TabBookResults that contain categorical array variables so that `prop.table` computations line up with those not containing array variables (i.e. move subvariables to the third array dimension in the result).
* Add `names`, `aliases`, and `descriptions` methods to `CrunchCube` (corresponding to variables of the dimensions in the cube), `MultitableResult` (corresponding to the "column" variables of the cubes in the result), and `TabBookResult` (corresponding to the "row"/"sheet" variables in each multitable result).
* Fix `names` method for TabBookResults following an API change.
* Extend `crtabs` formula parsing to support multiple, potentially named, measures

### Other new features

* `weightVariables` method to display the set of variables designated as valid weights. (Works like `hiddenVariables`.)
* In `appendDataset`, allow specifying a subset of rows to append (in addition to the already supported selection of variables)
* `loadDataset` can now load a dataset by its URL.

### Housekeeping

* Remove "confirm" argument from various delete functions (deprecated since 1.14.4) and the "cleanup" argument to append (deprecated since 1.13.4)
* All destructive actions now require 'consent', even in non-interactive mode. See `?with_consent` for more details.
* Improvements to validation when updating values in a dataset.
* Move mock API fixtures to `inst/` so that other packages depending on this package can access them more easily.

### crunch 1.15.2
* Support for additional dataset export arguments
* Add `is.derived` method for Variables
* Allow a 'message' when sharing a dataset (#27)
* More validation for the input to the various export functions
* Fix handling of "total" column in `TabBookResult`s when the row variable is a categorical array

## crunch 1.15.0
* `multitables` method to access catalog from a Dataset. `newMultitable` to create one. See `?multitables` and `?newMultitable` for docs and examples.
* `tabBook` to compute a tab book with a multitable. If `format="json"` (the default), returns a `TabBookResult` containing `CrunchCube` objects with which further analysis or formatting can be done.
* `bases` method for cubes and tab book responses to access unweighted counts and margin tables.
* Handle case of attempting to `saveVersion` when there are no changes since the last saved version.
* Update to work with [roxygen2 6.0.0 release](https://github.com/klutometis/roxygen/issues/568)

### crunch 1.14.4
* `newFilter` and `newProject` functions to create those objects more directly, rather than by assigning into their respective catalogs.
* Require confirmation before doing a "force" merge in `mergeFork`.
* Add `with_consent` as an alternative to `with(consent(), ...)`
* Deprecate the "confirm" argument to destructive functions and methods such as `delete` in favor of the `consent` context manager.
* Add deprecation warning that destructive actions will soon also require consent when running in a non-interactive R session.
* Use [httptest](https://github.com/nealrichardson/httptest) for mocking HTTP and the Crunch API.

### crunch 1.14.2
* Trivial change to DESCRIPTION to meet new, hidden CRAN requirement

## crunch 1.14.0
* `embedCrunchBox` to generate embeddable HTML markup for CrunchBoxes
* `duplicated` method for Crunch variables and expressions
* Prevent invalid expressions with incorrect variable references from making bad requests
* Print methods for Category/ies now show category ids
* Speed up `as.vector` and `as.data.frame` methods by smarter pagination of requests.
* Option "crunch.namekey.variableorder" to govern how VariableOrder is printed. Current default is "name", the status quo, but set it to "alias" to have `ordering` print aliases.
* Support for `is.na<-` to set missing values on a variable, equivalent to assigning `NA`
* Fix behavior and validation for subsetting datasets/variables that are already subsetted by a Crunch expression object.
* Allow setting a variable entity to `settings(ds)$weight` and not just its `self` URL.

### crunch 1.13.8
* `crunchBox` to make a public, embeddable analysis widget
* `settings` and `settings<-` to view and modify dataset-level controls, such as default "weight" and viewer permissions ("viewers_can_change_weight", "viewers_can_export")
* `flattenOrder` to strip out nested groups from an order
* Univariate statistics on variables, such as `mean`, `median`, and `sd`, now respect filter expressions, as does the `summary` method.
* "median" can now be used in `crtabs`
* Copying and deriving variables now bring in the "notes" attribute.
* Improve error handling when attempting to `loadDataset` from a nonexistent project.

### crunch 1.13.6
* More utility functions for working with order objects: `dedupeOrder`, `removeEmptyGroups`
* `appendDataset` can now append a subset of variables
* Update to changes in the dataset version API
* Fix bug in assigning NA to an array subvariable that didn't already have the "No Data" category

### crunch 1.13.4
* `flipArrays` function to generate derived views of array subvariables
* Add "autorollback" argument to `appendDataset`, defaulted to `TRUE`, which ensures that a failed append leaves the dataset in a clean state.
* `allVariables` is now ordered by the variable catalog's order, just as `variables` has always been.
* Add "force" argument to `mergeFork`.
* Support an `as_array` (pseudo-)function in `crtabs` that allows crosstabbing a multiple-response variable as if it were a categorical array.
* Fix bug in dataset export when attempting to export a single variable

### crunch 1.13.2
* Support deep copying of categorical array variables.
* Join (`merge`) a subset of variables and/or rows of a dataset.
* `moveToGroup` function and setter for easier adding of variables to existing groups.
* `locateEntity` function to find a variable or dataset within a potentially deeply nested order.
* Change default key for printing `hiddenVariables` from "name" to "alias", governed by `options(crunch.namekey.dataset)` as elsewhere
* Allow disabling of check for new package releases on load by setting `options(crunch.check.updates=FALSE)`.
* Return a Session object from `session()` that lazily fetches catalogs rather than when instantiated.

## crunch 1.13.0
* `as.vector` on a categorical-array or multiple-response variable now returns a `data.frame`. While a `matrix` is a more accurate representation of the data type, using `data.frame` allows for more intuitive accessing of subvariables by `$`, just as they are from the Crunch dataset.
* Enhancements to merge/extendDataset: a "by" argument as a shortcut for "by.x" and "by.y"; referencing "by" variables by alias; and aliasing the function also through `joinDatasets` with its (new) default `copy=TRUE` argument.
* POST new array variable definitions that are a series of subvariable definitions as a single request, rather than uploading each subvariable separately and then binding.
* Improve `addSubvariable` to PATCH rather than unbind and rebind; also extend it to accept more than one (sub)variable to add to the array.
* Remove `pattern` matching argument from `makeArray`, `makeMR`, `deleteVariables`, and `hideVariables`, deprecated since 1.9.6.
* Standardize `deleteSubvariable` to follow model of `deleteVariable`, including requiring consent to delete.
* [New vignette](inst/doc/export.md) on downloading data to your local R session and exporting datasets to file formats.
* Preparation for upcoming API changes.

### crunch 1.12.2
* Patch a test for handling duplicate factor levels, which is deprecated in current R releases but converted to an error in the upcoming release.

## crunch 1.12.0
* **Breaking change**: Accessing subvariables from array variables is now done by alias, just as variables are extracted from a Dataset. The "crunch.namekey.dataset" and "crunch.namekey.array" options have existed for a while, but they've had different default values. Now both default to "alias", which should offer a more consistent interface. If you want to maintain the old behavior, you can set `options(crunch.namekey.array="name")` in your script or in your .Rprofile.
* `deleteSubvariable` now follows "crunch.namekey.array" and will take either subvariable names or aliases, depending on the value of the setting.
* New `extendDataset` function, also aliased as `merge`, to allow you to add columns from one dataset to another, joining on a key variable from each.
* `compareDatasets` now checks the subvariable matching across array variables in the datasets to identify additional conflicts.
* Creating Crunch logical expressions that reference category names that do not exist for the given variable no longer errors; instead, a warning is given, and the unknown category names are dropped from the expression so that they evaluate as intended.
* `notes` and `notes<-` methods for datasets, variables, and variable catalogs to view and edit those new metadata fields.
* Update for API change in dataset export.
* Attempting to assign a `name<-` on `NULL` (i.e. when you reference a variable in a dataset using `$` and the variable does not exist) returns a helpful message.
* Fix dataset import via `newDataset` when passing a `data.frame` or similar that has spaces in the column names.
* Handle the (deprecated in R) case of duplicate factor levels when translating to categorical in `toVariable`

### crunch 1.11.2
* Fix issue with sharing datasets owned by a project.
* Support updating Categorical variables created from R logical-type vectors with logical values
* Remove "crunch.max.categories" option to govern converting factors to Crunch categorical variables only if fewer than that threshold. Use `as.character` if you have a factor and want it to be imported as type Text.
* Increase default "crunch.timeout" for long-running jobs to 15 minutes, after which point progress polling will give up.
* Add `cleanseBatches` function to remove batch records from failed append attempts. Remove deprecated code around batch conflict reporting.
* Validation to prevent attempting to set NA category names.

## crunch 1.11.0
* Generic `datasets` and `projects` functions to get dataset and project catalogs. (`datasets` previously existed only as a method for Project entities.)
* Add `project` argument to `listDatasets` and add `project` and `refresh` to `loadDatasets` to facilitate viewing and loading datasets that belong to projects.
* New function `compareDatasets` that shows how datasets will line up when appending. A `summary` method on its return value prints a report that highlights areas of possible mismatch.
* Support computing numeric aggregates (mean, max, etc.) of categorical variables with numeric values in `crtabs`
* Allow `NULL` assignment into Variable/DatasetGroups to remove elements

### crunch 1.10.6
* Fix refresh method for Datasets that have been transferred to a Project.
* (Re-)improve print method for expressions involving categorical variables
* Improve handling of filters when composing complex expressions of `CrunchExpr`, Variable, and Dataset objects
* Add expression support for operations involving a `DatetimeVariable` and a character vector, assumed to be ISO-8601 formatted.
* Export a `permissions` method for Datasets to work directly with sharing privileges.

### crunch 1.10.4
* Fix `as.data.frame`/`as.environment` for `CrunchDataset` when a variable alias contained an apostrophe.
* Better print method for project `MemberCatalog`.
* Fix for [change in 'jsonlite' API](https://github.com/jeroenooms/jsonlite/issues/130#issuecomment-225971209) in its v0.9.22
* Progress polling now returns the error message, if given, if a job fails.

### crunch 1.10.2
* `exportDataset` to download a CSV or SAV file of a dataset. `write.csv` convenience method for CSV export.
* Correctly parse datetimes that don't include timezone information.
* Add `icon` and `icon<-` methods for Projects to read the project's current icon URL and to set a new icon by supplying a local file name to upload.
* Get and set "archived" and "published" status of a dataset with `is.archived`, `is.draft`, and `is.published` (the inverse of `is.draft`). See `?publish` for more.
* Add `draft` argument to `forkDataset`
* Support for future API to handle failed long-running jobs.
* Assorted updates to new API usage

## crunch 1.10.0

#### New support for working with users and their permissions on datasets and projects

* Add `owner` and `owner<-` for datasets to read and modify the owner
* Add `owners` and `ownerNames` for DatasetCatalog
* `is.editor` and `is.editor<-` for project MemberCatalog
* `me` function to get the user entity for yourself

#### Other changes

* Add missing print method for DatasetOrder
* Support creating OrderGroups (for both Datasets and Variables) by assigning URLs into a new group name
* Improve support for parsing datetime data values
* Fix bug in setting nested groups inside DatasetOrder
* Fix failure on interactive login in R.app on OS X

### crunch 1.9.12
* Generalize and update to new Progress API. Add a progress bar.
* Remove deprecated query parameter on variable catalog

### crunch 1.9.10
* `variableMetadata` function to export all variable metadata associated with the dataset

### crunch 1.9.8
* Better support for deleting hidden variables
* Allow subsetting of datasets to include hidden variables
* Require that version names must be a single string value
* Fix bug in print method for VariableOrder that manifested when fixing the variable catalog's relative URL API

### crunch 1.9.6
* Add warning that the `pattern` argument for functions including `makeArray`, `makeMR`, `deleteVariables`, and `hideVariables` is being deprecated. The help pages for those functions advise you to grep for or otherwise identify your variables outside of these functions.
* `unshare` to revoke access of a user or a team to a dataset.
* Support for DatasetOrder, in particular for datasets within a project.
* Do more validation that `type<-` assignment is safe.
* Make paginated requests to GET /table/ (for `CrunchExpr`s) for greater reliability
* Finally fix bug that prevented sharing datasets with non-editors when the dataset had already been shared with a team.

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
