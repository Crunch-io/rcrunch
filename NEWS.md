# crunch (development version)
* You can now view and modify the weight on a slide using `weight()` and `weight()<-` (#440)
* https verification can be disabled during testing by setting environment variable `R_TEST_VERIFY_SSL=FALSE`
* You can now use a named `filter` or `filter` object when using `tabbook()`. Filtering by expression in the dataset argument is also supported again.
* `newMultiTable()` now correctly passes `...`, so arguments like `is_public` work (#424)
* `tabBook()` by default uses a new endpoint, which allows for more options. The old endpoint is deprecated, but while the server supports it, you can still use it. See [`?tabBook`](https://crunch.io/r/crunch/reference/tabBook.html) for more details.
* `importMultitable()` has been removed because it was deprecated on the server.
* `hiddenVariables()` works when the hidden variables folder has subdirectories (#372).

# crunch 1.26.3
* Can now `deriveArray()` using expressions to create the subvariables.
* New function `slideCategories()` helps you create overlapping categorical 
variables (#396).
* crunch will work with upcoming changes to `stringsAsFactors` defaults (#402).
* Exporting a Deck requires (and now includes) a valid JSON body in its request.
* Documentation for `newSlide` includes examples of `vizType` settings and other improvements.

# crunch 1.26.2
* A new `copyFolders()` function that copies folders and variable order from one dataset to another (similar to `copyOrder()` which was deprecated)
* Slides in decks can be manipulated more robustly (and their filters can be removed or added with `filters(slide_object) <- NULL` or `filters(slide_object) <- filter_object`) 
* There is a new helper function for dealing with API changes that lets you expect two different outcomes while testing (`expect_either()`)
* Extraneous `...` removed from documentation

# crunch 1.26.1
* Internally, the `(un)hideVariables()` functions are upgraded to use folder operations.

# crunch 1.26.0

## Personal folder
* You can now access your "personal folder" of datasets, which contains only those datasets you imported and that haven't been moved into another project. This dataset folder is denoted in paths by `~`, as in a `*nix` file system. `cd(projects(), "~")` takes you there; `mv(projects(), ds, "~")` moves `ds` into your personal folder.
* `listDatasets()` now by default only prints datasets in your personal folder, not a combination of your personal datasets and some of the datasets that have been shared with you.
* When you give a dataset name to `loadDataset()`, it now searches to find datasets exactly matching that name unless you specify a project to load from. If you have multiple datasets with the same name in different locations, `loadDataset("your dataset name")` may return a different one than it did previously. If you want to identify a dataset precisely in `loadDataset()`, either specify the dataset URL (most effective but not as human friendly) or provide `project = "path/to/folder"`.
* Calling `loadDataset(<integer>)` no longer is supported.

## Bug fixes
* Fix `is.public(multitables[[i]]) <- TRUE` and several other similar assignments of attributes on an element of a catalog, which previously successfully updated the value on the server but errored when returning to R (#303, #367)
* `subvariables()` on non-array variables returns `NULL` instead of an error (#237)
* Fixed a bug with the display of univariate cube `prop.table`s  

## Other enhancements

* `newExampleDataset()` creates a sample dataset for you to explore
* `exportDeck()` can now write to PowerPoint with `format = "pptx"`
* `newDataset()` now supports importing data in Triple-S format, providing a `schema` file in addition to the row data.
* `resolution()` lets you see the data units of a datetime variable ("Y", "M", "D", "ms", etc.); `resolution<-` lets you set it (#234)
* `deleteDataset()` accepts web app URLs, just as `loadDataset()` already did (#279)
* Set `options(crunch.warn.hidden=FALSE)` to suppress the "Variable x is hidden" messages when accessing hidden variables (#172)
* Support sharing decks with a team via `team(deck) <-`
* Added `upsert` argument to `appendDataset()` to allow datasets to be updated based on the primary-key variable; see `pk()` for details on primary keys (#49)
* `combineCategories()` and `combineResponses()` are aliases for `combine()`, providing a way to avoid accidental clashes with `dplyr::combine()` (#359)

# crunch 1.25.0

## Shared Crunch assets
* Initial support for deck creation and manipulation.
* Filters and multitables can now be shared with teams by assigning `teams()<-` on them. View which teams can access them by calling `teams()` on them.

## Folder enhancements
* Improved robustness of API usage for moving datasets in projects.
* Support for `"."` as a folder path/segment, referencing the current folder. `cd(project, ".")` returns `project`; `mv(project, ds, ".")` moves `ds` into `project`.
* Fix bug in printing folders that contain entities with excessively long names.

## Cube computation
* Summary statistic (means and medians) can now be calculated for any direction of a CrunchCube.
* Improved speed for calculating insertions (subtotals, headers, etc.) on large cubes (speed ups of ~25x on large, realistic cubes).

## Internal
* Remove (for now) support for the experimental virtual join feature.
* Remove previously deprecated variable order functions.
* 404 Not Found HTTP responses now print the request URL to aid in debugging.
* Fix a duplicated vignette title.
* Suppress check for new GitHub release of the package in non-interactive sessions.
* Fixed a bug that wouldn't allow vignette mocks to use `datasetReference()`
* Removed excess metadata in some cube fixtures in anticipation of Crunch not sending that information any more (no code changes were necessary).

# crunch 1.24.2
* The RStudio gadgets for `listDatasets()` and `makeArrayGadget()` have been moved to the [`crunchy`](https://crunch.io/r/crunchy) package. Wiring for them, including RStudio add-in configuration, remains here, but you'll have to install that package to use them.
* Minor fixes for backwards compatibility with the old projects API
* Remove code paths that modify the project dataset order, which was removed from the Crunch API.
* List as deprecated many functions that modify variable order, suggesting `mv()` and the other folder operations. These functions will be removed in December 2018.

# crunch 1.24.0

## Organization
* New folder methods (`cd()`, `mv()`, `mkdir()`, `rmdir()`) for organizing datasets within projects, following the pattern of variable folders. See `vignette("projects", package = "crunch")`.
* Added `setName()` and `setNames()` for renaming folders and folder contents, respectively.

## Bugfixes
* Requesting tab books for subsets of variables with weights no longer errors.
* `makeWeight()` is now correct for categorical variables with non-sequential IDs.
* Hidden variables are now included in the output of `write.csv` or `as.data.frame(force = TRUE)` if requested.
* The print method for empty dataset/variable folders now prints something informative.

## Other
* Adjusted the calculation of `index.table()` to better reflect analysts' intentions. Now, `index.table()` calculates the index with respect to the marginal proportion of the `margin` given, so for `index.table(cube, 2)` the column proportions of the table are indexed to the marginal row proportions. In other words: for each column how much larger or smaller is the proportion in that column when compared to the proportions for the row variable alone.
* Updated for compatibility with the upcoming release of the `haven` package and its new `haven_labelled` and `haven_labelled_spss` object classes.
* The package now warns when an API endpoint is deprecated.

# crunch 1.23.0

## Improved support for subtotals
* CrunchCubes can now be displayed with subtotals on any axis (not just rows).
* Subtotals in CrunchCubes have been improved and stabilized, and should work in many more places than they did before (e.g. `margin.table`, `prop.table`, etc.)

## Bugfixes
* Fix order of variables when using `mv()` to move them to a folder.
* `deleteVariables()` no longer tries to delete duplicate variables.
* Resolved a but when using `as.data.frame(..., force = TRUE)` with numeric variables that have missing values.
* Resolve missing `Suggests` reference for test packages, following new `check` requirement.

## Internal improvements
* `getDimTypes()` returns a richer set of cube dimension types differentiating multiple response from categorical array dimensions.
* Added support for `alias`, `description`, and `notes` on `VariableTuples`


# crunch 1.22.2
* New introductory vignette: `vignette("crunch")`
* `changeCategoryID()` tries to unset then reset the dataset exclusion if that impacts its progress. Best practice is to disable exclusions before running `changeCategoryID()` if at all possible.
* Setting the `ordering<-` of datasets within a project will now drop any invalid entries with a warning, rather than error.
* Fix a bug introduced in 1.22.0 in creating categorical variables from factors with missing values.
* Fix a similar yet unrelated bug in creating numeric and other types of variables with all-`NA` data.
* Fix `streamRows()` for case when sending only one row (#253).
* Internal: support for the "selected_array" method of multiple response calculation, deprecated since 1.20.0, has been removed.
* Internal: `getDimTypes()` returns a richer set of cube dimension types differentiating multiple response from categorical array dimensions.
* Internal: Added support for `alias`, `description`, and `notes` on `VariableTuples`
* `makeArrayGadget()` launches an RStudio gadget to help you build valid categorical arrays
and multiple response variables.

# crunch 1.22.0

## Analysis methods
* `CrunchCube`s can now be subset just like R arrays using the `[` method.
* Add summary statistics to CrunchCubes that have categorical variables with scale values (`numeric_values`). See `?addSummaryStat` for more information.
* `index.table()` to return tables indexed to a margin.

## Bug fixes and other enhancements
* Fix bug in assigning `subtotals(var) <- NULL` when it already was `NULL` (#231).
* Consistently return `""` for variable metadata fields if no value is set (#232).
* Better subvariable metadata methods for CrunchCubes (#215).
* Clarified the error message when using `makeMRFromText()` with a categorical variable.
* Export GitHub package version checking function so that other `crunch*` packages can use it.
* `%in%` and `==` on Crunch objects now follow R semantics more closely with regards to missing data.
* Add some forward-compatible code to prepare for API changes to logical variables. This led to a couple of trivial changes to internals around boolean types that should not affect package users.

# crunch 1.21.0

## Variable organization
* New functions for organizing variables in a dataset, modeled on file system operations: `cd()`, `mv()`, `mkdir()`, `rmdir()`. These functions use a new API for variable folders (unlike the experimental versions of some that were introduced in the 1.19.0 package release). This API is currently in a beta testing phase. See `vignettes("variable-order", package="crunch")` for examples and details.

## Loading and navigating
* `listDatasets(shiny = TRUE)` launches an RStudio addin which allows you to select your dataset in order to generate a valid `loadDataset()` call. You can also associate this addin with a hotkey using in RStudio through `Tools` > `Modify Keyboard Shortcuts`.
* `webApp()` now works for Crunch variables: it will take you to the "browse" view of the web application with the given variable card loaded on screen.

## New variable creation and derivation
* Create a derived view of a variable as another type without altering the underlying data. Have a text input that is only numbers, such as an ID, and want to have a variable that is a true numeric, but you also want to make sure that new (text) values can be appended to the dataset? Use `ds$id_var_numeric <- as.Numeric(ds$id_var)`. There are `as.*` methods for all Crunch data types except for array-like variables.
* Preliminary support for `haven`’s `labelled` class when converting to Crunch variable types.
* `makeMRFromText()` to take a variable imported as delimited strings, parse the multiple-response options, and return a (derived) `multiple_response` variable.

## Miscellaneous
* Added support for setting population sizes on datasets with `setPopulation(ds, size = 24.13e6, magnitude = 3)` and for getting population sizes (or magnitudes) with `popSize(ds)` and `popMagnitude(ds)` respectively.
* Added support for getting and setting rollup resolutions for displaying Datetime variables. Get resolution with `rollupResolution(ds$datetime)` and set with `rollupResolution(ds$datetime) <- "M"`.
* Add `options(crunch.show.progress)` to govern whether to report progress of long-running requests. Default is `TRUE`, but set it to `FALSE` to run quietly.
* Export `pollProgress()` and recommend using that when a long-running request fails to complete within the local timeout.

# crunch 1.20.0

## Subtotals and Headings
* Added support for subtotals and headings on categorical variables and CrunchCubes. Subtotals can be set with `subtotals(variable) <- Subtotal(name = 'subtotal', categories = c(1, 2), after = 2)`. Use `subtotals(variable)` to see what subtotals are set for a variable.
* By default, subtotals will be displayed on CrunchCube results. Arrays consisting of only subtotals can be created using `subtotalArray([cube])`
* See `?subtotals` or `vignette("subtotals", package="crunch")` for more information.

## Multiple response in CrunchCubes
* The default method for including multiple response variables in CrunchCubes has changed, allowing for better handling of variables with different missingness across subvariables. (Internally: queries with multiple response variables now use the `as_selected` function instead of `selected_array`, which is now deprecated).
* For now, the deprecated method can be restored by setting `options(crunch.mr.selection = "selected_array")`.

## Improvements to `conditionalTransform()`
* `conditionalTransform()` now has a `formulas` argument to specify a list of conditions to be used.
* Errors and warnings are now more helpful when using `conditionalTransform()`.

## Optimizations and bugfixes
* Improved efficiency when loading a dataset from URL.
* `refresh()` for Datasets is now more efficient.
* Fixed a bug where CrunchCubes with categorical variables that had categories "Selected", "Not selected", and "No data" might not display correctly.

# crunch 1.19.0

## New functions
* Variable groups (folders) can now be referenced by "path": either a vector of nested folder names (as in `ordering(ds)[[c("Top folder", "Nested folder")]]`) or a single string with nested folders separated by a delimiter (as in `ordering(ds)[["Top folder/Nested folder"]]`). "/" is the default path delimiter, and this is configurable via `options(crunch.delimiter)`. If you have folders that actually contain "/" in the folder name, this may be a breaking change. If so, set `options(crunch.delimiter="|")` or some other string so that folder names are not incorrectly interpreted as paths.
* Introduce new `mv()` and `mkdir()` functions for creating variable folders and moving variables into them. These take a Dataset as their argument and can be chained together for convenience/readability.
* Other helper functions `folder()` and `folder<-` to locate a variable in the folder hierarchy and to move it to a new folder. `folder(ds$var) <- "New folder/subfolder"` is equivalent to `ds <- mv(ds, "var", c("New folder", "subfolder"))`.
* Create new variables that take on different values when specific conditions are met using `conditionalTransform()` (#64, #153)
* `collapseCategories()` allows you to combine categories in place without creating a new variable

## Enhancements and fixes
* Deep copying variables with `copy()` has been made more efficient
* `CrunchDataFrames` have been improved to act more `data.frame`-like. You can now access and overwrite values with standard `data.frame` methods like `crdf$variable1` or `crdf[,"variable1"]` and `crdf$variable1 <- 1` or `crdf[,"variable1"] <- 1`. `CrunchDataFrames` now also support adding arbitrary columns, although it should be noted that these columns are not stored on the Crunch server, so if you want to keep that data outside of your current R session, you should send it back to your Dataset as a new variable.
* `is.selected()` is now vectorized to work with Categories, as `is.na()` has always been. You can also now assign into the function (#123)
* `addSubvariable()` now accepts variable definitions directly (#72)
* `makeCaseVariable()` has better errors when a user doesn't name all of their case definitions (#158).
* The size limit on `as.data.frame()` when `force = TRUE` has been removed (#150)

# crunch 1.18.4
* All catalog objects now have an `as.data.frame()` method.
* The list of dataset "weight variables" can now be set with `modifyWeightVariables()`, `weightVariables(ds) <- ds$newWeight` or `is.weightVariables(ds$var) <- TRUE`
* Users with account admin privileges can now `expropriateUser()` to transfer datasets, projects, and other objects owned by one user to another, as when that user has left your organization.
* Access members of user `UserCatalogs` by email (e.g. `catalog[["you@example.com"]]`) by default. All catalog extract methods (`[` and `[[`) now also accept a `secondary` argument for setting an index to match against to change that default.
* Crunch authentication email and password can be stored in and read from the environmental variables `R_CRUNCH_EMAIL` and `R_CRUNCH_PW` respectively.
* Cube queries with `as_selected` multiple-response variables have margin and prop.table methods
* Cube `variables()` now contain additional metadata, including "type"
* Fix `bases()` when called on a univariate statistic (#124)
* Update some tests and code to anticipate changes in an upcoming release of `testthat`

# crunch 1.18.2
* `makeWeight()` allows you to generate new weighting variables based on categorical variables (#80).
* `cut()`, equivalent to `base::cut`, allows you to generate a derived categorical variable based on a numeric variable (#93).
* Create a new Crunch dataset from a file by calling `newDataset()` directly instead of `newDatasetFromFile`. Also, you can now create a dataset from a hosted file passing its URL to `newDataset(FromFile)`.
* `as.data.frame()` method for `VariableCatalog` for a view of variable metadata (#75)
* `crunchBox()` now allows you to specify colors for branding or even category-specific coloring.
* RStudio users will now be prompted for their password on `login()` in a way that conceals the input.
* Changed the behavior of `changeCategoryID()` to only update numeric values of the category having its id changed when the id and the numeric value are the same.
* The `autorollback` argument of `appendDataset()` has been deprecated. The option no longer has any effect and a warning will be printed to notify users about the deprecation.
* Long-deprecated `newDatasetByCSV` was removed.

# crunch 1.18.0

## Support for mapping
* Crunch-hosted geographic data can now be set and updated. Use `geo()` on a variable to see if there is already associated geographic data.
* `addGeoMetadata()` function to match a text or categorical variable with available geodata based on the contents of the variable and metadata associated with Crunch-hosted geographic data.

## Better derived variable support
* Derivation expressions can now be retrieved from derived variables with `derivation()`
* Derived variables can be integrated or instantiated by setting `derivation() <- NULL`

## Other new functions
* `resetPassword()` function
* `copyOrder()` to copy the ordering of variables from one dataset to another.
* Pass a web app URL to `loadDataset()` and it will now load the same dataset in your R session.
* `webApp()` function to go the other way: open the dataset from your R session in your web browser.
* `categoriesFromLevels()` is now exported (#77)

## Fixes and adjustments
* Categories are now selectable by names as well as ids
* Fix issue where `deleteSubvariable()` by index instead deleted the parent variable
* Add a missing import from the `methods` package so that `Rscript` works (#90)
* Allow deep copying of multiple-response type variables
* Experimental support for merging `CrunchDataFrame`s with standard `data.frame`s

# crunch 1.17.8
Two attempts to fix download issues introduced by 1.17.4:

* Changed file downloads to `crGET` with `httr::write_disk()` to hopefully work around issues caused by `utils::download.file` with method "libcurl".
* Add a `retry` for downloads to hopefully work around a delay in CDN population.

# crunch 1.17.6
* `searchDatasets()` to use the Crunch search API.
* Added support for viewing and changing the number of digits after the decimal place to be printed with `digits()` (useful when exporting to SPSS files).
* `crtabs` and `table` where a dimension is a `CrunchLogicalExpr` now return a boolean dimension with names "FALSE" and "TRUE", rather than the previous behavior of dropping the dimension and only returning the `TRUE` value.

# crunch 1.17.4
* Added support for case variables (#36): `makeCaseVariable()` takes a sequence of case statements to derive a new variable based on the values from other variables.
* Added a function to create interactions of variables (#42): `interactVariables()` takes two or more categorical variables and derives a new variable with the combination of each.
* Fixed a bug where exports (data and tab book) might not work on Windows. If you're using a version of R older than 3.3, and you *now* have problems downloading, and you're not on Windows, try `options(download.file.method="curl")`.

# crunch 1.17.2
* Support for streaming data: check for received data with `pendingStream()`; append that pending stream data to the dataset with `appendStream()` (#40)
* Multitables can now be updated with `multitables(ds)[["Multitable name"]] <- ~ var1 + var2` syntax. Similarly, multitables can be deleted with `multitables(ds)[["Multitable name"]] <- NULL`. Multitables also have new `name()` and `delete()` methods.
* `toVariable()` now accepts (and then strips) arguments of class `AsIs` (#44)
* Fixed a bug where `changeCategoryID()` failed on multiple response variables.

# crunch 1.17.0
* `dashboard` and `dashboard<-` methods to view and set a dashboard URL on a dataset
* `changeCategoryID` function to map categorical data to a new "id" and value in the data (#38, #47)
* Added `importMultitable()` to copy a multitable form one dataset to another. Additionally, `Multitable`s now have a show method showing its name and column variables.
* Can now extract variables from a dataset by the variable URL
* `appendDataset()` now truly appends a dataset and no longer upserts if there is a primary key set. This is accomplished by removing the primary key before appending. (#35)
* Primary keys can now be viewed with `pk(dataset)` and set with `pk(dataset) <- variable`.
* Fix issue in printing filter expressions with long value columns (#39, #45)
* Progress bars now clean up after themselves and do not leave the prompt hanging out at the end of the line
* Test setup code moved to `inst/` so that other packages that depend on `crunch` can use the same setup.

# crunch 1.16.0

## Cube and tab book improvements
* Reshape TabBookResults that contain categorical array variables so that `prop.table` computations line up with those not containing array variables (i.e. move subvariables to the third array dimension in the result).
* Add `names`, `aliases`, and `descriptions` methods to `CrunchCube` (corresponding to variables of the dimensions in the cube), `MultitableResult` (corresponding to the "column" variables of the cubes in the result), and `TabBookResult` (corresponding to the "row"/"sheet" variables in each multitable result).
* Fix `names` method for TabBookResults following an API change.
* Extend `crtabs` formula parsing to support multiple, potentially named, measures

## Other new features
* `weightVariables` method to display the set of variables designated as valid weights. (Works like `hiddenVariables`.)
* In `appendDataset`, allow specifying a subset of rows to append (in addition to the already supported selection of variables)
* `loadDataset` can now load a dataset by its URL.

## Housekeeping
* Remove "confirm" argument from various delete functions (deprecated since 1.14.4) and the "cleanup" argument to append (deprecated since 1.13.4)
* All destructive actions now require 'consent', even in non-interactive mode. See `?with_consent` for more details.
* Improvements to validation when updating values in a dataset.
* Move mock API fixtures to `inst/` so that other packages depending on this package can access them more easily.

# crunch 1.15.2
* Support for additional dataset export arguments
* Add `is.derived` method for Variables
* Allow a 'message' when sharing a dataset (#27)
* More validation for the input to the various export functions
* Fix handling of "total" column in `TabBookResult`s when the row variable is a categorical array

# crunch 1.15.0
* `multitables` method to access catalog from a Dataset. `newMultitable` to create one. See `?multitables` and `?newMultitable` for docs and examples.
* `tabBook` to compute a tab book with a multitable. If `format="json"` (the default), returns a `TabBookResult` containing `CrunchCube` objects with which further analysis or formatting can be done.
* `bases` method for cubes and tab book responses to access unweighted counts and margin tables.
* Handle case of attempting to `saveVersion` when there are no changes since the last saved version.
* Update to work with [`roxygen2` 6.0.0 release](https://github.com/klutometis/roxygen/issues/568)

# crunch 1.14.4
* `newFilter` and `newProject` functions to create those objects more directly, rather than by assigning into their respective catalogs.
* Require confirmation before doing a "force" merge in `mergeFork`.
* Add `with_consent` as an alternative to `with(consent(), ...)`
* Deprecate the "confirm" argument to destructive functions and methods such as `delete` in favor of the `consent` context manager.
* Add deprecation warning that destructive actions will soon also require consent when running in a non-interactive R session.
* Use [`httptest`](https://enpiar.com/r/httptest) for mocking HTTP and the Crunch API.

# crunch 1.14.2
* Trivial change to DESCRIPTION to meet new, hidden CRAN requirement

# crunch 1.14.0
* `embedCrunchBox` to generate embeddable HTML markup for CrunchBoxes
* `duplicated` method for Crunch variables and expressions
* Prevent invalid expressions with incorrect variable references from making bad requests
* Print methods for Category/ies now show category ids
* Speed up `as.vector` and `as.data.frame` methods by smarter pagination of requests.
* Option "crunch.namekey.variableorder" to govern how VariableOrder is printed. Current default is "name", the status quo, but set it to "alias" to have `ordering` print aliases.
* Support for `is.na<-` to set missing values on a variable, equivalent to assigning `NA`
* Fix behavior and validation for subsetting datasets/variables that are already subsetted by a Crunch expression object.
* Allow setting a variable entity to `settings(ds)$weight` and not just its `self` URL.

# crunch 1.13.8
* `crunchBox` to make a public, embeddable analysis widget
* `settings` and `settings<-` to view and modify dataset-level controls, such as default "weight" and viewer permissions ("viewers_can_change_weight", "viewers_can_export")
* `flattenOrder` to strip out nested groups from an order
* Univariate statistics on variables, such as `mean`, `median`, and `sd`, now respect filter expressions, as does the `summary` method.
* "median" can now be used in `crtabs`
* Copying and deriving variables now bring in the "notes" attribute.
* Improve error handling when attempting to `loadDataset` from a nonexistent project.

# crunch 1.13.6
* More utility functions for working with order objects: `dedupeOrder`, `removeEmptyGroups`
* `appendDataset` can now append a subset of variables
* Update to changes in the dataset version API
* Fix bug in assigning NA to an array subvariable that didn't already have the "No Data" category

# crunch 1.13.4
* `flipArrays` function to generate derived views of array subvariables
* Add `autorollback` argument to `appendDataset`, defaulted to `TRUE`, which ensures that a failed append leaves the dataset in a clean state.
* `allVariables` is now ordered by the variable catalog's order, just as `variables` has always been.
* Add "force" argument to `mergeFork`.
* Support an `as_array` (pseudo-)function in `crtabs` that allows crosstabbing a multiple-response variable as if it were a categorical array.
* Fix bug in dataset export when attempting to export a single variable

# crunch 1.13.2
* Support deep copying of categorical array variables.
* Join (`merge`) a subset of variables and/or rows of a dataset.
* `moveToGroup` function and setter for easier adding of variables to existing groups.
* `locateEntity` function to find a variable or dataset within a potentially deeply nested order.
* Change default key for printing `hiddenVariables` from "name" to "alias", governed by `options(crunch.namekey.dataset)` as elsewhere
* Allow disabling of check for new package releases on load by setting `options(crunch.check.updates=FALSE)`.
* Return a Session object from `session()` that lazily fetches catalogs rather than when instantiated.

# crunch 1.13.0
* `as.vector` on a categorical-array or multiple-response variable now returns a `data.frame`. While a `matrix` is a more accurate representation of the data type, using `data.frame` allows for more intuitive accessing of subvariables by `$`, just as they are from the Crunch dataset.
* Enhancements to merge/extendDataset: a "by" argument as a shortcut for "by.x" and "by.y"; referencing "by" variables by alias; and aliasing the function also through `joinDatasets` with its (new) default `copy=TRUE` argument.
* POST new array variable definitions that are a series of subvariable definitions as a single request, rather than uploading each subvariable separately and then binding.
* Improve `addSubvariable` to PATCH rather than unbind and rebind; also extend it to accept more than one (sub)variable to add to the array.
* Remove `pattern` matching argument from `makeArray`, `makeMR`, `deleteVariables`, and `hideVariables`, deprecated since 1.9.6.
* Standardize `deleteSubvariable` to follow model of `deleteVariable`, including requiring consent to delete.
* [New vignette](inst/doc/export.md) on downloading data to your local R session and exporting datasets to file formats.
* Preparation for upcoming API changes.

# crunch 1.12.2
* Patch a test for handling duplicate factor levels, which is deprecated in current R releases but converted to an error in the upcoming release.

# crunch 1.12.0
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

# crunch 1.11.2
* Fix issue with sharing datasets owned by a project.
* Support updating Categorical variables created from R logical-type vectors with logical values
* Remove "crunch.max.categories" option to govern converting factors to Crunch categorical variables only if fewer than that threshold. Use `as.character` if you have a factor and want it to be imported as type Text.
* Increase default "crunch.timeout" for long-running jobs to 15 minutes, after which point progress polling will give up.
* Add `cleanseBatches` function to remove batch records from failed append attempts. Remove deprecated code around batch conflict reporting.
* Validation to prevent attempting to set NA category names.

# crunch 1.11.0
* Generic `datasets` and `projects` functions to get dataset and project catalogs. (`datasets` previously existed only as a method for Project entities.)
* Add `project` argument to `listDatasets` and add `project` and `refresh` to `loadDatasets` to facilitate viewing and loading datasets that belong to projects.
* New function `compareDatasets` that shows how datasets will line up when appending. A `summary` method on its return value prints a report that highlights areas of possible mismatch.
* Support computing numeric aggregates (mean, max, etc.) of categorical variables with numeric values in `crtabs`
* Allow `NULL` assignment into Variable/DatasetGroups to remove elements

# crunch 1.10.6
* Fix refresh method for Datasets that have been transferred to a Project.
* (Re-)improve print method for expressions involving categorical variables
* Improve handling of filters when composing complex expressions of `CrunchExpr`, Variable, and Dataset objects
* Add expression support for operations involving a `DatetimeVariable` and a character vector, assumed to be ISO-8601 formatted.
* Export a `permissions` method for Datasets to work directly with sharing privileges.

# crunch 1.10.4
* Fix `as.data.frame`/`as.environment` for `CrunchDataset` when a variable alias contained an apostrophe.
* Better print method for project `MemberCatalog`.
* Fix for [change in `jsonlite` API](https://github.com/jeroenooms/jsonlite/issues/130#issuecomment-225971209) in its v0.9.22
* Progress polling now returns the error message, if given, if a job fails.

# crunch 1.10.2
* `exportDataset` to download a CSV or SAV file of a dataset. `write.csv` convenience method for CSV export.
* Correctly parse datetimes that don't include timezone information.
* Add `icon` and `icon<-` methods for Projects to read the project's current icon URL and to set a new icon by supplying a local file name to upload.
* Get and set "archived" and "published" status of a dataset with `is.archived`, `is.draft`, and `is.published` (the inverse of `is.draft`). See `?publish` for more.
* Add `draft` argument to `forkDataset`
* Support for future API to handle failed long-running jobs.
* Assorted updates to new API usage

# crunch 1.10.0

## New support for working with users and their permissions on datasets and projects
* Add `owner` and `owner<-` for datasets to read and modify the owner
* Add `owners` and `ownerNames` for DatasetCatalog
* `is.editor` and `is.editor<-` for project MemberCatalog
* `me` function to get the user entity for yourself

## Other changes
* Add missing print method for DatasetOrder
* Support creating OrderGroups (for both Datasets and Variables) by assigning URLs into a new group name
* Improve support for parsing datetime data values
* Fix bug in setting nested groups inside DatasetOrder
* Fix failure on interactive login in R.app on OS X

# crunch 1.9.12
* Generalize and update to new Progress API. Add a progress bar.
* Remove deprecated query parameter on variable catalog

# crunch 1.9.10
* `variableMetadata` function to export all variable metadata associated with the dataset

# crunch 1.9.8
* Better support for deleting hidden variables
* Allow subsetting of datasets to include hidden variables
* Require that version names must be a single string value
* Fix bug in print method for VariableOrder that manifested when fixing the variable catalog's relative URL API

# crunch 1.9.6
* Add warning that the `pattern` argument for functions including `makeArray`, `makeMR`, `deleteVariables`, and `hideVariables` is being deprecated. The help pages for those functions advise you to grep for or otherwise identify your variables outside of these functions.
* `unshare` to revoke access of a user or a team to a dataset.
* Support for DatasetOrder, in particular for datasets within a project.
* Do more validation that `type<-` assignment is safe.
* Make paginated requests to GET /table/ (for `CrunchExpr`s) for greater reliability
* Finally fix bug that prevented sharing datasets with non-editors when the dataset had already been shared with a team.

# crunch 1.9.4
* Add a "session" object, retrievable by either `session()` or returned from `login()`, containing the various catalog resources (Datasets, etc.).
* Additional methods on the dataset catalog, such as `names<-`.
* Extract from most catalogs either by URL or name.
* Initial implementation of Projects API.
* `loadDataset` with a dataset catalog tuple, allowing some degree of tab completion by dataset name. (Example: `cr <- login(...); ds <- loadDataset(cr$datasets$My_Dataset_Name)`)
* Update tests to pass with forthcoming release of `testthat`.
* Remove `useAlias` attribute of datasets and move it to a global option, "crunch.namekey.dataset", defaulted to "alias". Implement the same for array variables, "crunch.namekey.array", and default to "name" for consistency with previous versions. This default will change in a future release.
* New Progress API for checking status of pending, long-running server jobs.
* Switch `as.vector` for `CrunchExpr` to GET rather than POST.

# crunch 1.9.2
* `forkDataset` to make a fork (copy) of a dataset; `mergeFork` to merge changes from a fork back to its parent (or vice versa)
* Remove a duplicate request made when setting variable order
* Update to new API to get a datetime variable's rollup resolution and save a request

# crunch 1.9.0

## Major changes
* Pull HTTP query cache out to the [httpcache](https://github.com/nealrichardson/httpcache) package and take dependency on that. Remove dependency on `digest` package (httpcache depends on it instead).
* New vignette on [filters and exclusions](inst/doc/filters.md)
* `combine` categories of categorical and categorical-array variables, and responses of multiple-response variables, into new derived variables
* `startDate` and `endDate` attributes and setters for dataset entities (#10, #11)
* Allow editing of filter expressions in UI filter objects (`CrunchFilter`)

## Other changes
* Improved validation for "name" setting, especially for categories
* Speed up `ncol(ds)` by removing a server request
* Speed up variable catalog editing by avoiding unnecessary updates to the variable order
* Fix cache invalidation when reordering subvariables
* Improve error message for subscript out of bounds in catalog objects
* Include active filter in print method for datasets and variables, if applicable

# crunch 1.8.0
* More formal support for creating and managing UI filters
* Better print method for Crunch expressions (`CrunchExpr`): prints an R formula-like expression
* Fix error in reading/writing query cache with a very long querystring. Requires new dependency on the `digest` package.
* Fix bug in assigning `name(ds$var$subvar) <- value`
* Fix overly rigid validation in `share`
* Update API usage to always send full variable URLs in queries

# crunch 1.7.12
* Add method for R logical &/| Crunch expression
* Upgrade for compatibility with httr 1.1

# crunch 1.7.10
* `addSubvariable` function to add to array and multiple response variables (#7)
* Make paginated requests to GET /values/ for greater reliability

# crunch 1.7.8
* Update to match changes in filter API

# crunch 1.7.6
* `dropRows` to permanently delete rows from a dataset.
* Better print method for catalog resources, using the new `catalogToDataFrame` function.
* Export a few more functions (`shojiURL`, `batches`)

# crunch 1.7.4
* Catch `NULL` in cube dimension when referencing subvariable that does not exist (as when using alias instead of name) and return a useful message.
* Fix for unintended substring matching in `%in%` expression translation.
* Internal change to match user catalog API update

# crunch 1.7.3
* Update docs to conform to R-devel changes to `as.vector`'s signature.

# crunch 1.7.2
* `addVariables` function to add multiple variables to a dataset efficiently
* Support aggregating with `CrunchExpr`s and filtered variables in `table`
* Save a variable catalog refresh on (un)dichotomize. Slight speedup as a result.
* Fix bug in creating VariableOrder with a named list.

# crunch 1.7.0
* Improve performance of many operations by more lazily loading variable entities from the server. Changes to several internal package APIs to make that happen, but the public package interface should be unchanged.
* Also speed up loading of variable catalogs by deferring resolution of relative subvariable URLs until requested. Eliminates significant load time for datasets with lots of array variables.
* Fix bug in results from `crtabs` when requesting a crosstab of three or more dimensions.

# crunch 1.6.1
* `VariableDefinition` (or `VarDef`) function and class for creating variable definitions with more metadata (rather than assigning R vectors into a dataset and having to add metadata after).
* Reworked various new variable functions, including `copy`, `makeArray`, and `makeMR`, to return `VariableDefinition`s rather than creating the new variables themselves. Creation happens on assignment into the dataset.
* Support adding No Data (`NA` for categoricals) even if No Data doesn't already exist
* Tools for logging and profiling HTTP requests and cache performance. See `?startLog` and `?logMessage`.
* Support deep copying of non-array variables.

# crunch 1.6.0
* Check for new version of the package on GitHub when the package is loaded.
* Make a shallow `copy` of a variable. See `?copyVariable`.
* Fix error in updating the values of a subvariable in an array.
* Handle case of assigning `NULL` into a dataset when the referenced variable (alias) does not exist.
* More support for `NA` assignment into variables.

# crunch 1.5.4
* Gradually slow the polling of `/batches/` while waiting for an append to complete. Improves the performance of the append operation.
* New `c` method for Categories, plus support for creating and adding new categories to variables. See `?Categories` and `?"c-categories"`
* Get category ids or numeric values from `as.vector` by specifying a "mode" of "id" or "numeric", respectively. See `?"variable-to-R"`
* Set values as missing by assigning `NA` into variables.

# crunch 1.5.3
* Always send No Data category when creating Categorical Variables.
* Fixed minor bugs in `margin.table` on `CrunchCube` objects.
* Better validation of category subsetting.

# crunch 1.5.2
* Add Python-esque context manager for use in `with` statements. Use it to give `consent()` to delete things.
* Delete variables by `<- NULL` into a dataset (like removing a column from a data.frame). Requires consent. Also create `deleteVariable(s)` functions that also return the dataset object. Use either method to prevent your dataset from getting out of sync with the server when you delete variables.
* Delete subvariables from within array variables with `deleteSubvariable(s)`.
* Better evaluation of formulas within `crtabs` to allow you to crosstab array subvariables.
* Update to new exclusion API.

# crunch 1.5.1
* Validate inputs on making filter expressions with categorical variables
* Very basic print methods for all Crunch objects

# crunch 1.5.0
* Subset rows of datasets and variables for analysis, using either `[` or `subset`
* Access and set `exclusion` filters on datasets to drop certain rows
* Fix some inconsistent handling in R of filters that are set on the server (i.e. for persistent viewing in the web application)
* `(un)lock` datasets for editing when there are multiple editors

# crunch 1.4.3
* Send better emails when sharing datasets

# crunch 1.4.2
* Support for auto-login in Jupyter notebooks
* One more CRANdated import

# crunch 1.4.1
* Import functions from methods, stats, and utils, per change in CRAN policy.

# crunch 1.4.0
* Functions `saveVersion` and `restoreVersion` for dataset versioning
* Update requirement to `httr` 1.0; remove dependency on `RCurl` in favor of `curl`
* Minor API updates
* Fix for some issues authenticating on Windows
* Fix bug in editing array variables with a single subvariable

## crunch 1.3.3
* More tools (not yet exported) for managing users

## crunch 1.3.2
* Adapt to minor updates in append API: new intermediate "appended" state for append operations.

## crunch 1.3.1
* More methods for managing teams
* Prepare for httr 1.0

# crunch 1.3.0
* Provisional interface for managing users and teams.
* Improved messaging for failure modes in `appendDataset`.
* Adapt to minor updates in append API
* Fix bug in updating an array with only one subvariable.

# crunch 1.2.2
* Add `types` method to VariableCatalog.

# crunch 1.2.1
* Additional methods for working with VariableOrder and VariableGroup. You can create new Groups by assigning into an Order or Group with a new name. And, with the new `duplicates` parameter, which is `FALSE` by default, adding new Groups to an Order "moves" the variable references to the new Group, rather than creating copies. See the [variable order vignette](https://crunch.io/r/crunch/articles/variable-order.html) for more details.
* Add `share` function for sharing a dataset with other users.

# crunch 1.2.0
* New vignettes for [deriving variables](https://crunch.io/r/crunch/articles/derive.html) and [analyzing datasets](https://crunch.io/r/crunch/articles/analyze.html).

* Update appending workflow to support new API.

# crunch 1.1.1
* Remove all non-ASCII from test files so that tests will run on Solaris.

# crunch 1.1.0
* Add query cache, on by default.

* `as.data.frame` now does not return an actual `data.frame` unless given the argument `force=TRUE`. Instead, it returns a `CrunchDataFrame`, and environment containing unevaluated promises. This allows R functions, particularly those of the form `function(formula, data)` to work with CrunchDatasets without copying the entire dataset from the server to local memory. Only the variables referenced in the formula fetch data when their promises evaluated.

* Remove `RJSONIO` dependency in favor of `jsonlite` for `toJSON`.

# crunch 1.0.0
* Rename package to `crunch`. Update all docs to reflect that. Make amendments to pass CRAN checks.

# rcrunch 0.11.1
* `newDataset2` renamed to `newDatasetByCSV` and made to be the default strategy in `newDataset`. The old `newDataset` has been moved to `newDatasetByColumn`.

* Support for `NA` and `NaN` in `crtabs` response.

# rcrunch 0.11.0
* `getCube` is now `crtabs`. Ready for more extensive beta testing. Has prop.table and margin.table methods. Vignette forthcoming.

* `newDataset2` that uses the CSV+JSON import method, rather than the columm-by-column strategy that `newDataset` uses.

# rcrunch 0.10.0

* Support for shoji:order document for hierarchical variable order. HTTP API change.

* Initial, limited support for `xtabs`-like crosstabbing with a formula with the `getCube` function.
