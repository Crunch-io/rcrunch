### crunch 1.2.3
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