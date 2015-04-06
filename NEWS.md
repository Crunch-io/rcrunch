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