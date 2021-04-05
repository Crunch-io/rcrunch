## Test environments
* local OS X install, R 4.0.3
* ubuntu 14.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1* notes

There's a NOTE on `oldrel` about the documentation linking to the wrong place. This
is because there's an inconsistency about where the documentation lives (`margin.table` in
R 3.6.4 and `marginSums` in R 4.0.3). I do not know a way to avoid the NOTE in both versions
so I have prioritized removing in the newer versions of R.


## Downstream dependencies
No regressions were found.

