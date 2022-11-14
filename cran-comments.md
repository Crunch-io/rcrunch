## Test environments
* local R installation
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 notes

Win builder reports that links are returning 403s. It seems our CDN is using
logic based on the user agent so CRAN's automated check is failing, but opening
the links in a browser is fine.