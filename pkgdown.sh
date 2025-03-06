#!/bin/bash
set -ev
Rscript -e 'pkgdown::build_site()'
git clone --branch src https://${GITHUB_PAT}@github.com/Crunch-io/ta-da.git ../ta-da
rm -rf ../ta-da/static/r/crunch
cp -r docs/. ../ta-da/static/r/crunch
cd ../ta-da

# These files have lower and upper case versions of the same file which makes
# working with them on macOS difficult. None of them are particularly useful
# so we just delete them
rm static/r/crunch/reference/transforms.html static/r/crunch/reference/subvariables.html static/r/crunch/reference/CrunchBox.html static/r/crunch/reference/contextManager.html static/r/crunch/reference/categories.html

git add .
git commit -m "Updating rcrunch pkgdown site (release ${RELEASE_VERSION})" || true
git push origin src || true
