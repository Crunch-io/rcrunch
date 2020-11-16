#!/bin/bash
set -ev
Rscript -e 'pkgdown::build_site()'
git clone --branch src https://${GITHUB_PAT}@github.com/Crunch-io/ta-da.git ../ta-da
rm -rf ../ta-da/static/r/crunch
cp -r docs/. ../ta-da/static/r/crunch
cd ../ta-da
git add .
git commit -m "Updating rcrunch pkgdown site (release ${RELEASE_VERSION})" || true
git push origin src || true
fi
