#!/bin/bash
set -ev
if [ "${TRAVIS_PULL_REQUEST}" = "false" ] && [ "${TRAVIS_BRANCH}" = "pkgdown" ]; then
    R -e 'devtools::install_github("hadley/pkgdown"); pkgdown::build_site()'
    git clone --branch src https://${GH_TOKEN}@github.com/Crunch-io/ta-da.git ../ta-da
    rm -rf ../ta-da/static/R/crunch
    cp -r docs/. ../ta-da/static/R/crunch
    cd ../ta-da
    git add .
    git commit -m "Updating rcrunch pkgdown site (rcrunch build ${TRAVIS_BUILD_NUMBER})" || true
    # git push origin src || true
fi
