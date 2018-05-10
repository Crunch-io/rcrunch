#!/bin/bash
set -ev
if [ "${TRAVIS_PULL_REQUEST}" = "false" ] &&
    [ "${TRAVIS_TAG}" != "" ] &&
    [ "${TRAVIS_R_VERSION_STRING}" = "release" ]; then

    R CMD INSTALL . ## Package needs to be installed, apparently
    R -e 'devtools::install_github("r-lib/pkgdown"); pkgdown::build_site()'
    git clone --branch src https://${GH_TOKEN}@github.com/Crunch-io/ta-da.git ../ta-da
    rm -rf ../ta-da/static/r/crunch
    cp -r docs/. ../ta-da/static/r/crunch
    cd ../ta-da
    git add .
    git commit -m "Updating rcrunch pkgdown site (release ${TRAVIS_TAG})" || true
    git push origin src || true
fi
