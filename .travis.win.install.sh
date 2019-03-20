#!/usr/bin/env sh

choco install -y r.project &&
choco install -y make &&
export R_VERSION=`ls 'C:\Program Files\R\'` &&
export PATH=$PATH:';C:\Program Files\R\'$R_VERSION'\bin\x64' &&
echo 'options(repos = "https://cloud.r-project.org", install.packages.compile.from.source = "always")' > ~/.Rprofile.site &&
export R_PROFILE=~/.Rprofile.site &&

echo which make &&

Rscript.exe -e 'sessionInfo()' &&
Rscript.exe -e 'getOption("install.packages.compile.from.source")' &&
Rscript.exe -e '!nzchar(Sys.which(Sys.getenv("MAKE", "make")))' &&
Rscript.exe -e 'install.packages("devtools", dependencies = TRUE, type = "both");if (!all("devtools" %in% installed.packages())) { q(status = 1, save = "no")}' &&
echo 'one' &&
Rscript.exe -e 'deps <- devtools::dev_package_deps(dependencies = TRUE); install.packages(deps$package, dependencies = TRUE, type = "both")' &&
echo 'two' &&
Rscript.exe -e 'devtools::session_info(installed.packages()[, "Package"])' &&
echo 'three' &&

export PKG_TARBALL="$(perl -ne '$version = $1 if (/^Version:\s(\S+)/); $package = $1 if (/^Package:\s*(\S+)/); END { print "${package}_$version.tar.gz" }' DESCRIPTION)"
