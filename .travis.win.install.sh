#!/usr/bin/env sh

choco install -y r.project &&
export R_VERSION=`ls 'C:\Program Files\R\'` &&
export PATH=$PATH:';C:\Program Files\R\'$R_VERSION'\bin\x64' &&
echo 'options(repos = "https://cloud.r-project.org")' > ~/.Rprofile.site &&
export R_PROFILE=~/.Rprofile.site &&

Rscript.exe -e 'sessionInfo()' &&
Rscript.exe -e 'install.packages("devtools");if (!all("devtools" %in% installed.packages())) { q(status = 1, save = "no")}' &&
Rscript.exe -e 'deps <- devtools::dev_package_deps(dependencies = NA);devtools::install_deps(dependencies = TRUE)'
Rscript.exe -e 'devtools::session_info(installed.packages()[, "Package"])'
