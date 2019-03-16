#!/usr/bin/env sh

choco install -y r.project &&
export R_VERSION=`ls 'C:\Program Files\R\'` &&
export PATH=$PATH:';C:\Program Files\R\'$R_VERSION'\bin\x64' &&
echo 'options(repos = "https://cloud.r-project.org")' > ~/.Rprofile.site &&
export R_PROFILE=~/.Rprofile.site &&
doskey R=R.exe &&
doskey Rscript=Rscript.exe &&

Rscript -e 'sessionInfo()' &&
Rscript -e 'install.packages("devtools");if (!all("devtools" %in% installed.packages())) { q(status = 1, save = "no")}' &&
Rscript -e 'devtools::session_info(installed.packages()[, "Package"])'
