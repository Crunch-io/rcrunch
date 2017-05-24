# Contributing to the Crunch package

## How to

1. Fork the repository and clone your fork locally :octocat:
1. `git remote add upstream git@github.com:Crunch-io/rcrunch.git`
1. Fetch and check out master from upstream: `git fetch && git checkout upstream/master`
1. Check out a new branch to collect the commits that comprise your awesome new feature :tada:
1. Do your work, commit, and push
1. Click “pull request”
1. Revise as needed from review comments — just push more commits on your branch :bookmark_tabs:
1. Celebrate when it gets merged! :squirrel:

## Guidelines

* Please observe and follow the style conventions of the package
* Be sure to fully cover your code with tests--the coverage report will alert if you do not
* It is recommended that you run `make check` before creating your pull request. Travis-CI will run check and will note if your code fails. Pull requests will only be accepted once the checks pass. 
* If you're adding functionality and not just fixing a bug, be sure to update the documentation (the `#' @roclets` in the code) to note any changed behavior and any  arguments added to functions.
* If you've added a function that you want to be available to package users, be sure to include `#' @export` in the docs.
* If you edit the documentation, be sure to run `make doc` before you commit--otherwise the `man` pages and `NAMESPACE` won't be updated. (`make check` will run `make doc` as well)
