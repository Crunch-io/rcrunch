#' Functions to manipulate variables' or project's folder structure
#'
#' Variables in Crunch datasets are organized into folders, like in a file
#' system. Datasets are similarly organized into hierarchical Projects.
#' These functions allow you to create new folders and move objects into
#' folders. Their names, `mv` and `mkdir`, suggest their Unix file utility
#' inspiration.
#'
#' The functions have some differences from the strict behavior of their Unix
#' ancestors. For one, they work recursively, without additional arguments:
#' `mkdir` will make every directory necessary to construct the requested path,
#' even if all parent directories didn't already exist; and `mv` doesn't
#' require that the directory to move to already exist---it will effectively
#' call `mkdir` along the way.
#'
#' @param x A `CrunchDataset` or `Folder` (`VariableFolder` or `ProjectFolder`)
#' @param path A character "path" to the folder: either a
#' vector of nested folder names or a single string with nested folders
#' separated by a delimiter ("/" default, configurable via
#' `options(crunch.delimiter)`). The path is interpreted as
#' relative to the location of the folder `x` (when `x` is a dataset, that
#' means the root, top-level folder). `path` may also be a `Folder` object.
#' @param what A Variable, selection of variables from `dataset`, or any
#' other object that can be moved to a folder (e.g. a dataset when organizing
#' projects).
#' @return `x`, with the folder at `path` guaranteed to be created, and for
#' `mv`, containing `what` moved into it.
#' @seealso [cd()] to select a folder by path; [rmdir()] to delete a folder;
#' [folder()] to identify and set an object's parent folder; [base::dir.create()]
#' if you literally want to create a directory in your local file system, which
#' `mkdir()` does not do
#' @examples
#' \dontrun{
#' ds <- loadDataset("Example survey")
#' ds <- mv(ds, c("gender", "age", "educ"), "Demographics")
#' ds <- mkdir(ds, "Key Performance Indicators/Brand X")
#' # These can also be chained together
#' require(magrittr)
#' ds <- ds %>%
#'     mv(c("aware_x", "nps_x"), "Key Performance Indicators/Brand X") %>%
#'     mv(c("aware_y", "nps_y"), "Key Performance Indicators/Brand Y")
#' # Can combine with cd() and move things with relative paths
#' ds %>%
#'     cd("Key Performance Indicators/Brand X") %>%
#'     mv("nps_x", "../Net Promoters")
#' # Can combine with folder() to move objects to the same place as something else
#' ds %>% mv("nps_y", folder(ds$nps_x))
#' # Now let's put ds in a Project
#' projects() %>%
#'     mv(ds, "Brand Tracking Studies")
#' }
#' @export
mv <- function(x, what, path) {
    ## TODO: add an "after" argument, pass to addToFolder
    ## dplyr/tidyselect-ish functions, hacked in here (inspired by how pkgdown does it)
    fns <- list(
        starts_with = function(str, ...) grep(paste0("^", str), names(x), ...),
        ends_with = function(str, ...) grep(paste0(str, "$"), names(x), ...),
        matches = function(str, ...) grep(str, names(x), ...),
        contains = function(str, ...) grep(str, names(x), ..., fixed = TRUE)
    )
    e2 <- substitute(substitute(zzz, fns), list(zzz = match.call()[["what"]]))
    what <- eval.parent(eval(e2))
    if (is.language(what)) {
        ## It's one of our fns. Eval it again
        what <- eval(what)
    }

    if (!is.shojiObject(what)) {
        ## Character, numeric, logical. Extract from the dataset/folder
        ## TODO: add a "*" special case for for ShojiFolder [ method
        what <- x[what]
    }
    f <- cd(x, path, create = TRUE)
    .moveToFolder(f, what)
    return(invisible(x))
}

#' @rdname mv
#' @export
mkdir <- function(x, path) {
    ## TODO: add an "after" argument, move created folder there
    cd(x, path, create = TRUE)
    # Refresh without busting cache, in case there was no change
    # If there had been a change, cd() would have busted cache already
    x <- do.call(class(x), crGET(self(x)))
    return(invisible(x))
}

#' Change the name of the current folder
#'
#' If you just need to change the name of the folder you are currently in, you
#' can use `setName()`. It doesn't move variables or change anything
#' other than the name of the current folder.
#'
#' @param object A `Folder`
#' @param nm A character that is the new name the folder should have
#' @return `object`, with its name duly changed
#' @seealso [cd()] and [mv()]
#' @examples
#' \dontrun{
#' ds <- ds %>%
#'     cd("Demographics") %>%
#'     setName("Key Demos.")
#' }
#' @export
setName <- function(object, nm) {
    # The arguments `object` for the folder object to be renamed and `nm` for
    # the new name are to maintain consistency with `setNames()`
    name(object) <- nm
    return(invisible(object))
}

#' Change to different folder
#'
#' Like `cd` in a file system, this function takes you to a different folder,
#' given a relative path specification.
#'
#' @inheritParams mv
#' @param create logical: if the folder indicated by `path` does not exist,
#' should it be created? Default is `FALSE`. Argument mainly exists for the
#' convenience of `mv()`, which moves entities to a folder and ensures that
#' the folder exists. You can call `cd` directly with `create=TRUE`, though that
#' seems unnatural.
#' @return A `Folder` (`VariableFolder` or `ProjectFolder`)
#' @seealso [mv()] to move entities to a folder; [rmdir()] to delete a folder;
#' [base::setwd()] if you literally want to change your working directory in your
#' local file system, which `cd()` does not do
#' @examples
#' \dontrun{
#' ds <- loadDataset("Example survey")
#' demo <- cd(ds, "Demographics")
#' names(demo)
#' # Or with %>%
#' require(magrittr)
#' ds <- ds %>%
#'     cd("Demographics") %>%
#'     names()
#' # Can combine with mv() and move things with relative paths
#' ds %>%
#'     cd("Key Performance Indicators/Brand X") %>%
#'     mv("nps_x", "../Net Promoters")
#' }
#' @export
cd <- function(x, path, create = FALSE) {
    if (is.character(x)) {
        ## Probably user error
        halt(
            dQuote("cd()"),
            " requires a Crunch Dataset or Folder as its first argument"
        )
    }
    if (is.folder(path)) {
        ## Great! No lookup required
        return(path)
    }
    if (is.dataset(x)) {
        ## Get the variable folders root catalog
        x <- folders(x)
    }
    if (!is.folder(x)) {
        ## Probably user error
        halt(
            dQuote("cd()"),
            " requires a Crunch Dataset or Folder as its first argument"
        )
    }
    out <- x[[path, create = create]]
    if (!is.folder(out)) {
        halt(deparse(path), " is not a folder")
    }
    return(out)
}

#' Delete a folder
#'
#' Like `rmdir` in a file system, this function removes a folder. Unlike the
#' file-system version, it does not require the folders to be empty.
#'
#' @inheritParams mv
#' @return `NULL`
#' @seealso [mv()] to move entities to a folder; [cd()] to select a folder;
#' [base::file.remove()] if you literally want to delete a directory from your local
#' file system, which `rmdir()` does not do
#' @examples
#' \dontrun{
#' ds <- loadDataset("Example survey")
#' rmdir(ds, "Demographics")
#' # Or with %>%
#' require(magrittr)
#' ds <- ds %>%
#'     rmdir("Demographics")
#' }
#' @export
rmdir <- function(x, path) {
    to_delete <- cd(x, path)
    if (is.project(x) && length(to_delete) > 0) {
        halt(
            "Cannot remove '", name(to_delete), "' because it is not empty. ",
            "Move its contents somewhere else and then retry."
        )
    }
    delete(to_delete)
    invisible(refresh(x))
}

#' Find and move entities to a new folder
#'
#' @param x For `folder`, a Variable to find. For folder assignment, a Variable,
#' selection of variables in a
#' Dataset, or any other object that can be moved to a folder.
#' @return `folder` returns the parent folder of `x`, or `NULL` if the `x` is the root
#' level. `folder<-` returns the
#' `x` input, having been moved to the requested location.
#' @export
#' @examples
#' \dontrun{
#' ds <- loadDataset("Example survey")
#' folder(ds$income) <- "Demographics/Economic"
#' folder(ds$income)
#' ## [1] "Demographics"    "Economic"
#' }
#' @seealso [mv()] [cd()]
folder <- function(x) {
    if (is.folder(x)) {
        ## Parent folder is of the same type
        cls <- class(x)
    } else if (is.variable(x)) {
        cls <- "VariableFolder"
    } else {
        halt("No folder for object of class ", class(x))
    }
    u <- parentFolderURL(x)
    if (is.null(u)) {
        ## This has no parent. Root level, right?
        return(NULL)
    }
    return(new(cls, crGET(u)))
}

#' @param value For assignment, a character "path" to the folder: either a
#' vector of nested folder names or a single string with nested folders
#' separated by a delimiter ("/" default)
#' @rdname folder
`folder<-` <- function(x, value) {
    if (is.character(value)) {
        ## Turn path into folder relative to folder(x)
        value <- cd(folder(x), value)
        ## TODO: update man page generally (including examples)
        ## Note in man page that this is relative; to absolute, use "/"
    }
    .moveToFolder(value, self(x))
    return(refresh(x)) ## Actually, just need to get entity again, cache already busted
}

.moveToFolder <- function(folder, what) {
    ## Get the URLs of things to move
    if (is.project(folder)) {
        if (is.dataset(what)) {
            # If moving a dataset into a project, get its self, but first
            # check that we're not trying to move dataset to root project,
            # which is not allowed
            if (is.null(parentFolderURL(folder))) {
                halt("Can't move a dataset to the top-level project")
            }
            what <- self(what)
        }
    } else {
        ## Variable folder
        if (is.dataset(what)) {
            ## If moving a "dataset" into a variable folder, it's a variable
            ## catalog subset. So get the catalog.
            what <- allVariables(what)
        } else if (is.variable(what)) {
            what <- self(what)
        }
    }
    if (inherits(what, "ShojiCatalog")) {
        what <- urls(what)
    }

    ## No need to include vars that already exist in this folder
    what <- setdiff(what, urls(folder))
    if (length(what)) {
        payload <- wrapCatalog(
            index = sapply(what, emptyObject, simplify = FALSE)
        )
        if (length(what) > 1) {
            # If we're only adding one thing, no need to specify the graph
            # because by default it will be added to the end on the server.
            # Only need to specify the graph if we care about the order of
            # what we're sending (i.e. we're sending several (ordered)
            # variables).
            # Choosing not to send the graph should make the operation more
            # robust to stale local state or other concurrency concerns
            # (or overzealous/mistaken server validation)
            # TODO later, also send if the mv() specifies a position ("after")
            payload$graph <- I(c(folder@graph, what))
        }
        crPATCH(self(folder), body = toJSON(payload))
        ## Additional cache invalidation
        ## Drop all variable entities because their catalogs.folder refs are stale
        dropOnly(what)
        ## Drop all folders
        ## Hard to be smarter about figuring out which folders are dirty without
        ## doing a bunch of GETs.
        ## TODO: should API provide a catalogs.parent for folders so we don't ../
        dropCache(absoluteURL("../", self(folder)))
        ## Also drop the old hierarchical order (forgive the ugly rel path)
        dropCache(absoluteURL("../../variables/hier/", self(folder)))
        folder <- refresh(folder)
    }
    return(invisible(folder))
}


# recursively go through folders returning folder names or variables. This should
# be used sparingly since it will take a while to touch each node in the tree.
folder_recurse <- function(folder) {
    # can't just use lapply(folder, ...) because we don't have a bespoke lapply
    # for folders
    out <- lapply(seq_along(folder), function(i) {
        dir <- folder[[i]]
        if (is.folder(dir)) {
            out <- list(folder_recurse(dir))
            names(out) <- name(dir)
            return(out)
        }
        # otherwise, this is a variable, return the alias
        return(alias(dir))
    })
}

# take a folder/var tree (from folder_recurse) and copy it to dataset
plant_tree <- function(tree, dataset, folder_position = folders(dataset)) {
    lapply(seq_along(tree), function(i) {
        item <- tree[[i]]
        if(!is.null(names(item))) {
            # we have a folder!
            make_folder_continue(item, dataset, folder_position)
        } else {
            # deal with the variables we have
            vars <- Filter(Negate(is.list), item)
            # make sure the vars are in dataset
            vars <- vars[vars %in% aliases(allVariables(dataset))]
            if (length(vars) > 0) {
                mv(dataset, unlist(vars), folder_position)
                # TODO: this should be possible without refreshing here.
                folder_position <- refresh(folder_position)
                setOrder(folder_position, unlist(vars))
            }

            folders <- Filter(is.list, item)
            if (length(folders) > 0) {
                lapply(folders, make_folder_continue, dataset, folder_position)
            }
        }
    })
}

# make a folder, then continue down the tree specified in items (if it exists)
make_folder_continue <- function(item, dataset, folder_position) {
    position <- cd(mkdir(cd(dataset, folder_position), names(item)), names(item))
    plant_tree(item, dataset, folder_position = position)
}

#' Copy the folder structure from one dataset to another.
#'
#' @param source the dataset you want to copy the order from
#' @param target the dataset you want to copy the order to
#' @return returns the target dataset with source's folder structure
#' @examples
#' \dontrun{
#' ds <- copyFolders(ds1, ds)
#' }
#' @export
copyFolders <- function(source, target) {
    if (!is.dataset(source) | !is.dataset(target)) {
        halt("Both source and target must be Crunch datasets.")
    }
    source_tree <- folder_recurse(folders(source))
    plant_tree(list(source_tree), target)
    return(invisible(refresh(target)))
}


# Recursively get all variables below a folder
# TODO: Use trampoline? https://community.rstudio.com/t/tidiest-way-to-do-recursion-safely-in-r/1408
# My initial tests say it's slower, but is safer if we ever expect a large number of folders
variablesBelowFolder <- function(folder, namekey = "alias") {
    vars <- variables(folder)
    dirs <- folder[types(folder) %in% "folder"]
    
    out <- list(
        vapply(index(vars), vget(namekey), "", USE.NAMES = FALSE),
        lapply(seq_along(dirs), function(i) variablesBelowFolder(dirs[[i]], namekey))
    )
    unlist(out)
}