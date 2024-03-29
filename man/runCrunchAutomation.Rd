% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/automation.R
\name{runCrunchAutomation}
\alias{runCrunchAutomation}
\alias{showScriptErrors}
\title{Run a crunch automation script}
\usage{
runCrunchAutomation(
  x,
  script,
  is_file = string_is_file_like(script),
  encoding = "UTF-8",
  ...
)

showScriptErrors()
}
\arguments{
\item{x}{A crunch dataset or project folder (for backwards compatibility,
\code{dataset} is also accepted)}

\item{script}{A path to a text file containing a Crunch Automation script
or a character vector of length 1 or more with Crunch Automation commands (see \code{Details})}

\item{is_file}{The default guesses whether a file or string was
used in the \code{script} argument, but you can override the heuristics
by specifying \code{TRUE} for a file, and \code{FALSE} for a string.}

\item{encoding}{Optional encoding to convert \strong{from}, defaults to UTF-8.
The API accepts only UTF-8, so all text will be converted to UTF-8 before
being sent to the server.}

\item{...}{Additional options, such as \code{dry_run = TRUE}, passed on
to the API if x is a dataset (if x is a project folder, an error is thrown)}
}
\value{
For \code{runCrunchAutomation()}: an updated dataset/project folder (invisibly),
For \code{showScriptErrors()}, when run after a failure, a list with two items:
\code{script}: that contains the script string sent to the server and \code{errors} which is a
\code{data.frame} with details about the errors sent from the server.
}
\description{
Crunch Automation is a custom scripting language that allows you to
execute common Crunch commands. The syntax is described in the
\href{https://help.crunch.io/hc/en-us/categories/360004356012-Crunch-Automation}{Crunch API documentation}.
}
\details{
If a character vector with length bigger than 1 is passed to \code{script},
it's converted to a string by concatenating its elements together using
line breaks.
}
\examples{
\dontrun{
# Can use a path to a file:
script_file <- "crunch_automation.txt"
ds <- runCrunchAutomation(ds, script_file)

# Or a string directly:
ds <- runCrunchAutomation(ds, "RENAME v1 TO age;")

# A "dry run" that validates the script but does not run it:
runCrunchAutomation(ds, "RENAME V1 TO age;", dry_run = TRUE)

# After a failed run, some error information prints to console,
# But more details are available with function:
showScriptErrors()

# After a successful run, can look at scripts:
scripts(ds)

# Run Crunch Automation on a folder:
my_folder <- cd(projects(), "folder1")
runCrunchAutomation(my_folder, 'CREATE FOLDER "folder2";')
}
}
\seealso{
\code{\link{automation-undo}} & \code{\link{script-catalog}}
}
