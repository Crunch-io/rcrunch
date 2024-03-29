% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expressions.R
\name{expressions}
\alias{expressions}
\alias{crunchdbFunc}
\title{Construct Crunch Expressions from Crunch Database Functions}
\usage{
crunchdbFunc(fun, x, ...)
}
\arguments{
\item{fun}{The name of the crunch database function to call}

\item{x}{An input, a crunch variable, expression or R object}

\item{...}{Other arguments passed to the database function}
}
\description{
Crunch Expressions, i.e. \code{CrunchExpr} and \code{CrunchLogicalExpr},
encapsulate derivations of Crunch variables, possibly composed of other functions
which are only evaluated when sent to the server when creating a variable using \code{\link[=VarDef]{VarDef()}}
or using \code{\link[=as.vector]{as.vector()}} to get data. The crunch database functions can be found in the
\href{https://help.crunch.io/hc/en-us/articles/360042411792-Expressions}{Help Center},
and can be called directly via \code{crunchdbFunc()}m but many have also been wrapped
in native R functions, and are described in the details section below.
}
\details{
Logical expressions
\itemize{
\item These logical operators \code{==}, \code{!=}, \code{&}, \code{|}, \code{!},\code{\%in\%}  work the same way as their
base R counterparts
\item \code{is.selected(x)} return \code{CrunchLogicalExpr} whether a value is in a selected category
\item \code{rowAny(x)} and \code{rowAll(x)} work row-wise on \code{MultipleResponse} Variables (and expressions),
though \code{na.rm} is not implemented for \code{all(x)}.
\verb{\%ornm\%} is similar to \code{|}, but where "not selected" beats "missing" (so \code{FALSE \%ornm\% NA}
is \code{FALSE} instead of \code{NA} as it would be with \code{FALSE | NA})
}

Comparisons
\itemize{
\item Comparison operators \code{<}, \code{<=}, \code{>}, \code{>=} work the same way as their base R counterparts.
\item \code{crunchBetween(x, lower, upper, inclusive)} to provide lower and upper bounds in a single
expression.
}

Missing data expressions
\itemize{
\item \code{is.na(x)}, \code{is.valid(x)} return \code{CrunchLogicalExpr} whether a single variable (or expression
that creates one) is missing (or not missing).
\item \code{rowAnyNA(x)}, \code{rowAllNA(x)} return \code{CrunchLogicalExpr} whether any/all values in an
array variable (or expression that creates one) are missing.
\item \code{complete.cases(x)} returns an expression that is "selected" if all cases are non-missing,
"missing" if they are all missing, and "other" otherwise.
}

Selection expressions
\itemize{
\item \code{selectCategories(x, selections, collapse = TRUE)} takes a categorical variable (or array)
and marks categories as selected. \code{selections} should be a list of category names or
values. If \code{collapse} is \code{TRUE}, (the default), it collapses the categories to "selected",
"other" and "missing", but it is \code{FALSE}, then the old categories are preserved.
\item \code{asSelected(x)} returns an expression that condenses a categorical into 3 categories
("selected", "other" or "missing")
\item \code{selectedDepth(x)} returns an expression that creates a numeric variable that counts the
number of selections across rows of an array variable (or expression that creates one)
\item \code{arraySelections(x)} returns an expression that takes an array and creates an array with
each variable condensed to "selected", "other" or "missing" and an extra subvariable
"\strong{any}" that indicates whether any is selected.
\item \code{alterCategoriesExpr(x, categories = NULL, category_order = NULL, subvariables = NULL)}
Change the category names, order, or subvariable names of categorical or Array variables
(can only modify existing ones, not add or remove categories or subvariables).
\code{categories} is a \code{Categories} object or a list of lists, each with a \code{name} indicating
the new name, as well as an \code{id} or \code{old_name} to identify which category to modify.
\code{category_order} is either a numeric vector indicating category ids or a character vector
indicating the names of the categories in the order they should be displayed
(note that all categories must be specified). \code{subvariables} is  a list of lists, each with
a \code{name} to rename the subvariable and an \code{alias}, \code{old_nam} or \code{id} to identify the
subvariable. When \code{x} is an expression, all categories and subvariables must be identified
by \code{id}.
}

Array expressions
\itemize{
\item \code{makeFrame(x, numeric = NULL)} an expression that creates an array from existing
variables or expressions, see \code{deriveArray()} for more details
\item \code{arraySubsetExpr(x, subvars, subvar_id = c("alias", "name", "id"))} Take a subset of an
existing array variable, identifying the subvariables by alias, name, or id (if \code{x} is
an expression, you must use id).
\item 

\if{html}{\out{<div class="sourceCode">}}\preformatted{ alterArrayExpr(
   x,
   add = NULL,
   order = NULL,
   order_id = c("alias", "name", "id"),
   remove = NULL,
   remove_id = c("alias", "name", "id"),
   subreferences = NULL,
   subreferences_id = c("alias", "name", "id")
)
}\if{html}{\out{</div>}}

Add, reorder, remove or rename subvariables on an an array variable \code{x}. The \code{add} argument is
a list of variables or expressions, optionally named with the id they should have. \code{order}
and \code{remove} are vectors of aliases, names or ids (specify which with \code{order_id}/\code{remove_id}).
The \code{subreferences} object is a list of lists that are named the alias, name, or id (again
specify which with \code{subreferences_id}) with metadata information like name and alias in the
list.
}

Miscellaneous expressions
\itemize{
\item \code{caseExpr(..., cases)} Create a categorical variable from
a set of logical expressions that when met are assigned to a category. See
\code{\link[=makeCaseVariable]{makeCaseVariable()}} for more details.
\item \code{bin(x)} returns a column's values binned into equidistant bins.
\item \code{nchar(x)} returns a numeric value indicating the length of a string (or missing reason)
in a \code{TextVariable} (or expression that creates one)
\item \code{unmissing(x)} for a \code{NumericVariable} (or expression that creates one) return the values of
the data, ignoring the ones set to missing.
\item \code{trim(x, min, max)} for a \code{NumericVariable} (or expression that creates one) return values
that where all values less than \code{min} have been replaced with \code{min} and all values greater
than \code{max} have been
\item \code{crunchDifftime(e1, e2, resolution)} Gets the difference between two datetimes as a number
with specified resolution units (one of \code{c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms")}).
\item \code{datetimeFromCols(year, month, day, hours, minutes, seconds)} create a \code{Datetime} variable
from numeric variables or expressions (\code{year}, \code{month}, and \code{day} are required, but \code{hours},
\code{minutes}, and \code{seconds} are optional)
\item \code{rollup(x, resolution)} sets the resolution of a datetime variable or expression, see
\code{\link[=resolution]{resolution()}}
}
}
