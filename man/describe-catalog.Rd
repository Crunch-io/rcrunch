% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/abstract-categories.R,
%   R/automation.R, R/batches.R, R/categories.R, R/crunch-data-frame.R,
%   R/cube-variables.R, R/dataset.R, R/shoji-catalog.R, R/decks.R,
%   R/multitables.R, R/shoji-folder.R, R/shoji-order-slots.R, R/slides.R,
%   R/subvariables.R, R/tab-book.R, R/variable-catalog.R, R/variable-folder.R,
%   R/variable-type.R, R/versions.R
\name{aliases}
\alias{aliases}
\alias{describe-catalog}
\alias{aliases<-}
\alias{descriptions}
\alias{descriptions<-}
\alias{types}
\alias{emails}
\alias{timestamps}
\alias{scriptBody}
\alias{names}
\alias{names<-}
\alias{ids}
\alias{ids<-}
\alias{values}
\alias{values<-}
\alias{dates}
\alias{dates<-}
\alias{names,AbstractCategories-method}
\alias{names<-,AbstractCategories-method}
\alias{ids,AbstractCategories-method}
\alias{timestamps,ScriptCatalog-method}
\alias{timestamps,Script-method}
\alias{scriptBody,Script-method}
\alias{names,BatchCatalog-method}
\alias{ids<-,Categories-method}
\alias{values,Categories-method}
\alias{values<-,Categories-method}
\alias{dates,Categories-method}
\alias{dates<-,Categories-method}
\alias{names.CrunchDataFrame}
\alias{names,CrunchCube-method}
\alias{aliases,CrunchCube-method}
\alias{descriptions,CrunchCube-method}
\alias{types,CrunchCube-method}
\alias{notes,CrunchCube-method}
\alias{names,CrunchDataset-method}
\alias{names,ShojiCatalog-method}
\alias{names<-,ShojiCatalog-method}
\alias{emails,ShojiCatalog-method}
\alias{names,CrunchDeck-method}
\alias{names<-,CrunchDeck-method}
\alias{types,CrunchDeck-method}
\alias{names<-,MultitableCatalog-method}
\alias{types,ShojiFolder-method}
\alias{names,ShojiOrder-method}
\alias{names,OrderGroup-method}
\alias{names,SlideCatalog-method}
\alias{names<-,SlideCatalog-method}
\alias{types,SlideCatalog-method}
\alias{names,ArrayVariable-method}
\alias{names,TabBookResult-method}
\alias{aliases,TabBookResult-method}
\alias{descriptions,TabBookResult-method}
\alias{names,MultitableResult-method}
\alias{aliases,MultitableResult-method}
\alias{descriptions,MultitableResult-method}
\alias{aliases,VariableCatalog-method}
\alias{aliases<-,VariableCatalog-method}
\alias{notes,VariableCatalog-method}
\alias{notes<-,VariableCatalog-method}
\alias{descriptions,VariableCatalog-method}
\alias{descriptions<-,VariableCatalog-method}
\alias{types,VariableCatalog-method}
\alias{ids,VariableCatalog-method}
\alias{aliases,VariableFolder-method}
\alias{types,list-method}
\alias{names,VersionCatalog-method}
\alias{descriptions,VersionCatalog-method}
\alias{timestamps,VersionCatalog-method}
\title{Get and set names, aliases on Catalog-type objects}
\usage{
aliases(x)

aliases(x) <- value

descriptions(x)

descriptions(x) <- value

emails(x)

types(x)

timestamps(x)

ids(x)

ids(x) <- value

values(x)

values(x) <- value

scriptBody(x)

dates(x)

dates(x) <- value

\S4method{names}{AbstractCategories}(x)

\S4method{names}{AbstractCategories}(x) <- value

\S4method{ids}{AbstractCategories}(x)

\S4method{timestamps}{ScriptCatalog}(x)

\S4method{timestamps}{Script}(x)

\S4method{scriptBody}{Script}(x)

\S4method{names}{BatchCatalog}(x)

\S4method{ids}{Categories}(x) <- value

\S4method{values}{Categories}(x)

\S4method{values}{Categories}(x) <- value

\S4method{dates}{Categories}(x)

\S4method{dates}{Categories}(x) <- value

\method{names}{CrunchDataFrame}(x)

\S4method{names}{CrunchCube}(x)

\S4method{aliases}{CrunchCube}(x)

\S4method{descriptions}{CrunchCube}(x)

\S4method{types}{CrunchCube}(x)

\S4method{notes}{CrunchCube}(x)

\S4method{names}{CrunchDataset}(x)

\S4method{names}{ShojiCatalog}(x)

\S4method{names}{ShojiCatalog}(x) <- value

\S4method{emails}{ShojiCatalog}(x)

\S4method{names}{CrunchDeck}(x)

\S4method{names}{CrunchDeck}(x) <- value

\S4method{types}{CrunchDeck}(x)

\S4method{names}{MultitableCatalog}(x) <- value

\S4method{types}{ShojiFolder}(x)

\S4method{names}{ShojiOrder}(x)

\S4method{names}{OrderGroup}(x)

\S4method{names}{SlideCatalog}(x)

\S4method{names}{SlideCatalog}(x) <- value

\S4method{types}{SlideCatalog}(x)

\S4method{names}{ArrayVariable}(x)

\S4method{names}{TabBookResult}(x)

\S4method{aliases}{TabBookResult}(x)

\S4method{descriptions}{TabBookResult}(x)

\S4method{names}{MultitableResult}(x)

\S4method{aliases}{MultitableResult}(x)

\S4method{descriptions}{MultitableResult}(x)

\S4method{aliases}{VariableCatalog}(x)

\S4method{aliases}{VariableCatalog}(x) <- value

\S4method{notes}{VariableCatalog}(x)

\S4method{notes}{VariableCatalog}(x) <- value

\S4method{descriptions}{VariableCatalog}(x)

\S4method{descriptions}{VariableCatalog}(x) <- value

\S4method{types}{VariableCatalog}(x)

\S4method{ids}{VariableCatalog}(x)

\S4method{aliases}{VariableFolder}(x)

\S4method{types}{list}(x)

\S4method{names}{VersionCatalog}(x)

\S4method{descriptions}{VersionCatalog}(x)

\S4method{timestamps}{VersionCatalog}(x)
}
\arguments{
\item{x}{a \code{VariableCatalog}, \code{Subvariables}, or similar object}

\item{value}{For the setters, an appropriate-length character vector to
assign}
}
\value{
Getters return the character object in the specified slot; setters
return \code{x} duly modified.
}
\description{
These methods let you get and set names and aliases for variables in a
Dataset's catalog, or within \code{\link{Subvariables}} in an array
variable. They work like the base R names methods.
}
\details{
Note that the Dataset \code{names} method returns the aliases of its
variables by default. This behavior is controlled by
\code{envOrOption("crunch.namekey.dataset")}.
Set \code{options(crunch.namekey.dataset="name")} if you wish to use
variable names. See the variables vignette for more information.
}
\seealso{
\code{\link{Subvariables}} \code{\link{Categories}} \code{\link[base:names]{base::names()}}
\code{vignette("variables", package="crunch")}
}
