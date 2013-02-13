\name{ShojiObject}
\alias{ShojiObject}
\title{Mix-in class for multiple inheritance of variables and datasets.}
\usage{
  ShojiObject(...)
}
\description{
  Exists for common methods in interacting with Crunch API
  only. Has no Extract methods declared so as not to
  conflict with the vector/list/data.frame methods jointly
  inherited in CrunchVariable and CrunchDataset.
}

