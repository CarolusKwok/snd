% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_list-mode.R
\name{list_mode}
\alias{list_mode}
\title{Find the statistical mode of a list.}
\usage{
list_mode(list)
}
\arguments{
\item{list}{the list of objects}
}
\value{
A list of objects, that it's item(s) are the statistical mode of the given list
}
\description{
A list may contain many different classes of objects. This function returns the object that appears in the list the most, regardless of object class.
}
\examples{
list_mode(list)
}
