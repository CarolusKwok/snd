% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatWO_key1mtx.R
\name{formatWO_key}
\alias{formatWO_key}
\title{System tools: Checking and Formatting the keys during write-out.}
\usage{
formatWO_key(key, mtx, mtxName)
}
\arguments{
\item{key}{The key in \code{character}. Only 1 key will be accepted.}

\item{mtx}{The matrix in \code{data.frame}. The matrix must contain the key.}

\item{mtxName}{The name of the \code{mtx} in character.}
}
\value{
A matrix, checked (or/and modified) by the key.
}
\description{
In SND, a \code{key} is a column within the matrix that can be used to check and modify itself, it's matrix and other matrixes. This function checks and modifies it's own matrix only, using a key. To modify other matrices, check the function \code{snd:::formatWO_key2mtx}. To enhance security, only 1 key will be accepted.
}
\keyword{internal}
