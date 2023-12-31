% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sys_msg_abort.R, R/sys_msg_standard.R
\name{sys_abort}
\alias{sys_abort}
\alias{sys_abort_NoArg}
\alias{sys_abort_WrongClass}
\alias{sys_abort_WrongLength}
\title{System tools: A slightly better abort messenger}
\usage{
sys_abort(message, x, arg = rlang::caller_arg(x), ...)

sys_abort_NoArg(x)

sys_abort_WrongClass(x, class)

sys_abort_WrongLength(x, length)
}
\arguments{
\item{message}{Message to be displayed, formatted via \code{cli::cli_bullets()}. For more information, please read \code{cli::cli_abort}.}

\item{x}{The missing argument}

\item{arg}{Argument to be traced. When referring to this in argument \code{message}, use \verb{\{.arg \{arg\}\}}.}

\item{...}{Additional items to consider in the message. Any items that uses glue (\code{{}}) must be passed through here.}

\item{class}{Class}
}
\value{
An abort message

A standard abort message
}
\description{
When an error occurs, it will automatically refer to the function name that the user typed in, instead of the previous function name. Additionally, custom \code{cli} formating can be used to refer different stuff.
This function is based on \code{cli::cli_abort} for formatting. Note that when writing functions, \strong{do not} use pipe (\verb{\%>\%}, \verb{|>}) before this function.

Currently supported inline-markup themes
\itemize{
\item .na for NA values,
\item .mtx for referring to matrices
\item .col for referring to columns in the matrix
}
}
\examples{
sys_abort(message = "test")
sys_abort_NoArg(x = xlsxFile)
}
\keyword{internal}
