% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dryadFileDownload.R
\name{dryadFileDownload}
\alias{dryadFileDownload}
\title{Interactively Download a File from a Specific Dryad Dataset Version}
\usage{
dryadFileDownload(doi, path = "~/Downloads", name = NA)
}
\arguments{
\item{doi}{A character string specifying the dataset DOI, in the form of "https://doi.org/..."}

\item{path}{A character string specifying the location of the file. Default: "~/Downloads"}

\item{name}{A character string specifying the name of the file. Default: NA (name of the file on DRYAD)}
}
\value{
Invisibly downloadeds a file, or \code{NULL} if no file was selected.
}
\description{
\code{dryadFileDownload()} allows interactive access to files from different versions of a dataset hosted on Dryad, using the dataset's DOI.
The user can select a version and a file from the list of available options. The file will be downloaded into a prior determine folder under a prior determined name.
}
\details{
This function first queries the Dryad API to obtain all available versions of a dataset associated with the given DOI.
The user is then prompted to select a version via an interactive menu. After selecting a version, all associated files are listed, and the user selects one for download.
The user is prompted to download the file to their \verb{~/Downloads} folder if not specified otherwise.
The function exits if the user cancels any step.
}
\examples{
\dontrun{
dryadFileDownload("https://doi.org/10.5061/dryad.z08kprrk1")
}

}
\seealso{
\code{\link[=dryad_version_overview]{dryad_version_overview()}}, \code{\link[=dryadFileDownloadID]{dryadFileDownloadID()}}, \code{\link[=dryadFileRead]{dryadFileRead()}}
}
