% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/protein.R
\name{get.ProteinDetail.accession}
\alias{get.ProteinDetail.accession}
\title{Returns a list of proteins from a specific project from Pride}
\usage{
get.ProteinDetail.accession(
  accession,
  page.size = 10,
  page.number = 0,
  sort.direction = "DESC"
)
}
\arguments{
\item{accession}{The project accession}

\item{page.size}{The number of proteins from Pride put into the ProteinDetail list}

\item{page.number}{The number of the page retrieved from Pride}

\item{sort.direction}{The direction Pride returns the proteins. Can be either DESC (descending) or ASC (ascending)}
}
\value{
The list of protein details
}
\description{
Returns a list of proteins from a specific project from Pride
}
\details{
i dunno
}
\author{
Tremayne Booker
}
