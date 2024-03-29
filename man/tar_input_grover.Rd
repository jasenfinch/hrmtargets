% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_input_grover.R
\name{tar_input_grover}
\alias{tar_input_grover}
\title{Workflow input using a grover API}
\usage{
tar_input_grover(
  name,
  instrument,
  experiment,
  grover_client,
  conversion_args = grover::conversionArgsPeakPick(),
  data_dir = "data/mzML"
)
}
\arguments{
\item{name}{Symbol. The name for the collection of targets. This serves as a prefix for target names.}

\item{instrument}{The grover API repository instrument name.}

\item{experiment}{The experiment name referring to a directory available on the grover API.}

\item{grover_client}{S4 object of class \code{GroverClient} specifying the host details of the grover API.}

\item{conversion_args}{The file conversion arguments to be passed to the \code{args} argument of \code{grover::convertFile()}.}

\item{data_dir}{The path to the directory in which to output converted mzML files.}
}
\value{
A list of target objects specifically for analysis pipeline input using the specified grover API.
}
\description{
Targets for workflow inputs using a grover API
}
\examples{
## Grover API input
targets::tar_dir({
targets::tar_script({
    library(hrmtargets)
    list(
        tar_input_grover(example,
                         'an_instrument',
                         'an_experiment',
                         grover::grover('a_host',80,'1234'))
)
})
targets::tar_manifest()
})
}
