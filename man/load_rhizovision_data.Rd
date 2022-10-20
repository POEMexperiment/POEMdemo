\name{load_rhizovision_data}
\alias{load_rhizovision_data}
\title{Load RhizoVision Explorer data in R}
\description{
Load a csv file (with extracted features) created with RhizoVision Explorer in R.}
\usage{
load_rhizovision_data(file, POEM=TRUE)}
\arguments{
\item{file}{A mandatory character string. The string is a path to a csv file (with extracted features) created by RhizoVision Explorer (Seethepalli et al, 2021).}
\item{POEM}{Boolean. Set to TRUE if POEM minirhizotron images are analysed.}
}
\value{
Returns a data frame containing the features extracted by RhizoVision Explorer.

If \code{POEM=TRUE}, the exported data frame will also contain the following columns:

\bold{Name_experiment}: the name of each sub-experiment

\bold{Plot}: the ID number of the plot

\bold{Arrival}: the PFG order of arrival scenario

\bold{Replicate}: a numeric value for the replicate number (1 to 5)

\bold{Tube}: the ID number of the minirhizotron tube

\bold{Location}: the location at which an image was taken along a minirhizotron tube

\bold{Depth_cm}: the depth (expressed in cm) at which an image was taken along a minirhizotron tube. This value is estimated as the shortest distance between the top surface of the minirhizotron tube at each given location and the soil surface.

\bold{Date}: the date at which an image was taken (YYYY-MM-DD)

\bold{Days}: the number of days since the first sowing event

\bold{Time}: the exact time at which an image was taken (YYYY-MM-DD HH:MM:SS)

\bold{Session}: the session number

\bold{User}: the initials of the names of the person who took the image in the field
}
\references{
Seethepalli A., Dhakal K., Griffiths M., Guo H., Freschet G.T., York L.M. (2021) RhizoVision Explorer: open-source software for root image analysis and measurement standardization, \emph{AoB Plants}, DOI: 10.1093/aobpla/plab056.
}
\author{Benjamin M. Delory}
\examples{
path <- system.file("extdata", package="POEM")

data <- load_rhizovision_data(paste(path, "RVE/features.csv", sep="/"))
}
