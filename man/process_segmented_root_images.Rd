\name{process_segmented_root_images}
\alias{process_segmented_root_images}
\title{Extract features from segmented root images}
\description{
Calculate root density and distribution indices from segmented root images. This function was mainly designed to use segmented images produced with RootPainter. This image analysis pipeline is described in \cite{Alonso-Crespo et al (2022)}}
\usage{
process_segmented_root_images(path, root.distribution=TRUE, res=2500, units="mm",
                              POEM=TRUE, median.filter=TRUE, kernel.size=3,
                              D=90, DRF=30)}
\arguments{
\item{path}{A mandatory character string. The string is a path to a folder containing segmented root images.}
\item{root.distribution}{Boolean. Set to TRUE if root distribution indices should be calculated. These root distribution indices include the mean and maximun rooting depth in each image, the deep root fraction (DRF), and the depth value above which a certain proportion (D) of the root pixels are located.}
\item{res}{The image resolution in dpi.}
\item{units}{Desired units to express the results. Either "px", "mm" or "cm".}
\item{POEM}{Boolean. Set to TRUE if POEM minirhizotron images are analysed.}
\item{median.filter}{Boolean. Set to TRUE if a median filter must be applied to each image before feature extraction.}
\item{kernel.size}{The size of the kernel used for the median filtering.}
\item{D}{Numeric value used to calculated the depth value above which a certain proportion of the root pixels are located. For example, setting D=90 will calculate the depth value above which 90 percent of the root pixels are located.}
\item{DRF}{Numeric value used to calculate the deep root fraction. For example, setting DRF=30 will calculate the proportion of root pixels located in the lower 30 percent of the image.}
}
\value{
Returns a data frame including the following columns:

\bold{image}: the image file name

\bold{height}: the image height

\bold{width}: the image width

\bold{mrd}: the average rooting depth (i.e., the depth value above which 50 percent of the root pixels are located). Only calculated if \code{root.distribution=TRUE}.

\bold{maxrd}: the maximum rooting depth (i.e., the depth of the deepest pixelrow in which a root pixel is found). Only calculated if \code{root.distribution=TRUE}.

\bold{d..}: the depth value above which a certain proportion (D) of the root pixels are located. Only calculated if \code{root.distribution=TRUE}.

\bold{drf}: the deep root fraction. Only calculated if \code{root.distribution=TRUE}.

\bold{rsa}: the absolute root surface area in an image

\bold{rrsa}: the relative root surface area in an image [0,1]

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
Alonso-Crespo I.M., Weidlich E.W.A., Temperton V.M., Delory B.M. (2022) Assembly history modulates vertical root distribution in a grassland experiment, \emph{Oikos}, DOI: 10.1111/oik.08886.
}
\author{Benjamin M. Delory}
\examples{
path <- system.file("extdata", package="POEM")

data <- process_segmented_root_images(path=path, root.distribution=FALSE, res=2500, units="mm",
                                      POEM=TRUE, median.filter=TRUE, kernel.size=3)
}
