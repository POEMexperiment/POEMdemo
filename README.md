# POEMdemo
R package with functions to process segmented (mini)rhizotron images. This R package was used for the Phenome Force workshop "Using (mini)rhizotrons to analyse root distribution: experimental set-up, image acquisition and analysis pipelines" (October 28, 2022).

# Using Python with RStudio and reticulate

Some functions of this R package use Python code. In order to be able to use the package, you will have to install a base version of Python, create and activate your Python environment, install Python packages in your environment, as well as install and configure reticulate to use Python.  

All these steps are described in the following video tutorial:  

https://docs.rstudio.com/tutorials/user/using-python-with-rstudio-and-reticulate/

Required Python packages: numpy, PIL, pandas, scipy, shutil, os

# Install the R package POEMdemo

To install the package, run this code in your R console:

install.packages("devtools")  
devtools::install_github("POEMexperiment/POEMdemo")

# Run the rhizotron explorer (shiny app)
POEMdemo::rhizotron_explorer()
