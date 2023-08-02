# terrapin

## Overview
The terrapin package is a personal package that slightly builds on the [terra](https://rspatial.org) package. It is mainly composed of functions that allow SpatRasters to be easily subset based on their dates in different ways.

## Instructions
This package is mainly a personal package, so it is not available on CRAN.
To download this package directly from GitHub, you'll need to use the "devtools" package.
It's easiest to do this within RStudio.

1) Install the [devtools](https://github.com/hadley/devtools) and [terra](https://rspatial.org) packages from CRAN: 
``` R
install.packages("devtools")
install.packages("terra")     # fundamental to terrapin
```

2) Load the devtools package:
```R
library(devtools)
```

3) Install terrapin directly from GitHub:
```R
devtools::install_github("polarSaunderson/terrapin")
```

4) Load the terrapin package:
```R
library(terrapin)
```
