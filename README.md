# terrapin

## Overview
`terrapin` is (currently) a personal package that builds on the excellent [terra](https://rspatial.org) package. `terrapin` is mainly composed of 
functions that allow SpatRasters to be easily subset based on their dates in 
different ways. 

**WARNING** Some of this is highly experimental and may not *always* follow the
current documentation. Some functions also need extreme care when using, 
particularly the `handle_x` and `exclude_x` functions which are prone not to
work in new situations.

**WARNING** The examples in most functions are "conceptual" at the moment, and 
are nearly all wrapped in `\dontrun`. The documentation and examples will 
continue to be updated.

## Instructions
This package is mainly a personal package, so it is not available on CRAN.
To download this package directly from GitHub, you'll need to use the "devtools" 
package.
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
