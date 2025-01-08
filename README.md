# catRlog
### A photo-ID project management system based in R

**`catRlog`** was originally produced as a downloadable folder system -- essentially a simplistic version of an `R` package -- in association with [this publication](https://link.springer.com/article/10.1007%2Fs42991-021-00158-7) in a special issue in the *Journal of Mammalogy* (also viewable [here](https://rdcu.be/csdPC)). We have since converted `catRlog` to a proper `R` package in order to make it easier to use, maintain, and improve upon. The documentation in this vignette, and also within the help documentation for the `R` functions, provide the most up-to-date support. 

**[A user's guide vignette is available here.](https://ericmkeen.github.io/catRlog/)** 

### Installation
```{r, echo=TRUE, eval=FALSE, include=TRUE, suppressWarnings=TRUE, suppressMessages=TRUE}
# Make sure you have "devtools" installed
if (!require('devtools')) install.packages('devtools')

# Install from GitHub
devtools::install_github('ericmkeen/catRlog')

# Load the libraray
library(catRlog)

# Explore help files and their examples
?catRlog
```

Code written by Eric Keen. 
System developed by [Eric Keen](https://new.sewanee.edu/our-faculty-staff/eric-ezell/), [Éadin O'Mahony](https://eadinomahony.wordpress.com/), Julia Wren and [Janie Wray](https://bcwhales.org/our-team/).

