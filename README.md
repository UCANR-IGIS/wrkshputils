
<!-- README.md is generated from README.Rmd. Edit this file, not that one -->

# Workshop Utilities for R

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)  
[![wrkshputils status
badge](https://ajlyons.r-universe.dev/badges/wrkshputils)](https://ajlyons.r-universe.dev)  
<!-- badges: end -->

Workshop Utilities for R contains utility functions for planning and
conducting R workshops in a virtual environment. These include:

-   Create custom HTML summary reports from a Google Form survey (i.e.,
    workshop registration)

-   Display PNG, JPG, or SVG images in the RStudio viewer pane, as a
    kind of popup slide that can be embedded within a workshop R script

-   *Coming soon:* Create HTML pages containing hints and tips,
    populated by a Google Sheet, that can be embedded in a R Notebook as
    hints and solutions. Option to create bitly links.

# Installation

`wrkshputils` is on the r-universe. You can install it with:

    # Enable universe(s) by ajlyons
    options(repos = c(
      ajlyons = 'https://ajlyons.r-universe.dev',
      CRAN = 'https://cloud.r-project.org'))

    # Install some packages
    install.packages('wrkshputils')

To see if its working:

    library(wrkshputils)
    wu_popup("http://placekitten.com/800/600")
