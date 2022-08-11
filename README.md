Workshop Utilities for R
================

<!-- README.md is generated from README.Rmd. Edit this file, not that one -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)  
[![wrkshputils status
badge](https://ajlyons.r-universe.dev/badges/wrkshputils)](https://ajlyons.r-universe.dev)  
<!-- badges: end -->

Workshop Utilities for R is a lightweight R package with functions for
students taking R workshops in a virtual environment. These include:

-   Downloading zip files with an option to extract them

-   Display PNG, JPG, or SVG images in the RStudio viewer pane, as a
    kind of popup slide that can be embedded within a workshop R script

Note that these functions are probably only going to be useful if the
instructor has prepared content to go with them.

# Installation

`wrkshputils` is on the r-universe as well as GitHub. You can install it
with:

    # Enable universe(s) by ajlyons
    options(repos = c(
      ajlyons = 'https://ajlyons.r-universe.dev',
      CRAN = 'https://cloud.r-project.org'))

    # Install some packages
    install.packages('wrkshputils')

or:

    remotes::install_github("ucanr-igis/wrkshputils")

To see if its working:

    library(wrkshputils)
    wu_popup("http://placekitten.com/800/600")

## History

Earlier versions of `wrkshputils` also had several additional utility
functions to help instructors create R Markdown slide decks. These have
now been split off into a separate package called
[`slideutils`](https://github.com/ucanr-igis/slideutils).
