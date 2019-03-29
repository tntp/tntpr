Last generated March 29, 2019

[![Travis-CI Build
Status](https://travis-ci.org/tntp/tntpr.svg?branch=master)](https://travis-ci.org/tntp/tntpr)

About
-----

The `tntpr` package makes data science at TNTP easier and more accurate
by supplying tools that are needed for common TNTP analyses. Because
this package specifically serves the TNTP analysis community, functions
can be tailored to our exact use cases.

To get a sense of the possibilities for an internal TNTP package,
consider this [blog post from
Airbnb](https://medium.com/airbnb-engineering/using-r-packages-and-education-to-scale-data-science-at-airbnb-906faa58e12d#.6xtwqtk4m)
on their internal R package, “Rbnb”.

Package summary
---------------

Some of the highlights of the package include:

-   TNTP-themed RMarkdown templates, for starting a new analysis with a
    shell that can already generate a TNTP-themed .docx report
-   TNTP-specific ggplot2 theme and color palette
-   Functions for initializing a new repository or project folder with
    TNTP-standard directories and documentation
-   Survey analysis tools
-   Wrappers for quickly making typical TNTP-style charts (e.g., bar
    chart of means on variable 1 grouped by variable 2)
-   Education-specific data management functions (e.g., `date_to_SY()`
    to convert continuous hire dates into school years using a specified
    cutoff date), and a built-in fake student achievement dataset to
    play with called `wisc`.

`tntpr` is built to work with the tidyverse set of packages.

Installing the package
----------------------

This package is not on CRAN, and probably will not ever be. You’ll need
to install this package from its GitHub repository. You can add this to
the top of your analysis script:

    # install tntpr if you don't already have it, then load it
    library(devtools) # for install_github()
    if(!require("tntpr")) install_github("tntp/tntpr")
    library(tntpr)

Once installed, you can update the package with `update_tntpr()`.

Using the package
-----------------

See the
[vignette](https://github.com/tntp/tntpr/blob/master/vignettes/introduction.md)
for demonstrations of how to use these tntpr functions.

Feature Requests and Bug Reports
--------------------------------

Have a data problem you think tntpr could help with? Find a bug while
working with the `tntpr` package? Please create an issue.
