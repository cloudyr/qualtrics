
qualtrics
=========

qualtrics is an R client for the Qualtrics survey platform.

You will need an account and an [API token](https://www.qualtrics.com/support/integrations/api-integration/api-integration) for most functionality. Set the environment variable `QUALTRICS_TOKEN` to your token. You can do this [during R startup](https://www.rdocumentation.org/packages/base/versions/3.3.1/topics/Startup) (recommended), using a configuration file and `read_config()`, or interactively with [`Sys.setenv()`](https://www.rdocumentation.org/packages/base/versions/3.3.1/topics/Sys.setenv):

``` r
Sys.setenv("QUALTRICS_TOKEN" = "mykey")
```

Usage
-----

``` r
library("qualtrics")
```

Installation
------------

[![CRAN](https://www.r-pkg.org/badges/version/qualtrics)](https://cran.r-project.org/package=qualtrics) ![Downloads](https://cranlogs.r-pkg.org/badges/qualtrics) [![Travis Build Status](https://travis-ci.org/cloudyr/qualtrics.svg?branch=master)](https://travis-ci.org/cloudyr/qualtrics)

[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/x3hbvp55a52h7a7k?svg=true)](https://ci.appveyor.com/project/jamesdunham/qualtrics) [![codecov.io](https://codecov.io/github/cloudyr/qualtrics/coverage.svg?branch=master)](https://codecov.io/github/cloudyr/qualtrics?branch=master)

This package is not yet on CRAN. To install the latest development version you can install from the cloudyr drat repository:

``` r
# latest stable version
install.packages("qualtrics", repos = c(cloudyr = "http://cloudyr.github.io/drat", getOption("repos")))
```

Or, to pull a potentially unstable version directly from GitHub:

``` r
if (!require("ghit")) {
  install.packages("ghit")
}
ghit::install_github("cloudyr/qualtrics")
```

Related
-------

-   qualtrics is written by the authors of [qualtRics](https://github.com/JasperHG90/qualtRics), [qualtricsR](https://github.com/saberry/qualtricsR), and [qsurvey](https://jdunham.io/qsurvey/).

-   [QualtricsTools](https://github.com/ctesta01/QualtricsTools) generates reports from Qualtrics data via Shiny.

-   Python: [PyQualtrics](https://github.com/Baguage/pyqualtrics); [SurveyHelper](https://github.com/cwade/surveyhelper).

------------------------------------------------------------------------

[![cloudyr project logo](https://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
