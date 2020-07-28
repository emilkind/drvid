[![Travis build status](https://travis-ci.org/natverse/drvid.svg?branch=master)](https://travis-ci.org/natverse/drvid)

# drvid

Provides access from R to [DVID](https://github.com/janelia-flyem/dvid) as used to store electron microscopy image data for large scale connectomics 

## Installation

You can install the development version of drvid from GitHub

``` r
devtools::install_github("natverse/drvid")
```
## Setup

You can specify a default DVID server by specifying the following two
environment variables in your [Renviron](https://www.rdocumentation.org/packages/base/topics/Startup)
file.

```
drvid.server="http://dvid.connectomesrus.com:8900"
drvid.node="a32b"
```
Make sure you have a blank line at the end of the file

## Use

```
library(drvid)
nl=read.neurons.dvid(c(635062078, 859507274))
library(nat)
plot3d(nl)
```

