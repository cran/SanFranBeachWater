<!-- README.md is generated from README.Rmd. Please edit that file -->
SanFranBeachWater
=================

`SanFranBeachWater` is a little R package that downloads the Beach Water Quality Monitoring Program data that the the San Francisco Public Utilities Commission maintain [here](http://sfwater.org/cfapps/lims/beachmain1.cfm). The data can be downloaded in raw form, or tables for individual beaches can be downloaded.

Installation
------------

``` r
devtools::install_github("RobertMyles/SanFranBeachWater")
```

Usage
-----

Get the data for China Beach:

``` r
library(SanFranBeachWater)

san_fran_china_beach()
#>          Date Total_Coliform E_Coli Entero_coccus
#> 1  2017-06-05             20      0             0
#> 2  2017-05-30             20      0             0
#> 3  2017-05-22             41     10            10
#> 4  2017-05-15             10      0             0
#> 5  2017-05-08             30      0             0
#> 6  2017-05-01              0      0             0
#> 7  2017-04-24            171      0             0
#> 8  2017-04-17            110     20             0
#> 9  2017-04-10            110      0             0
#> 10 2017-04-03             41     20             0
#> 11 2017-03-27             98      0            10
#> 12 2017-03-24            132     20             0
#> 13 2017-03-20              0      0             0
#> 14 2017-03-13             20     20             0
```

:poop:

Or alternatively, just get the status of the beach ("Open" or "Closed"):

``` r
san_fran_windsurfer_circle(status = TRUE)
#> [1] "Current Beach Status: Open"
```

Sweet! :beach_umbrella:
