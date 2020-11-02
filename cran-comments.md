## devRate version 0.2.0
In this new version, we included:
* new testing units for better code coverage
* new function devRateIBMparam for phenology models from known parameter estimates
* new function devRateQlStat to characterize goodness-of-fit
* new function devRateQlBio to assess biological likelihood
* new function devRateModelAll to adjust all known models to dataset
* delete unused function arguments: 
    - devRatePrint: delete T and rT arguments
    - devRatePlot: delete T and rT arguments
* fix argument name: df replaced by dfData in devRateModel (backward compatibility
  thanks to prefix matching) ; reason for version change from 0.1 to 0.2.

## Test environments
* Windows 10, R 4.0.2
* Linux Ubuntu 20.10, R 4.0.3
* win-builder (R Under development 2020-10-17 r79346).
* Ubuntu 16.04.6 LTS with Travis-ci.com : oldrel, release, devel

## R CMD check results
There were no ERRORs, or WARNINGs. 

Note : URLs are correct.
