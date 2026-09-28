## Resubmission

This is a resubmission of a package archived on 2024-04-20. The archived
version documented an argument named `X` that was absent from the usage of
`wind.fit_int()`. The function and its documentation now consistently use and
document the `tmpx` argument.

The external data providers have also been reviewed. Historical GFS requests
now use the NOAA/NCEI archive, current GFS requests use PacIOOS, and OSCAR
requests use the current `jplOscar` NOAA ERDDAP dataset.

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission because the previous release was archived.
* HTML validation was skipped because HTML Tidy is not installed in the local
  check environment. The HTML manual itself was generated successfully.

## Downstream dependencies

There are currently no reverse dependencies on CRAN because the package is
archived.
