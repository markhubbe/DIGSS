## Test environments
* windows-latest (release) 
* macOS-latest (release)
* ubuntu-20.04 (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTES. 

## Resubmission changes
- The resubmitted package removed the function `parametersCreator` as it is now considered obsolete and it was flagged by CRAN reviewer as not following ideal R practices (printing information in the console messages to the console that cannot be easily suppressed).
- References describing the methods in the package have been added to the DESCRIPTION file.
- All help files and vignette were updated to reflect the removal of `parametersCreator`.