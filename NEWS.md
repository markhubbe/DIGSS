# DIGSS 1.0.2
- Removed `parametersCreator` from package. With the creation of the Shiny version of DIGSS (https://markhubbe.shinyapps.io/digss/), this function becomes obsolete, and it was flagged by CRAN reviewer as problematic because it was printing information messages to the console that cannot be easily suppressed.
- Updated documentation and vignette to reflect the removal of `parametersCreator`
- Updated DESCRIPTION file to include the reference to Kintigh (1988), which details part of the simulation methods used in DIGSS   

# DIGSS 1.0.1
-   Changed `parametersCreator` to not assign values to the global environment
-   Cleaned a few code lines that were printing unnecessary information to the console
-   Updated documentation to reflect changes in `parametersCreator`
