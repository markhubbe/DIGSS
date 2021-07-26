## Test environments
* windows-latest (release) 
* macOS-latest (release)
* ubuntu-20.04 (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* Found the following assignments to the global environment:
  File ‘DIGSS/R/ParametersCreator.R’:
    assign("SurveyParameters", SurveyParameters, globalenv())

  `parameterCreator` is a text-based input for parameters in the simulations run by DIGGS, created for archaeologists not familiar with R. The function explicitly warns the user that the list `SurveyParameters` will be replaced in the global environment if the user edits its values. Most users familiar with R will not use this function, as detailed in the Package's Vignette.
  
* Examples with CPU (user + system) or elapsed time > 5s
                   user system elapsed
  SurveyLoops    74.205  2.546  77.616
  SurveySim      66.444  1.838  68.933
  PlotSurveySumm 19.993  0.613  21.069

  Examples in DIGSS require long times, because DIGSS is running multiple simulations of site and objects creation with each iteration. Examples represent this process. All these functions have a progress bar to show the user how the simulations are advancing.     
