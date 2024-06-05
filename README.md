# Overview
This app uses RShiny to visualise contour plots from my paper [PAPER NAME]

An online version is hosted at [LINK] and can be used directly in a web browser. 

This repository contains the files and instructions to run the app locally. It requires R to be installed on your system, and the shiny, bslib, and R.matlab packages.

# Setup Instructions - Windows

1) If you don't have R, go to https://cran.r-project.org/mirrors.html and choose an appropriate mirror to download from. The app was developed on R version 4.4.0, it may work on different versions but I haven't tested it.
2) Create a folder somewhere to use as your R working directory.
  - E.g. C:\Users\qboesley\Documents\RShinyApps
4) Inside this folder, create a folder named "OptoContourViewer".
  - C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer
5) Inside this folder, create a folder named "data".
  - C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer\data
6) Download the files "app.r", "grey.mat", and "white.mat"
  - Put "app.r" in the "OptoContourViewer" folder: C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer\app.r
  - Put "grey.mat" and "white.mat" in the "data" folder: C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer\data\grey.mat & C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer\data\white.mat
6) Run R and change your working directory to the folder "RShinyApps" you made in step 2, by clicking on "File" in the toolbar and choosing "Change dir...".
![image](https://github.com/qboesley/optical-irradiance-contour-viewer/assets/127060519/20e75cc2-803b-46dd-bc31-67fac6f5a0d9)
7) Before you run the app for the first time input the following commands into the R console:
  ```
  install.packages("shiny")
  install.packages("bslib")
  install.packages("R.matlab")
  ```
8) To start the app run the following commands in the R console:
  ```
  library(shiny)
  runApp("OptoContourViewer")
  ```
  The app should open in your system default web browser. It has been tested in Firefox. 
  
![image](https://github.com/qboesley/optical-irradiance-contour-viewer/assets/127060519/cf03471a-7c12-40cd-8b0d-93b6e5eba49e)

# Using the App
## Input Options
The sidebar on the left hand side contains all the app controls. 
-**Tissue Type** toggles between the grey matter and white matter simulation datasets.
- **Wavelength (nm)** toggles between the source wavelengths used in the simulations.
  - Note that 580 nm light is actually yellow rather than green, but it is plotted in green here for better visibility.
- **Light Source** is a drop down menu to select particular light sources. There are two main source types:
  - Optical Fibres, abbreviated as OF. The length in brackets refers to the diameter and NA is the numeric aperture.
  - LEDs. These were all modelled as square light sources. The length in brackets refers to the edge length of the square.
- **Total Optical Power (mW)** sets the total power output from the light source. The user can type values in to the box. You can also use the clicker icons, or scroll the mouse wheel to change the value by 0.1 mW at a time.
- **Threshold Irradiance (mW/mm^2)** sets the irradiance value to draw the contour line at. The user can type values in to the box. You can also use the clicker icons, or scroll the mouse wheel to change the value by 0.1 mW at a time.
- **Download Plot** button will save the currently displayed plot as a .png file. The filename is automatically generated:
  - {SourceInfo}_{TissueType}_{Wavelength}nm_P{SourcePower}_T{ThresholdValue}.png
  - Decimal points in {SourcePower} are replaced by hyphens (-)

## Outputs
Whenever any options are changed the contour plot and summary data are updated automatically.

The light source dimensions (diamter for optical fibres, edge length for LEDs) are drawn on the plot as a black horizontal line. 
The following summary data are included below the contour plot:
- **Max Irradiance** is the maximum irradiance at a single point within the model domain.
- **Vol. illuminated** is the volume of tissue illuminated over the specified threshold value.
- **Forward spread** is the maximum distance 'forwards' from the light source (i.e. positive depth value on the y-axis) with irradiance equal to or greater than the threshold value.
- **Backward spread** is the maximum distance 'backwards' from the light source (i.e. positive depth value on the y-axis) with irradiance equal to or greater than the threshold value.
- **Lateral spread** is the maximum distance 'sideways' from the light source (i.e. distance on the x-axis) with irradiance equal to or greater than the threshold value. The full extent of lateral spread is twice this value.
