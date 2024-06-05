# Overview
This app uses RShiny to visualise contour plots from my paper [PAPER NAME]

An online version is hosted at [LINK] and can be used directly in a web browser. 

This repository contains the files and instructions to run the app locally. It requires R to be installed on your system, and the shiny, bslib, and R.matlab packages.

# Setup Instructions - Windows

1) If you don't have R, go to https://cran.r-project.org/mirrors.html and choose an appropriate mirror to download from. The app was developed on R version 4.4.0, it may work on different versions but I haven't tested it.
2) Create a folder somewhere to use as your R working directory. E.g. C:\Users\qboesley\Documents\RShinyApps
3) Inside this folder, create a folder named "OptoContourViewer". C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer
4) Inside this folder, create a folder named "data". C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer\data
5) Download the files "app.r", "grey.mat", and "white.mat"
  - Put "app.r" in the "OptoContourViewer" folder: C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer\app.r
  - Put "grey.mat" and "white.mat" in the "data" folder: C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer\data\grey.mat & C:\Users\qboesley\Documents\RShinyApps\OptoContourViewer\data\white.mat
6) Run R and change your working directory to the folder "RShinyApps" you made in step 2, by clicking on "File" in the toolbar and choosing "Change dir...".
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
  
# Using the App

![image](https://github.com/qboesley/optical-irradiance-contour-viewer/assets/127060519/cf03471a-7c12-40cd-8b0d-93b6e5eba49e)
![image](https://github.com/qboesley/optical-irradiance-contour-viewer/assets/127060519/cf03471a-7c12-40cd-8b0d-93b6e5eba49e)
