# optical-irradiance-model
This app uses RShiny to visualise contour plots from my paper [PAPER NAME]


Download the files "app.r", "grey.mat", and "white.mat".

Create a folder "OptoContourAppMain" to use as your working directory in R.

app.r:
[WorkingDirectory]\OptoContourApp\app.r

"grey.mat" and "white.mat"
[WorkingDirectory]\OptoContourApp\data\grey.mat
[WorkingDirectory]\OptoContourApp\data\white.mat

Go to https://cran.r-project.org/mirrors.html and choose an appropriate mirror to download from. The app was developed on R version 4.4.0

Open R and input the following commands into the console (only necessary the first time you run R)
install.packages("shiny")
install.packages("bslib")
install.packages("R.matlab")

In R click on "File" in the toolbar then choose "Change dir..." and set the working directory to the folder you created earlier

Run the following commands to start the app:
library(shiny)
runApp("OptoContourApp")

The app will open in your system default web browser. 
