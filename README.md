# Overview
This app uses RShiny to visualise contour plots from my paper [PAPER NAME]

An online version is hosted at [LINK] and can be used directly in a web browser. 

This repository contains the files and instructions to run the app locally. It requires R to be installed on your system, and the shiny, bslib, and R.matlab packages.
  
![image](https://github.com/qboesley/optical-irradiance-contour-viewer/assets/127060519/cf03471a-7c12-40cd-8b0d-93b6e5eba49e)

# Get started

## Prerequisites

- R: you can [download it from a mirror here](https://cran.r-project.org/mirrors.html). Tested on v4.4.0.

## Run

First, download this repository to your local machine into a folder called `optical-irradiance-contour-viewer/`. You can do this in one step by [cloning this repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository).

Next, run the app using one of the following methods.

### Using command line

1. Open your command line or terminal and navigate to the `optical-irradiance-contour-viewer/` directory.
1. Run the following command:

    ```sh
    Rscript run.R
    ```

1. Once it has finished installing packages and loading the app, go to the URL printed in the terminal using any browser. It will look something like `http://127.0.0.1:XXXX`.

### Using RGui

1. Run R, which willl open the R graphical user interface (GUI).
1. `File` > `Change dir...` > select the `optical-irradiance-contour-viewer/` directory.

    ![image](https://github.com/qboesley/optical-irradiance-contour-viewer/assets/127060519/20e75cc2-803b-46dd-bc31-67fac6f5a0d9)

1. Run the following command in the console window:
    
    ```r
    source("run.R")
    ```

To quit, press `ctrl+C` on the RGui console to stop the app, then type `q()` to quit the console.


### Using RStudio

1. Install [RStudio](https://posit.co/downloads/).
1. Open RStudio, and select `File` > `Open Project...`. Select the `optical-irradiance-contour-viewer/` folder and `Open`.
1. In the `Files` navigator, click `run.R` to open it.
1. Click `Run App` (green arrow in top-right corner of top-left quadrant). Once it has finished installing and loading the app, a new window will appear with the viewer.

# Using the App
## Input Options
The sidebar on the left hand side contains all the app controls. 
- **Tissue Type** toggles between the grey matter and white matter simulation datasets.
- **Wavelength (nm)** toggles between the source wavelengths used in the simulations.
  - Note that 580 nm light is actually yellow rather than green, but it is plotted in green here for better visibility.
- **Light Source** is a drop down menu to select particular light sources. There are two main source types:
  - Optical Fibres, abbreviated as OF. The length in brackets refers to the diameter and NA is the numeric aperture.
  - LEDs. These were all modelled as square light sources. The length in brackets refers to the edge length of the square.
- **Total Optical Power (mW)** sets the total power output from the light source. The user can type values in to the box. You can also use the clicker icons, or scroll the mouse wheel to change the value by 0.1 mW at a time.
- **Threshold Irradiance (mW/mm^2)** sets the irradiance value to draw the contour line at. The user can type values in to the box. You can also use the clicker icons, or scroll the mouse wheel to change the value by 0.1 mW at a time.
- **Download Plot** button will save the currently displayed plot as a .png file. The filename is automatically generated.
  - General format: {SourceInfo}\_{TissueType}_{Wavelength}nm_P{SourcePower}_T{ThresholdValue}.png
  - LED example: L_0500_W_480nm_P1-00_T1-00.png
  - Optical fibre example: OF_0200_37_G580nm_P2-30_T1-30.png
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

## Limitations
These are discussed extensively in the paper. A shortened list is given here:
- The max irradiance values calculated for the LED sources below are drastically underestimated. This is because the key dimension of these sources is equal to or less than the resolution of the original model (10 µm).
  - LED (1 µm)
  - LED (2 µm)
  - LED (5 µm)
  - LED (10 µm)
- Higher optical power values and lower irradiance threshold values may result in contours that do not fit within the 2&nbsp;mm&nbsp;x&nbsp;2&nbsp;mm plot region. When this occurs the max irradiance value is still correct, but the volume illuminated calculation will be incorrect. Values for forward, backward, and lateral spread will not increase beyond 1.00 mm either and so the relevant parameters will be incorrect when contours do not fit within the plot region.
- As contour lines get closer to the edge of the plot region, they tend to become more ragged. The reported values for light spread in these cases will overestimate the 'true' spread of light. For example, in the figure below backward spread is reported to be 0.58&nbsp;mm but I would offer approximately 0.4&nbsp;mm as a more resonable estimate in this instance.
  - ![image](https://github.com/qboesley/optical-irradiance-contour-viewer/assets/127060519/62dc05fa-a252-477e-8bf1-ffccccea0158)

