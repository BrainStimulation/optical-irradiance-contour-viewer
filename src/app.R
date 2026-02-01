# Load packages ----
library(bslib)
library(jsonlite)
library(R.matlab)
library(shiny)
library(shinyjs)
library(tibble)

# FUNCTION TO CHECK SPREAD OF LIGHT
SequenceIndices <- function(vec){
  if(identical(which(vec>0), integer(0))){
    result <- "NA"
    return(result)
  }else{
    
    start_pad <- 0
    if(vec[1]==1){
      vec <- c(0, vec)
      start_pad <- 1
    }
    if(vec[length(vec)]==1){
      vec <- c(vec, 0)
    }
    
    # numerically differentiate vector to find start and end locations of each sequence of 1s
    dv <- diff(vec)
    seq_start <- which(dv==1) + 1
    seq_end <- which(dv==-1)
    seq_length <- seq_end - seq_start + 1
    i_max <- which(seq_length==max(seq_length))
    if(length(i_max)>1){
      i_max <- i_max[1]
    }
    
    # calculate start and end indices of longest sequence
    if(start_pad==1){
      i_start <- seq_start[i_max] - 1
      i_end <- seq_end[i_max] - 1
    }else{
      i_start <- seq_start[i_max]
      i_end <- seq_end[i_max]
    }
    result <- list("i0" = i_start, "i1" = i_end) 
    return(result)
  }
}

# FUNCTION TO MAP SPREAD INDICES TO PHYSICAL DISTANCES
MapIndexToDistance <- function(index){
  if(index<=150){
    dist <- (index-1)/100 - 1.5
  }else{
    dist <- index/100 - 1.5
  }
  return(dist)
}

# find local .mat files
current_dir <- getwd()
version_json_file = file.path(current_dir, "data/version.json")
data_mat_file = file.path(current_dir, "data/absorptionData.mat")
stored_data <- list(
  version_json_exists = file.exists(version_json_file),
  data_mat_exists = file.exists(data_mat_file)
)

# load slice data for plots
if (stored_data$data_mat_exists){
  d <- readMat("data/absorptionData.mat")
} else {
  d <- readMat(url("https://brainstimulation.github.io/optical-irradiance-contour-webapp/data/absorptionData.mat")) # need to upload the new data .mat file, it's large though ~88 MB
}
d <- d$testStruct[,,1]

# set up tibble for tidy access of parameters
source_map <- tribble(
  ~label,        ~index, ~filename_str, ~wx,    ~wy,
  #-------------|-------|---------------|------|------|
  "CREE EZ280",  1,      "CREE-EZ280",   0.235, 0.235,
  "CREE EZ400",  2,      "CREE-EZ400",   0.350, 0.350,
  "CREE EZ500",  3,      "CREE-EZ500",   0.450, 0.450,
  "CREE EZ700",  4,      "CREE-EZ700",   0.650, 0.650,
  "CREE EZ950",  5,      "CREE-EZ950",   0.900, 0.900,
  "CREE EZ1350", 6,      "CREE-EZ1350",  1.300, 1.300,
  "CREE EZ1950", 7,      "CREE-EZ1950",  1.900, 1.900,
  "CREE EZ7591", 8,      "CREE-EZ7591",  0.700, 0.700,
  "CREE EZ1012", 9,      "CREE-EZ1012",  0.990, 0.990,
  "CREE EZ1614", 10,     "CREE-EZ1614",  1.555, 1.190,
  "CREE SA1438", 11,     "CREE-SA1438",  0.360, 0.120,
  "CREE SA3280", 12,     "CREE-SA3280",  0.780, 0.300,
  "CREE SA1000", 13,     "CREE-SA1000",  0.960, 0.960,
  "CREE SA1150", 14,     "CREE-SA1150",  1.120, 1.120,
  "CREE SR1321", 15,     "CREE-SR1321",  0.190, 0.110,
  "CREE SR2025", 16,     "CREE-SR2025",  0.230, 0.180,
  "CREE SR2130", 17,     "CREE-SR2130",  0.285, 0.185,
  "CREE SR5283", 18,     "CREE-SR5283",  0.820, 0.515,
  "CREE SR6492", 19,     "CREE-SR6492",  0.885, 0.605,
  "CREE SR260",  20,     "CREE-SR260",   0.240, 0.240,
  "CREE SR370",  21,     "CREE-SR370",   0.350, 0.350,
  "CREE SR550",  22,     "CREE-SR550",   0.530, 0.530,
  "CREE TR2227", 23,     "CREE-TR2227",  0.230, 0.190
)

# Workaround for Chromium download bug in shinylive
# By default the downloadButton() function creates an <a> tag with a download 
# attribute to suggest the filename.Chromium browsers ignore this. By modifying
# the downloadButton here, plots can be downloaded with the correct filename
# (determined by reactive variable values) and filetype (.png).
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# User interface ----
ui <- page_sidebar(
  useShinyjs(),
  title = "Optogenetics Contour Visualiser App [PAPER TITLE, CITATION INFO, & DOI HERE]",
  # SIDEBAR WITH INPUT OPTIONS
  sidebar = sidebar(
    # title = "Input Options",
    # Tissue type radio buttons
    selectInput(
      "tissue",
      "Tissue Type",
      choices = list("Grey matter", "White matter"),
      selected = "Grey matter",
      multiple = FALSE
    ),
    # Wavelength radio buttons
    selectInput(
      "wavelength",
      "Wavelength (nm)",
      choices = list("480", "560", "580", "640"),
      selected = "480",
      multiple = FALSE
    ),
    # source selector
    selectInput(
      "source",
      "Light Source",
      choices = source_map$label,
      selected = "CREE TR2227",
      multiple = FALSE
    ),
    # plot plane selector
    selectInput(
      "plotPlane",
      "Plane for Contour Plot",
      choices = list("XZ", "YZ"),
      selected = "XZ",
      multiple = FALSE
    ),
    # irradiance orientation selector
    selectInput(
      "irrOrientation",
      "View for Irradiance Plot",
      choices = list("vertical", "horizontal"),
      selected = "vertical",
      multiple = FALSE
    ),
    # set power
    numericInput(
      "power",
      "Total Optical Power (mW)",
      value = 1,
      min = 0,
      step = 0.1
    ),
    # set threshold
    numericInput(
      "threshold",
      HTML("Threshold Irradiance<br/>(mW/mm^2)"),
      value = 1,
      min = 0,
      step = 0.1
    ),
    # gridline checkbox
    checkboxInput("drawgridlines", "Show Gridlines", value = TRUE),
    # irradiance slice on contour plot
    checkboxInput("drawirrsliceline", "Show Irradiance Plot Location", value = TRUE),
    # log plot for irradiance slice
    checkboxInput("irrslicelogplot", "Logarithmic Irradiance Plot", value = FALSE),
    # irradiance slice slider - vertical line through Z
    sliderInput(
      "irrsliderZ",
      "Irradiance Plot Location - Z line",
      min = -1.5,
      max = 1.5,
      value = 0,
      step = 0.01,
      ticks = FALSE
    ),
    sliderInput(
      "irrsliderX",
      "Irradiance Plot Location - X/Y line",
      min = -1.5,
      max = 1.5,
      value = 0,
      step = 0.01,
      ticks = FALSE
    ),
    # plot download buttons
    downloadButton("downloadcontourplot", "Download Contour Plot"),
    downloadButton("downloadirrplot", "Download Irradiance Plot")
  ),
  # plot and data cards
  layout_columns(
    card(card_body(
      plotOutput("pdata", width = "100%", height = "500")
    )),
    card(card_body(plotOutput("irrplotdata"), height = "500")),
    # col_widths = c(7, 5)
  ),
  # summary data
  card(
    card_body(verbatimTextOutput("tdata"))
  ),
  card(
    card_body(verbatimTextOutput("version_info"))
  )
)
# Server logic
server <- function(input, output, session) { # added session argument, may affect web hosting?
  # grey out YZ plane selection when symmetric light source is selected
  observeEvent(input$source, {
    thisRow = source_map[source_map$label == input$source, ]
    if (thisRow$wx == thisRow$wy){
      updateSelectInput(session = session, "plotPlane", selected = "XZ")
      disable("plotPlane")
    } else {
      enable("plotPlane")
    }
  })
  
  # colour of contour line
  pcolour <- reactive({
    req(input$wavelength)
    colour <- switch(
      input$wavelength,
      "480" = "#00d5ff",
      "560" = "#066317", # real colour is "#c3ff00",
      "580" = "#30d110", # real colour is "#ffff00",
      "640" = "#ff2100"
      #https://academo.org/demos/wavelength-to-colour-relationship/
    )
  })
  
  selected_source_data <- reactive({
    req(input$source)
    # Filter the map to find the matching row
    source_map[source_map$label == input$source, ]
  })
  
  #absorption coefficient
  ua <- reactive({
    req(input$tissue, input$wavelength)
    # ua
    if (input$tissue == "White matter") {
      val <- switch(
        input$wavelength,
        "480" = 0.35,
        "560" = 0.21,
        "580" = 0.19,
        "640" = 0.09
      )
    } else {
      val <- switch(
        input$wavelength,
        "480" = 0.37,
        "560" = 0.26,
        "580" = 0.19,
        "640" = 0.05
      )
    }
  })
  
  # extract data slice for plotting
  sliceData <- reactive({
    req(input$source, input$tissue, input$power, input$plotPlane, d, ua)
    # set fields for accessing data from imported matlab struct
    thisTissue <- switch(
      input$tissue,
      "White matter" = paste("W", input$wavelength, sep=""),
      "Grey matter" = paste("G", input$wavelength, sep="")
    )
    thisSource <- gsub(" ", ".", input$source)
    
    # check if selected source is symmetric or not, force plotPlane to XZ for symmetric sources
    thisRow = source_map[source_map$label == input$source, ]
    if (thisRow$wx == thisRow$wy){
      thisPlane = "XZ"
    } else {
      thisPlane = input$plotPlane
    }
    
    # imported data has already been scaled to PDF/mm^3, later is scaled by input$power
    arr <- d[[thisTissue]][,,1][[thisSource]][,,1][[thisPlane]]
    
    # mirror and concatenate slice arr for full plane of slice data
    sliceData = input$power * t(cbind(t(arr[c(nrow(arr):1),]), t(arr)))
  })
  
  # summary data calculation for contour plot
  cData <- reactive({ 
    req(input$threshold, input$irrsliderZ, input$irrsliderX)
    dr = 0.01
    # max irradiance
    dmax <- max(sliceData())
    
    # get data index of line through slice from irradiance plot location
    if(input$irrsliderZ == 1.5){
      vli = 300
    }else{
      vli = (input$irrsliderZ + 1.5)*100 + 1
    }
    
    if(input$irrsliderX == 1.5){
      hli = 300
    }else{
      hli = (input$irrsliderX + 1.5)*100 + 1
    }
    
    hline <- sliceData()[1:300, hli]
    vline <- sliceData()[vli, 1:300]
    dloc <- sliceData()[vli, hli]
    
    hmask <- hline >= input$threshold
    vmask <- vline >= input$threshold
    
    # indices showing edges of regions definitely under threshold
    ut_t2b_diff_vec <- diff(which(vmask==0)) # top to bottom
    ut_l2r_diff_vec <- diff(which(hmask==0)) # left to right
    ut_i_t2b <- which(ut_t2b_diff_vec!=1)[1] # region is vline[1:ut_i_t2b]
    ut_i_b2t <- 300 - (length(ut_t2b_diff_vec) - which(ut_t2b_diff_vec!=1)[length(which(ut_t2b_diff_vec!=1))]) # region is vline[ut_i_b2t:300]
    ut_i_l2r <- which(ut_l2r_diff_vec!=1)[1] # region is hline[1:ut_i_l2r]
    ut_i_r2l <- 300 - (length(ut_l2r_diff_vec) - which(ut_l2r_diff_vec!=1)[length(which(ut_l2r_diff_vec!=1))])# region is hline[ut_i_rl2:300]
    
    # indices showing edges of regions definitely over threshold
    h_in <- SequenceIndices(hmask)
    v_in <- SequenceIndices(vmask)
    
    # define certainty regions for spread (check indexing, might be out by 1)
    # certain over threshold
    if(h_in[1] == "NA"){
      h_spread_ot <- 0
      h_spread_ut <- 0
    }else{
      h_spread_ot <- MapIndexToDistance(h_in$i1)
      h_spread_ut <- MapIndexToDistance(ut_i_r2l)
    }
    
    if(v_in[1] == "NA"){
      f_spread_ot<- 0
      b_spread_ot <- 0
      f_spread_ut <- 0
      b_spread_ut <- 0
    }else{
      f_spread_ot<- MapIndexToDistance(v_in$i1)
      b_spread_ot <- MapIndexToDistance(v_in$i0)
      f_spread_ut <- MapIndexToDistance(ut_i_b2t)
      b_spread_ut <- MapIndexToDistance(ut_i_t2b)
    }
    
    # output as vector
    cData <- c(dmax, dloc, f_spread_ot, f_spread_ut, b_spread_ot, b_spread_ut, h_spread_ot, h_spread_ut)
  })
  
  # data for filenames of downloaded plot figures
  fnameData <- reactive({
    req(input$tissue, input$power, input$threshold, input$irrsliderZ, input$wavelength,
        !is.null(input$drawgridlines), !is.null(input$drawirrsliceline), !is.null(input$irrslicelogplot),
        !is.null(selected_source_data()), input$plotPlane)
    list(
      source = selected_source_data()$filename_str,
      tissue = switch(
        input$tissue,
        "White matter" = "W",
        "Grey matter" = "G"
      ),
      wavelength = input$wavelength,
      power = gsub("\\.", "-", sprintf("%.2f", input$power)),
      threshold = gsub("\\.", "-", sprintf("%.2f", input$threshold)),
      gridlines = ifelse(input$drawgridlines, "on", "off"),
      slice_line = ifelse(input$drawirrsliceline, "on", "off"),
      slice_location_Z = gsub("\\.", "-", sprintf("%.2f", input$irrsliderZ)),
      slice_location_XY = gsub("\\.", "-", sprintf("%.2f",input$irrsliderX)),
      log_lin = ifelse(input$irrslicelogplot, "LOG", "LIN"),
      fname_plane = input$plotPlane
    )
  })
  
  #CONTOUR PLOT - Function, for generating contour plot
  draw_contour <- function(threshold, source, tissue, wavelength, power, drawgridlines, drawirrsliceline, irrsliderZ, irrsliderX, plotPlane){
    req(threshold, source, tissue, wavelength, power, irrsliderZ, irrsliderX, plotPlane, !is.null(drawgridlines), !is.null(drawirrsliceline), !is.null(selected_source_data()))
    # contour plot
    contour(
      seq(-1.5, 1.5, length.out = 300),
      seq(-1.5, 1.5, length.out = 300),
      sliceData(),
      col = pcolour(),
      levels = threshold,
      drawlabels = FALSE,
      xlim = c(-1.5, 1.5),
      ylim = c(-1.5, 1.5)
    )
    # plot title and axis labels
    if(plotPlane == "YZ"){
      xAxisLabel <- "Lateral spread in Y (mm)"
    }else{
      xAxisLabel <- "Lateral spread in X (mm)"
    }
    title(
      main = sprintf("%s in %s @ %s nm (%s Plane)\nPower: %.2f mW - Threshold: %.2f mW/mm^2", source, tissue, wavelength, plotPlane, power, threshold),
      xlab = xAxisLabel,
      ylab = "Depth in Z (mm)",
      sub = sprintf("Max Irradiance = %.2f mW/mm^2    Vol. Illuminated = %.3f mm^3", cData()[1], cData()[2])
    )
    par(new=TRUE) # keep contour visible while other lines are overlaid
    # # grid lines
    if(drawgridlines){
      abline(v=(seq(-1.5, 1.5, length.out = 31)), col = 'lightgray', lty = 'dotted')
      abline(h=(seq(-1.5, 1.5, length.out = 31)), col = 'lightgray', lty = 'dotted')
      abline(v=(seq(-1.5, 1.5, length.out = 7)), col = 'lightgray')
      abline(h=(seq(-1.5, 1.5, length.out = 7)), col = 'lightgray')
    }
    # draw source on contour plot
    if(input$plotPlane == "YZ"){
      sourceWidth <- selected_source_data()$wy
    } else{
      sourceWidth <- selected_source_data()$wx
    }
    lines(
      c(-0.5*sourceWidth, 0.5*sourceWidth),
      c(0, 0),
      xlim = c(-1.5, 1.5),
      ylim = c(-1.5, 1.5)
    )
    # draw irradiance slice location
    if(drawirrsliceline == TRUE){
      lines(
        c(irrsliderZ, irrsliderZ),
        c(-1.5, 1.5),
        #xlim = c(-1, 1),
        #ylim = c(-1, 1),
        col = 'purple')
      lines(
        c(-1.5, 1.5),
        c(irrsliderX, irrsliderX),
        col='orange'
      )
    }
  }
  
  #CONTOUR PLOT - renderPlot call to display in app
  output$pdata <- renderPlot({
    draw_contour(
      threshold = input$threshold,
      source = input$source,
      tissue = input$tissue,
      wavelength = input$wavelength,
      power = input$power,
      drawgridlines = input$drawgridlines,
      drawirrsliceline = input$drawirrsliceline,
      irrsliderZ = input$irrsliderZ,
      irrsliderX = input$irrsliderX,
      plotPlane = input$plotPlane)
  }, height = 500, width = 500)
  
  # IRRADIANCE LINE PLOT - function
  draw_irr <- function(irrsliderZ, irrsliderX, irrslicelogplot, threshold, drawgridlines, plotPlane, irrOrientation, source, tissue, wavelength, power){
    req(irrsliderZ, threshold, !is.null(irrslicelogplot), !is.null(drawgridlines))
    
    if(irrOrientation == "vertical"){
      sindex <- (irrsliderZ + 1.5)*100 + 1
      if(sindex > 300){sindex <- 300}
      lineData <- sliceData()[sindex,]
      xplt <- lineData
      yplt <- seq(-1.5, 1.5, length.out = 300)
      xlstr <- "Irradiance (mW/mm^2)"
      ylstr <- "Depth in Z (mm)"
      colstr <- 'purple'
      coord_str <- substr(plotPlane, 1, 1)
      coord_val <- irrsliderZ
    }else{ # orientation == "horizontal"
      sindex <- (irrsliderX + 1.5)*100 + 1
      if(sindex > 300){sindex <- 300}
      lineData <- sliceData()[,sindex]
      xplt <- seq(-1.5, 1.5, length.out = 300)
      yplt <- lineData
      xlstr <- "Lateral Position (mm)"
      ylstr <- "Irradiance (mW/mm^2)"
      colstr <- 'orange'
      coord_str <- "Z"
      coord_val <- irrsliderX
    }
    
    plot_args <- list(
      x = xplt,
      y = yplt,
      main = sprintf("%s in %s @ %s nm (%s Plane)\nPower: %.2f mW - Threshold: %.2f mW/mm^2\n%s Coordinate: %.2f mm", source, tissue, wavelength, plotPlane, power, threshold, coord_str, coord_val),
      xlab = xlstr,
      ylab = ylstr,
      col = colstr
    )
    
    # change labels for log plot
    if(irrslicelogplot){
      if(irrOrientation == "vertical"){
        plot_args$xlab <- "Log10 Irradiance (mW/mm^2)"
        plot_args$log <- 'x'
      }else{
        plot_args$ylab <- "Log10 Irradiance (mW/mm^2)"
        plot_args$log <- 'y'
      }
    }else{
      if(irrOrientation == "vertical"){
        plot_args$xlab <- "Irradiance (mW/mm^2)"
      }else{
        plot_args$ylab <- "Irradiance (mW/mm^2)"
      }
    }
    do.call(plot, plot_args)
    
    # threshold lines
    if(irrOrientation=="vertical"){
      lines( # irradiance threshold
        c(threshold, threshold),
        c(-1.5, 1.5),
        # xlim = c(-1.5, 1.5),
        ylim = c(-1.5, 1.5),
        lty = 'dotted'
    )}else{
      lines(
        c(-1.5, 1.5),
        c(threshold, threshold),
        xlim = c(-1.5, 1.5),
        lty = 'dotted'
      )
    }
    
    par(new = TRUE)
    # grid lines
    if (drawgridlines == TRUE) {
      if(irrOrientation=="vertical"){
        abline(h = (seq(-1.5, 1.5, length.out = 31)),col = 'lightgray',lty = 'dotted')
        abline(h = (seq(-1.5, 1.5, length.out = 7)), col = 'lightgray')
      }else{
        abline(v = (seq(-1.5, 1.5, length.out = 31)),col = 'lightgray',lty = 'dotted')
        abline(v = (seq(-1.5, 1.5, length.out = 7)), col = 'lightgray')
      }
    }
  }
  
  # IRRADIANCE LINE PLOT - renderPlot
  output$irrplotdata <- renderPlot({
    draw_irr(
      irrsliderZ = input$irrsliderZ,
      irrsliderX = input$irrsliderX,
      irrslicelogplot = input$irrslicelogplot,
      threshold = input$threshold,
      drawgridlines = input$drawgridlines,
      plotPlane = input$plotPlane,
      irrOrientation = input$irrOrientation,
      source = input$source, 
      tissue = input$tissue, 
      wavelength = input$wavelength, 
      power = input$power
    )
  }, height = 500, width = 500)

  # DOWNLOAD CONTOUR PLOT
  output$downloadcontourplot <- downloadHandler(
    filename = function(){
      data <- fnameData()
      fname <- sprintf("%s_%s_%snm_P%s_T%s_G%s_S%s_%s%smm_Z%smm_CONTOUR.png", data$source, data$tissue, data$wavelength, data$power,
                       data$threshold, data$gridlines, data$slice_line, substr(input$plotPlane, 1, 1), data$slice_location_Z, data$slice_location_XY)
      return(fname)
    },
    content = function(file) {
      png(file, width = 500, height = 500, units = "px")
      draw_contour(input$threshold, input$source, input$tissue, input$wavelength, input$power, input$drawgridlines, input$drawirrsliceline, input$irrsliderZ, input$irrsliderX, input$plotPlane)
      dev.off()
    }
  )
  
  # DOWNLOAD IRRADIANCE PLOT
  output$downloadirrplot <- downloadHandler(
    filename = function(){
      data <- fnameData()
      
      if(input$irrOrientation=='vertical'){
        fname <- sprintf("%s_%s_%snm_%s-plane_P%s_T%s_G%s_S%s_%s%smm_IRRADIANCE_%s.png", data$source, data$tissue, data$wavelength, input$plotPlane, data$power,
                data$threshold, data$gridlines, data$slice_line, substr(input$plotPlane, 1, 1), data$slice_location_Z, data$log_lin)
      }else{
        fname <- sprintf("%s_%s_%snm_%s-plane_P%s_T%s_G%s_S%s_Z%smm_IRRADIANCE_%s.png", data$source, data$tissue, data$wavelength, input$plotPlane, data$power,
                         data$threshold, data$gridlines, data$slice_line, data$slice_location_XY, data$log_lin)
      }
      return(fname)
      
    },
    content = function(file){
      png(file, width = 500, height = 500, units = "px")
      draw_irr(
        irrsliderZ = input$irrsliderZ,
        irrsliderX = input$irrsliderX,
        irrslicelogplot = input$irrslicelogplot,
        threshold = input$threshold,
        drawgridlines = input$drawgridlines,
        plotPlane = input$plotPlane,
        irrOrientation = input$irrOrientation,
        source = input$source, 
        tissue = input$tissue, 
        wavelength = input$wavelength, 
        power = input$power
      )
      dev.off()
    }
  )
  
  #SUMMARY DATA
  output$tdata <- renderText({
    # display data on app card
    str_irr_max <- sprintf("Max Irradiance:\t\t%.3f mW/mm^2", cData()[1])
    str_irr_loc <- sprintf("\nIrr. at crosshair:\t%.3f mW/mm^2", cData()[2])
    str_fspread <- sprintf("\nFront of Region:\t%.2f mm to %.2f mm", cData()[3], cData()[4])
    str_bspread <- sprintf("\nBack of Region:\t\t%.2f mm to %.2f mm", cData()[5], cData()[6])
    str_lspread <- sprintf("\nLateral spread:\t\t%.2f mm to %.2f mm", cData()[7], cData()[8])
    paste(str_irr_max, str_irr_loc, str_fspread, str_bspread, str_lspread)
  })
  
  # RENDER VERSION INFO
  output$version_info <- renderText({
    # Initialize default values
    build_date <- "local"
    git_sha <- "dev"
    
    # Read version info from the JSON file
    # The 'try' block prevents errors if the file doesn't exist during local dev
    try({
      if (stored_data$version_json_exists) {
        version_file = version_json_file
      } else {
        version_file = "version.json"
        download.file("https://brainstimulation.github.io/optical-irradiance-contour-webapp/data/version.json", version_file)
      }
      version_data <- read_json(version_file)
      build_date <- version_data$build_date
      git_sha <- version_data$git_sha
    }, silent = TRUE)
    
    # Combine them into a single string
    paste("Build:", build_date, "|", git_sha)
  })
  

}

# Run the app
app <- shinyApp(ui = ui, server = server)
