# Load packages ----
library(bslib)
library(jsonlite)
library(R.matlab)
library(shiny)
library(tibble)

current_dir <- getwd()
grey_mat_file = file.path(current_dir, "data/grey.mat")
white_mat_file = file.path(current_dir, "data/white.mat")
version_json_file = file.path(current_dir, "data/version.json")
stored_data <- list(
  grey_mat_exists = file.exists(grey_mat_file),
  white_mat_exists = file.exists(white_mat_file),
  version_json_exists = file.exists(version_json_file)
)

# load slice data for plots
if (stored_data$grey_mat_exists) {
  g <- readMat(grey_mat_file)
} else {
  g <- readMat(url("https://brainstimulation.github.io/optical-irradiance-contour-webapp/data/grey.mat"))
}
if (stored_data$white_mat_exists) {
  w <- readMat("data/white.mat")
} else {
  w <- readMat(url("https://brainstimulation.github.io/optical-irradiance-contour-webapp/data/white.mat"))
}

source_map <- tribble(
        ~label,            ~index, ~filename_str,   ~coordsx1, ~coordsx2,
  #-----------------------|------|---------------|------------|--------------
  "LED (20 µm)",            5,      "L-0020",       -0.010,     0.010,
  "LED (50 µm)",            6,      "L-0050",       -0.025,     0.025,
  "LED (100 µm)",           7,      "L-0100",       -0.050,     0.050,
  "LED (200 µm)",           8,      "L-0200",       -0.100,     0.100,
  "LED (500 µm)",           9,      "L-0500",       -0.250,     0.250,
  "LED (1000 µm)",          10,     "L-1000",       -0.500,     0.500,
  "OF (25 µm, NA 0.66)",    11,     "F-0025-66",    -0.0125,    0.0125,
  "OF (50 µm, NA 0.22)",    12,     "F-0050-22",    -0.025,     0.025,
  "OF (100 µm, NA 0.22)",   13,     "F-0100-22",    -0.050,     0.050,
  "OF (100 µm, NA 0.37)",   14,     "F-0100-37",    -0.050,     0.050,
  "OF (200 µm, NA 0.22)",   15,     "F-0200-22",    -0.100,     0.100,
  "OF (200 µm, NA 0.37)",   16,     "F-0200-37",    -0.100,     0.100,
  "OF (200 µm, NA 0.50)",   17,     "F-0200-50",    -0.100,     0.100,
  "OF (400 µm, NA 0.50)",   18,     "F-0400-50",    -0.200,     0.200,
  "OF (600 µm, NA 0.22)",   19,     "F-0600-22",    -0.300,     0.300,
  "OF (600 µm, NA 0.37)",   20,     "F-0600-37",    -0.300,     0.300
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
  title = "Optogenetics Contour Visualiser App [PAPER TITLE, CITATION INFO, & DOI HERE]",
# SIDEBAR WITH INPUT OPTIONS
  sidebar = sidebar(
    title = "Input Options",
# Tissue type radio buttons
    radioButtons(
      "tissue",
      "Tissue Type",
      choices = list("Grey matter", "White matter"),
      selected = "Grey matter"
    ),
# Wavelength radio buttons
    radioButtons(
      "wavelength",
      "Wavelength (nm)",
      choices = list("480", "580", "640"),
      selected = "480"
    ),
# source selector
    selectInput(
      "source",
      "Light Source",
      choices = source_map$label,
      selected = "OF (200 µm, NA 0.37)",
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
# irradiance slice slider
    sliderInput(
      "irrslider",
      "Irradiance Plot Location",
      min = -1,
      max = 1,
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
server <- function(input, output) {
  # colour of contour line
  pcolour <- reactive({
    req(input$wavelength)
    colour <- switch(
      input$wavelength,
      "480" = "blue",
    #accurate colour is #00d5ff
      "580" = "green",
    #note in reality it is yellow #ffff00
      "640" = "#ff2100" #accurate to 640 nm
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
        "580" = 0.19,
        "640" = 0.09
      )
    } else {
      val <- switch(
        input$wavelength,
        "480" = 0.37,
        "580" = 0.19,
        "640" = 0.05
      )
    }
  })

  # index to access from data arrays
  sliceIndex <- reactive({ 
    req(input$wavelength, input$tissue, !is.null(selected_source_data()))
    # source index
    si <- selected_source_data()$index
    
    # wavelength index
    wi <- switch(
      input$wavelength,
      "480" = 1,
      "580" = 2,
      "640" = 3
    )
    # tissue index
    ti <- switch(input$tissue,
                 "White matter" = 1,
                 "Grey matter" = 2)

    # slice index
    sliceIndex = (wi - 1) * 20 + si
  })

  # array of data for contour and irradiance plots
  sliceData <- reactive({
    req(input$tissue, input$power, w$white, g$grey, sliceIndex, ua)
    arr <- switch(
      input$tissue,
      "White matter" = (input$power * w$white[,, sliceIndex()]) / (ua() * 0.000001),
      "Grey matter" = (input$power * g$grey[,, sliceIndex()]) / (ua() * 0.000001)
    )
  })

  # summary data calculation for contour plot
  cData <- reactive({ 
    req(input$threshold)  
      # max irradiance
      dmax <- max(sliceData())
      # volume over threshold
      mask <- sliceData() >= input$threshold
      vol <- 0
      cs = colSums(mask)
      dr = 0.01
      for(i in (cs)){
        vol <- vol + dr * (pi * (dr * i * 0.5)^2)
      }
      # spread distance
      rs = rowSums(mask)
      cfirst <- min(which(cs > 0))
      clast <- max(which(cs > 0))
      rfirst <- min(which(rs > 0))
      fspread <- (clast - 100) * dr #forward spread
      bspread <- (101 - cfirst) * dr #backward spread
      if(bspread < 0){
        bspread <- 0
      }
      lspread <- (101 - rfirst) * dr #lateral spread
      # output as vector
      cData <- c(dmax, vol, fspread, bspread, lspread)
    })
  
  # data for filenames of downloaded plot figures
  fnameData <- reactive({
    req(input$tissue, input$power, input$threshold, input$irrslider, input$wavelength,
        !is.null(input$drawgridlines), !is.null(input$drawirrsliceline), !is.null(input$irrslicelogplot),
        !is.null(selected_source_data()))
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
      slice_location = gsub("\\.", "-", sprintf("%.2f", input$irrslider)),
      log_lin = ifelse(input$irrslicelogplot, "LOG", "LIN")
    )
  })
    
  #CONTOUR PLOT - Function, for generating contour plot
  draw_contour <- function(threshold, source, tissue, wavelength, power, drawgridlines, drawirrsliceline, irrslider){
    req(threshold, source, tissue, wavelength, power, irrslider, !is.null(drawgridlines), !is.null(drawirrsliceline), !is.null(selected_source_data()))
    # contour plot
      contour(
        seq(-1, 1, length.out = 200),
        seq(-1, 1, length.out = 200),
        sliceData(),
        col = pcolour(),
        levels = threshold,
        drawlabels = FALSE,
        xlim = c(-1, 1), 
        ylim = c(-1, 1)
      )
      # plot title
      title(
        main = sprintf("%s in %s @ %s nm\nPower: %.2f mW - Threshold: %.2f mW/mm^2", source, tissue, wavelength, power, threshold),
        xlab = "Lateral spread (mm)",
        ylab = "Depth (mm)",
        sub = sprintf("Max Irradiance = %.2f mW/mm^2    Vol. Illuminated = %.3f mm^3", cData()[1], cData()[2])
      )
      par(new=TRUE) # keep contour visible while other lines are overlaid
      # grid lines
      if(drawgridlines){
        abline(v=(seq(-1, 1, length.out = 21)), col = 'lightgray', lty = 'dotted')
        abline(h=(seq(-1, 1, length.out = 21)), col = 'lightgray', lty = 'dotted')
        abline(v=(seq(-1, 1, length.out = 5)), col = 'lightgray')
        abline(h=(seq(-1, 1, length.out = 5)), col = 'lightgray')
      }
      # draw source on contour plot
      lines(
        c(selected_source_data()$coordsx1, selected_source_data()$coordsx2), 
        c(0, 0), 
        xlim = c(-1, 1), 
        ylim = c(-1, 1)
      )
      # draw irradiance slice location
      if(drawirrsliceline == TRUE){
        lines(
          c(irrslider, irrslider), 
          c(-1, 1), 
          #xlim = c(-1, 1), 
          #ylim = c(-1, 1),
          col = 'purple')
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
        irrslider = input$irrslider)
    }
  )
  
  # IRRADIANCE LINE PLOT - function
  draw_irr <- function(irrslider, irrslicelogplot, threshold, drawgridlines){
    req(irrslider, threshold, !is.null(irrslicelogplot), !is.null(drawgridlines))
    sindex <- (irrslider + 1)*100 + 1
    if(sindex > 200){
      sindex <- 200
    }
    lineData <- sliceData()[sindex,]
    
    plot_args <- list(
      x = lineData,
      y = seq(-1, 1, length.out = 200),
      main = "Irradiance as Function of Depth",
      ylab = "Depth (mm)",
      col = 'purple'
    )
    if (irrslicelogplot) {
      plot_args$xlab <- "Log10 Irradiance (mW/mm^2)"
      plot_args$log <- 'x'
    } else {
      plot_args$xlab <- "Irradiance (mW/mm^2)"
    }
    
    do.call(plot, plot_args)

    lines( # irradiance threshold
      c(threshold, threshold),
      c(-1, 1),
      xlim = c(-1, 1),
      ylim = c(-1, 1),
      lty = 'dotted'
    )
    par(new = TRUE)
    # grid lines
    if (drawgridlines == TRUE) {
      abline(h = (seq(-1, 1, length.out = 21)),
             col = 'lightgray',
             lty = 'dotted')
      abline(h = (seq(-1, 1, length.out = 5)), col = 'lightgray')
    }
  }

  # IRRADIANCE LINE PLOT - renderPlot
  output$irrplotdata <- renderPlot({
      draw_irr(
        irrslider = input$irrslider,
        irrslicelogplot = input$irrslicelogplot,
        threshold = input$threshold,
        drawgridlines = input$drawgridlines
      )
    }
  )
    
  # DOWNLOAD CONTOUR PLOT
  output$downloadcontourplot <- downloadHandler(
    filename = function(){
      data <- fnameData()
      sprintf("%s_%s_%snm_P%s_T%s_G%s_S%s_X%smm_CONTOUR.png", data$source, data$tissue, data$wavelength, data$power,
              data$threshold, data$gridlines, data$slice_line, data$slice_location)
    },
    content = function(file) {
      png(file, width = 1024, height = 1024, units = "px")
      draw_contour(input$threshold, input$source, input$tissue, input$wavelength, input$power, input$drawgridlines, input$drawirrsliceline, input$irrslider)
      dev.off()
    }
  )

  # DOWNLOAD IRRADIANCE PLOT
  output$downloadirrplot <- downloadHandler(
    filename = function(){
      data <- fnameData()
      sprintf("%s_%s_%snm_P%s_T%s_G%s_S%s_X%smm_IRRADIANCE_%s.png", data$source, data$tissue, data$wavelength, data$power,
              data$threshold, data$gridlines, data$slice_line, data$slice_location, data$log_lin)
    },
    content = function(file){
        png(file, width = 1024, height = 1024, units = "px")
        draw_irr(
          irrslider = input$irrslider,
          irrslicelogplot = input$irrslicelogplot,
          threshold = input$threshold,
          drawgridlines = input$drawgridlines
        )
        dev.off()
      }
  )

  #SUMMARY DATA
  output$tdata <- renderText({
    # display data on app card
    str_irr <- sprintf("Max Irradiance:\t  %.2f mW/mm^2", cData()[1])
    str_volume <- sprintf("\nVol. illuminated: %.3f mm^3", cData()[2])
    str_fspread <- sprintf("\nForward spread:\t  %.2f mm", cData()[3])
    str_bspread <- sprintf("\nBackward spread:  %.2f mm", cData()[4])
    str_lspread <- sprintf("\nLateral spread:\t  %.2f mm", cData()[5])
    paste(str_irr, str_volume, str_fspread, str_bspread, str_lspread)
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
