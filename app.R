# Load packages ----
library(shiny)
library(bslib)
library(R.matlab)

# load slice data for plots
g <- readMat("data/grey.mat")
w <- readMat("data/white.mat")

# User interface ----
ui <- page_sidebar(
  title = "Optogenetics Contour Visualiser",
  sidebar = sidebar(
    title = "Options",
    # Tissue type radio buttons
    radioButtons(
      "tissue",
      "Tissue Type",
      choices =
        list("Grey matter", "White matter"),
      selected = "Grey matter"
    ),
    # Wavelength radio buttons
    radioButtons(
      "wavelength",
      "Wavelength (nm)",
      choices =
        list("480", "580", "640"),
      selected = "480"
    ),
    # source selector
    selectInput(
      "source",
      "Light Source",
      choices =
        list(
          "LED (1 µm)",
          "LED (2 µm)",
          "LED (5 µm)",
          "LED (10 µm)",
          "LED (20 µm)",
          "LED (50 µm)",
          "LED (100 µm)",
          "LED (200 µm)",
          "LED (500 µm)",
          "LED (1000 µm)",
          "OF (25 µm, NA 0.66)",
          "OF (50 µm, NA 0.22)",
          "OF (100 µm, NA 0.22)",
          "OF (100 µm, NA 0.37)",
          "OF (200 µm, NA 0.22)",
          "OF (200 µm, NA 0.37)",
          "OF (200 µm, NA 0.50)",
          "OF (400 µm, NA 0.50)",
          "OF (600 µm, NA 0.22)",
          "OF (600 µm, NA 0.37)"
        ),
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
    # plot download button
    downloadButton(
      "downloadPlot",
      "Download Plot"
    )
  ),
  
  layout_column_wrap(
    width = "600px",
    height = "600px",
    fixed_width = TRUE,
    card(
      card_header("Irradiance Contour"), 
      card_body(plotOutput("pdata", width = "100%", height = "600px"))
      )
  ),
  layout_column_wrap(
    width = "600px",
    fixed_width = TRUE,
    card(
      card_header("Summary Data"),
      card_body(verbatimTextOutput("tdata"))
    )
  )
)



# Server logic
server <- function(input, output) {
  
  #SLICE INDEX AND UA VALUES
  setVars <- reactive({
    # source index
    si <- switch(
      input$source,
      "LED (1 µm)" = 1,
      "LED (2 µm)" = 2,
      "LED (5 µm)" = 3,
      "LED (10 µm)" = 4,
      "LED (20 µm)" = 5,
      "LED (50 µm)" = 6,
      "LED (100 µm)" = 7,
      "LED (200 µm)" = 8,
      "LED (500 µm)" = 9,
      "LED (1000 µm)" = 10,
      "OF (25 µm, NA 0.66)" = 11,
      "OF (50 µm, NA 0.22)" = 12,
      "OF (100 µm, NA 0.22)" = 13,
      "OF (100 µm, NA 0.37)" = 14,
      "OF (200 µm, NA 0.22)" = 15,
      "OF (200 µm, NA 0.37)" = 16,
      "OF (200 µm, NA 0.50)" = 17,
      "OF (400 µm, NA 0.50)" = 18,
      "OF (600 µm, NA 0.22)" = 19,
      "OF (600 µm, NA 0.37)" = 20
    )
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
    
    if (ti == 1) {
      ua <- switch(
        input$wavelength,
        "480" = 0.35,
        "580" = 0.19,
        "640" = 0.09
      )
    } else {
      ua <- switch(
        input$wavelength,
        "480" = 0.37,
        "580" = 0.19,
        "640" = 0.05
      )
    }
    
    # slice index
    sliceIndex = (wi - 1) * 20 + si
    
    # output vector
    vout <- c(sliceIndex, ua)
  })
  
  #CONTOUR PLOT - Reactive, for display in app
  draw_contour_r <- reactive({
    # par(bg = "#e0e0e0") grey background helps visibility with true colour plots
    pcolour <- switch(
      input$wavelength,
      "480" = "blue",#accurate colour is #00d5ff
      "580" = "green",#note in reality it is yellow #ffff00
      "640" = "#ff2100" #accurate to 640 nm
      #https://academo.org/demos/wavelength-to-colour-relationship/
    )
    vars <- setVars()
    sliceData <- switch(input$tissue,
                        "White matter" = (input$power * w$white[, , vars[1]]) / (vars[2] * 0.000001),
                        "Grey matter" = (input$power * g$grey[, , vars[1]]) / (vars[2] * 0.000001)
    )
    contour(
      seq(-1, 1, length.out = 200),
      seq(-1, 1, length.out = 200),
      sliceData,
      col = pcolour,
      levels = input$threshold,
      drawlabels = FALSE,
    )
    title(main = sprintf("%s in %s @ %s nm\nPower: %.2f mW - Threshold: %.2f mW/mm^2", input$source, input$tissue, input$wavelength, input$power, input$threshold),
          xlab = "Lateral spread (mm)",
          ylab = "Depth (mm)")
    par(new=TRUE)
    # coordinates to draw source surface
    sx <- switch(
      input$source,
      "LED (1 µm)" = c(-0.0005, 0.0005),
      "LED (2 µm)" = c(-0.001, 0.001),
      "LED (5 µm)" = c(-0.0025, 0.0025),
      "LED (10 µm)" = c(-0.005, 0.005),
      "LED (20 µm)" = c(-0.010, 0.010),
      "LED (50 µm)" = c(-0.025, 0.025),
      "LED (100 µm)" = c(-0.050, 0.050),
      "LED (200 µm)" = c(-0.100, 0.100),
      "LED (500 µm)" = c(-0.250, 0.250),
      "LED (1000 µm)" = c(-0.500, 0.500),
      "OF (25 µm, NA 0.66)" = c(-0.0125, 0.0125),
      "OF (50 µm, NA 0.22)" = c(-0.025, 0.025),
      "OF (100 µm, NA 0.22)" = c(-0.050, 0.050),
      "OF (100 µm, NA 0.37)" = c(-0.050, 0.050),
      "OF (200 µm, NA 0.22)" = c(-0.100, 0.100),
      "OF (200 µm, NA 0.37)" = c(-0.100, 0.100),
      "OF (200 µm, NA 0.50)" = c(-0.100, 0.100),
      "OF (400 µm, NA 0.50)" = c(-0.200, 0.200),
      "OF (600 µm, NA 0.22)" = c(-0.300, 0.300),
      "OF (600 µm, NA 0.37)" = c(-0.300, 0.300)
    )
    sy <- c(0, 0)
    lines(sx, sy, xlim = c(-1, 1), ylim = c(-1, 1))
  })
  
  #CONTOUR PLOT - Function, for generating png to download
  draw_contour_f <- function(){
    # par(bg = "#e0e0e0") grey background helps visibility with true colour plots
    pcolour <- switch(
      input$wavelength,
      "480" = "blue",#accurate colour is #00d5ff
      "580" = "green",#note in reality it is yellow ffff00
      "640" = "#ff2100" #accurate to 640 nm
      #https://academo.org/demos/wavelength-to-colour-relationship/
    )
    vars <- setVars()
    sliceData <- switch(input$tissue,
                        "White matter" = (input$power * w$white[, , vars[1]]) / (vars[2] * 0.000001),
                        "Grey matter" = (input$power * g$grey[, , vars[1]]) / (vars[2] * 0.000001)
    )
    contour(
      seq(-1, 1, length.out = 200),
      seq(-1, 1, length.out = 200),
      sliceData,
      col = pcolour,
      levels = input$threshold,
      drawlabels = FALSE,
    )
    title(main = sprintf("%s in %s @ %s nm\nPower: %.2f mW - Threshold: %.2f mW/mm^2", input$source, input$tissue, input$wavelength, input$power, input$threshold),
          xlab = "Lateral spread (mm)",
          ylab = "Depth (mm)")
    par(new=TRUE)
    # coordinates to draw source surface
    sx <- switch(
      input$source,
      "LED (1 µm)" = c(-0.0005, 0.0005),
      "LED (2 µm)" = c(-0.001, 0.001),
      "LED (5 µm)" = c(-0.0025, 0.0025),
      "LED (10 µm)" = c(-0.005, 0.005),
      "LED (20 µm)" = c(-0.010, 0.010),
      "LED (50 µm)" = c(-0.025, 0.025),
      "LED (100 µm)" = c(-0.050, 0.050),
      "LED (200 µm)" = c(-0.100, 0.100),
      "LED (500 µm)" = c(-0.250, 0.250),
      "LED (1000 µm)" = c(-0.500, 0.500),
      "OF (25 µm, NA 0.66)" = c(-0.0125, 0.0125),
      "OF (50 µm, NA 0.22)" = c(-0.025, 0.025),
      "OF (100 µm, NA 0.22)" = c(-0.050, 0.050),
      "OF (100 µm, NA 0.37)" = c(-0.050, 0.050),
      "OF (200 µm, NA 0.22)" = c(-0.100, 0.100),
      "OF (200 µm, NA 0.37)" = c(-0.100, 0.100),
      "OF (200 µm, NA 0.50)" = c(-0.100, 0.100),
      "OF (400 µm, NA 0.50)" = c(-0.200, 0.200),
      "OF (600 µm, NA 0.22)" = c(-0.300, 0.300),
      "OF (600 µm, NA 0.37)" = c(-0.300, 0.300)
    )
    sy <- c(0, 0)
    lines(sx, sy, xlim = c(-1, 1), ylim = c(-1, 1))
  }
  
  #CONTOUR PLOT - renderPlot call to display in app
  output$pdata <- renderPlot({
   draw_contour_r()
  }, height = 500, width = 500)
  
  #SUMMARY DATA
  output$tdata <- renderText({
    dr = 0.01 # voxel edge length in mm
    # get current slice info
    vars <- setVars()
    sliceData <- switch(
      input$tissue,
      "White matter" = (input$power * w$white[, , vars[1]]) / (vars[2] * 0.000001),
      "Grey matter" = (input$power * g$grey[, , vars[1]]) / (vars[2] * 0.000001)
    )
    # calculate summary data
    # max irradiance
    dmax <- max(sliceData)
    # volume over threshold
    mask <- sliceData >= input$threshold
    vol <- 0
    for(i in (colSums(mask))){
      vol <- vol + dr * (pi * (dr*i)^2)
    }
    # spread distance in mm
    cs = colSums(mask)
    rs = rowSums(mask)
    cfirst <- min(which(cs > 0))
    clast <- max(which(cs > 0))
    rfirst <- min(which(rs > 0))
    fspread <- (clast - 100) * dr
    bspread <- (100 - cfirst) * dr
    if(bspread < 0){
      bspread <- 0
    }
    lspread <- (100 - rfirst) * dr
    # display data on app card
    str_irr <- sprintf("Max Irradiance:\t  %.2f mW/mm^2", dmax)
    str_volume <- sprintf("\nVol. illuminated: %.3f mm^3", vol)
    str_fspread <- sprintf("\nForward spread:\t  %.2f mm", fspread)
    str_bspread <- sprintf("\nBackward spread:  %.2f mm", bspread)
    str_lspread <- sprintf("\nLateral spread:\t  %.2f mm", lspread)
    paste(str_irr, str_volume, str_fspread, str_bspread, str_lspread)

  })
  
  #DOWNLOAD PLOT
  output$downloadPlot <- downloadHandler(
    filename <- function(){
      s_str <- switch(
        input$source,
        "LED (1 µm)" = "L_0001",
        "LED (2 µm)" = "L_0002",
        "LED (5 µm)" = "L_0005",
        "LED (10 µm)" = "L_0010",
        "LED (20 µm)" = "L_0020",
        "LED (50 µm)" = "L_0050",
        "LED (100 µm)" = "L_0100",
        "LED (200 µm)" = "L_0200",
        "LED (500 µm)" = "L_0500",
        "LED (1000 µm)" = "L_1000",
        "OF (25 µm, NA 0.66)" = "F_0025_66",
        "OF (50 µm, NA 0.22)" = "F_0050_22",
        "OF (100 µm, NA 0.22)" = "F_0100_22",
        "OF (100 µm, NA 0.37)" = "F_0100_37",
        "OF (200 µm, NA 0.22)" = "F_0200_22",
        "OF (200 µm, NA 0.37)" = "F_0200_37",
        "OF (200 µm, NA 0.50)" = "F_0200_50",
        "OF (400 µm, NA 0.50)" = "F_0400_50",
        "OF (600 µm, NA 0.22)" = "F_0600_22",
        "OF (600 µm, NA 0.37)" = "F_0600_37"
      )
      t_str <- switch(
        input$tissue,
        "White matter" = "W",
        "Grey matter" = "G"
      )
      pow_str <- gsub("\\.", "-", sprintf("%.2f", input$power))
      thr_str <- gsub("\\.", "-", sprintf("%.2f", input$threshold))
      sprintf("%s_%s_%snm_P%s_T%s.png", s_str, t_str, input$wavelength, pow_str, thr_str)
    },
    content = function(file){
      png(file)
      draw_contour_f()
      dev.off()
    }
  )
}

# Run the app
shinyApp(ui, server)
