# Load packages ----
library(shiny)
library(bslib)
library(R.matlab)
# load slice data for plots
g <- readMat(url("https://brainstimulation.github.io/optical-irradiance-contour-webapp/data/grey.mat"))
w <- readMat(url("https://brainstimulation.github.io/optical-irradiance-contour-webapp/data/white.mat"))
# User interface ----
ui <- page_sidebar(
  title = "Optogenetics Contour Visualiser [PAPER TITLE, CITATION INFO, & DOI HERE]",
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
      choices =
        list(
          # "LED (1 µm)",
          # "LED (2 µm)",
          # "LED (5 µm)",
          # "LED (10 µm)",
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
    # gridline checkbox
    checkboxInput(
      "drawgridlines",
      "Show Gridlines",
      value = TRUE
    ),
    # irradiance slice on contour plot
    checkboxInput(
      "drawirrsliceline",
      "Show Irradiance Plot Location",
      value = TRUE
    ),
    # log plot for irradiance slice
    checkboxInput(
      "irrslicelogplot",
      "Logarithmic Irradiance Plot",
      value = FALSE
    ),
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
    downloadButton(
      "downloadcontourplot",
      "Download Contour Plot"
    ),
    downloadButton(
      "downloadirrplot",
      "Download Irradiance Plot"
    )
  ),
  # plot and data cards
  layout_columns(
    card(
      card_body(plotOutput("pdata", width = "100%", height = "500"))
    ),
    card(
      card_body(plotOutput("irrplotdata"), height = "500")
    ),
    # col_widths = c(7, 5)
  ),
  
  # summary data
  card(
    card_body(verbatimTextOutput("tdata"))
  )
)
# Server logic
server <- function(input, output) {
  
  # colour of contour line  
  pcolour <- reactive({
    req(input$wavelength)
    colour <- switch(
      input$wavelength,
      "480" = "blue",#accurate colour is #00d5ff
      "580" = "green",#note in reality it is yellow #ffff00
      "640" = "#ff2100" #accurate to 640 nm
      #https://academo.org/demos/wavelength-to-colour-relationship/
    )
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
    req(input$source, input$wavelength, input$tissue)
    
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
    ti <- switch(
      input$tissue,
      "White matter" = 1,
      "Grey matter" = 2
    )
    
    # slice index
    sliceIndex = (wi - 1) * 20 + si
  })
  
  # array of data for contour and irradiance plots
  sliceData <- reactive({ 
    req(input$tissue, input$power, w$white, g$grey)
    arr <- switch(
      input$tissue,
      "White matter" = (input$power * w$white[, , sliceIndex()]) / (ua() * 0.000001),
      "Grey matter" = (input$power * g$grey[, , sliceIndex()]) / (ua() * 0.000001)
    )
  })
    
  # x coordinates to plot source on contour plot
  sx <- reactive({ 
    req(input$source)  
    val <- switch(
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
        vol <- vol + dr * (pi * (dr*i*0.5)^2)
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
    req(input$source, input$tissue, input$drawgridlines, input$drawirrsliceline, input$power, input$threshold, input$irrslider, input$wavelength)  
    s_str <- switch(
        input$source,
        "LED (1 µm)" = "L-0001",
        "LED (2 µm)" = "L-0002",
        "LED (5 µm)" = "L-0005",
        "LED (10 µm)" = "L-0010",
        "LED (20 µm)" = "L-0020",
        "LED (50 µm)" = "L-0050",
        "LED (100 µm)" = "L-0100",
        "LED (200 µm)" = "L-0200",
        "LED (500 µm)" = "L-0500",
        "LED (1000 µm)" = "L-1000",
        "OF (25 µm, NA 0.66)" = "F-0025-66",
        "OF (50 µm, NA 0.22)" = "F-0050-22",
        "OF (100 µm, NA 0.22)" = "F-0100-22",
        "OF (100 µm, NA 0.37)" = "F-0100-37",
        "OF (200 µm, NA 0.22)" = "F-0200-22",
        "OF (200 µm, NA 0.37)" = "F-0200-37",
        "OF (200 µm, NA 0.50)" = "F-0200-50",
        "OF (400 µm, NA 0.50)" = "F-0400-50",
        "OF (600 µm, NA 0.22)" = "F-0600-22",
        "OF (600 µm, NA 0.37)" = "F-0600-37"
      )
      t_str <- switch(
        input$tissue,
        "White matter" = "W",
        "Grey matter" = "G"
      )
      if(input$drawgridlines == TRUE){
        grd_str = "on"
      }else{
        grd_str = "off"
      }
      if(input$drawirrsliceline == TRUE){
        irr_str = "on"
      }else{
        irr_str = "off"
      }
      
      pow_str <- gsub("\\.", "-", sprintf("%.2f", input$power))
      thr_str <- gsub("\\.", "-", sprintf("%.2f", input$threshold))
      irr_loc_str <- gsub("\\.", "-", sprintf("%.2f", input$irrslider))
      fnameData <- c(s_str, t_str, input$wavelength, pow_str, thr_str, grd_str, irr_str, irr_loc_str)
    })
    
  #CONTOUR PLOT - Reactive, for display in app
  draw_contour_r <- reactive({
    req(input$threshold, input$source, input$tissue, input$wavelength, input$power, input$drawgridlines, input$drawirrsliceline, input$irrslider)  
    # contour plot
      contour(
        seq(-1, 1, length.out = 200),
        seq(-1, 1, length.out = 200),
        sliceData(),
        col = pcolour(),
        levels = input$threshold,
        drawlabels = FALSE,
      )
      # plot title
      title(
        main = sprintf("%s in %s @ %s nm\nPower: %.2f mW - Threshold: %.2f mW/mm^2", input$source, input$tissue, input$wavelength, input$power, input$threshold),
        xlab = "Lateral spread (mm)",
        ylab = "Depth (mm)",
        sub = sprintf("Max Irradiance = %.2f mW/mm^2    Vol. Illuminated = %.3f mm^3", cData()[1], cData()[2])
      )
      par(new=TRUE) # keep contour visible while other lines are overlaid
      # grid lines
      if(input$drawgridlines == TRUE){
        abline(v=(seq(-1, 1, length.out = 21)), col = 'lightgray', lty = 'dotted')
        abline(h=(seq(-1, 1, length.out = 21)), col = 'lightgray', lty = 'dotted')
        abline(v=(seq(-1, 1, length.out = 5)), col = 'lightgray')
        abline(h=(seq(-1, 1, length.out = 5)), col = 'lightgray')
      }
      # draw source on contour plot
      lines(
        sx(), 
        c(0, 0), 
        xlim = c(-1, 1), 
        ylim = c(-1, 1))
      # draw irradiance slice location
      if(input$drawirrsliceline == TRUE){
        lines(
          c(input$irrslider, input$irrslider), 
          c(-1, 1), 
          xlim = c(-1, 1), 
          ylim = c(-1, 1),
          col = 'purple')
      }
    })
    
  #CONTOUR PLOT - Function, for generating png to download
  draw_contour_f <- function(){
    req(input$threshold, input$source, input$tissue, input$wavelength, input$power, input$drawgridlines, input$drawirrsliceline, input$irrslider)
    # contour plot
      contour(
        seq(-1, 1, length.out = 200),
        seq(-1, 1, length.out = 200),
        sliceData(),
        col = pcolour(),
        levels = input$threshold,
        drawlabels = FALSE,
      )
      # plot title
      title(
        main = sprintf("%s in %s @ %s nm\nPower: %.2f mW - Threshold: %.2f mW/mm^2", input$source, input$tissue, input$wavelength, input$power, input$threshold),
        xlab = "Lateral spread (mm)",
        ylab = "Depth (mm)",
        sub = sprintf("Max Irradiance = %.2f mW/mm^2    Vol. Illuminated = %.3f mm^3", cData()[1], cData()[2])
      )
      par(new=TRUE) # keep contour visible while other lines are overlaid
      # grid lines
      if(input$drawgridlines == TRUE){
        abline(v=(seq(-1, 1, length.out = 21)), col = 'lightgray', lty = 'dotted')
        abline(h=(seq(-1, 1, length.out = 21)), col = 'lightgray', lty = 'dotted')
        abline(v=(seq(-1, 1, length.out = 5)), col = 'lightgray')
        abline(h=(seq(-1, 1, length.out = 5)), col = 'lightgray')
      }
      # draw source on contour plot
      lines(
        sx(), 
        c(0, 0), 
        xlim = c(-1, 1), 
        ylim = c(-1, 1))
      # draw irradiance slice location
      if(input$drawirrsliceline == TRUE){
        lines(
          c(input$irrslider, input$irrslider), 
          c(-1, 1), 
          xlim = c(-1, 1), 
          ylim = c(-1, 1),
          col = 'purple')
      }
    }
    
  #CONTOUR PLOT - renderPlot call to display in app
  output$pdata <- renderPlot({
      draw_contour_r()
    }, height = 500, width = 500)
    
  # IRRADIANCE LINE PLOT - reactive
  draw_irr_r <- reactive({
    req(input$irrslider, input$irrslicelogplot, input$threshold, input$drawgridlines)  
    sindex <- (input$irrslider + 1)*100 + 1
      if(sindex > 200){
        sindex <- 200
      }
      lineData <- sliceData()[sindex,]
      if(input$irrslicelogplot == TRUE){
        plot( # irradiance profile
          lineData,
          seq(-1, 1, length.out = 200),
          main = "Irradiance as Function of Depth",
          xlab = "Log10 Irradiance (mW/mm^2)",
          ylab = "Depth (mm)",
          col = 'purple',
          log='x'
          # sub = "CITATION INFO HERE" 
        )
      }else{
        plot( # irradiance profile
          lineData,
          seq(-1, 1, length.out = 200),
          main = "Irradiance as Function of Depth",
          xlab = "Irradiance (mW/mm^2)",
          ylab = "Depth (mm)",
          col = 'purple',
          # sub = "CITATION INFO HERE" 
        )
      }
      lines( # irradiance threshold
        c(input$threshold, input$threshold),
        c(-1, 1),
        xlim = c(-1, 1),
        ylim = c(-1, 1),
        lty = 'dotted'
      )
      par(new=TRUE)
      # grid lines
      if(input$drawgridlines == TRUE){
        abline(h=(seq(-1, 1, length.out = 21)), col = 'lightgray', lty = 'dotted')
        abline(h=(seq(-1, 1, length.out = 5)), col = 'lightgray')
      }
    })
    
  # IRRADIANCE LINE PLOT - function
  draw_irr_f <- function(){
    req(input$irrslider, input$irrslicelogplot, input$threshold, input$drawgridlines) 
    sindex <- (input$irrslider + 1)*100 + 1
    if(sindex > 200){
      sindex <- 200
    }
    lineData <- sliceData()[sindex,]
    if(input$irrslicelogplot == TRUE){
      plot( # irradiance profile
        lineData,
        seq(-1, 1, length.out = 200),
        main = "Irradiance as Function of Depth",
        xlab = "Log10 Irradiance (mW/mm^2)",
        ylab = "Depth (mm)",
        col = 'purple',
        log='x'
        # sub = "CITATION INFO HERE" 
      )
    }else{
      plot( # irradiance profile
        lineData,
        seq(-1, 1, length.out = 200),
        main = "Irradiance as Function of Depth",
        xlab = "Irradiance (mW/mm^2)",
        ylab = "Depth (mm)",
        col = 'purple',
        # sub = "CITATION INFO HERE" 
      )
    }
    lines( # irradiance threshold
      c(input$threshold, input$threshold),
      c(-1, 1),
      xlim = c(-1, 1),
      ylim = c(-1, 1),
      lty = 'dotted'
    )
    par(new=TRUE)
    # grid lines
    if(input$drawgridlines == TRUE){
      abline(h=(seq(-1, 1, length.out = 21)), col = 'lightgray', lty = 'dotted')
      abline(h=(seq(-1, 1, length.out = 5)), col = 'lightgray')
    }
  }
  
  # IRRADIANCE LINE PLOT - renderPlot
  output$irrplotdata <- renderPlot({
      draw_irr_r()
    }, height = 500, width = 500)
    
  # DOWNLOAD CONTOUR PLOT
  output$downloadcontourplot <- downloadHandler(
    filename = function(){
        sprintf("%s_%s_%snm_P%s_T%s_G%s_S%s_X%smm_CONTOUR.png", fnameData()[1], fnameData()[2], fnameData()[3], fnameData()[4], fnameData()[5], fnameData()[6], fnameData()[7], fnameData()[8])
      },
      content = function(file){
        png(file)
        draw_contour_f()
        dev.off()
      }
  )
    
  # DOWNLOAD IRRADIANCE PLOT
  output$downloadirrplot <- downloadHandler(
    filename = function(){
      if(input$irrslicelogplot == TRUE){
        sprintf("%s_%s_%snm_P%s_T%s_G%s_S%s_X%smm_IRRADIANCE_LOG.png", fnameData()[1], fnameData()[2], fnameData()[3], fnameData()[4], fnameData()[5], fnameData()[6], fnameData()[7], fnameData()[8])
      }else{
        sprintf("%s_%s_%snm_P%s_T%s_G%s_S%s_X%smm_IRRADIANCE_LIN.png", fnameData()[1], fnameData()[2], fnameData()[3], fnameData()[4], fnameData()[5], fnameData()[6], fnameData()[7], fnameData()[8])
      }
    },
    content = function(file){
        png(file)
        draw_irr_f()
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
}
# Run the app
app <- shinyApp(ui = ui, server = server)
