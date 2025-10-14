# Load packages ----
library(shiny)
library(bslib)

# User interface ----
ui <- page_sidebar(
  title = "Optogenetics Contour Visualiser [PAPER TITLE, CITATION INFO, & DOI HERE]",
  # SIDEBAR WITH INPUT OPTIONS
  sidebar = sidebar(
    title = "Input Options",
    # Wavelength radio buttons
    radioButtons(
      "wavelength",
      "Wavelength (nm)",
      choices = list("480", "580", "640"),
      selected = "480"
    )
  )
)

# Server logic
server <- function(input, output) {
  # REACTIVE EXPRESSIONS FOR VARIABLES IN PLOTTING AND DOWNLOADING FUNCTIONS

  # colour of contour line
  pcolour <- reactive({
    req(input$wavelength)
    colour <- switch(
      input$wavelength,
      "480" = "blue", #accurate colour is #00d5ff
      "580" = "green", #note in reality it is yellow #ffff00
      "640" = "#ff2100" #accurate to 640 nm
      #https://academo.org/demos/wavelength-to-colour-relationship/
    )
    return(colour)
  })
  observe({
    val <- pcolour()
    message(paste0("pcolour updated to ", val))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
