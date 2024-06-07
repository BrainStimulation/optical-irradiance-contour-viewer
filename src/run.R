cat("\n\n===============================================================")
cat("\n\n      Welcome to the optical irradiance contour viewer!\n\n")
cat("===============================================================\n\n\n\n")

cat("Installing packages, this may take a few minutes...\n\n\n\n")

install.packages("shiny", repos = "http://cran.us.r-project.org")
install.packages("bslib", repos = "http://cran.us.r-project.org")
install.packages("R.matlab", repos = "http://cran.us.r-project.org")

cat("\n\nInstall complete!")

cat("\n\n\n\n===============================================================\n\n\n\n")

cat("Loading app...\n")
cat("Once you see a URL that looks like this: http://127.0.0.1:XXXX\n")
cat("go to the URL on your browser to access the viewer.\n\n")
cat("You can press Ctrl+C at any time on this terminal window, or close this terminal window, to stop the app.\n\n\n\n")

library(shiny)
runApp("app.R")
