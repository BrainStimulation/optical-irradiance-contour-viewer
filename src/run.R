cat("\n\n===============================================================")
cat("\n\n      Welcome to the optical irradiance contour viewer!\n\n")
cat("===============================================================\n\n\n\n")

# Get the full path of the script that is currently running
initial_options <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", initial_options[grep(file_arg, initial_options)])

# If the script is run interactively, this might be empty. Fallback to current dir.
if (length(script_path) == 0) {
  script_path <- "."
}

# Get the directory of the script
script_dir <- dirname(script_path)

install_script_path <- file.path(script_dir, "install.R")
source(install_script_path)

cat("\n\n\n\n===============================================================\n\n\n\n")

cat("Loading app...\n")
cat("Once you see a URL that looks like this: http://127.0.0.1:XXXX\n")
cat("go to the URL on your browser to access the viewer.\n\n")
cat("You can press Ctrl+C at any time on this terminal window, or close this terminal window, to stop the app.\n\n\n\n")

# Construct the full path to the app.R file
app_path <- file.path(script_dir, "app.R")


library(shiny)
runApp(app_path)
