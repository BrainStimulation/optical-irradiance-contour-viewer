# Get the full path of the script that is currently running

if (!require("digest")) install.packages("digest", repos = "http://cran.us.r-project.org")
if (!require("httr")) install.packages("httr", repos = "http://cran.us.r-project.org")
if (!require("jsonlite")) install.packages("jsonlite", repos = "http://cran.us.r-project.org")

library(digest)
library(httr)
library(jsonlite)

get_figshare_md5 <- function(article_id, file_id) {
  # Construct the API URL for file details
  api_url <- paste0("https://api.figshare.com/v2/articles/", article_id, "/files/", file_id)
  
  # Make the GET request
  response <- GET(api_url)
  
  # Check for a successful response
  if (status_code(response) == 200) {
    # Parse the JSON content and extract the MD5 hash
    content <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content)
    return(data$computed_md5)
  } else {
    # Handle errors
    warning("Failed to fetch metadata for file ID: ", file_id)
    return(NULL)
  }
}

initial_options <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", initial_options[grep(file_arg, initial_options)])

if (length(script_path) == 0) {
  script_path <- "."
}

script_dir <- dirname(script_path)

data_dir_path <- file.path(script_dir, "data")
# Create a data directory if it doesn't exist
dir.create(data_dir_path, showWarnings = FALSE)

FILES_TO_DOWNLOAD <- list(
  absorptionData = list(
    article_id = 61413808,
    dest = file.path(data_dir_path, 'absorptionData.mat')
  )
)

# Create the data directory if it doesn't exist
dir.create("data", showWarnings = FALSE)

# Loop through the files and download only if necessary
for (name in names(FILES_TO_DOWNLOAD)) {
  
  file_info <- FILES_TO_DOWNLOAD[[name]]
  cat("Checking file:", file_info$dest, "\n")
  
  should_download <- TRUE
  
  if (file.exists(file_info$dest)) {
    local_hash <- digest::digest(file = file_info$dest, algo = "md5")
    md5_hash <- get_figshare_md5(file_info$article_id, file_info$file_id)
    
    cat("Checking file:", local_hash, "\n")
    cat("Checking file:", md5_hash, "\n")
    
    # !is.null(md5_hash)
    if (local_hash == md5_hash) {
      cat("  -> Hash matches. File is up to date. Skipping download.\n")
      should_download <- FALSE
    } else {
      cat("local hash:", local_hash, "\n")
      cat("remote hash:", md5_hash, "\n")
      cat("  -> Hash mismatch! File is corrupt or outdated. Re-downloading.\n")
      file.remove(file_info$dest) # Remove the bad file
    }
  } else {
    cat("  -> File not found. Downloading.\n")
  }
  
  if (should_download) {
    api_url <- paste0("https://api.figshare.com/v2/file/download/", file_info$file_id)
    download.file(url = api_url, destfile = file_info$dest)
    cat("  -> Download complete.\n")
  }
}

cat("\nData preparation is finished.\n")
