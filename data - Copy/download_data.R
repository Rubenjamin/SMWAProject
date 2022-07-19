#' download_data.R
#'
#' contributors: INSERT_NAME
#'
#' File downloads the data used in your analysis from online.
#' This way you, and the instructors can access your data, 
#' and help you as needed.
#' 
#' We recommend uploading the data to Google Drive and using 
#' this script as a set of steps to download the data to an
#' individual computer to be analyzed.
#'

# Libraries
## NOTES: Add the libraries you need for this script to run sucessfully
# --- Load Libraries --- #
library(googledrive)
# --- Info --- #
data_id <- "13wL1KWRbi1WEK4rBBL8zq4KlOoLFV5v7" # you can get this from the shareable link
out_file <- "data/MILEDATA.zip"  # example: "data/wine_reviews.csv"
#---- Download --- #
drive_download(
    as_id(data_id), 
    path = out_file, 
    overwrite = TRUE)

unzip(out_file,
      exdir = "data")
file.remove(out_file)




