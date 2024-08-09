library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(glue)
library(reticulate) # For interfacing Python in R (if needed)
library(text)
library(countrycode) 
library(httr)       
library(jsonlite)   
library(dplyr)      
library(purrr)      
library(readr)      
library(tidyr)      
library(stringr)    

# Set path using getwd()
path <- getwd()

# Strip '02_programs' from the end of the path
root_path <- substr(path, 1, nchar(path) - 16)


api_url <- 'https://search.worldbank.org/api/v2/wds?format=json&strdate=2010-01-01&docty_key=620265&rows=40000&fl=abstracts,authr,alt_title,colti,closedt,count,display_title,docdt,docna,docty,entityid,envcat,geo_reg,id,isbn,issn,keywd,majdocty,origu'

# Create a list of years from 2000 to 2024
years <- 2000:2024

year <- 2024
# Define the root path
root_path <- substr(getwd(), 1, nchar(getwd()) - 16)

# Define the filepath for saving the data
filepath <- file.path(root_path, '01_raw_data', 'prwp_papers')

# Create the directory if it doesn't exist
if (!dir.exists(filepath)) {
  dir.create(filepath, recursive = TRUE)
}

required_columns <- c("id", "authors", "abstracts", "display_title", "url", "origu")

for (year in years) {
  # Add filename to filepath
  filename <- paste0('prwp_', year, '.csv')
  file <- file.path(filepath, filename)
  
  # Skip if file already exists
  if (file.exists(file)) {
    next
  } else {
    api_url <- paste0('https://search.worldbank.org/api/v2/wds?format=json&strdate=',
                      year, '-01-01&enddate=', year, '-12-31&docty_key=620265&rows=40000&fl=abstracts,authr,alt_title,colti,closedt,count,display_title,docdt,docna,docty,entityid,envcat,geo_reg,id,isbn,issn,keywd,majdocty,origu')
    
    # Fetch the data from the API
    response <- fromJSON(api_url)
    
    # Debug: Print the structure of the response
    print(paste('Response structure for year', year))
    #str(response)
    
    # Check if the 'documents' field exists and is not empty
    if (!is.null(response$documents) && length(response$documents) > 0) {
      
      documents_modified <- lapply(response$documents, function(doc) {
        # Keep only the elements specified in required_columns
        doc[names(doc) %in% required_columns]
      })
      
      # Convert JSON data to a dataframe
      df <- as.data.frame(documents_modified)
      
      # Check if the dataframe is empty or not
      if (nrow(df) == 0) {
        warning(paste('Dataframe is empty for year:', year))
      } else {
        # Save dataframe to CSV
        write.csv(df, file, row.names = FALSE)
      }
    } else {
      warning(paste('No data found for year:', year))
    }
  }
  
  print(paste('Finished', year))
}
# Initialize an empty data frame
prwp_df <- data.frame()

# Loop through each year and read the corresponding CSV file
for (year in years) {
  temp_df <- read_csv(file.path(filepath, paste0('prwp_', year, '.csv')))
  prwp_df <- bind_rows(prwp_df, temp_df)
}


