library(jsonlite)
library(dplyr)

check_columns <- function(years,
                          base_path = getwd()) {
  all_columns <- list()

  for (year in years) {
    api_url <- paste0('https://search.worldbank.org/api/v2/wds?format=json&strdate=',
                      year, '-01-01&enddate=',
                      year, '-12-31&docty_key=620265&rows=40000&fl=abstracts,authr,alt_title,colti,closedt,count,display_title,docdt,docna,docty,entityid,envcat,geo_reg,id,isbn,issn,keywd,majdocty,origu')

    # Fetch the data from the API
    response <- fromJSON(api_url)

    # Debug: Print the structure of the response
    print(paste('Checking columns for year', year))

    # Check if the 'documents' field exists and is not empty
    if (!is.null(response$documents) && length(response$documents) > 0) {

      # Extract all the columns (names) available in each document
      year_columns <- lapply(response$documents, function(doc) {
        return(names(doc))
      })

      # Combine all columns found into a single vector and remove duplicates
      unique_columns <- unique(unlist(year_columns))

      # Store unique columns by year
      all_columns[[as.character(year)]] <- unique_columns

    } else {
      warning(paste('No data found for year:', year))
    }

    print(paste('Finished checking columns for year', year))
  }

  # Combine all columns found across the years
  combined_columns <- unique(unlist(all_columns))

  # Return the list of all unique columns
  return(list(
    year_wise_columns = all_columns,
    all_unique_columns = combined_columns
  ))
}

# Use:
years_to_check <- 2024:2024
column_summary <- check_columns(years_to_check)

