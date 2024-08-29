library(jsonlite)
library(dplyr)
library(readr)





fetch_prwp_data <- function(years,
                            base_path = getwd(),
                            required_columns = c("id", "authors", "abstracts", "display_title", "url", "origu"),
                            save_csv = FALSE,
                            return_list = FALSE) {
  # Define the filepath for saving the data
  filepath <- file.path(base_path, 'test_raw_data', 'prwp_papers')

  # Create the directory if it doesn't exist and save_csv is TRUE
  if (save_csv && !dir.exists(filepath)) {
    dir.create(filepath, recursive = TRUE)
  }

  all_data <- list()

  for (year in years) {
    # Add filename to filepath
    filename <- paste0('prwp_', year, '.csv')
    file <- file.path(filepath, filename)

    api_url <- paste0('https://search.worldbank.org/api/v2/wds?format=json&strdate=',
                      year, '-01-01&enddate=',
                      year, '-12-31&docty_key=620265&rows=40000&fl=abstracts,authr,alt_title,colti,closedt,count,display_title,docdt,docna,docty,entityid,envcat,geo_reg,id,isbn,issn,keywd,majdocty,origu')

    # Fetch the data from the API
    response <- fromJSON(api_url)

    # Debug: Print the structure of the response
    print(paste('Processing year', year, '...'))

    # Check if the 'documents' field exists and is not empty
    if (!is.null(response$documents) && length(response$documents) > 0) {

      documents_modified <- lapply(response$documents, function(doc) {
        # Keep only the elements specified in required_columns
        filtered_doc <- doc[names(doc) %in% required_columns]

        # Flatten lists and handle missing fields
        filtered_doc <- lapply(filtered_doc, function(x) {
          if (is.list(x)) {
            return(paste(unlist(x), collapse = "; "))
          } else if (is.null(x)) {
            return(NA)
          } else {
            return(x)
          }
        })

        # Ensure the list has all required columns
        missing_cols <- setdiff(required_columns, names(filtered_doc))
        filtered_doc[missing_cols] <- NA

        return(filtered_doc)
      })

      # Convert list to dataframe and ensure proper column names
      df <- do.call(rbind, lapply(documents_modified, as.data.frame))
      names(df) <- required_columns

      # Add the year column
      df$year <- year

      # Check if the dataframe is empty or not
      if (nrow(df) == 0) {
        warning(paste('Dataframe is empty for year:', year))
      } else {
        # Save dataframe to CSV if save_csv is TRUE
        if (save_csv) {
          write.csv(df, file, row.names = FALSE)
        }
        # Collect data in memory
        all_data <- bind_rows(all_data, df)
      }
    } else {
      warning(paste('No data found for year:', year))
    }

    print(paste('Finished', year))
  }

  # Convert collected data to a tibble
  prwp_df <- as_tibble(all_data)

  # Remove row names
  rownames(prwp_df) <- NULL

  # Return as a list if return_list is TRUE
  if (return_list) {
    return(split(prwp_df, seq(nrow(prwp_df))))
  }

  return(prwp_df)
}

# Example usage:
years_to_fetch <- 2023:2024
prwp_data <- fetch_prwp_data(years_to_fetch, save_csv = FALSE, return_list = FALSE)
