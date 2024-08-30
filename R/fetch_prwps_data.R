library(jsonlite)
library(dplyr)
library(readr)

fetch_prwp_data <- function(years,
                            base_path = getwd(),
                            required_columns = c("id", "authors", "abstracts", "display_title", "url", "origu", "keywd"),
                            save_csv = FALSE,
                            return_list = FALSE) {

  filepath <- file.path(base_path, 'test_raw_data', 'prwp_papers')


  if (save_csv && !dir.exists(filepath)) {
    dir.create(filepath, recursive = TRUE)
  }

  all_data <- list()

  for (year in years) {

    filename <- paste0('prwp_', year, '.csv')
    file <- file.path(filepath, filename)

    api_url <- paste0('https://search.worldbank.org/api/v2/wds?format=json&strdate=',
                      year, '-01-01&enddate=',
                      year, '-12-31&docty_key=620265&rows=40000&fl=abstracts,authr,alt_title,colti,closedt,count,display_title,docdt,docna,docty,entityid,envcat,geo_reg,id,isbn,issn,keywd,majdocty,origu')

    response <- fromJSON(api_url)

    print(paste('Processing year', year, '...'))

    if (!is.null(response$documents) && length(response$documents) > 0) {

      documents_modified <- lapply(response$documents, function(doc) {

        filtered_doc <- doc[names(doc) %in% required_columns]

        filtered_doc <- lapply(filtered_doc, function(x) {
          if (is.list(x)) {
            return(paste(unlist(x), collapse = "; "))
          } else if (is.null(x)) {
            return(NA)
          } else {
            return(x)
          }
        })

        missing_cols <- setdiff(required_columns, names(filtered_doc))
        filtered_doc[missing_cols] <- NA

        return(filtered_doc)
      })

      if (return_list) {

        all_data <- c(all_data, documents_modified)
      } else {

        df <- do.call(rbind, lapply(documents_modified, as.data.frame))

        df$year <- year

        # Check if empty
        if (nrow(df) == 0) {
          warning(paste('Dataframe is empty for year:', year))
        } else {

          if (save_csv) {
            write.csv(df, file, row.names = FALSE)
          }

          all_data <- bind_rows(all_data, df)
        }
      }
    } else {
      warning(paste('No data found for year:', year))
    }

    print(paste('Finished', year))
  }

  if (return_list) {
    return(all_data)
  } else {

    prwp_df <- as_tibble(all_data)

    rownames(prwp_df) <- NULL

    return(prwp_df)
  }
}

# Example
years_to_fetch <- 2021:2022 # 2023 and 2024 have no keywords

required_columns <- c("id", "display_title", "docty", "docdt", "pdfurl", "url", "authors", "abstracts", "keywd")

prwp_list <- fetch_prwp_data(years_to_fetch, required_columns, save_csv = FALSE, return_list = FALSE)


print(head(prwp_list))
