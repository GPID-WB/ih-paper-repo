extract_unique_keywords <- function(prwp_list) {

  all_keywords <- c()

  # Loop through docs
  for (doc in prwp_list) {
    if (!is.null(doc$keywd)) {

      clean_keywords <- gsub("\n", " ", doc$keywd)
      clean_keywords <- gsub("\\s+", " ", clean_keywords)
      keywords <- unlist(strsplit(clean_keywords, ";\\s*"))

      all_keywords <- c(all_keywords, keywords)
    }
  }

  # Get unique
  unique_keywords <- unique(all_keywords)

  return(unique_keywords)
}

# Example
prwp_list <- fetch_prwp_data(2021:2022, return_list = TRUE)

unique_keywords <- extract_unique_keywords(prwp_list)

print(unique_keywords)


