extract_unique_keywords <- function(prwp_list) {

  all_keywords <- c()


  for (doc in prwp_list) {
    if (!is.null(doc$keywd)) {

      clean_keywords <- gsub("\n", " ", doc$keywd)

      clean_keywords <- gsub("\\s+", " ", clean_keywords) # too much whitespace

      keywords <- unlist(strsplit(clean_keywords, ";\\s*"))


      all_keywords <- c(all_keywords, keywords)
    }
  }


  unique_keywords <- unique(all_keywords)

  return(unique_keywords)
}



prwp_list <- fetch_prwp_data(2021:2022, return_list = TRUE)

# Extract unique keywords
unique_keywords <- extract_unique_keywords(prwp_list)

# View the unique keywords
print(unique_keywords)

