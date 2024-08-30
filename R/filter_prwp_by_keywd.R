filter_prwp_by_keywords <- function(papers_list, keywords) {

  keywords <- tolower(keywords)

  # Clean keywds
  clean_and_extract_keywords <- function(keywd) {
    if (!is.null(keywd)) {
      clean_keywords <- gsub("\\s+", " ", gsub("\n", " ", keywd))
      return(unlist(strsplit(clean_keywords, ";\\s*")))
    }
    return(character(0))
  }

  # Filter papers
  filtered_papers <- lapply(papers_list, function(paper) {

    paper_keywords <- tolower(clean_and_extract_keywords(paper$keywd))

    if (any(sapply(keywords, function(k) any(grepl(k, paper_keywords))))) {
      return(paper)
    }
    return(NULL)
  })

  # Remove NULL entries
  filtered_papers <- Filter(Negate(is.null), filtered_papers)

  return(filtered_papers)
}


# Example
prwp_list <- fetch_prwp_data(2021:2022, return_list = TRUE)

keywds <- c("education", "health")

filtered_papers_list <- filter_prwp_by_keywords(prwp_list, keywds)

print(filtered_papers_list)



