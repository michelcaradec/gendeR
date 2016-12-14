#' Remove special characters
#'
#' @param text vector of characters containing firstnames
#' @param country country to which first names belong to
#' @return vector of characters containing genders (male or female).
#' @examples
#' getGender(c("michel", "mathilde", "yann", "gaëlle", "grégory"))
.cleanName <- function(name) {
  name <- gsub("à", "a", name)
  name <- gsub("â", "a", name)
  name <- gsub("ä", "a", name)
  
  name <- gsub("é", "e", name)
  name <- gsub("è", "e", name)
  name <- gsub("ë", "e", name)
  name <- gsub("ê", "e", name)
  
  name <- gsub("î", "i", name)
  name <- gsub("ï", "i", name)
  
  name <- gsub("ô", "o", name)
  
  name <- gsub("û", "u", name)
  name <- gsub("ù", "u", name)
  
  return(name)
}

#' Get gender from first names using genderize.io API.
#'
#' @param firstname vector of characters containing firstnames
#' @param country country to which first names belong to
#' @return vector of characters containing genders (male or female).
#' @examples
#' getGender(c("michel", "mathilde", "yann", "gaëlle", "grégory"))
getGender <- function(firstname, country = "fr") {
  batch.count <- 10
  
  if (!is.vector(firstname)) {
    stop("firstname must be a vector")  
  }
  
  pass.count <- length(firstname) %/% batch.count + ifelse(length(firstname) %% batch.count == 0, 0, 1)

  gender <- c()
  
  for (i in 0:(pass.count - 1)) {
    first <- i * batch.count + 1
    last <- first + ifelse(i == (pass.count - 1) & length(firstname) %% batch.count != 0, length(firstname) %% batch.count, batch.count) - 1
    items <- .cleanName(firstname[first:last])
    
    url <- paste0(
      "https://api.genderize.io/?",
      paste(sapply(1:length(items), function(n) { paste0("name[", n - 1, "]=", items[n]) }), sep = "", collapse = "&"),
      "&country_id=",
      country
    )
    url <- utils::URLencode(url)
    
    request <- httr::GET(url)
    if (request$status_code != 200) {
      stop("Request error (", request$status_code, ")")
    }
    
    content <- rjson::fromJSON(rawToChar(request$content))
    
    gender <- append(
      gender,
      unlist(sapply(content, function(c) { ifelse(is.null(c$gender), NA, c$gender) }))
    )
  }
  
  return(gender)
}
