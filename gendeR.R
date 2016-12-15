#' Remove special characters
#' Disclaimer: code taken from blog post https://data.hypotheses.org/564 by Alexandre Hobeika
#'
#' @param text vector of characters containing firstnames
#' @return vector of characters containing cleaned firstnames.
#' @examples
#' .cleanName(c("gaëlle", "grégory"))
.cleanName <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to = "ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  
  return(text)
}


#' Generate indices for multi-pass processing
#'
#' @param values vector of values to process
#' @param batch.count maximum number of value processed in a single pass
#' @return list of indices, one list per pass.
#' @examples
#' .getBatchIndices(letters)
.getBatchIndices <- function(values,
                             batch.count = 10) {
  l <- length(values)
  
  indices <- lapply(
    1:(l %/% batch.count + ifelse(l %% batch.count == 0, 0, 1)),
    function(p) {
      first <- (p - 1) * batch.count + 1
      last <- min(l, first + batch.count - 1)
      return(first:last)
    }
  )
  
  return(indices)
}


#' Get gender from first names using genderize.io API.
#'
#' @param firstname vector of characters containing first names
#' @param country country to which first names belongs to
#' @param clean remove special characters before submitting first names
#' @param verbose display information messages
#' @return vector of characters containing genders (male or female).
#' @examples
#' gender <- getGender(c("Michel", "Mathilde", "Yann", "Gaëlle", "Grégory"))
.getGender <- function(firstname,
                       country = "fr",
                       clean = F,
                       verbose = F) {
  indices <- .getBatchIndices(firstname, batch.count = 10)

  mapper <- lapply(
    indices,
    function(i) {
      items <- firstname[i]
      
      if (clean) {
        items <- .cleanName(items)
      }
      
      url <- paste0(
        "https://api.genderize.io/?",
        paste(sapply(1:length(items), function(n) { paste0("name[", n - 1, "]=", items[n]) }), sep = "", collapse = "&"),
        "&country_id=",
        country
      )
      #url <- utils::URLencode(url)
      
      if (verbose) {
        message(url)
      }
      
      response <- httr::GET(url)
      if (response$status_code != 200) {
        stop("Request error (", response$status_code, ")")
      }
      
      content <- rjson::fromJSON(rawToChar(response$content))
      
      return(unlist(sapply(content, function(c) { ifelse(is.null(c$gender), NA, c$gender) })))
    }
  )
  
  return(unlist(mapper))
}


#' Get gender from first names using genderize.io API.
#' In case of failure, a second attempt to get gender will be submitted by removing special characters.
#'
#' @param firstname vector of characters containing first names
#' @param country country to which first names belongs to
#' @param verbose display information messages
#' @return vector of characters containing genders (male or female).
#' @examples
#' gender <- getGender(c("Michel", "Mathilde", "Yann", "Gaëlle", "Grégory"))
getGender <- function(firstname,
                      country = "fr",
                      verbose = F) {
  if (!is.vector(firstname)) {
    stop("firstname must be a vector")  
  }
  
  gender <- .getGender(firstname, country, verbose = verbose)
  
  na.indices <- which(is.na(gender))
  if (length(na.indices) > 0) {
    if (verbose) {
      message("Trying with clean names")
    }
    # Resubmit missing guesses with clean names
    gender2 <- .getGender(firstname[na.indices], country, clean = T, verbose = verbose)
    gender[na.indices] <- gender2
  }
  
  return(gender)
}
