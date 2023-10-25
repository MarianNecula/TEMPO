#' @title tempo_toc
#' 
#' @description Get TOC for Tempo Online
#' function gets a parameter full_description implicitly false 
#' if fulldescription == TRUE then starts collecting dates for last updates 
#' 
#' @param full_description - implicitly set as false  
#' if fulldescription == TRUE then starts collecting dates for last updates
#' 
#' @param language - set the language for data download. Options romanian ro or english en
#' @return Returns a dataframe object. 
#' 
#' 
#' @details This functions sends one or multiple GET requests and parses the content into 
#' a dataframe
#' 
#' @examples 
#' tempo_toc(full_description = FALSE)
#' 
#' @import curl
#' @import jsonlite
#' @import utils 
#' @export

tempo_toc <- function(full_description = FALSE, language = c("ro")) {
  if(length(language) != 1) {
    message("Invalid argument for language. Arguments accepted: \"ro\" or \"en\".\n")
    return(invisible(NULL))
  }
  if (language[1] == "ro") {
    response <- curl_fetch_memory("http://statistici.insse.ro:8077/tempo-ins/matrix/matrices")
    lang <- ""
  } else if (language[1] == "en") {
    response <- curl_fetch_memory("http://statistici.insse.ro:8077/tempo-ins/matrix/matrices/?lang=en/")
    lang <- "/?lang=en/"
  } else{
    message("Invalid argument for language: ", language[1], "\nArguments accepted: \"ro\" or \"en\".\n")
    return (invisible(NULL))
  }
  if(!is.logical(full_description)) {
    message("full_description should have a logical value!\n")
    return(invisible(NULL))
  }
  tempo_logger(response)
  responsetext <- readBin(response$content, what = "text")
  tempo_toc <- fromJSON(responsetext, flatten = TRUE)
  tempo_toc <- tempo_toc[,c(1,2)]
  if (full_description == TRUE) {
    message("This will take a while. Grab some coffee!")
    for (i in 1:length(tempo_toc[, 2])) {
      lu_response <-
        curl_fetch_memory(paste0(
          "http://statistici.insse.ro:8077/tempo-ins/matrix/",
          tempo_toc[i, 2], lang
        ))
      lu_content <- readBin(lu_response$content, what = "text")
      lu_content <- fromJSON(lu_content, flatten = TRUE)
      tempo_toc$Statistical_domain[i] <-
        lu_content$ancestors$name[2]
      tempo_toc$Statistical_sub_domain[i] <-
        lu_content$ancestors$name[3]
      tempo_toc$Survey_name[i] <- lu_content$ancestors$name[4]
      tempo_toc$Last_update[i] <-
        lu_content$ultimaActualizare
    }
  }
  return(tempo_toc)
}