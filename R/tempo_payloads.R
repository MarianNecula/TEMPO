tempo_payloads <-
  function(matrices = c(),
           language = NULL,
           last_update = FALSE) {
    url_get_matrix <-
      "http://statistici.insse.ro:8077/tempo-ins/matrix/"
    
    results <- list()
    cb1 <- function(req) {
      results <<- append(results, list(req))
    }
    
    pool = new_pool()
    
    for (i in 1:length(matrices)) {
      url_tempo <- paste0(url_get_matrix, matrices[i])
      h1 <- new_handle(url = url_tempo)
      multi_add(h1, done = cb1)
    }
    
    multi_run()
    
    results_post <- list()
    for (j in 1:length(results)) {
      parsed_url <- readBin(results[[j]]$content, "text")
      parsed_list <- jsonlite::fromJSON(parsed_url)
      
      options_list <- NULL
      encQuery <- NULL
      payload_vector <- list()
      split_length <- 300
      
      
      for (i in 1:length(parsed_list$dimensionsMap$options)) {
        options_vector <-
          unlist(parsed_list$dimensionsMap$options[[i]]$nomItemId)
        if (length(options_vector) > split_length) {
          options_list <-
            split(options_vector, ceiling(seq_along(options_vector) / 100))
          encQuery <- paste0(encQuery, "replace_me", ":")
        } else {
          options_vector <- paste0(options_vector, sep = ",", collapse = " ")
          encQuery <- paste0(encQuery, options_vector, sep = ":")
          encQuery <- gsub(",:", ":", encQuery)
        }
      }
      encQuery <- sub("\\:$", "", encQuery)
      payload_vector <-
        list(
          language = language,
          encQuery = encQuery,
          replace_me = options_list,
          matCode = sub(".*/", "", results[[j]]$url),
          matMaxDim = parsed_list$details$matMaxDim,
          matUMSpec = parsed_list$details$matUMSpec
        )
      results_post[[j]] <- payload_vector
      
    }
    return(results_post)
  }
