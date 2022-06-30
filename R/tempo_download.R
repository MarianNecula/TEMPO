tempo_download <- function(payloads = list(), path = NULL){
  
  url_csv <- "http://statistici.insse.ro:8077/tempo-ins/pivot"
  
  for(i in 1:length(payloads)){
  
  if(is.null(payloads[[i]]$replace_me)){
    
    payload <- payloads[[i]]
    
    payload[3] <- NULL
    
    payload_to_json <- toJSON(payload, pretty = TRUE, auto_unbox = TRUE, null = "null")
    
    h1 <- new_handle()
    
    handle_setheaders(h1,"Content-Type" = "application/json", encode = "json")
    
    handle_setopt(h1, .list = list(customrequest = "POST" ,  postfields = payload_to_json))
    
    curl_download(url_csv, paste0(path, payload$matCode, ".csv"), quiet = FALSE, mode = "wb", handle =  h1)
    
    h1 <- NULL  

  } else {
    
    payload <- payloads[[1]]
    
    replace_list <- payload$replace_me
    
    payload$replace_me <- NULL
    
    results <- data.frame()
    
    succes2 <- function(req){
      w <- readBin(req$content, "character")
      z <- read.csv2(text = w, sep = ",", stringsAsFactors = FALSE)
      z <- as.data.frame(z)
      results <<- rbind(results, z)
    }
    
    my_pool <- new_pool()
    
    for(j in 1:length(replace_list)){
      
      x <- paste0(replace_list[[j]], sep = ",", collapse = " ")
      
      payload_to_json <- payload
      
      payload_to_json$encQuery <- gsub("replace_me", x, payload_to_json$encQuery)
      
      payload_to_json$encQuery <- gsub(",:", ":", payload_to_json$encQuery)
      
      payload_to_json <- toJSON(payload_to_json, pretty = TRUE, auto_unbox = TRUE, null = "null") 
     
      h2 <- new_handle()
      
      handle_setheaders(h2,"Content-Type" = "application/json", encode = "json")
      
      handle_setopt(h2,  postfields = payload_to_json)
      
      curl_fetch_multi(url_csv, done = succes2, handle =  h2, pool = my_pool)
      
    }
    
  multi_run(pool = my_pool)   
  
  write.csv(results, paste0(path, payload$matCode, ".csv"), row.names = FALSE)
  }
}
    
  message(paste0("Files are stored in: ", path))

}


