tempo_logger <- function(request){
  tmpdir = gettmpdir()
  logTEMPO <- file.path(tmpdir, "logTEMPO.txt")
  
  con <- file(description = logTEMPO, open = "a")
  
  requestName <- strsplit(request$url,  "/")
  
  requestName <- unlist(requestName)
  
  requestName <- requestName[length(requestName)]
  
  
  if(request$status_code == 200){
    
    m <- paste0("INFO: ", request$url, "| Status: ",  request$status_code,  "| Matrix: ", requestName,  "| TIME: ", Sys.time())
    
    writeLines(text = m, con = con, sep = "\n")
  
  } else{
    
    m <- paste0("ERROR: ", request$url, "| Status: ",  request$status_code,  "| Matrix: ", requestName,  "| TIME: ", Sys.time())
    
    writeLines(text = m, con = con, sep = "\n")
  }
  
  close(con)
}



df.funname <- function(df.name = NULL){
  df.name <- data.frame()
  return(df.name)
}