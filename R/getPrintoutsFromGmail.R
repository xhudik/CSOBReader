
#' Download CSOB printouts from Gmail
#' 
#' This function can be called from an environment which has a browser (for authenitification). Once the user is authentificated, 
#' his gmail account is searched for particular messages based on the input arguments.
#' 
#' @param account  your CSOB account
#' @param year for what year printouts should be downloaded
#' @param subject Subject of printouts (usuallym default is fine)
#' @return A tibble with all transactions
#' @examples
#' \dontrun{
#' getPrintoutFromGmail(account = "173961977", year = 2018)
#' }
#' 
#' @import tibble gmailr
#' @export
getPrintoutFromGmail <- function(account = "173961977", year = 2017, subject = "ČSOB Info 24 - Výpis z účtu"){

  after <- paste0(year,"/01/01")
  before <- paste0(year+1,"/01/01")
  gmail_search_query = paste0("after:", after, " AND before:", before, " AND subject:\"", subject, "\" AND has:attachment AND ", account)
  
  #get ID of messages that satify conditions(gmail_search_query)
  ids = gmailr::id(gmailr::messages(search = gmail_search_query))
  
  #get the messages/mails
  m <- sapply(ids,function(x){gmailr::message(x, format = "full")}, simplify = FALSE, USE.NAMES = FALSE)
  
  #get only mails with relevant attachments
  relev_mails <- sapply(m, function(x){
    #take attachment filename
    fname <- x$payload$parts[[2]]$filename; 
    #match only those that are needed
    mfname <- stringr::str_match(fname, paste0(".*",account,".*.txt"));
    #return matched mail or NA
    if(is.na(mfname)) NA
    else x
  }
  )
  
  #create a temp dir
  dir <- tempdir()
  
  #save attachments into dir
  files <- sapply(relev_mails,function(x){
    #take only gmail_messages
    if(is(x,"gmail_message")){
      gmailr::save_attachments(x,path = dir)
    }
  })
  
  #get all attachments as files
  files <- list.files(pattern = "\\.txt$",path = dir)
  
  printout <- tibble()
  
  for(f in files){
    cat(paste0("Processing:",dir,"/",f,"\n"))
    tmpfile <- tempfile(pattern = paste0(f,".tmp"))
    system(paste0("iconv -f cp1250 -t utf-8 ",dir,"/",f," | dos2unix > ", tmpfile))
    
    df <- parse_CSOBprintout(file = tmpfile)  
    printout <- dplyr::bind_rows(printout,df)
    
    #delete the file
    file.remove(tmpfile)
    file.remove(paste0(dir,"/",f))
    
  }
  
  printout
}
