
#' Download CSOB printouts from Gmail
#' @import tibble gmailr
#' @param year for what year printouts should be downloaded
#' @return A tibble with all transactions
#' @examples
#' \dontrun{
#' getPrintoutFromGmail(year = 2018)
#' }
#' 
#' @export
getPrintoutFromGmail <- function(year = 2017){
  subject = "ČSOB Info 24 - Výpis z účtu"
  account = "173961977"
  #for what year we want to get printouts (NOT IMLEMENTED YET!!!)
  year = 2017
  
  gmail_search_query = paste0("subject:", subject, " AND has:attachment AND ", account)
  
  #get ID of messages that satify conditions(gmail_search_query)
  ids = gmailr::id(messages(search = gmail_search_query))
  
  #get the messages/mails
  m <- sapply(ids,function(x){message(x, format = "full")}, simplify = FALSE, USE.NAMES = FALSE)
  
  #get only mails with relevant attachments
  relev_mails <- sapply(m, function(x){
    #take attachment filename
    fname <- x$payload$parts[[2]]$filename; 
    #match only those that are needed
    mfname <- str_match(fname, paste0(".*",account,".*.txt"));
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
      save_attachments(x,path = dir)
    }
  })
  
  #get all attachments as files
  files <- list.files(pattern = "\\.txt$",path = dir)
  
  #read function for parsing CSOB printouts 
  source("/home/tomas/meee/koterka/domovnik/CSOB_reader/parse_CSOBprintout.R")
  
  printout <- tibble()
  
  for(f in files){
    cat(paste0("Processing:",dir,"/",f,"\n"))
    tmpfile <- tempfile(pattern = paste0(f,".tmp"))
    system(paste0("iconv -f cp1250 -t utf-8 ",dir,"/",f," | dos2unix > ", tmpfile))
    
    df <- parse_CSOBprintout(file = tmpfile)  
    printout <- bind_rows(printout,df)
    
  }
  
  printout
}
