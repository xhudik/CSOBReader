#!/usr/bin/env Rscript

library(optparse)
library(CSOBReader)



option_list <- list(
  make_option(c("-f", "--file"), type="character", 
              help="File name of CSOB printout (txt)"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
             help="Print extra output (debug purposes)")
  )


#parse options from command line
opt_obj <- OptionParser(option_list=option_list,
             usage = "Usage: %prog -f CSOBprintout.txt > CSOB_1707.csv")
opt <- parse_args(opt_obj)

#set variables based on command line options
options(echo = opt$verbose)
file <- opt$file

if((is.null(file)==TRUE) || (file.access(file) == -1)) {
  print_help(opt_obj)
  
  stop(sprintf("File was not specified (-f parameter), or doesnt exists. \tExit ...\n", stderr()))
}

tmpfile <- tempfile(pattern = "tmp__")
system(paste0("iconv -f cp1250 -t utf-8 ",file," | dos2unix > ", tmpfile))

#get pritnout for 1 month
mprintout <- CSOBReader::parse_CSOBprintout(file = tmpfile)


