#!/usr/bin/env Rscript

library(dplyr, warn.conflicts=FALSE)
library(tibble)
library(stringr)
library(optparse)



option_list <- list(
  make_option(c("-f", "--file"), type="character", 
              help="File name of CSOB printout (txt)"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
             help="Print extra output (debug purposes)")
  )


#parse options from command line
opt <- parse_args(OptionParser(option_list=option_list,
                               usage = "Usage: %prog -f CSOBprintout.txt > CSOB_1707.csv"))

#set variables based on command line options
options(echo = opt$verbose)
file <- opt$file

if((is.null(file)==TRUE) || (file.access(file) == -1)) {
  stop(print("File was not specified (-f parameter), or doesnt exists. \tExit ..."))
}

tmpfile <- tempfile(pattern = "tmp__")
system(paste0("iconv -f cp1250 -t utf-8 ",file," | dos2unix > ", tmpfile))

#read whole file as a string; file NEEDS to be converted to UTF8 and unix line ends
b <-  readChar(con = tmpfile, nchars = 100000)

#subsitute \n for a special UTf-8 - working with \n is not convenient
c <- b %>% str_replace_all(pattern = "\\n","Ξ")

header <- (c %>% str_match(pattern="(.* Odúčtováno\\s*Ξ-{10,})Ξ"))[2]
tail <-   (c %>% str_match("(Ξ-{10,}Ξ\\s+Počáteční zůstatek:.*)$"))[2]
pattern <- paste0(header,"(.*)",tail)
body <-   (c %>% str_match(pattern = pattern))[2]

#get a year and month
yearmonth <- str_match(header, "Rok/Poř.č. výpisu: (\\d{4}/\\d{1,2})")[2] %>% str_split("/",simplify = TRUE)

#collect records (each payment as a separate item)
records <- unlist(str_split(body,"-{10,}"))

originals <- records

amounts <- str_match(records, " (-?\\d{1,8},\\d{2})Ξ")[,2] %>% str_replace(",","\\.")
tmp <- str_match(records, "Ξ (\\d\\d\\.\\d\\d\\.)")[,2]
dates <- format(as.Date(paste0(tmp, yearmonth[1]),format = "%d.%m.%Y"),"%Y-%m-%d")
ids <- str_match(records, "\\s*(\\d{4})\\s+-?\\d{1,8},\\d{2}Ξ")[,2] 

#get the first line (except date, ID, amount)
tmp <- str_match(records, "Ξ \\d\\d\\.\\d\\d\\. (.*)\\s*(\\d{4})\\s+-?\\d{1,8},\\d{2}Ξ")[,2] 


types <- ifelse(str_extract(tmp,"(.{11})") == "Došlá platb", "Došlá platba",
               ifelse(str_extract(tmp,"(.{11})") == "Trvalý přík", "Trvalý příkaz elektronicky čísl",
                    ifelse(str_extract(tmp,"(.{11})") == "Bezhotovost", "Bezhotovostní převod el. bankov",
                          ifelse(str_extract(tmp,"(.{11})") == "Nezpoplatně", "Nezpoplatněná splátka úvěru",
                                ifelse(str_extract(tmp,"(.{11})") == "Za vedení ú", "Za vedení účtu, výpisy a transa",
                                    ifelse(str_extract(tmp,"(.{11})") == "Zúčtování k", "Zúčtování kladných úroků",
                                           ifelse(str_extract(tmp,"(.{11})")=="Příkaz el. ","Příkaz el. bankovnictví k úhrad",
                                                  ifelse(str_extract(tmp,"(.{11})")=="Výběr hotov","Výběr hotovosti", "Jine")
                                           )))))))

               
acc_names <- str_trim(str_replace(tmp, types, ""))


#line 2 (remove Ξ from the 1st position; match second Ξ till third )
line2_raw <- (str_replace(records,"^Ξ","") %>% str_match("Ξ\\s+(.*?)Ξ"))[,2] 
line2 <- str_split(str_trim(line2_raw),"\\s+", simplify = TRUE)
accounts <- line2[,1]
VSs <- line2[,2]
KSs <- line2[,3]
SSs <- line2[,4]


patt <- paste0("Ξ\\s+",line2_raw,"Ξ(.*)$")
tmp <- str_trim(str_match(records, pattern = patt)[,2])
comments <- tmp %>% str_replace("Ξ$","") %>%  str_trim()



db <- tibble(id=ids, date = dates, type=types, amount = amounts, acc_name = acc_names, account = accounts, VS = VSs, KS = KSs, SS = SSs, comment = comments, original = originals)

write.csv(x = db,file = stdout(),row.names = FALSE)
