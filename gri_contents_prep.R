#install.packages("devtools")
#library(devtools)
#devtools::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))

library(magrittr)
library(stringi)
library(stringr)
library(rJava)
library(tabulizer)
library(tidyverse)
library(pdftools)
library(jsonlite)

getwd()
setwd("../")

# Try to get the table with GRI codes and the page numbers

gri_list_df <- read.csv("reports/GriTempUrlList2.csv", stringsAsFactors=FALSE)
saving_dir <- "reports"
gri_list_df<-gri_list_df %>% mutate(fullpath=str_c(saving_dir, gri_list_df$folder,gri_list_df$Publication_Year,gri_list_df$filename, sep = "/") )


reg_pn <- regex("[[:alpha:]]")
gri_list_df %<>% filter(gri_list_df$type_dld=="application/pdf", !str_detect(gri_list_df$pdf_contents_page,reg_pn), !gri_list_df$pdf_contents_page=="")
gri_list_df %<>% mutate(pdf_contents_page=as.numeric(pdf_contents_page))

extract_one_cpage <- function(i,j){
  contents_page <- extract_tables(
    file   = gri_list_df$fullpath[i],
    pages = gri_list_df$pdf_contents_page[i]+j,
    method = "decide", 
    output = "data.frame")
  return(contents_page)
}


cp <-c()
all_cp <-c()

for(i in 1:nrow(gri_list_df)){
  cp <-c()
  print(gri_list_df$Organization[i])
  for(j in 0:4){
    out <- tryCatch(
      expr = {
        cp <- c(cp,extract_one_cpage(i,j))
        print(cat("page #", gri_list_df$pdf_contents_page[i]+j, "OK "))
      },
      error = function(e){ 
        print(cat("Fail: ",gri_list_df$pdf_contents_page[i]+j))
        return(NULL)
      }
    )
  }
  
  all_cp <-c(all_cp,cp)
  print(cat(gri_list_df$Organization[i], "processed "))
}
  
#_____________________

# proceccing GRI standard
getwd()
gs_toc <- pdf_toc("reports/standards.pdf")
gs_toc_j <- toJSON(gs_toc, auto_unbox = T, pretty = T)


gs_toc_jr <- fromJSON(gs_toc_j,simplifyDataFrame=T,flatten = T)

gs_toc_jr$children$title[[6]] %>% glimpse()

str(gs_toc_jr[[2]][[2]][[5]], max.level=1, list.len=10)

g400<-gs_toc_jr[[2]][[2]][[6]]

ug4<-unlist(g400)

gri_code <- regex("(4[0-1][0-9])(?:-[0-9])?", ignore_case = FALSE)

ug4f <- ug4[str_detect(ug4,gri_code)]

rem_word <- regex("(\w*).(4[0-1][0-9])(?:-[0-9])?..", ignore_case = FALSE)


mycolnames <-c("code", "name")
ug4df <- data.frame(str_extract(ug4f,gri_code), ug4f)
names(ug4df) <- mycolnames
ug4df$name %<>% str_remove_all(rem_word)

ug4df <-  ug4df[-c(1:19),]
getwd()
write.csv(ug4df, "reports/gri_codes.csv")

gri_list_df$pdf_contents_page[1]+22
cp<-bind_rows(extract_one_cpage(1,0))
