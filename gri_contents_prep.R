#install.packages("devtools")
#library(devtools)
#devtools::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))

library(magrittr)
library(stringi)
library(stringr)
library(rJava)
library(tabulizer)
library(tidyverse)

getwd()
setwd("../")

# Try to get contents table



gri_list_df <- read.csv("reports/GriTempUrlList2.csv", stringsAsFactors=FALSE)
saving_dir <- "reports"
gri_list_df<-gri_list_df %>% mutate(fullpath=str_c(saving_dir, gri_list_df$folder,gri_list_df$Publication_Year,gri_list_df$filename, sep = "/"))


reg_pn <- regex("[[:alpha:]]")
gri_list_df<-gri_list_df %>% filter(gri_list_df$type_dld=="application/pdf", !str_detect(gri_list_df$pdf_contents_page,reg_pn), !gri_list_df$pdf_contents_page=="")


extract_one_cpage <- function(i,j){
  contents_page <- extract_tables(
    file   = gri_list_df$fullpath[i],
    pages = as.numeric(gri_list_df$pdf_contents_page[i])+j,
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
        print("page #", gri_list_df$pdf_contents_page[i], "+", j, "OK")
      },
      error = function(e){ 
        print("page #")
        print(gri_list_df$pdf_contents_page[i])
        return(NULL)
      }
    )
  }
  
  all_cp <-c(all_cp,cp)
  print("Processed")
}
  
#_____________________

gri_list_df$pdf_contents_page[1]
cp<-bind_rows(extract_one_cpage(1,0))
