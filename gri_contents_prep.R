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
library(rlang)

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


extract_gri_indexes <- function (){  
  all_cp <-c()
  
  # pages_nl <- list(pn=c(),pc=)
  
  for(i in 1:nrow(gri_list_df)){
    cp <-c()
    
    compname <- gri_list_df$Organization[i]
    print(compname)
    
    for(j in 0:4){
      
      pagenum  <- gri_list_df$pdf_contents_page[i]+j
      
      out <- tryCatch(
        expr = {
          
          
          pagecont <- extract_one_cpage(i,j)
          cp       <- c(cp, compname, list(pagenum, list(pagecont)))
          
          print(cat("page #", pagenum, "OK "))
          
        },
        error = function(e){ 
          cp       <- c(cp, compname, list(pagenum, list("Fail to read due to error")))
          
          print(cat("Fail: ", pagenum))
          return(NULL)
        }
      )
    }
    
    all_cp <-c(all_cp, compname, cp)
    print(cat(compname, "processed "))
  }
  return(all_cp)
}



temp_indexes<-extract_gri_indexes()










#_____________________

# prossessing GRI standard

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

gs_list <-ug4df

gri_list_df$pdf_contents_page[1]+22
cp<-bind_rows(extract_one_cpage(1,0))



### nested list test

nested_list <- list(l1 = list(1:5, 5:1 ),
                    l2 = list(100:105, 105:100 ),
                    l3 = list(200:205, 205:200 ))
nested_list$l1 <- c(nested_list$l1, list(10:20))


gic<-c()
for(i in 1:nrow(gri_list_df)){
  cp <-c()
  
  compname <- gri_list_df$Organization[i]
  # print(compname)
  
  for(j in 0:4){
    
    pagenum  <- gri_list_df$pdf_contents_page[i]+j
    cp       <- c(cp, list(pagenum,list("pagecont")))
        
  }
  
  gic <-c(gic, compname=list(cp))
  
}

deparse(substitute(compname))
compname
as.name(compname)

substitute(eval(pagenum)) 
quote(pagenum) <- list("o here is","pagecont")
pagenum
