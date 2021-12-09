library(magrittr)
library(stringi)
library(stringr)
library(rJava)
library(tabulizer)
library(tidyverse)
library(pdftools)
library(jsonlite)
library(rlang)

### REGEXes and constants

gri_code <- regex("(4[0-1][0-9])(?:-[0-9])?", ignore_case = FALSE)
reg_pn <- regex("[[:alpha:]]")

num_regex <-regex("\\d{1,3}")
#range_case <- regex("\\d{1,3}-\\d{1,3}")

saving_dir <- "reports"

###

#list_of_pdf_reports <-function(){
  
  # Loading the list of available reports to process (results of gri_df_prep.R)
  
  gri_list_df <- read.csv("reports/GriTempUrlList2.csv", stringsAsFactors=FALSE)
  
  # Adding the fullpath for retrieving files
  gri_list_df<-gri_list_df %>% mutate(fullpath=str_c(saving_dir, gri_list_df$folder,gri_list_df$Publication_Year,gri_list_df$filename, sep = "/") )
  
  
  # Filtering only pdfs with available GRI index page number
  gri_list_df %<>% filter(gri_list_df$type_dld=="application/pdf", !str_detect(gri_list_df$pdf_contents_page,reg_pn), !gri_list_df$pdf_contents_page=="")
  
  # Making the page number into numeric format
  gri_list_df %<>% mutate(pdf_contents_page=as.numeric(pdf_contents_page))
  
#  return(gri_list_df)
#}

#### functions to get the info from pages of reports

extract_one_cpage <- function(i,j){
  contents_page <- extract_tables(
    file   = gri_list_df$fullpath[i],
    pages = gri_list_df$pdf_contents_page[i]+j,
    method = "decide", 
    output = "data.frame")
  return(contents_page)
}

extract_gri_indexes <- function (){  # get the list of companies with pages that possibly contain gri indexes
  gic <-c()
  for(i in 1:nrow(gri_list_df)){
    cp <-c()
    compname <- gri_list_df$Organization[i]
    print(compname)
    for(j in 0:4){
      pagenum  <- gri_list_df$pdf_contents_page[i]+j
      out <- tryCatch(
        expr = {
          pagecont <- extract_one_cpage(i,j)
          cp       <- c(cp, list(pagecont))
          names(cp)[j+1] <- pagenum
          print(paste("page #", pagenum, "OK "))
        },
        error = function(e){ 
          cp       <- c(cp, list("Fail to read due to error",e))
          print(paste("page #", pagenum, "FAIL ", e))
          return(NULL)
        }
      )
    }
    gic <-c(gic, list(cp))
    names(gic)[i] <- compname
    print(cat(compname, "processed "))
    gc()
  }

  return(gic)
}





numextract <- function(string){
  # extracting all the numbers from a string
  return(unlist(regmatches(string,gregexpr(num_regex,string))))
}

get_gri_index <- function(data_tp){ # picking dataframes on a page
  tl<-c()
  for (n in 1:length(data_tp)) {
    temp_index_table <-data_tp[[n]]
    m_result <- combine_codenpage(filter_index(temp_index_table))
    tl <- c(tl,m_result)
  }
  return(tl)
}



filter_index <- function(table_tp){
  my_if_statement <- !is.null(dim(table_tp))   # nrow(table_tp)>0 && ncol(table_tp)>0
  if(my_if_statement){
    table_tp[str_detect(table_tp[,1],gri_code),] # we assume gri codes are in the first column
  }else{
    return("Filtering index failed")
  }
}

choose_column <- function(table_tp){
  num_regex <-regex("\\d{1,3}")
  for(i in 2:ncol(table_tp)){
    x<-sum(str_detect(table_tp[,i],num_regex),TRUE)
    z<-nrow(table_tp)
    if((!is.na(x))&&(x/z>0.5)){
      return(i)
    }
  }
}


combine_codenpage <- function(table_tp){
  doc_gri_index <-c()
  if(!is.null(dim(table_tp))){
    k<-choose_column(table_tp)
  }else{
    return(NULL)
  }
  for(j in 1:nrow(table_tp)){
    my_if_statement <- ((!toString(table_tp[j,1])=="") && (!is.na(table_tp[j,1])))
    if (my_if_statement){
      doc_gri_index <- c(doc_gri_index, list(numextract(table_tp[j,k])))
      names(doc_gri_index)[j] <- table_tp[j,1]
    }else{
      doc_gri_index <- "No GRI code found"
    }
  }
  return(doc_gri_index)
}


