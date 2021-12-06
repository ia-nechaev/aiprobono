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

source("dprep_functions.R")




getwd()
setwd("../")

gri_code <- regex("(4[0-1][0-9])(?:-[0-9])?", ignore_case = FALSE)

# Try to get the table with GRI codes and the page numbers

# gri_list_df <- read.csv("reports/GriTempUrlList2.csv", stringsAsFactors=FALSE)
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
  gic <-c()
  
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
          cp       <- c(cp, list(pagecont))
          
          names(cp)[j+1] <- pagenum
          
          print(paste("page #", pagenum, "OK "))
          
        },
        error = function(e){ 
          
          cp       <- c(cp, list("Fail to read due to error",e))
          
          #names(cp)[j+1] <- pagenum
          
          print(paste("page #", pagenum, "FAIL ", e))
          return(NULL)
        }
      )
    }
    
    # gic <-c(gic, compname=list(cp))
 
    gic <-c(gic, list(cp))
    names(gic)[i] <- compname
    print(cat(compname, "processed "))
  }
  return(gic)
}



temp_indexes<-extract_gri_indexes()






tg <-c()
j<-length(temp_indexes[["Euroclear"]][["79"]])
# tg<-get_gri_index(temp_indexes[["GEDI Spa"]], "71", 0)

length(temp_indexes[["GEDI Spa"]][[1]])

populate_all <- function(df_tp){
  tv<- c()
  df_tp <- temp_indexes
  
  for (i in 1:length(df_tp)) { # picking firms
    tv <- c()


      for (k in 1:length(df_tp[[i]])) { #picking pages
        
        if (length(df_tp[[i]][[k]])>0){
          #if (!is.na(df_tp[[i]][[k]])){
  
          tv <-c(tv,get_gri_index(df_tp[[i]][[k]])) # sending a page to a get_gri_function
        
          # names(df_tp[[i]])[k]
        
        
          # print(length(df_tp[[i]][[k]]))
        
        
        
        }else{
          tv<-c("No tables found")
        }
      }  
    
    print(names(df_tp)[i])
    print(tv)
  }
  
  return(df_tp)
}


get_gri_index <- function(data_tp){ # picking dataframes on a page
  tl<-c()
  # temp_index_table <-data_tp[[page_aschar]][[j]]
  
  # data_tp <- temp_indexes[["Euroclear"]][[3]]
  # print(length(data_tp))
  for (n in 1:length(data_tp)) {
    # print(n)
    
    temp_index_table <-data_tp[[n]]
    
    #if(!is.null(dim(temp_index_table))){
      m_result <- combine_codenpage(filter_index(temp_index_table))
      #if(is.null(m_result)){
      #  tl <- "No table to process"
      #}else{
        tl <- c(tl,m_result)
      #}
    
    #}
  
  }
  
  return(tl)
  
  # temp_indexes[["Euroclear"]] <- c(temp_indexes[["Euroclear"]],list(gri_index=combine_codenpage(filter_index(temp_index_table))))
}


yyy<-get_gri_index(temp_indexes[[13]][[3]]) # test for one page
length(temp_indexes[[3]][[1]][[2]])

yyy
mmm <-c()


for (l in 1:length(temp_indexes[[13]])) {
  mmm<-c(mmm,get_gri_index(temp_indexes[[13]][[l]]))
  print(length(temp_indexes[[13]][[l]]))
}





# testing the page search function

temp_index_table <- temp_indexes[["Euroclear"]][["79"]][[1]]

length(temp_indexes[["Euroclear"]][["79"]])

pn <- parse_number(temp_index_table[4,3])
pn <- temp_index_table[4,3]



# extracting all the numbers from a string
numextract <- function(string){
  num_regex <-regex("\\d{1,3}")
  #range_case <- regex("\\d{1,3}-\\d{1,3}")
  
  return(unlist(regmatches(string,gregexpr(num_regex,string))))
}

# pn1<-numextract(pn)
# pn1

# temp_index_table[1,]

#we assume gri codes are in the first column

filter_index <- function(table_tp){
  
  my_if_statement <- !is.null(dim(table_tp))   # nrow(table_tp)>0 && ncol(table_tp)>0
  if(my_if_statement){
    table_tp[str_detect(table_tp[,1],gri_code),]
  }else{
    return("Filtering index failed")
  }
}


# temp_index_table[str_detect(temp_index_table[,1],gri_code),]

# str_detect(temp_index_table[,1],gri_code)

choose_column <- function(table_tp){
  
  num_regex <-regex("\\d{1,3}")
  for(i in 2:ncol(table_tp)){
    
    # corresponding ideas:
    # get vector of strings from the column
    # process the numbers
    # figure out whether they are integer and lowe than 500 and might be find out the distributions

    x<-sum(str_detect(table_tp[,i],num_regex),TRUE)
    z<-nrow(table_tp)

    #print(x)
    if((!is.na(x))&&(x/z>0.5)){
      
      #for(j in 1:z){
        
      #  vector_ofallnumbers<-c(vector_ofallnumbers, numextract(table_tp[z,i]))
        
      #}
      
      # check whether they are integers
      # 
      # col_probability <- c()  
      
      return(i)
    }
  }
}


# num_regex <-regex("\\d{1,3}")
# ncol(temp_index_table)
# tstr<-str_extract_all(temp_index_table[,3],num_regex)
# if (sum(str_detect(filtered_index[,3],num_regex),TRUE)/nrow(filtered_index)>0.5) print("yes")

# k<-choose_column(filtered_index)



combine_codenpage <- function(table_tp){
  
  doc_gri_index <-c()
  
  if(!is.null(dim(table_tp))){
    k<-choose_column(table_tp)
  }else{
    return(NULL)
  }
  
  for(j in 1:nrow(table_tp)){
  
    # names(doc_gri_index)[j] <- str_extract(table_tp[j,1], gri_code)
    
    my_if_statement <- (!toString(table_tp[j,1])=="") && (!is.na(table_tp[j,1]))
    
    if (my_if_statement){
      doc_gri_index <- c(doc_gri_index, list(numextract(table_tp[j,k])))
      names(doc_gri_index)[j] <- table_tp[j,1]
    }else{
      doc_gri_index <- "No GRI code found"
      # names(doc_gri_index)[j] <- "No Code"
    }
    
  }
  
  return(doc_gri_index)
  
}


y <- combine_codenpage(filter_index(temp_indexes[[2]][[2]][[4]]))
length(temp_indexes[[2]][[2]])


dim(temp_indexes[[1]][[1]][[1]])

# prossessing GRI standard

getwd()
gs_toc <- pdf_toc("reports/standards.pdf")
gs_toc_j <- toJSON(gs_toc, auto_unbox = T, pretty = T)

gs_toc_csv <- read.csv("reports/gri_codes.csv")
gs_toc_jr <- fromJSON(gs_toc_j,simplifyDataFrame=T,flatten = T)

# gs_toc_jr$children$title[[6]] %>% glimpse()

# str(gs_toc_jr[[2]][[2]][[5]], max.level=1, list.len=10)

g400<-gs_toc_jr[[2]][[2]][[6]]

ug4<-unlist(g400)

ug4f <- ug4[str_detect(ug4,gri_code)]

rem_word <- regex("(\\w*).(4[0-1][0-9])(?:-[0-9])?..", ignore_case = FALSE)


mycolnames <-c("code", "name")
ug4df <- data.frame(str_extract(ug4f,gri_code), ug4f)
names(ug4df) <- mycolnames
ug4df$name %<>% str_remove_all(rem_word)

ug4df <-  ug4df[-c(1:19),]
getwd()
write.csv(ug4df, "reports/gri_codes.csv")

# gs_list <-ug4df

# gri_list_df$pdf_contents_page[1]+22
# cp<-bind_rows(extract_one_cpage(1,0))


### _______________________
### nested list test

nested_list <- list(l1 = list(1:5, 5:1 ),
                    l2 = list(100:105, 105:100 ),
                    l3 = list(200:205, 205:200 ))
nested_list$l1 <- c(nested_list$l1, list(10:20))
mynames <- c("com1", "com2", "com3")

setNames(nested_list,mynames)
names(nested_list) <- mynames

nested_list$com1

gic<-c()
for(i in 1:nrow(gri_list_df)){
  cp <-c()
  
  compname <- gri_list_df$Organization[i]
  # print(compname)
  
  for(j in 0:4){
    
    pagenum  <- gri_list_df$pdf_contents_page[i]+j
    cp       <- c(cp, list("pagecont"))
    names(cp)[j+1] <- pagenum
    
  }
  
  gic <-c(gic, compname=list(cp))
  names(gic)[i] <- compname
  
}

deparse(substitute(compname))
compname
as.name(compname)

substitute(eval(pagenum)) 
quote(pagenum) <- list("o here is","pagecont")
pagenum

