library(tidyverse)
library(magrittr)
library(stringi)
library(stringr)
library(readtext)
library(dplyr)
library(curl)

#read the database from gri website
gri_rep_df <- read.csv("reports/GRIStandardsReportsListEncGround.csv")

gri_rep_df %>% glimpse

#get rid of awkward letters and signs from the names of organizations
gri_rep_df <- gri_rep_df %>% 
  mutate(Organization=iconv(Organization, from="UTF-8", to="ASCII//TRANSLIT"),
         Organization_Legal_Name=iconv(Organization_Legal_Name, from="UTF-8", to="ASCII//TRANSLIT"))

#get rid of spaces in the names
gri_rep_df <- gri_rep_df %>% 
  mutate(Organization=str_squish(Organization),
         Organization_Legal_Name=str_squish(Organization_Legal_Name))

gri_rep_df %>% filter(!Report_PDF_Address=="") %>% count
gri_rep_df %>% group_by(Organization) %>% summarize()

# make the list of pdf links to download
url_list <- gri_rep_df %>% filter(!Report_PDF_Address=="") %>% filter(!is.na(Organization))

# make the html report list
#rem_pdf <- regex("(\\.pdf)", ignore_case = TRUE )
#hurl_list <- gri_rep_df %>% filter(!Report_HTML_Address=="", !str_detect(Report_HTML_Address,rem_pdf))
#hurl_list %<>% mutate(Report_Title=iconv(Report_Title,from="UTF-8", to="ASCII//TRANSLIT"))

# remove p. page pages in pdf contents column
rem_letters <- regex("(?:p\\. |page.? |p )", ignore_case = TRUE )

url_list <- url_list %>% mutate(pdf_contents_page=str_remove(Content_index_location_PDF, rem_letters))
url_list <-url_list %>% mutate(Report_Title=iconv(Report_Title,from="UTF-8", to="ASCII//TRANSLIT"))

url_list$Organization_Legal_Name %>% is.na() %>% sum()

getwd()
saving_dir <- "reports/"
path_to_save <- str_c(getwd(), saving_dir, sep = "/")
print(str_c("Saving to ", path_to_save))

# saving reports in a company name folder 

# making a test list of urls and paths
#ran_gen_id <- floor(runif(30, min=1, max=3443))
#ran_gen_id <- c(ran_gen_id,3168)
# temp_url_list <- url_list %>% slice(ran_gen_id) #dplyr solution
# temp_url_list <- url_list[ran_gen_id,]


rem_punct <- regex("[[:punct:]]")

# making folder and filename columns
url_list %<>% mutate(
  folder=str_replace_all(
            str_squish(
              str_sub(
                str_to_lower(
                  str_remove_all(Organization, rem_punct), locale = "en"),
              start=1, end=20)
            ),
          " ", "_"))

url_list %<>% mutate(
  ffolder=str_c(folder,"/",Publication_Year))

url_list %<>% mutate(Report_Title=ifelse(is.na(Report_Title),url_list$Organization,Report_Title))

#url_list$Report_Title %>% is.na() %>% sum() 

url_list %<>% mutate(
  filename=str_replace_all(
            str_squish(
              str_sub(
                str_to_lower(
                  str_remove_all(Report_Title, rem_punct), locale = "en"),
              )
            ),
            " ", "_") %>% str_c(".pdf"))


temp_url_list <- url_list[1:3432,]
temp_url_list %<>% mutate(request_status="", type_dld="", url_dld="", dld_status="") 
# setwd(path_to_save)
# dir.create(temp_url_list$ffolder[2], recursive = T)
# temp_url_list$folder[1]
# temp_url_list$Report_PDF_Address %>% is.na() %>% sum()
# temp_url_list <- temp_url_list[!(is.na(temp_url_list$Organization)),]

create_directories()

# cat(req$status_code," ", req$type,  "\n")

curl_options(filter="alive")
curl_symbols("error")

#download all reports (var 2)
setwd(path_to_save)
h <-new_handle(failonerror = F, ssl_verifyhost = F, ssl_verifypeer = F)
handle_setopt(h, use_ssl=1)
# handle_setopt(h, tcp_keepalive=F)

i<-1
j<-1

while(i<=nrow(temp_url_list)){
  out <- tryCatch(
    expr = {
      my_statement1 <-!(file.exists(str_c(temp_url_list$ffolder[i], temp_url_list$filename[i], sep = "/")))
      if(my_statement1){
        req <- curlGetHeaders(temp_url_list$Report_PDF_Address[i], redirect = TRUE, verify = FALSE)
        temp_url_list$request_status[i] <- attr(req, "status")
        # close.connection(req)
        my_statement <- (attr(req, "status")==200)
        if (my_statement){
          curl_download(temp_url_list$Report_PDF_Address[i],
                          str_c(temp_url_list$ffolder[i],
                                #temp_url_list$Publication_Year[i],
                                temp_url_list$filename[i], sep = "/"),
                          handle = h,
                          quiet = T)
          temp_url_list$dld_status[i] = "OK"
          #cat(req$status_code," ", req$type,  "\n")
          
        } else{
          temp_url_list$dld_status[i] = "ERR 1"
        }
        #closeAllConnections()
      }else{
        temp_url_list$dld_status[i] = "FE"
      }
      #cat(req$status_code," ", req$type,  "\n")
      
      
      #temp_url_list$url_dld[i]        <- req$url
      #temp_url_list$type_dld[i]       <- req$type
      cat(temp_url_list$ffolder[i], temp_url_list$request_status[i], temp_url_list$dld_status[i], "\n", sep = "  ")
      

      
    },
    error = function(e){ 
      temp_url_list$request_status[i] <- e
      temp_url_list$dld_status[i] = "ERR 2"
      cat("Error: ", temp_url_list$Organization[i], req$status_code," ", req$type,  "\n")
      return(NULL)
    },
    warning = function(w){
      # (Optional)
      # Do this if an warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
  )
  
  if(j==200){
    write.csv(temp_url_list, "GriTempUrlList3.csv")
    gc()
    j<-0
  }
  
  i<-i+1
  j<-j+1

}


create_directories <- function(){
  
  for(i in 1:nrow(temp_url_list)){
    #create company and year dirs
    setwd(path_to_save)
    if (file.exists(temp_url_list$ffolder[i])) {
      cat("The folder ", temp_url_list$ffolder[i], " already exists", "\n")
      
    } else {
      dir.create(temp_url_list$ffolder[i], recursive = T)
    }
    
  }
  
}

write.csv(temp_url_list, "GriTempUrlList2.csv")

write.csv(hurl_list, "GriUrlList.csv")




# _______________________________ DRAFTS __________________________



tmpf <-tempfile()

curl_download("https://vp292.alertir.com/afw/files/press/handelsbanken/202002135907-1.pdf", dir)

dir<-str_c(temp_url_list$folder[1],
      temp_url_list$Publication_Year[1],
      temp_url_list$filename[1],
      sep = "/")

curl_fetch_disk("https://ww3.viabcp.com/Connect/ViaBCP2019/Relaciones%20con%20Inversionistas/Reporte%20de%20Sostenibilidad%20BCP%202018%20Final.pdf",
                str_c(temp_url_list$folder[4],
                      temp_url_list$Publication_Year[4],
                      temp_url_list$filename[4], sep = "/"),
                handle = h  
)

#temp_url_list %<>% mutate(Organization) %>% 
#  str_remove(rem_punct) %>% 
#  str_to_lower() %>% 
#  str_sub(start=1, end=15)
  


# Save contents in separate csv file or might be just tabularize it?

out <- tryCatch(
  expr = {

    
  },
  error = function(e){ 

    return(NULL)
  },
  warning = function(w){
    # (Optional)
    # Do this if an warning is caught...
  },
  finally = {
    # (Optional)
    # Do this at the end before quitting the tryCatch structure...
  }
)

out <- tryCatch(
  expr = {
    
    
  },
  error = function(e){ 
    
    return(NULL)
  }
)  


### old create directory procedure (faulty)


for(i in 1:nrow(temp_url_list)){
  #create company dir
  setwd(path_to_save)
  if (file.exists(temp_url_list$ffolder[i])) {
    cat("The folder ", temp_url_list$ffolder[i], " already exists")
    
    #create year directory
    if (file.exists(toString(temp_url_list$Publication_Year[i]))) {
      cat("The folder ", temp_url_list$Publication_Year[i], " already exists")
    } else {
      dir.create(toString(temp_url_list$Publication_Year[i]))
    }
  } else {
    
    dir.create(temp_url_list$folder[i])
    setwd(str_c(path_to_save,temp_url_list$folder[i]))
    
    #create year directory
    if (file.exists(toString(temp_url_list$Publication_Year[i]))) {
      cat("The folder ", temp_url_list$Publication_Year[i], " already exists")
    } else {
      dir.create(toString(temp_url_list$Publication_Year[i]))
    }
  }
}

while(i<=nrow(temp_url_list)){
  curl_download(temp_url_list$Report_PDF_Address[i],
                str_c(temp_url_list$ffolder[i],
                      temp_url_list$filename[i], sep = "/"),
                handle = h,
                quiet = F
  )
  i<-i+1
}

curl("https://httpbin.org/status/418", "r")

req <- curlGetHeaders(temp_url_list$Report_PDF_Address[10], redirect = TRUE, verify = FALSE)
attr(req, "status")

#download all reports var 1
#setwd(path_to_save)
#for(i in 1:nrow(temp_url_list)){
#  download.file(temp_url_list$Report_PDF_Address[i],
#                str_c(temp_url_list$folder[i],
#                      temp_url_list$Publication_Year[i],
#                      temp_url_list$filename[i], sep = "/"))
#}
