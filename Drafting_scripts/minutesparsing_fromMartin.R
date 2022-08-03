rm(list = ls())

library(stortingscrape)
library(rvest)
library(stringr)
library(dplyr)
library(pbapply);pboptions(type="timer")
library(parallel)
library(readr)

# Reading transcipt file names (locally)
transcript_files <- list.files("~/gits/hdo/hdo-transcript-search/data/", pattern = ".xml", full.names = TRUE)

# 
transcripts <- pblapply(transcript_files, function(y){
  
  # Reading file
  tmp_raw <- read_html(y)  
  
  # Extracting all elements under each case
  tmp_parsed <- tmp_raw %>% html_elements("sak") %>% html_children()
  
  test_types <- tmp_parsed %>% html_name() %>% unique()
  
  if(all(test_types %in% 
         c("sakshode", "subtit", "saktit", "handling", 
           "merknad", "votering", "voteringer", "referanse", 
           "presinnl", "presinnlegg", 
           "innlegg", "hovedinnlegg", "replikk")) == FALSE){
    stop(paste(test_types, collapse = ", "))
  }
  
  tmp_parsed <- mclapply(tmp_parsed, function(x){
    
    type <- x %>% html_name()
    
    if(type == "saktit"){
      name <- ""
      
      text <- x %>% html_text() 
    }
    
    if(type %in% c("sakshode", "subtit", "handling", "merknad", "votering", "voteringer", "referanse")){
      
      name <- ""
      element <- ifelse(type == "sakshode", "saktit",
                        ifelse(type == "subtit" | type == "merknad", "uth", 
                               ifelse(type == "handling" | type == "referanse", "a", 
                                      ifelse(type == "votering" | type == "voteringer", "merknad", NA))))
      
      text <- x %>% html_elements(element) %>% html_text() %>% 
        str_c(collapse = " ") %>% 
        str_replace_all("\\s+", " ") %>% 
        str_trim()
      
    }
    
    if(type == "presinnl" | type == "presinnlegg"){
      
      name <- "Presidenten"
      
      text <- x %>% html_elements("a") %>% html_text() %>% 
        str_replace_all("\\s+", " ") %>% 
        str_c(collapse = " ") %>% 
        str_trim()
      
    }
    
    if(type == "innlegg" | type == "hovedinnlegg" | type == "replikk"){
      name <- x %>% html_elements("navn") %>% html_text() %>% 
        str_replace_all("\\s+", " ") %>% 
        str_c(collapse = " ") %>% 
        str_trim()
      
      text <- x %>% html_elements("a") %>% html_text() %>% 
        str_replace_all("\\s+", " ") %>% 
        str_c(collapse = " ") %>% 
        str_trim()
      
    } 
    
    
    df <- tibble(name, type, text)
    
  }, mc.cores = detectCores()-2)
  
  tmp_df <- bind_rows(tmp_parsed)  
  
  tmp_df$date <- y %>% 
    str_extract("\\/\\/(.*)$") %>% 
    str_remove_all("\\/|\\.xml")
  
  return(tmp_df)
  
})

transcripts <- bind_rows(transcripts)

write_csv(transcripts, file = "data/minutes.csv")