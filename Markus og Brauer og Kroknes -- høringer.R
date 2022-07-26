#Hearings in parliament

install.packages("stortingscrape")

library(stortingscrape)

library(tidyverse)


sessions_storting <- get_parlsessions()

sessions_storting <- sessions_storting %>%
  filter(id %in% c("2011-2012", "2012-2013", "2013-2014",
                   "2014-2015", "2015-2016", "2016-2017",
                   "2017-2018", "2018-2019", "2019-2020", 
                   "2020-2021", "2021-2022"))

get_session_hearings()

## The id's are then entered into this loop, which *should* retrieve 
## ALL question id's for our relevant period. 



10004634:10003330


V2sessions <- sessions_storting

rm(V2sessions)






a<-list()



?get_session_hearings


for(x in unique(sessions_storting$id)){
  a[[x]] <- get_session_hearings(sessionid = x, good_manners = 0, cores = 1)
  #paste0("stortingsporsmal", x) = rbind(a, b, c)
}

#Er dette en loop? Hva gjør denne koden her? Hvordan funker dette? 

allhearings <- do.call(rbind, a)

?rbind
#Why not cbind? 

allhearings_new <- allhearings

allhearings_new <- allhearings %>% as_tibble() %>% 
  unnest()
#unnest() does not seem to fix this select-problem
#hearing_case_info is a list of 5 variables - what if I take out the discrete variables?


hearing_case_info <- hearing_case_info %>% hearing_case_info %>% 
  select(case_reference, case_id)

hearing_case_info <- unnest(allhearings_new$hearing_case_info) 


allhearings_new

#retrive hearing ids and extract text into a data set
weird_hearingtest <- get_hearing_input(hearingid = "10004481", good_manners = 0) %>% select(hearing_input_text)


weird_hearingtest <- get_hearing_input(hearingid = "10004481", good_manners = 0)




weird_hearingtest

weird_hearingtest <- get_written_hearing_input(hearingid = "10004481", good_manners = 0)

WEIRDESTOFWEIRD <- get_written_hearing_input(hearingid = c("10004432", "10004433", "10004434", "10004435", "10004436", "10004437", "10004438", "10004439"),good_manners = 0)

d <- list()
for(x in unique(allhearings_new$hearing_id)){
  it <- 100*(which(unique(allhearings_new$hearing_id) == x) / length(unique(allhearings_new$hearing_id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  d[[x]] <- get_written_hearing_input(hearingid = x, good_manners = 0) %>% select(hearing_in)
}



dkopi <- tibble(d) %>% select(hearing_info_text)




#Error in parse_url(url) : length(url) == 1 is not TRUE --- why? 

d$hearing_input_text

weird_hearingtest$hearing_id

allhearings_new$hearing_id

weird_hearingtest$hearing_input_text


#try all different hearing commands
get_hearing_program
get_hearing_input
get_written_hearing_input



a[["2021-2022"]][["hearing"]][["hearing_id"]]

## Next step is to take these id's from the resulting dataframes, using 
## get_question, to get all the actual text of responses. 

hearinglista <- do.call("rbind", a)


clist <- list(hearinglista)

hearinglist <- do.call("rbind", clist)

d <- list()

for(x in unique(hearinglist$id)){
  it <- 100*(which(unique(questionlist$id) == x) / length(unique(hearinglist$id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  d[[x]] <- get_question(questionid = x, good_manners = 0)
  #paste0("stortingsporsmal", x) = rbind(a, b, c)
}


b <- list()
for(x in unique(allhearings_new$hearing_case_info)){try(b[[x]]<-get_hearing_input(hearingid = x,good_manners = 0),silent = TRUE)
  }

