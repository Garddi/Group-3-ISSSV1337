#Hearings in parliament

library(stortingscrape)
sessions_storting <- get_parlsessions()

sessions_storting <- sessions_storting %>%
  filter(id %in% c("2011-2012", "2012-2013", "2013-2014",
                   "2014-2015", "2015-2016", "2016-2017",
                   "2017-2018", "2018-2019", "2019-2020", 
                   "2020-2021", "2021-2022"))


## The id's are then entered into this loop, which *should* retrieve 
## ALL question id's for our relevant period. 


a<-list()

?get_session_hearings
  

for(x in unique(sessions_storting$id)){
  a[[x]] <- get_session_hearings(sessionid = x, good_manners = 0, cores = 1)
  #paste0("stortingsporsmal", x) = rbind(a, b, c)
}

allhearings <- do.call(rbind, a)



hearing_case_info <- allhearings %>% 
  select(hearing_case_info)

allhearings_new <- allhearings %>% as_tibble() %>% 
  unnest()

allhearings_new

#retrive hearing ids and extract text into a data set
weird_hearingtest <- get_hearing_input(hearingid = "10004481", good_manners = 0)
weird_hearingtest <- get_written_hearing_input(hearingid = "10004481", good_manners = 0)


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