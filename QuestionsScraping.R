##### ------- Stortingscraping all documents to create a corpus --------


## Add packages as they become necessary, edit freely
library(tidyverse)
library(stortingscrape)

get_parlsessions()


## Following codes check the functionality and resulting dataframes 
## that we can mine through some of the functions

weird_check <- get_session_cases("2014-2015", good_manners = 0, cores = 1)

weird_check$root$id[1]

get_case(caseid = "60920", good_manners = 0)

weirdercheck <- get_session_meetings("2007-2008", good_manners = 0)

topics <- get_topics(keep_sub_topics = TRUE)

subtopics <- topics$topics

get_session_publications(sessionid = "2007-2008", type = "referat", good_manners = 0)

enpub <- get_publication(publicationid = "h080130", good_manners = 0)


vedtak <- get_session_decisions(sessionid = "2021-2022", good_manners = 0)

vedtak2 <- get_vote(caseid = "40029298", good_manners = 0)



##### ------ Code Test ------

## This chunk of code reads and writes csv files of all questions of a 
## certain type in a certain session

questionstest <- get_session_questions(sessionid = "2007-2008", q_type = "interpellasjoner", status = NA, good_manners = 0)

## the actual loop over the question id's, swap out the string for local file path for
## your own

for(x in unique(questionstest$id)){
  
  it <- 100*(which(unique(questionstest$id) == x) / length(unique(questionstest$id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  if(file.exists(paste0("C:/Users/gardi/documents/data/w_quest/", x, ".csv"))) next
  
  tmp <- get_question(x, good_manners = 2)
  
  if(is.null(tmp)) next
  
  write_csv(tmp, file = paste0("C:/Users/gardi/documents/data/w_quest", x, ".csv"))
  
}

get_session_questions(sessionid = "2007-2008", q_type = c("interpellasjoner"),
                      status = NA, good_manners = 0)






####### -------Main Focus ----------

## Here we attempt to get all questions and merge into one frame with a whole 
## code.

## This gets all the session id's, the id variable in this frame can be entered
## into other loops (hopefully), to retrieve all data. 

## For now the sessions we are interested in are limited to 2011-2022

sessions_storting <- get_parlsessions()

sessions_storting <- sessions_storting %>%
  filter(id %in% c("2011-2012", "2012-2013", "2013-2014",
                   "2014-2015", "2015-2016", "2016-2017",
                   "2017-2018", "2018-2019", "2019-2020", 
                   "2020-2021", "2021-2022"))
?cat
## The id's are then entered into this loop, which *should* retrieve 
## ALL question id's for our relevant period. 

a<-list()
b<-list()
c<-list()

for(x in unique(sessions_storting$id)){
  a[[x]] <- get_session_questions(sessionid = x, q_type = "interpellasjoner", status = NA, good_manners = 0)
  b[[x]] <- get_session_questions(sessionid = x, q_type = "sporretimesporsmal", status = NA, good_manners = 0)
  c[[x]] <- get_session_questions(sessionid = x, q_type = "skriftligesporsmal", status = NA, good_manners = 0)
  
  #paste0("stortingsporsmal", x) = rbind(a, b, c)
}

## Next step is to take these id's from the resulting dataframes, using 
## get_question, to get all the actual text of responses. 

## Unlisting all of the resulting lists, they are lists of identically 
## sized dataframes.

questionlista <- do.call("rbind", a)
questionlistb <- do.call("rbind", b)
questionlistc <- do.call("rbind", c)

clist <- list(questionlista, questionlistb, questionlistc)

## Combining all for a dataframe we can fetch 

questionlist <- do.call("rbind", clist)

d <- list()

for(x in unique(questionlist$id)){
  it <- 100*(which(unique(questionlist$id) == x) / length(unique(questionlist$id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  d[[x]] <- get_question(questionid = x, good_manners = 0)
  #paste0("stortingsporsmal", x) = rbind(a, b, c)
}


questiontext <- do.call("rbind", d)
## All questions received and combined, ready for analysis.

## Next step is to find the relevant keywords, filter questions and ascertain
## the dates and attached documents that they exist in. 
library(stringr)

sum(str_detect(questiontext$question_text,"fn-sambandet"))

sum(str_detect(questiontext$question_text, "Utdanning for Bærekraftig Utvikling"))

sum(str_detect(questiontext$answer_text, "FN-sambandet"))


questiontext$question_text[str_which(questiontext$question_text, "4.7")]

str_detect(questiontext$answer_text,"Samfunnsøkonomisk")

questionsFN <- questiontext %>%
  filter(str_detect(questiontext$question_text, "FN-sambandet"))

questionsFN$answer_text

save(questionlist, file = "Question_Data/MetadataQuestionList.Rdata")
save(questiontext, file = "Question_Data/All_Questions.Rdata")

print("æøå")

Sys.setlocale(category = "LC_ALL", "")
