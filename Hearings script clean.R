#Guide to scraping the hearings for info on FN-Sambandet

#library in relevant packages
library(tidyverse)
library(tidytext)
install.packages("quanteda")
library(quanteda)
install.packages("stm")
library(stm)
library(tidyr)

#library in the stortingpackage
library(stortingscrape)

#there are three relevant functions for hearings. With the questionmark we learn more about them
?get_session_hearings()
?get_hearing_input()
?get_written_hearing_input()

#before running one of these functions, we want to get all parliament sessions and specify that we only scrape from 2011 onwards
#therefore we create an object for the sessions and get all parliament sessions
sessions_storting <- get_parlsessions()

#as you now see, the sessions appear as an object in your environment. Let's filter out the relevant years
#filter out the sessions after 2011, which have relevant data
sessions_storting <- sessions_storting %>%
  filter(id %in% c("2011-2012", "2012-2013", "2013-2014",
                   "2014-2015", "2015-2016", "2016-2017",
                   "2017-2018", "2018-2019", "2019-2020", 
                   "2020-2021", "2021-2022"))

# now we are creating three lists to fill in information afterwards. We create three lists, as we have three relevant functions to run.
a <- list()
b <- list()
c <- list()

#you can now see the three empty lists in your environment, let's fill them

#now we can run a for loop for all three relevant functions

#We need to start with get_session_hearings, as we need hearing ids for the other functions.
#run a for-loop to retrieve session hearings. I also added the progress bar
for(x in unique(sessions_storting$id)){
  it <- 100*(which(unique(sessions_storting$id) == x) / length(unique(sessions_storting$id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  a[[x]] <- get_session_hearings(sessionid = x, good_manners = 0, cores = 1)
}

#place the retrieved data from list a in a new object
allsessionshearings <- do.call(rbind, a)

?unnest
#unnesting the data into columns that we can work with
allsessionshearings <- allsessionshearings %>% as_tibble() %>% 
  unnest(cols = root, hearing, hearing_case_info, hearing_date )

#no we find a large dataset with a lot of variables and a good overview over hearings in our allsessionhearings object
#as we now know the hearing ids, we can run the other two functions

?get_hearing_input
#First, running a loop to retrieve hearing inputs. I am amending the loop according to where the hearing id is found and what arguments the function demands me to specify.
#I am also directing the data towards list b.
#as it returns an error message, we use the "try" function
for(x in unique(allsessionshearings$hearing_id)){
  it <- 100*(which(unique(allsessionshearings$hearing_id) == x) / length(unique(allsessionshearings$hearing_id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  try(b[[x]] <- get_hearing_input(hearingid = x, good_manners = 0), silent = TRUE)
}

#place the retrieved data from list b into another object we can work with
hearinginput <- do.call(rbind, b)

#unnesting again. I can choose which variables are relevant and leave the rest out. In case I want to include e.g. response date at a later stage, I have to amend the code here.
#You can check all variables in the hearinginput object
hearinginput <- hearinginput %>% as_tibble() %>% 
  unnest(cols = hearing_id, committee_id, hearing_input_organization, hearing_input_title, hearing_input_text)

?get_written_hearing_input
#Second, running a loop to retrieve written hearing input. Remember amending for the data to be put in list c.
#For some reason, the loop only returns NA values - quickly stop the execution of the loop!! Nneed to investigate why! Maybe many hearings don't have written hearing input?
#or the function is not really relevant as it only retrieves the same variables as get_hearing_input (input text and title most relevant)
for(x in unique(allsessionshearings$hearing_id)){
  it <- 100*(which(unique(allsessionshearings$hearing_id) == x) / length(unique(allsessionshearings$hearing_id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  c[[x]] <- get_written_hearing_input(hearingid = x, good_manners = 0)
}

#let's save the dataset we have obtained before continuing
getwd()
save(allsessionshearings, file = "C:/Users/hanne/Desktop/Coding R and Python/summer course/Group-3-ISSSV1337/cleaned docs/allsessionhearings.RData")
save(hearinginput, file = "C:/Users/hanne/Desktop/Coding R and Python/summer course/Group-3-ISSSV1337/cleaned docs/hearinginput.RData")

#for now I will continue especially with the data from hearinginput, which has the hearing title and text to see where UNA is mentioned

#loading relevant packages again
library(tidyverse)

#creating a new object by selecting relevant variables and merging text and titles of hearings into one variable
hearingtext <- hearinginput %>% 
  select(hearing_id, hearing_input_organization, committee_id, hearing_input_date, hearing_input_text, hearing_input_title) %>% 
  mutate(fulltext = paste(hearing_input_title, hearing_input_text))

#now we can use the fulltext variable to check for FN sambandet and other keywords
mentionsFNS <- hearingtext %>% 
  filter(str_detect(fulltext, "FN-sambandet"))
#we observe three mentions in hearings


library(stringr)

sum(str_detect(hearingtext$fulltext,"fn-sambandet"))
#O results

sum(str_detect(hearingtext$fulltext, "Utdanning for Bærekraftig Utvikling"))
#0 results

sum(str_detect(hearingtext$fulltext, "FN-sambandet"))
#3 results

#this next code prints all the text input that contains "4.7"
hearingtext$fulltext[str_which(hearingtext$fulltext, "4.7")]

#this string detects whether or not an input contains "Samfunnsøokonomisk"
str_detect(hearingtext$fulltext,"Samfunnsøkonomisk")
sum(str_detect(hearingtext$fulltext, "Samfunnsøkonomisk"))
#26 mentions

#trying to plot all these mentions, we create a subset that unites all the keyword searches
keywordmentions <- hearingtext$fulltext %>% 
  filter(str_detect(fulltext, "FN-sambandet")|
           str_detect(fulltext, "Samfunnsøkonomisk")|
           str_detect(fulltext, "4.7")|
           str_detect(fulltext, "utviklingsmål"))
