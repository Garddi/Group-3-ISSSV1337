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

#we are observing that data from before the 2020-2021 sessions is missing
#we continue with the input data we can extract

#this is just a test for the last hearing that returns data
get_hearing_input(hearingid = "10004183")

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

#place retrievend data in list c
written_hearing_input <- do.call(rbind, c)

#unnesting data
written_hearing_input <- written_hearing_input %>% as_tibble() %>% 
  unnest(cols = hearing_id, committee_id, hearing_input_organization, hearing_input_title, hearing_input_text)

#let's save the dataset we have obtained before continuing
getwd()
save(allsessionshearings, file = "C:/Users/hanne/Desktop/Coding R and Python/summer course/Group-3-ISSSV1337/cleaned docs/allsessionhearings.RData")
save(hearinginput, file = "C:/Users/hanne/Desktop/Coding R and Python/summer course/Group-3-ISSSV1337/cleaned docs/hearinginput.RData")
save(written_hearing_input, file = "C:/Users/hanne/Desktop/Coding R and Python/summer course/Group-3-ISSSV1337/cleaned docs/written_hearing_input.RData")

load(file = "C:/Users/hanne/Desktop/Coding R and Python/summer course/Group-3-ISSSV1337/cleaned docs/allsessionhearings.RData")
load(file = "C:/Users/hanne/Desktop/Coding R and Python/summer course/Group-3-ISSSV1337/cleaned docs/hearinginput.RData")
load(file = "C:/Users/hanne/Desktop/Coding R and Python/summer course/Group-3-ISSSV1337/cleaned docs/written_hearing_input.RData")
#for now I will continue especially with the data from hearinginput, which has the hearing title and text to see where UNA is mentioned

#loading relevant packages again
library(tidyverse)

#creating a new object by selecting relevant variables and merging text and titles of hearings into one variable
hearingtext <- hearinginput %>% 
  select(hearing_id, hearing_input_organization, committee_id, hearing_input_date, hearing_input_text, hearing_input_title) %>% 
  mutate(fulltext = paste(hearing_input_title, hearing_input_text))

#Test: now we can use the fulltext variable to check for FN sambandet and other keywords
mentionsFNS <- hearingtext %>% 
  filter(str_detect(fulltext, "FN-sambandet"))
#we observe three mentions in hearings

#In a more professional manner we first homogenize the text to make our search easier
library(stringr)

#changing all text to lower case and removing html patterns
hearingtext <- hearingtext %>% 
  mutate(fulltext_lo = str_remove_all(str_to_lower(fulltext), pattern = "<p>"))

#removing all text that is written in <> parentheses (leftover html code)
hearingtext <- hearingtext %>% 
  mutate(fulltext_lo = str_remove_all(fulltext_lo, pattern = "<.*>"), 
         fulltext_lo = str_replace_all(fulltext_lo, pattern =  "\\s*\\([^\\)]+\\\\", ""))

#Test: we can now check for keywords
sum(str_detect(hearingtext$fulltext_lo,"fn-sambandet"))
#1 results 
#we observe less mentions here, as only hearing text and title are searched (FN Sambandet as the hearing organisation is omitted)

sum(str_detect(hearingtext$fulltext_lo, "utdanning for bærekraftig utvikling"))
#0 results

#this next code prints all the text input that contains "4.7"
hearingtext$fulltext[str_which(hearingtext$fulltext_lo, "4.7")]

#this string detects whether or not an input contains "samfunnsøokonomisk"
str_detect(hearingtext$fulltext_lo,"samfunnsøkonomisk")
# and this one counts the mentions again
sum(str_detect(hearingtext$fulltext_lo, "samfunnsøkonomisk"))
#53 mentions

#Now we prepare plotting by creating new variables that unite all the keyword searches after priority
keywordmentions <- hearingtext %>% 
  mutate(priority_level = case_when(
    str_detect(fulltext_lo, "fn-sambandet") ~ "1",
    str_detect(fulltext_lo, " una ") ~ "1", #find another funtion as this find all words that include "una"
    str_detect(fulltext_lo, "united nations association") ~ "1",
    str_detect(fulltext_lo, "fn sambandet") ~ "1",
    str_detect(fulltext_lo, " ubu ") ~ "2",
    str_detect(fulltext_lo, "utdanning for bærekraftig utvikling") ~ "2",
    str_detect(fulltext_lo, "utdanning i bærekraftig utvikling") ~ "2",
    str_detect(fulltext_lo, "utdanning om bærekraftig utvikling") ~ "2",
    str_detect(fulltext_lo, "education for sustainable development") ~ "2",
    str_detect(fulltext_lo, "education for sustainability") ~ "2",
    str_detect(fulltext_lo, " esd ") ~ "2",
    str_detect(fulltext_lo, "delmål 4.7") ~ "2",
    str_detect(fulltext_lo, "bærekraftsmål 4") ~ "2",
    str_detect(fulltext_lo, " sdg 4 ") ~ "2",
    str_detect(fulltext_lo, " unicef ") ~ "3",
    str_detect(fulltext_lo, " ilo ") ~ "3",
    str_detect(fulltext_lo, " unesco ") ~ "3",
    str_detect(fulltext_lo, " who ") ~ "3",
    str_detect(fulltext_lo, " wfp ") ~ "3",
    str_detect(fulltext_lo, " undp ") ~ "3",
    str_detect(fulltext_lo, " unep ") ~ "3",
    str_detect(fulltext_lo, " fao ") ~ "3",
  ))
#keywordmentions includes NAs and is therefore not fit for presentation

  
#therefore we create a subset that only contains the rows with relevant mentions
keyword_subset <- keywordmentions %>% 
  filter(str_detect(fulltext_lo, "fn-sambandet")|
           str_detect(fulltext_lo, " una ")| #find another funtion as this find all words that include "una"
           str_detect(fulltext_lo, "united nations association")|
           str_detect(fulltext_lo, "fn sambandet")|
           str_detect(fulltext_lo, " ubu ")|
           str_detect(fulltext_lo, "utdanning for bærekraftig utvikling")|
           str_detect(fulltext_lo, "utdanning i bærekraftig utvikling")|
           str_detect(fulltext_lo, "utdanning om bærekraftig utvikling")|
           str_detect(fulltext_lo, "education for sustainable development")|
           str_detect(fulltext_lo, "education for sustainability")|
           str_detect(fulltext_lo, " esd ")|
           str_detect(fulltext_lo, "delmål 4.7")|
           str_detect(fulltext_lo, "bærekraftsmål 4")|
           str_detect(fulltext_lo, " sdg 4 ")|
           str_detect(fulltext_lo, " ilo ")|
           str_detect(fulltext_lo, " unesco ")|
           str_detect(fulltext_lo, " who ")|
           str_detect(fulltext_lo, " fao ")|
           str_detect(fulltext_lo, " wfp ")|
           str_detect(fulltext_lo, " undp ")|
           str_detect(fulltext_lo, " unep ")|
           str_detect(fulltext_lo, " unicef "))
  
#now we create a full subset called "subfullset" in which we include hearing ids beside the keyword_subset variables
#changing the full subset to contain hearing ids and session ids selected from allsessionhearings dataset
?left_join
subfullset <- allsessionshearings %>% 
  select(hearing_id, session_id)

#now we join the full subset together with the keyword_subset
keyword_subset <- left_join(keyword_subset, subfullset, by = c("hearing_id"))

#this table is just to 
table(allsessionshearings$session_id)


#with newkeywordmentions containing session ids and keywordmentions we can now plot

#now we are trying to plot these variables
ggplot(keywordmentions, aes(x = committee_id, fill = priority_level)) + 
  geom_bar() + 
  theme_bw()
#keywordmentions includes NAs and is therefore not fit for presentation


#creating a color palette for colorblind people as an object for later use
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", 
                             "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", 
                             "#661100", "#6699CC", "#888888")

#plotting only the relevant keyword mentions after priority
ggplot(keyword_subset, aes(x = session_id, fill = priority_level)) + 
  geom_bar(position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Keyword Mentions in Hearings",
       x = "Storting Sessions",
       y = "Number of Mentions",
       fill = "Priority level",
       caption = "Mentions after priority level: 
       1 - direct mentions of UNA and related terms 
       2 - mentions of terms related to SDG 4.7 on education
       3 - mentions of associated UN Organisations") +
  theme(legend.position = "right", legend.fill = "Priority Level",
        plot.caption.position = "plot") +
  theme_light() +
  coord_cartesian(ylim = c(0,10))


 

