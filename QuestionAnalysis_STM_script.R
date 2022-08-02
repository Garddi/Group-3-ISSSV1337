###### ---------Script for analysing data -------------

load("Question_Data/MetadataQuestionList.Rdata")
load("Question_Data/All_Questions.Rdata")

Sys.getlocale()
Sys.setlocale(category = "LC_ALL", "")
print("æøå")

library(stortingscrape)
library(tidyverse)
library(tidytext)
library(quanteda)
library(stm)
library(tidyr)
library(viridis)

### This script provides an outline into a structured topic modelling 
### It can be modified to run more or less documents.


## First step is to clean out our data, I try here to limit the amount
## of columns in the dataframe i work with, so it takes less space
## in the resulting token dataset.

## since the answers are in the same rows as the questions, I first
## isolate the answer texts.

load("Question_Data/All_Questions.Rdata")

answerlist <- questiontext %>%
  select(id, text = answer_text, title, type, question_from_id, 
         qustion_to_id)

## Dropping the observations with no text, note that no text in 
## Stortingscrape is coded as empty string, not as an NA

answerlist <- answerlist %>%
  mutate(notapplicable = ifelse(text == "", NA, 1)) %>%
  drop_na(notapplicable) %>%
  select(-notapplicable)

## Limiting the dataset

questionshort <- questiontext %>%
  select(id, text = question_text, title, type, question_from_id,
         qustion_to_id)

## Binding the sets together, so I have one dataframe with complete
## texts from either answers or questions. **OF NOTE** I should probably
## have added a string to the id's so that i separate answer texts 
## from question texts, given their identical id.

assembledtextquestions <- rbind(questionshort, answerlist)

## removing reduntant objects

rm(answerlist, questionlist, questionshort, questiontext)

## First i turn all strings into lower case

assembledtextquestions <- assembledtextquestions %>% 
  mutate(text_l = str_to_lower(text))

## Before tokenizing I create a stopword dataset, I add br, because it
## is not properly removed from some observations. 

stop_words <- get_stopwords(language = "no")

removeword <- data.frame(word = "br", lexicon = "own library")

stop_words <- rbind(stop_words, removeword)

## Next I tokennize the text, this takes a moment. 

questiontokens <- assembledtextquestions %>%
  unnest_tokens(input = text, # Which variable to get the text from
                output = word, # What the new variable should be called
                token = "words") # Type of tokens to split the text into

## Removing all stopwords from my stop words book

questiontokens <- questiontokens %>%
  anti_join(stop_words, by = "word") # Joining against the stopwords dataframe to get rid of cells with stopwords


## Brief category investigation, stock companies are frequent

questiontokens %>%
  count(id, word, sort = TRUE)

## Stemming the tokens, note specification of language is needed

questiontokens <- questiontokens %>%
  mutate(stem = wordStem(word, language = "norwegian"))

questiontokens_dfm <- questiontokens %>%
  count(id, stem, name = "count") %>% # By default, the count-variable is called "n". Use name = "" to change it.
  cast_dfm(id, # Specify the douments in your analysis (becoming the rows)
           stem, # Specify the tokens in your analysis (becoming the columns)
           count) # Specify the number of times each token shows up in each document (becoming the cells)

## Saving these objects, ready for analysis, note that the tokens dataframe
## is far too large to be uploaded to GitHub

save(questiontokens_dfm, file = "Question_Data/questionsdfm.Rdata")
save(questiontokens, file = "Question_Data/questiontokens.Rdata")

###### --------- Structural Topic Modelling for K = 8 -------------

questiontokens_lda <- stm(questiontokens_dfm, # Use a document feature matrix as input
                    init.type = "LDA", # Specify LDA as the type of model
                    K = 8, # Specify the number of topics to estimate
                    seed = 910, # Make the model reproducible - does not work so well :/
                    verbose = FALSE) # Set to FALSE to avoid lots of text while running the model


questiontopics <- tidy(questiontokens_lda, 
                       matrix = "beta")


questiontopics_group <- questiontopics %>%
  group_by(topic) %>% # Getting the top term per topic, thus using group_by
  slice_max(beta, n = 10) %>% # Fetching the 10 terms with the highest beta
  ungroup() # Ungrouping to get the dataframe back to normal

questiontopics_group

questiontopics_group %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

save(questiontokens_lda, file = "Question_Analysis/QuestionSTM_K8.RData")

rm(questiontokens_lda, questiontopics, questiontopics_group)


###### ------------ Structured topical model with K = 75 -----------

## Forewarning, this step takes some 3-4 hours to complete
## depending on your pc's processing power

questiontokens_lda_75 <- stm(questiontokens_dfm,
                             init.type = "LDA",
                             K = 75,
                             seed = 910,
                             verbose = TRUE, 
                             max.em.its = 100, ## You can adjust these numbers
                             emtol = 1e-5) ## However, too high numbers will drastically increase computing time


save(questiontokens_lda_75, file = "Question_Analysis/QuestionSTM_K75.Rdata")


questiontopics_75 <- tidy(questiontokens_lda_75, 
                       matrix = "beta")


questiontopics_group_75 <- questiontopics_75 %>%
  group_by(topic) %>% # Getting the top term per topic, thus using group_by
  slice_max(beta, n = 10) %>% # Fetching the 10 terms with the highest beta
  ungroup() # Ungrouping to get the dataframe back to normal

questiontopics_group_75

questiontopics_group_75_1 <- questiontopics_group_75 %>%
  filter(topic %in% c(1:8))

questiontopics_group_75_2 <- questiontopics_group_75 %>%
  filter(topic %in% c(9:15))

questiontopics_group_75_3 <- questiontopics_group_75 %>%
  filter(topic %in% c(16:23))

questiontopics_group_75_4 <- questiontopics_group_75 %>%
  filter(topic %in% c(24:30))

questiontopics_group_75_5 <- questiontopics_group_75 %>%
  filter(topic %in% c(31:38))

questiontopics_group_75_6 <- questiontopics_group_75 %>%
  filter(topic %in% c(39:45))

questiontopics_group_75_7 <- questiontopics_group_75 %>%
  filter(topic %in% c(46:53))

questiontopics_group_75_8 <- questiontopics_group_75 %>%
  filter(topic %in% c(54:60))

questiontopics_group_75_9 <- questiontopics_group_75 %>%
  filter(topic %in% c(61:68))

questiontopics_group_75_10 <- questiontopics_group_75 %>%
  filter(topic %in% c(68:75))


questiontopics_group_75_1 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically


questiontopics_group_75_2 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

questiontopics_group_75_3 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

questiontopics_group_75_4 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

questiontopics_group_75_5 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

questiontopics_group_75_6 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

questiontopics_group_75_7 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

questiontopics_group_75_8 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

questiontopics_group_75_9 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

questiontopics_group_75_10 %>%
  ggplot(aes(term, beta, fill = topic)) + # Plotting the terms, the beta values (probabilities) and coloring them after topic
  geom_bar(stat = "identity") + # Creating a bar chart and making the y-axis the same as beta
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") + # Make the y-axes so that they can range freely, i.e. do not depend on each other
  labs(x = "", y = "Word-Topic probability") + # Adding names to the x-axis and y-axis
  theme_bw() + # Making the color background white
  theme(legend.position = "none", # Removing the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Making the text on the x-axis appear vertically

questiontokens_lda_75


question_doc_prob_75 <- tidy(questiontokens_lda_75, matrix = "gamma", # Calculating document probabilities
                    document_names = rownames(questiontokens_dfm)) # Adding the names of the songs to the dataframe

### Isolating the top documents that charge on topic 47 

topic47docs <- question_doc_prob_75 %>% 
  filter(topic == 47) %>% 
  slice_max(order_by = gamma, n = 650) 

summary(topic47docs$gamma)

### Here i create topic assignment for each document

top_docs <- question_doc_prob_75 %>%
  group_by(document) %>% # Find the next statistic per document
  slice_max(gamma, n = 3) # Find the max value

topic_assignment <- top_docs %>%
  group_by(document) %>%
  summarise(TopTopic = first(topic),
            SecTopic = nth(topic, 2),
            ThirdTopic = nth(topic, 3),
            TopGamma = first(gamma),
            SecGamma = nth(gamma, 2), 
            ThirdGamma = nth(gamma, 3))

topic_assignment <- topic_assignment %>%
  rename(id = document)

## Merging this with the relevant questions 

load("Question_Data/All_Questions.Rdata")

questiontext <- questiontext %>%
  mutate(text_q_l = str_to_lower(question_text),
         text_a_l = str_to_lower(answer_text),
         justification_l = str_to_lower(justification))

textrelevance <- questiontext %>%
  filter(str_detect(text_q_l, "fn-sambandet") |
           str_detect(text_q_l, "utdanning for bærekraftig utvikling") |
           str_detect(text_q_l, "utdanning for berekraftig utvikling") |
           str_detect(text_q_l, "unicef") | 
           str_detect(text_q_l, "wfp") | 
           str_detect(text_q_l, "unaids") |
           str_detect(text_a_l, "fn-sambandet") |
           str_detect(text_a_l, "utdanning for bærekraftig utvikling") |
           str_detect(text_a_l, "utdanning for berekraftig utvikling") |
           str_detect(text_a_l, "unicef") | 
           str_detect(text_a_l, "wfp") | 
           str_detect(text_a_l, "unaids") |
           str_detect(justification_l, "fn-sambandet") |
           str_detect(justification_l, "utdanning for bærekraftig utvikling") |
           str_detect(justification_l, "utdanning for berekraftig utvikling") |
           str_detect(justification_l, "unicef") | 
           str_detect(justification_l, "wfp") | 
           str_detect(justification_l, "unaids") |
           str_detect(text_q_l, "unesco") |
           str_detect(text_a_l, "unesco") |
           str_detect(justification_l, "unesco") |
           str_detect(text_q_l, "undp") |
           str_detect(text_a_l, "undp") |
           str_detect(justification_l, "undp") |
           str_detect(text_q_l, "\\b(ilo)\\b") |
           str_detect(text_a_l, "\\b(ilo)\\b") |
           str_detect(justification_l, "\\b(ilo)\\b") |
           str_detect(text_q_l, "fns matvareprogram") |
           str_detect(text_a_l, "fns matvareprogram") |
           str_detect(justification_l, "fns matvareprogram") |
           str_detect(text_q_l, "who") |
           str_detect(text_a_l, "who") |
           str_detect(justification_l, "who") |
           str_detect(text_q_l, "verdens helseorganisasjon") |
           str_detect(text_a_l, "verdens helseorganisasjon") |
           str_detect(justification_l, "verdens helseorganisasjon") |
           str_detect(text_q_l, "ipcc") |
           str_detect(text_a_l, "ipcc") |
           str_detect(justification_l, "ipcc") |
           str_detect(text_q_l, "fns klimapanel") |
           str_detect(text_a_l, "fns klimapanel") |
           str_detect(justification_l, "fns klimapanel") |
           str_detect(text_q_l, "bærekraftsmål") |
           str_detect(text_a_l, "bærekraftsmål") |
           str_detect(justification_l, "bærekraftsmål") |
           str_detect(text_q_l, "berekraftsmål") |
           str_detect(text_a_l, "berekraftsmål") |
           str_detect(justification_l, "berekraftsmål") |
           str_detect(text_q_l, "2030-agendaen") |
           str_detect(text_a_l, "2030-agendaen") |
           str_detect(justification_l, "2030-agendaen") |
           str_detect(text_q_l, "agenda 2030") |
           str_detect(text_a_l, "agenda 2030") |
           str_detect(justification_l, "agenda 2030"))

textrelevancesamband <- textrelevance %>% 
  filter(str_detect(text_a_l, "fn-sambandet") |
           str_detect(text_q_l, "fn-sambandet") |
           str_detect(justification_l, "fn-sambandet"))

textrelevance <- left_join(textrelevance, topic_assignment, by = "id")

ggplot(textrelevance, aes(x = TopTopic)) + 
  geom_histogram(binwidth = 1)

ggplot(textrelevance, aes(x = SecTopic)) + 
  geom_histogram(binwidth = 1)

ggplot(textrelevance, aes(x = ThirdTopic)) + 
  geom_histogram(binwidth = 1)

table(textrelevance$SecTopic)

table(textrelevance$ThirdTopic)

textrelevancemeta <- questionlist %>%
  filter(id %in% textrelevance$id)

### Plotting the relevant categories for our found variables. 

topcatdf <- data.frame(topics = c(47, 54, 2, 26, 39), number = c(38, 8, 6, 4, 2))

ggplot(topcatdf, aes(x = as.character(topics), y = number)) + 
  geom_bar(fill = "blue", stat = "identity") + 
  theme_bw()

questiontopics_group_topcat <- questiontopics_group_75 %>%
  filter(topic %in% topcatdf$topics)

questiontopics_group_topcat %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_bar(stat = "identity") + 
  facet_wrap( ~ topic, 
              ncol = 3,
              scales = "free") + 
  labs(x = "", y = "Word-Topic probability") + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

### second highest cat

seccatdf <- data.frame(topics = c(39, 47, 26, 54, 6, 2, 38, 51, 5, 73), 
                       number = c(11, 9, 6, 6, 5, 4, 3, 3, 2, 2))

ggplot(seccatdf, aes(x = as.character(topics), y = number)) + 
  geom_bar(fill = "blue", stat = "identity") + 
  theme_bw()

questiontopics_group_seccat <- questiontopics_group_75 %>%
  filter(topic %in% seccatdf$topics)

questiontopics_group_seccat %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_bar(stat = "identity") + 
  facet_wrap( ~ topic, 
              ncol = 3,
              scales = "free") + 
  labs(x = "", y = "Word-Topic probability") + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

### Third category

thirdcatdf <- data.frame(topics = c(38, 30, 55, 54, 51, 26, 39, 20, 41, 47, 65),
                         number = c(10, 8, 6, 5, 5, 5, 4, 4, 3, 3, 2))

ggplot(thirdcatdf, aes(x = as.character(topics), y = number)) + 
  geom_bar(fill = "blue", stat = "identity") + 
  theme_bw()

questiontopics_group_thirdcat <- questiontopics_group_75 %>%
  filter(topic %in% thirdcatdf$topics)

questiontopics_group_thirdcat %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_bar(stat = "identity") + 
  facet_wrap( ~ topic, 
              ncol = 3,
              scales = "free") + 
  labs(x = "", y = "Word-Topic probability") + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

#### But what can we dow with some precise topics?

### Topics 30, 33, 43, 52, 54, 63.

#Topic 30 is related to questions asked that require the minister in charge to do
# something. 

top_docs100topic30 <- question_doc_prob_75 %>%
  group_by(document) %>% # Find the next statistic per document
  filter(topic == 30 & document %in% top100question$id)

plot(density(top_docs100topic30$gamma))

#Topic 33 is about emissions. 

top_docs100topic33 <- question_doc_prob_75 %>%
  group_by(document) %>% # Find the next statistic per document
  filter(topic == 33 & document %in% top100question$id)

plot(density(top_docs100topic33$gamma))

#Topic 43 is about sustainability. 

top_docs100topic43 <- question_doc_prob_75 %>%
  group_by(document) %>% # Find the next statistic per document
  filter(topic == 43 & document %in% top100question$id)

plot(density(top_docs100topic43$gamma))

#Topic 52, Higher education

top_docs100topic52 <- question_doc_prob_75 %>%
  group_by(document) %>% # Find the next statistic per document
  filter(topic == 52 & document %in% top100question$id)

plot(density(top_docs100topic52$gamma))

#Topic 54, more climate emissions

top_docs100topic54 <- question_doc_prob_75 %>%
  group_by(document) %>% # Find the next statistic per document
  filter(topic == 54 & document %in% top100question$id)

plot(density(top_docs100topic54$gamma))

#Topic 63, Environmentalism

top_docs100topic63 <- question_doc_prob_75 %>%
  group_by(document) %>% # Find the next statistic per document
  filter(topic == 63 & document %in% top100question$id)

plot(density(top_docs100topic63$gamma))

###### -------- plotting some results -------

textrelevance <- textrelevance %>%
  mutate(priority_word_level = case_when(
    str_detect(text_q_l, "fn-sambandet") ~ "1",
    str_detect(text_a_l, "fn-sambandet") ~ "1",
    str_detect(text_q_l, "utdanning for bærekraftig utvikling") ~ "2",
    str_detect(text_a_l, "utdanning for bærekraftig utvikling") ~ "2",
    str_detect(text_q_l, "unicef") ~ "3",
    str_detect(text_a_l, "unicef") ~ "3",
    str_detect(text_q_l, "wfp") ~ "3",
    str_detect(text_a_l, "wfp") ~ "3",
    str_detect(text_q_l, "unaids") ~ "3", 
    str_detect(text_a_l, "unaids") ~ "3",
    str_detect(justification_l, "fn-sambandet") ~ "1",
    str_detect(justification_l, "utdanning for bærekraft utvikling") ~ "2",
    str_detect(justification_l, "unicef") ~ "2",
    str_detect(justification_l, "wfp") ~ "2", 
    str_detect(justification_l, "unaids") ~ "2"
  ))

ggplot(textrelevance, aes(x = session_id, fill = priority_word_level)) + 
  geom_bar() + 
  theme_bw()

textrelevance <- left_join(textrelevance, textrelevancemeta, by = c("id"))

#### importing mp's and joining with the questions

periods_storting <- get_parlperiods()

periods_storting <- periods_storting %>%
  filter(id %in% c("2021-2025", "2017-2021", "2013-2017", "2009-2013"))

j <- list()

for (x in periods_storting$id) {
  j[[x]] <- get_parlperiod_mps(periodid = x, substitute = TRUE, 
                               good_manners = 0)
}


allmps <- do.call("rbind", j)

allmpsparties <- allmps %>% 
  select(question_from_id = mp_id, gender, party_id, county_id, period_id)

textrelevance <- textrelevance %>%
  mutate(period_id = case_when(session_id == "2021-2022" ~ "2021-2025",
                               session_id == "2020-2021" ~ "2017-2021",
                               session_id == "2019-2020" ~ "2017-2021",
                               session_id == "2018-2019" ~ "2017-2021",
                               session_id == "2017-2018" ~ "2017-2021",
                               session_id == "2016-2017" ~ "2013-2017",
                               session_id == "2015-2016" ~ "2013-2017",
                               session_id == "2014-2015" ~ "2013-2017",
                               session_id == "2013-2014" ~ "2013-2017",
                               session_id == "2012-2013" ~ "2009-2013",
                               session_id == "2011-2012" ~ "2009-2013"))

textrelevancewparty <- left_join(textrelevance, allmpsparties, 
                                 by = c("question_from_id", "period_id"))

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", 
                             "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", 
                             "#661100", "#6699CC", "#888888")

textrelevancewparty <- textrelevancewparty %>% 
  mutate(govposition = case_when(party_id == "A" & session_id == "2021-2022" ~ "Regjering",
                                 party_id == "Sp" & session_id == "2021-2022" ~ "Regjering",
                                 party_id == "SV" & session_id == "2021-2022" ~ "Støtteparti",
                                 party_id == "H" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "FrP" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "KrF" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "MDG" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "A" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2020-2021" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2020-2021" ~ "Støtteparti",
                                 party_id == "KrF" & session_id == "2020-2021" ~ "Regjering",
                                 party_id == "MDG" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2020-2021" ~ "Regjering",
                                 party_id == "A" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2019-2020" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2019-2020" ~ "Støtteparti",
                                 party_id == "KrF" & session_id == "2019-2020" ~ "Regjering",
                                 party_id == "MDG" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2019-2020" ~ "Regjering",
                                 party_id == "A" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2018-2019" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2018-2019" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2018-2019" ~ "Regjering",
                                 party_id == "MDG" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2018-2019" ~ "Regjering",
                                 party_id == "A" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2017-2018" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2017-2018" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2017-2018" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2017-2018" ~ "Regjering",
                                 party_id == "A" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2016-2017" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2016-2017" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2016-2017" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2016-2017" ~ "Støtteparti",
                                 party_id == "A" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2015-2016" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2015-2016" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2015-2016" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2015-2016" ~ "Støtteparti",
                                 party_id == "A" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2014-2015" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2014-2015" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2014-2015" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2014-2015" ~ "Støtteparti",
                                 party_id == "A" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2013-2014" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2013-2014" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2013-2014" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2013-2014" ~ "Støtteparti",
                                 party_id == "A" & session_id == "2012-2013" ~ "Regjering",
                                 party_id == "Sp" & session_id == "2012-2013" ~ "Regjering",
                                 party_id == "SV" & session_id == "2012-2013" ~ "Regjering",
                                 party_id == "H" & session_id == "2012-2013" ~ "Opposisjon", 
                                 party_id == "FrP" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "KrF" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "MDG" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "A" & session_id == "2011-2012" ~ "Regjering",
                                 party_id == "Sp" & session_id == "2011-2012" ~ "Regjering",
                                 party_id == "SV" & session_id == "2011-2012" ~ "Regjering",
                                 party_id == "H" & session_id == "2011-2012" ~ "Opposisjon", 
                                 party_id == "FrP" & session_id == "2011-2012" ~ "Opposisjon",
                                 party_id == "KrF" & session_id == "2011-2012" ~ "Opposisjon",
                                 party_id == "MDG" & session_id == "2011-2012" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2011-2012" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2011-2012" ~ "Opposisjon"
))

ggplot(textrelevancewparty, aes(x = session_id, fill = party_id)) + 
  geom_bar() + 
  scale_fill_discrete(type = safe_colorblind_palette) + 
  theme_bw()

ggplot(textrelevancewparty, aes(x = session_id, fill = gender)) + 
  geom_bar() + 
  scale_fill_viridis(discrete = TRUE)

ggplot(textrelevancewparty, aes(x = session_id, fill = govposition)) + 
  geom_bar() + 
  scale_fill_viridis(discrete = TRUE) + 
  theme_bw()



questiontext2 <- questiontext %>%
  mutate(period_id = case_when(session_id == "2021-2022" ~ "2021-2025",
                               session_id == "2020-2021" ~ "2017-2021",
                               session_id == "2019-2020" ~ "2017-2021",
                               session_id == "2018-2019" ~ "2017-2021",
                               session_id == "2017-2018" ~ "2017-2021",
                               session_id == "2016-2017" ~ "2013-2017",
                               session_id == "2015-2016" ~ "2013-2017",
                               session_id == "2014-2015" ~ "2013-2017",
                               session_id == "2013-2014" ~ "2013-2017",
                               session_id == "2012-2013" ~ "2009-2013",
                               session_id == "2011-2012" ~ "2009-2013"))

questiontextwparty <- left_join(questiontext2, allmpsparties, 
                                 by = c("question_from_id", "period_id"))


questiontextwparty <- questiontextwparty %>% 
  mutate(govposition = case_when(party_id == "A" & session_id == "2021-2022" ~ "Regjering",
                                 party_id == "Sp" & session_id == "2021-2022" ~ "Regjering",
                                 party_id == "SV" & session_id == "2021-2022" ~ "Støtteparti",
                                 party_id == "H" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "FrP" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "KrF" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "MDG" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2021-2022" ~ "Opposisjon",
                                 party_id == "A" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2020-2021" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2020-2021" ~ "Støtteparti",
                                 party_id == "KrF" & session_id == "2020-2021" ~ "Regjering",
                                 party_id == "MDG" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2020-2021" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2020-2021" ~ "Regjering",
                                 party_id == "A" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2019-2020" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2019-2020" ~ "Støtteparti",
                                 party_id == "KrF" & session_id == "2019-2020" ~ "Regjering",
                                 party_id == "MDG" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2019-2020" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2019-2020" ~ "Regjering",
                                 party_id == "A" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2018-2019" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2018-2019" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2018-2019" ~ "Regjering",
                                 party_id == "MDG" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2018-2019" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2018-2019" ~ "Regjering",
                                 party_id == "A" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2017-2018" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2017-2018" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2017-2018" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2017-2018" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2017-2018" ~ "Regjering",
                                 party_id == "A" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2016-2017" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2016-2017" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2016-2017" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2016-2017" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2016-2017" ~ "Støtteparti",
                                 party_id == "A" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2015-2016" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2015-2016" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2015-2016" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2015-2016" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2015-2016" ~ "Støtteparti",
                                 party_id == "A" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2014-2015" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2014-2015" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2014-2015" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2014-2015" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2014-2015" ~ "Støtteparti",
                                 party_id == "A" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "Sp" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "SV" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "H" & session_id == "2013-2014" ~ "Regjering", 
                                 party_id == "FrP" & session_id == "2013-2014" ~ "Regjering",
                                 party_id == "KrF" & session_id == "2013-2014" ~ "Støtteparti",
                                 party_id == "MDG" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2013-2014" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2013-2014" ~ "Støtteparti",
                                 party_id == "A" & session_id == "2012-2013" ~ "Regjering",
                                 party_id == "Sp" & session_id == "2012-2013" ~ "Regjering",
                                 party_id == "SV" & session_id == "2012-2013" ~ "Regjering",
                                 party_id == "H" & session_id == "2012-2013" ~ "Opposisjon", 
                                 party_id == "FrP" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "KrF" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "MDG" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2012-2013" ~ "Opposisjon",
                                 party_id == "A" & session_id == "2011-2012" ~ "Regjering",
                                 party_id == "Sp" & session_id == "2011-2012" ~ "Regjering",
                                 party_id == "SV" & session_id == "2011-2012" ~ "Regjering",
                                 party_id == "H" & session_id == "2011-2012" ~ "Opposisjon", 
                                 party_id == "FrP" & session_id == "2011-2012" ~ "Opposisjon",
                                 party_id == "KrF" & session_id == "2011-2012" ~ "Opposisjon",
                                 party_id == "MDG" & session_id == "2011-2012" ~ "Opposisjon",
                                 party_id == "R" & session_id == "2011-2012" ~ "Opposisjon",
                                 party_id == "V" & session_id == "2011-2012" ~ "Opposisjon"
  ))


ggplot(questiontextwparty, aes(x = session_id, fill = govposition)) + 
  geom_bar() + 
  scale_fill_viridis(discrete = TRUE) + 
  theme_bw()

ggplot(questiontextwparty, aes(x = session_id, fill = gender)) + 
  geom_bar() + 
  scale_fill_viridis(discrete = TRUE)


ggplot(questiontextwparty, aes(x = session_id, fill = party_id)) + 
  geom_bar() + 
  scale_fill_discrete(type = safe_colorblind_palette) + 
  theme_bw()


## In terms of questions, the Labour party seems strangely underrepresented
## Especially when compared to the activity of the Conservatives. 

## There is the possibility that this a sort of "opposition effect"
## Responding to questions regarding Current topics. However
## the Labour party did not seem to return the favour during the 
## first crimea crisis. 

## The progress party is (unsurprisingly) severely underrepresented. 
## Somewhat interesting given their insistance on "helping where they are"

## Socialist Left is also mildly underrepresented, but within statistical
## insignificance (probably). 

### What is the reccommended course of action?


ukr_texts <- textrelevancewparty %>%
  filter(str_detect(text_q_l, "ukraina") |
           str_detect(text_a_l, "ukraina"))


###Ukraine seems to account for the entire increase. 


UBUfiltered <- questiontextwparty %>%
  filter(str_detect(text_q_l, "bærekraftig") & str_detect(text_q_l, "utdanning"))

UBUfiltered2 <- questiontextwparty %>%
  filter(str_detect(text_a_l, "bærekraftig") & str_detect(text_a_l, "utdanning"))

questiontextwparty <- questiontextwparty %>%
  mutate(justification_l = str_to_lower(justification))

UBUfiltered3 <- questiontextwparty %>%
  filter(str_detect(justification_l, "bærekraftig") & str_detect(justification_l, "utdanning"))


sum(str_detect(questiontextwparty$justification_l, "\\b(ilo)\\b"))


sum(str_detect(questiontextwparty$text_a_l, "\\b(sdg)\\b"))

sum(str_detect(questiontextwparty$text_a_l, "2030-agendaen"))


ilocheck <- questiontextwparty %>% 
  filter(str_detect(questiontextwparty$text_a_l, "bærekraftsmål"))

### Lets check the top chargers on the topic

top100question <- questiontextwparty %>% 
  filter(id %in% topic47docs$document)


ggplot(top100question, aes(x = session_id, fill = party_id)) + 
  geom_bar() +
  scale_fill_discrete(type = safe_colorblind_palette) +
  theme_bw()

ggplot(top100question, aes(x = session_id, fill = govposition)) + 
  geom_bar() +
  scale_fill_discrete(type = safe_colorblind_palette) +
  theme_bw()

ggplot(top100question, aes(x = session_id, fill = gender)) + 
  geom_bar() + 
  scale_fill_discrete(type = safe_colorblind_palette) + 
  theme_bw()


