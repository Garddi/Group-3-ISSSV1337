###### ---------Script for analysing data -------------

load("Question_Data/MetadataQuestionList.Rdata")
load("Question_Data/All_Questions.Rdata")

Sys.setlocale(category = "LC_ALL", "")
print("æøå")

library(stortingscrape)
library(tidyverse)
library(tidytext)
library(quanteda)
library(stm)
library(tidyr)


answerlist <- questiontext %>%
  select(id, text = answer_text, title, type, question_from_id, 
         qustion_to_id)

answerlist <- answerlist %>%
  mutate(notapplicable = ifelse(text == "", NA, 1)) %>%
  drop_na(notapplicable) %>%
  select(-notapplicable)

questionshort <- questiontext %>%
  select(id, text = question_text, title, type, question_from_id,
         qustion_to_id)

assembledtextquestions <- rbind(questionshort, answerlist)

rm(answerlist, questionlist, questionshort, questiontext)

assembledtextquestions <- assembledtextquestions %>% 
  mutate(text_l = str_to_lower(text))

sum(str_detect(assembledtextquestions$text_l))

stop_words <- get_stopwords(language = "no")

removeword <- data.frame(word = "br", lexicon = "own library")

stop_words <- rbind(stop_words, removeword)

questiontokens <- assembledtextquestions %>%
  unnest_tokens(input = text, # Which variable to get the text from
                output = word, # What the new variable should be called
                token = "words") # Type of tokens to split the text into

questiontokens <- questiontokens %>%
  anti_join(stop_words, by = "word") # Joining against the stopwords dataframe to get rid of cells with stopwords

questiontokens

questiontokens %>%
  count(id, word, sort = TRUE)

questiontokens <- questiontokens %>%
  mutate(stem = wordStem(word, language = "norwegian"))

questiontokens_dfm <- questiontokens %>%
  count(id, stem, name = "count") %>% # By default, the count-variable is called "n". Use name = "" to change it.
  cast_dfm(id, # Specify the douments in your analysis (becoming the rows)
           stem, # Specify the tokens in your analysis (becoming the columns)
           count) # Specify the number of times each token shows up in each document (becoming the cells)

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

questiontokens_lda_75 <- stm(questiontokens_dfm,
                             init.type = "LDA",
                             K = 75,
                             seed = 910,
                             verbose = TRUE)
