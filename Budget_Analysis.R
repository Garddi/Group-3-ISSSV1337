######### ----------Decisions -----------------

rm(list=ls())
library(stortingscrape)
library(tidyverse)
library(tidytext)
library(quanteda)
library(stm)
library(tidyr)
library(viridis)
library(rvest)
library(xml2)

Sys.setlocale(category = "LC_ALL", "")
print("æøå")

?get_session_decisions


## As usual, setting the relevant sessions

sessions_storting <- get_parlsessions()

sessions_storting <- sessions_storting %>%
  filter(id %in% c("2011-2012", "2012-2013", "2013-2014",
                   "2014-2015", "2015-2016", "2016-2017",
                   "2017-2018", "2018-2019", "2019-2020", 
                   "2020-2021", "2021-2022"))


a <- list()

## For loop that fetches all of our fun stuff

for(x in unique(sessions_storting$id)){
  it <- 100*(which(unique(sessions_storting$id) == x) / length(unique(sessions_storting$id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  a[[x]] <- get_session_decisions(sessionid = x, good_manners = 0)
}

all_decisions <- do.call("rbind", a)

rm(a)

## Filtering out where the decisions are about the UN or their activities

UN_decisions <- all_decisions %>% 
  mutate(decision_text_l = str_to_lower(decision_text),
         title_text_l = str_to_lower(decision_title)) %>% 
  filter(str_detect(decision_text_l, "fn-sambandet") | 
           str_detect(title_text_l, "fn-sambandet") |
           str_detect(decision_text_l, "utdanning for bærekraftig utvikling") |
           str_detect(title_text_l, "utdanning for bærekraftig utvikling")| 
           str_detect(decision_text_l, "unicef") |
           str_detect(decision_text_l, "wfp") |
           str_detect(decision_text_l, "unaids") |
           str_detect(title_text_l, "unicef") |
           str_detect(title_text_l, "wfp") | 
           str_detect(title_text_l, "unaids"))

a<-list()

for (x in UN_decisions$case_id) {
  it <- 100*(which(unique(UN_decisions$case_id) == x) / length(unique(UN_decisions$case_id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  try(a[[x]] <- get_case_fancey(caseid = x, good_manners = 0))
}

b <- list()

for (x in unique(UN_decisions$case_id)) {
  b[[x]] <- a[[x]][["root"]]
}

save(UN_decisions, file = "Budget_Analysis/UN_decisions.Rdata")
save(UN_cases, file = "Budget_Analysis/UN_cases.Rdata")


### This seems to capture all budgets, which we can still use. 

UN_cases <- do.call("rbind", b)

str_extract(UN_decisions$decision_text_l[1], "[0-9]{2}")

print(UN_decisions$decision_text_l[1])

read_html(UN_decisions$decision_text_l[1], encoding = "UTF-8")

weirdcheck <- read_html(UN_decisions$decision_text_l[1]) %>%
  html_text()

weirdcheck2 <- str_squish(str_remove_all(weirdcheck, "\r\n"))

str_locate_all(weirdcheck, "unicef")

str_extract(weirdcheck, "unicef")

grab_text <- function(text, target, before, after){
  min <- which(unlist(map(str_split(text, "\\s"), ~grepl(target, .x))))-before
  max <- which(unlist(map(str_split(text, "\\s"), ~grepl(target, .x))))+after
  
  paste(str_split(text, "\\s")[[1]][min:max], collapse = " ")
}

unicefbudget <- grab_text(text = weirdcheck2, target = "unicef", before = 1, after  = 14)

parse_number(unicefbudget, trim_ws = TRUE)

sub(pattern = "\\s", replacement = "", x = unicefbudget) %>%
  str_extract("\\d+\\s\\d+\\s\\d+")

UN_decisions <- UN_decisions %>% 
  mutate(cleanedtext_l = str_squish(str_remove_all(html_text(read_xml(UN_decisions$decision_text_l)), "\r\n")))

for (i in 1:length(UN_decisions$decision_text_l)) {
  tmp1 <- read_html(UN_decisions$decision_text_l[i])
  
  tmp2 <- html_text(tmp1)
  
  UN_decisions$cleanedtext_l[i] <- str_squish(str_remove_all(tmp2, "\r\n")) 
  
}


unicefbudget <- list()
wfpbudget <- list()
unaidsbudget <- list()

unicefbudgetn <- list()
wfpbudgetn <- list()
unaidsbudgetn <- list()
grab_text(text = UN_decisions$cleanedtext_l[1], target = "wfp", before = 1, after = 14)
grab_text(text = UN_decisions$cleanedtext_l[7], target = "unaids", before = 1, after = 14)


## UNICEF GRANTS
for (x in 1:length(UN_decisions$cleanedtext_l)) {
  if(str_detect(UN_decisions$cleanedtext_l[x], "unicef")){
  unicefbudget[[x]] <- grab_text(text = UN_decisions$cleanedtext_l[x], target = "unicef", before = 1, after  = 14)
  
  if(str_detect(unicefbudget[[x]], "reduseres")){
    tmpred <- grab_text(text = unicefbudget[[x]], target = "til", before = 0, after = 4)
    tmp1 <- str_extract(tmpred, "[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{1,3}")
    unicefbudgetn[[x]] <- (as.numeric(str_remove_all(tmp1, " ")))
  }else{
  
  tmp1 <- str_extract(unicefbudget[[x]], "([:digit:]{1,3}\\s?)[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{3}")
  
  unicefbudgetn[[x]] <- (as.numeric(str_remove_all(tmp1, " ")))
  }
  }else{next}
}


unicefbudgets_t <- do.call("rbind", unicefbudgetn)

unicefbudget_f <- data.frame(Year = c(2022,2021,2020,2019,2018,2017,
                                      2016,2015,2014,2013,2012,2011),
                             UnicefMoney = unicefbudgets_t)

ggplot(unicefbudget_f, aes(x = Year, y = UnicefMoney)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(title = "UNICEF grants by year", y = "NOK", x = "") + 
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016,
                                2017, 2018, 2019, 2020, 2021, 2022))



## WFP GRANTS

for (x in 1:length(UN_decisions$cleanedtext_l)) {
  if(str_detect(UN_decisions$cleanedtext_l[x], "wfp")){
    wfpbudget[[x]] <- grab_text(text = UN_decisions$cleanedtext_l[x], target = "wfp", before = 1, after  = 14)
    if(str_detect(wfpbudget[[x]], "reduseres")){
      tmpred <- grab_text(text = wfpbudget[[x]], target = "til", before = 0, after = 4)
      tmp1 <- str_extract(tmpred, "[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{1,3}")
      wfpbudgetn[[x]] <- (as.numeric(str_remove_all(tmp1, " ")))
    }else{
      
      if(str_detect(wfpbudget[[x]], "auka")){
      tmpinc <- grab_text(text = UN_decisions$cleanedtext_l[x], target = "wfp", before = 1, after = 20)
      tmpinc2 <- grab_text(text = tmpinc, target = "til", before = 0, after = 5)
      tmpinc3 <- str_extract(tmpinc2, "[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{1,3}")

      wfpbudgetn[[x]] <- (as.numeric(str_remove_all(tmpinc3, " ")))
      
      }else{

      tmp1 <- str_extract(wfpbudget[[x]], "([:digit:]{1,3}\\s?)[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{3}")

      wfpbudgetn[[x]] <- (as.numeric(str_remove_all(tmp1, " ")))
    }}
  }else{next}
}


wfpbudget_t <- do.call("rbind", wfpbudgetn)

wfpbudget_f <- data.frame(Year = c(2022,2021,2020,2019,2018,2017,
                                      2016,2015,2014,2013,2012,2011),
                             WFPMoney = wfpbudget_t)

ggplot(wfpbudget_f, aes(x = Year, y = WFPMoney)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(title = "WFP grants by year", y = "NOK", x = "") + 
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016,
                                2017, 2018, 2019, 2020, 2021, 2022))


## UNDP

undpbudget <- list()
undpbudgetn <- list()

for (x in 1:length(UN_decisions$cleanedtext_l)) {
  if(str_detect(UN_decisions$cleanedtext_l[x], "undp")){
    undpbudget[[x]] <- grab_text(text = UN_decisions$cleanedtext_l[x], target = "undp", before = 1, after  = 14)
    if(str_detect(undpbudget[[x]], "reduseres")){
      tmpred <- grab_text(text = undpbudget[[x]], target = "til", before = 0, after = 4)
      tmp1 <- str_extract(tmpred, "[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{1,3}")
      undpbudgetn[[x]] <- (as.numeric(str_remove_all(tmp1, " ")))
    }else{
      
      if(str_detect(undpbudget[[x]], "auka")){
        tmpinc <- grab_text(text = UN_decisions$cleanedtext_l[x], target = "undp", before = 1, after = 20)
        tmpinc2 <- grab_text(text = tmpinc, target = "til", before = 0, after = 5)
        tmpinc3 <- str_extract(tmpinc2, "[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{1,3}")
        
        undpbudgetn[[x]] <- (as.numeric(str_remove_all(tmpinc3, " ")))
        
      }else{
        
        tmp1 <- str_extract(undpbudget[[x]], "([:digit:]{1,3}\\s?)[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{3}")
        
        undpbudgetn[[x]] <- (as.numeric(str_remove_all(tmp1, " ")))
      }}
  }else{next}
}


undpbudget_t <- do.call("rbind", undpbudgetn)

undpbudget_f <- data.frame(Year = c(2022,2021,2020,2019,2018,2017,
                                   2016,2015,2014,2013,2012,2011),
                          UNDPMoney = undpbudget_t)

ggplot(undpbudget_f, aes(x = Year, y = UNDPMoney)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(title = "UNDP grants by year", y = "NOK", x = "") + 
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016,
                                2017, 2018, 2019, 2020, 2021, 2022))



## UNAIDS

for (x in 1:length(UN_decisions$cleanedtext_l)) {
  if(str_detect(UN_decisions$cleanedtext_l[x], "unaids")){
    unaidsbudget[[x]] <- grab_text(text = UN_decisions$cleanedtext_l[x], target = "unaids", before = 1, after  = 14)
    if(str_detect(unaidsbudget[[x]], "reduseres")){
      tmpred <- grab_text(text = unaidsbudget[[x]], target = "til", before = 0, after = 4)
      tmp1 <- str_extract(tmpred, "[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{1,3}")
      unaidsbudgetn[[x]] <- (as.numeric(str_remove_all(tmp1, " ")))
    }else{
      
      if(str_detect(unaidsbudget[[x]], "auka")){
        tmpinc <- grab_text(text = UN_decisions$cleanedtext_l[x], target = "unaids", before = 1, after = 20)
        tmpinc2 <- grab_text(text = tmpinc, target = "til", before = 0, after = 5)
        tmpinc3 <- str_extract(tmpinc2, "[:digit:]{1,3}\\s[:digit:]{1,3}\\s[:digit:]{1,3}")
        
        unaidsbudgetn[[x]] <- (as.numeric(str_remove_all(tmpinc3, " ")))
        
      }else{
        
        tmp1 <- str_extract(unaidsbudget[[x]], "([:digit:]{1,3}\\s?)[:digit:]{1,3}\\s[:digit:]{1,3}\\s000")
        
        unaidsbudgetn[[x]] <- (as.numeric(str_remove_all(tmp1, " ")))
      }}
  }else{next}
}
UN_decisions$decision_title
unaidsbudget_t <- do.call("rbind", unaidsbudgetn)

unaidsbudget_f <- data.frame(Year = c(2021,2020, 2019, 2018.5,2018,2017,
                                    2016, 2015.5,2015,2014,2013,2012,2011),
                           UNAIDSMoney = unaidsbudget_t)

ggplot(unaidsbudget_f, aes(x = Year, y = UNAIDSMoney)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(title = "UNAIDS grants by year", y = "NOK", x = "") + 
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016,
                                2017, 2018, 2019, 2020, 2021, 2022))

