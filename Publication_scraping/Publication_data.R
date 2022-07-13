


library(tidyverse)
library(stortingscrape)
library(rvest)

referat <- get_publication(good_manners = 0, publicationid = "refe-202122-01-31")


get_session_publications(sessionid = "2021-2022", type = "referat", good_manners = 0)


sessions_storting <- get_parlsessions()

sessions_storting <- sessions_storting %>%
  filter(id %in% c("2011-2012", "2012-2013", "2013-2014",
                   "2014-2015", "2015-2016", "2016-2017",
                   "2017-2018", "2018-2019", "2019-2020", 
                   "2020-2021", "2021-2022"))

## The id's are then entered into this loop, which *should* retrieve 
## ALL question id's for our relevant period. 

a<-list()
b<-list()
c<-list()
d<-list()
e<-list()
f<-list()
g<-list()
h<-list()

for(x in unique(sessions_storting$id)){
  a[[x]] <- get_session_publications(sessionid = x, type = "referat", good_manners = 0)
  b[[x]] <- get_session_publications(sessionid = x, type = "innstilling", good_manners = 0)
  c[[x]] <- get_session_publications(sessionid = x, type = "innberetning", good_manners = 0)
  d[[x]] <- get_session_publications(sessionid = x, type = "lovvedtak", good_manners = 0)
  e[[x]] <- get_session_publications(sessionid = x, type = "lovanmerkning", good_manners = 0)
  f[[x]] <- get_session_publications(sessionid = x, type = "dok8", good_manners = 0)
  g[[x]] <- get_session_publications(sessionid = x, type = "dok12", good_manners = 0)
  h[[x]] <- get_session_publications(sessionid = x, type = "dokumentserie", good_manners = 0)
  #paste0("stortingsporsmal", x) = rbind(a, b, c)
}

## Next step is to take these id's from the resulting dataframes, using 
## get_question, to get all the actual text of responses. 

publicationlista <- do.call("rbind", a)
publicationlistb <- do.call("rbind", b)
publicationlistc <- do.call("rbind", c)
publicationlistd <- do.call("rbind", d)
publicationliste <- do.call("rbind", e)
publicationlistf <- do.call("rbind", f)
publicationlistg <- do.call("rbind", g)
publicationlisth <- do.call("rbind", h)

clist <- list(publicationlista, publicationlistb, publicationlistc,publicationlistd, publicationliste, publicationlistf, 
              publicationlistg, publicationlisth)

publicationlist <- do.call("rbind", clist)

j <- list()

for(x in unique(publicationlist$publication_id)){
  it <- 100*(which(unique(publicationlist$publication_id) == x) / length(unique(publicationlist$publication_id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  j[[x]] <- get_publication(publicationid = x, good_manners = 0)
  #paste0("stortingsporsmal", x) = rbind(a, b, c)
}
