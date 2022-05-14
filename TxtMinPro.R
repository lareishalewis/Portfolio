library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(quanteda)
library(ggplot2)
library(colorRamps)
library(dplyr)
library(grDevices)
library(tm.plugin.webmining)
library(tidytext)
library(gutenbergr)


article_info <- read_csv("article_info.csv",
                         col_types = cols(publication_date = col_date(format = "%B %d, %Y")))


article_known_info <- article_info$what_is_known %>%data.frame()
article_implication <- article_info$implications %>% data.frame()


articles_known <- Corpus(VectorSource(article_known_info))
articles_implication <- Corpus(VectorSource(article_implication))

# convert text to lower case
articles_known <- tm_map(articles_known, content_transformer(tolower))
articles_implication <- tm_map(articles_implication, content_transformer(tolower))


# remove numbers
articles_known <- tm_map(articles_known, removeNumbers)
articles_implication <- tm_map(articles_implication, removeNumbers)

# Remove english common stopwords
articles_known <- tm_map(articles_known, removeWords, stopwords("english"))
articles_implication <- tm_map(articles_implication, removeWords, stopwords("english"))

# Remove punctuations
articles_known <- tm_map(articles_known, removePunctuation)
articles_implication <- tm_map(articles_implication, removePunctuation)

# Eliminate extra white spaces
articles_known <- tm_map(articles_known, stripWhitespace)
articles_implication <- tm_map(articles_implication, stripWhitespace)

# stemming the text
articles_known <- tm_map(articles_known, stemDocument)
articles_implication <- tm_map(articles_implication, stemDocument)

# term document matrix
dtm <- TermDocumentMatrix(articles_known)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
known_info_d <- data.frame(word = names(v),freq=v)

dtm <- TermDocumentMatrix(articles_implication)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
impli_d <- data.frame(word = names(v),freq=v)

# filtering out hypens / dashes
impli_d = impli_d %>% filter(word != "—" & word != "–")
knwon_info_d = known_info_d %>% filter(word != "—" & word != "–")

head(impli_d)
str(impli_d)

head(known_info_d, 10)


# Generate the bar graphs to compare what is known
#and implications

impli_word_counts <- impli_d %>%
  inner_join(get_sentiments("bing")) %>%
  #count( sort=TRUE) %>%
  ungroup()
impli_word_counts

top_impli_count <- impli_word_counts %>% filter(freq > 10)

top_impli_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,freq)) %>%
  ggplot(aes(word,freq,fill=sentiment)) +
  geom_col(show.legend=F)+
  facet_wrap(~sentiment,scales="free_y")+
  labs(y="Implication words contribute to sentiment",
       x=NULL)+
  coord_flip()

known_info_word_counts <- known_info_d %>%
  inner_join(get_sentiments("bing")) %>%
  #count( sort=TRUE) %>%
  ungroup()
known_info_word_counts

top_known_info <- known_info_word_counts %>% filter(freq > 10)

top_known_info %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,freq)) %>%
  ggplot(aes(word,freq,fill=sentiment)) +
  geom_col(show.legend=F)+
  facet_wrap(~sentiment,scales="free_y")+
  labs(y="Whats known words contribute to sentiment",
       x=NULL)+
  coord_flip()

library(quanteda.textstats)
#Wordcloud
KnownRaw <- paste(article_known_info, collapse=" // ")
known.info <- textstat_collocations(KnownRaw,size=3)
print(known.info)
top_known_info <- known.info %>% filter(`count` > 10)
print(top_known_info)
ggplot(top_known_info,aes(x=reorder(collocation,`count`),y=`count`,fill=collocation))+
  geom_bar(stat="identity")+
  labs(x='Words from Article', y='Count')+
  ggtitle("What Is Known Top Words")+
  coord_flip()


ImpliRaw <- paste(article_implication, collapse=" // ")
Implications <- textstat_collocations(ImpliRaw,size=3)
print(Implications)
top_implications <- Implications %>% filter(`count` > 10)
print(top_implications)
ggplot(top_implications,aes(x=reorder(collocation,`count`),y=`count`,fill=collocation))+
  geom_bar(stat="identity")+
  labs(x='Words from Article', y='Count')+
  ggtitle("Implications Top Words")+
  coord_flip()



