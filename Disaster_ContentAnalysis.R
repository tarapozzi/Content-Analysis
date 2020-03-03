#Install packages
install.packages("readtext")
install.packages("quanteda")
install.packages("tidytext")
install.packages("topicmodels")
install.packages("tidyverse")
install.packages("ggplot2")
#load packages
library(readtext)
library(quanteda)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(tidyverse)
library(spacyr)
library(textdata)
library(spacyr)

#read in pdfs
#folder where files are located
ioem_path <- "C:/Users/tarapozzi/Documents/Spring 2020/HES 600/Content-Analysis/IOEM"
news_path <- "C:/Users/tarapozzi/Documents/Spring 2020/HES 600/Content-Analysis//NEWS"
#read pdfs
ioem_pdfs <- list.files(path = ioem_path, pattern = 'pdf$',  full.names = TRUE) 
ioem_texts <- readtext(ioem_pdfs, 
                        sep = "_", 
                        docvarnames = c("First_author", "Year"))

news_pdfs <- list.files(path = news_path, pattern = 'pdf$',  full.names = TRUE) 
news_texts <- readtext(news_pdfs, 
                      sep = "_", 
                      docvarnames = c("First_author", "Year"))

#convert to corpus
ioem_corpus  <- corpus(ioem_texts)
news_corpus <- corpus(news_texts)

# Some stats about the news releases
summary(ioem_corpus)
summary(news_corpus)


metadoc(ioem_corpus, 'language') <- "english" 
metadoc(news_corpus, 'language') <- "english" 

#buidliing a document frequency matrix (DFM)
# you are getting rid of stop words which are specified under remove
ioem_DFM <- dfm(ioem_corpus, tolower = TRUE, stem = FALSE, 
                  remove = c("et", "al", "fig", "table", "ml", "http", "cookies", "cookie", "S", "|"
                             stopwords("smart")),
                  remove_punct = TRUE, remove_numbers = TRUE)

news_DFM <- dfm(news_corpus, tolower = TRUE, stem = FALSE, 
               remove = c("et", "al", "fig", "table", "ml", "http","cookies", "cookie", "S", "|"
                          stopwords("smart")),
               remove_punct = TRUE, remove_numbers = TRUE)


#summarise the two DFM's
topfeatures(ioem_DFM, 20) 
topfeatures(news_DFM, 20) 
#this gives us an idea of the conversation across these articles 

#visualize commonly used words
textplot_wordcloud(ioem_DFM, min.freq = 15, random.order=F, 
                   rot.per = .10,  
                   colors = RColorBrewer::brewer.pal(8,'Dark2')) 

textplot_wordcloud(news_DFM, min.freq = 15, random.order=F, 
                   rot.per = .10,  
                   colors = RColorBrewer::brewer.pal(8,'Dark2')) 

## Unsupervised classification of topics
ioem_topicmod <- convert(ioem_DFM, to="topicmodels")
ioem_lda <- LDA(ioem_topicmod, k=3) #k=3 tells you about three topics 
ioem_topics <- tidy(ioem_lda, matrix="beta")

ioem_top_terms <- ioem_topics %>% 
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ioem_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()  


news_topicmod <- convert(news_DFM, to="topicmodels")
news_lda <- LDA(news_topicmod, k=3)
news_topics <- tidy(news_lda, matrix="beta")

news_top_terms <- news_topics %>% 
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

news_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()  

# this uses machine learning to determine the topics so everytime it is run the answer will be slightly different, however when you have a large sample size you will reach a converging point of common topics

#sentiment analysis
tidy(ioem_DFM) %>%   
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>% 
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

tidy(news_DFM) %>%   
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>% 
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

tidy(ioem_DFM) %>%   
  inner_join(get_sentiments("nrc"), by = c(term = "word")) %>% 
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

tidy(news_DFM) %>%   
  inner_join(get_sentiments("nrc"), by = c(term = "word")) %>% 
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()


## Using additional words for context
ioem_bigrams <- tidy(ioem_corpus) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% # this looks for a two word phrase
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

news_bigrams <- tidy(news_corpus) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

negation_words <- c("not", "no", "never", "without")

ioem_notwords <- tidy(ioem_corpus) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

news_notwords <- tidy(news_corpus) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)


ioem_notwords %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()

news_notwords %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()
