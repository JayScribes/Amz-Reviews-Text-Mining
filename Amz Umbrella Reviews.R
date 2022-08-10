install.packages("rvest")
install.packages("tidyverse")
install.packages("tm")
install.packages("tidytext")
install.packages("widyr")
install.packages("igraph")
install.packages("ggraph")
install.packages("wordcloud")
install.packages("syuzhet")
install.packages("textdata")

library(rvest)
library(tidyverse)
library(dplyr)
library(tm)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(wordcloud)
library(syuzhet)
library(textdata)
## Links

link = ("https://www.amazon.com/Windproof-Travel-Umbrella-Rain-Compact/product-reviews/B0160HYB8S/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews$pageNumber=")


page = read_html(link)

Name = page %>% 
  html_nodes(".a-profile-name") %>% 
  html_text()

Rating = page %>% 
  html_nodes(".review-rating") %>% 
  html_text()

Review = page %>% 
  html_nodes(".review-text-content span") %>% 
  html_text()


## Scraping pages with for loops

amz_links = page %>% 
  html_nodes(".b-link") %>% 
  html_attr("href")

Amz_Reviews2 <- data.frame()

for (page_result in seq(from = 106, to = 500, by = 1)) {
  link = paste0("https://www.amazon.com/Windproof-Travel-Umbrella-Rain-Compact/product-reviews/B0160HYB8S/ref=cm_cr_arp_d_viewopt_srt?ie=UTF8&reviewerType=all_reviews%24pageNumber%3D7&sortBy=recent&pageNumber=",page_result)
  
  page = read_html(link)
  
  Name = page %>% 
    html_nodes(".a-profile-name") %>% 
    html_text()
  
  Rating = page %>% 
    html_nodes(".review-rating") %>% 
    html_text()
  
  Review = page %>% 
    html_nodes(".a-spacing-top-mini .a-size-base , .review-text-content span") %>% 
    html_text()
  
  Date = page %>% 
    html_nodes(".review-date") %>% 
    html_text()
  
  Amz_Reviews2 <- rbind(Amz_Reviews2, data.frame(Name, Rating, Review, Date))
  
  print(paste("Page:", page_result))  
}


## Cleaning Scraped Data

Amz_Reviews2$Rating <- substr(Amz_Reviews2$Rating, 1, 1)
Amz_Reviews2$Date <- substr(Amz_Reviews2$Date, 33, 50)

## Cleaning Data for NLP

Reviews <- Amz_Reviews2

Reviews %>%  count(Name, sort = TRUE)

Reviews$outlier = Reviews$Name == "C. Winton"   
Reviews = filter(Reviews, outlier != TRUE)
Reviews <- subset(Reviews, , -c(outlier))

Reviews$outlier = Reviews$Name == "Michael Chorost"   
Reviews = filter(Reviews, outlier != TRUE)
Reviews <- subset(Reviews, , -c(outlier))

Reviews %>%  count(Name, sort = TRUE)

Reviews %>% head(10) %>% pull(Review)

Rev_C <- Reviews %>% 
  select(Review, Name, Rating)

## Prepping Data 

Rev_C1 <- tibble(text = str_to_lower(Rev_C$Review))
Rev_C2 <- tibble(text = str_to_lower(Rev_C$Name))
Rev_C3 <- tibble(text = str_to_lower(Rev_C$Rating))

Rev_C <- cbind(data.frame(Rev_C1, Rev_C2, Rev_C3))


Rev_words <- Rev_C %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(str_detect(word, "[:alpha:]")) %>% 
  distinct()


## Finding Most-Used Words
users_who_mention_word <- Rev_words %>% 
  count(word, name = "users_n") %>% 
  filter(users_n >= 10)

## Finding Word Associations
word_correlation <- Rev_words %>% 
  semi_join(users_who_mention_word, by = "word") %>% 
  pairwise_cor(item = word, feature = text.1) %>% 
  filter(correlation >= 0.23)

## Word Network Plot
word_correlation <- as.data.frame(word_correlation)
users_who_mention_word <- as.data.frame(users_who_mention_word)

graph_from_data_frame(d = word_correlation,
                      vertices = users_who_mention_word %>% 
                        semi_join(word_correlation, by = c("word" ="item1"))) %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point() +
  geom_node_text(aes(color = users_n, label = name), repel = TRUE)



## Looking at Word Graphs for Negative Reviews 

Rev_C %>% 
  count(text.2)

Rev_words_Neg <- Rev_words %>% 
  filter(text.2 < 4)

word_correlation_N <- Rev_words_Neg %>% 
  semi_join(users_who_mention_word, by = "word") %>% 
  pairwise_cor(item = word, feature = text.1) %>% 
  filter(correlation >= 0.3)

word_correlation_N <- as.data.frame(word_correlation_N)
users_who_mention_word <- as.data.frame(users_who_mention_word)

graph_from_data_frame(d = word_correlation_N,
                      vertices = users_who_mention_word %>% 
                        semi_join(word_correlation_N, by = c("word" ="item1"))) %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point() +
  geom_node_text(aes(color = users_n, label = name), repel = TRUE)


## Looking at word graphs for Positive Reviews

Rev_words_Pos <- Rev_words %>% 
  filter(text.2 > 3)

word_correlation_P <- Rev_words_Pos %>% 
  semi_join(users_who_mention_word, by = "word") %>% 
  pairwise_cor(item = word, feature = text.1) %>% 
  filter(correlation >= 0.25)

graph_from_data_frame(d = word_correlation_P,
                      vertices = users_who_mention_word %>% 
                        semi_join(word_correlation_P, by = c("word" ="item1"))) %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point() +
  geom_node_text(aes(color = users_n, label = name), repel = TRUE)


## Wordcloud Negative Reviews

wordcloud(Rev_words_Neg$word, min.freq=15, max.freq=20, random.order=FALSE, scale=c(3,0.5), color=rainbow(7))

## Wordcloud Positive Reviews

wordcloud(Rev_words_Pos$word, min.freq=15, max.freq=20, random.order=FALSE, scale=c(3,0.5), color=rainbow(7))

## Sentiment Analysis of Negative Reviews

Rev_C_N <- Rev_C %>% 
  filter(text.2 < 3)

loughran_word_counts_n <- Rev_C_N %>% 
  unnest_tokens(output = word, input = text) %>% 
  inner_join(get_sentiments("loughran")) %>% 
  count(word, sentiment, sort = TRUE)

loughran_top_10_sentiment_n <- loughran_word_counts_n %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))

loughran_top_10_sentiment_n %>% 
  ggplot(aes(word, n, fill = sentiment))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip()


## Sentiment Analysis of Positive Reviews

Rev_C_P <- Rev_C %>% 
  filter(text.2 > 3)

loughran_word_counts_p <- Rev_C_P %>% 
  unnest_tokens(output = word, input = text) %>% 
  inner_join(get_sentiments("loughran")) %>% 
  count(word, sentiment, sort = TRUE)

loughran_top_10_sentiment_p <- loughran_word_counts_p %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))

loughran_top_10_sentiment_p %>% 
  ggplot(aes(word, n, fill = sentiment))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip()