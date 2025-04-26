# Load libraries
library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(reshape2)
library(ggrepel)

# Load dataset
tuesdata <- tidytuesdayR::tt_load(2025, week = 12)
report_words_clean <- tuesdata$report_words_clean

# Use tidytext package for sentiment analysis
library(tidytext)
sent <- get_sentiments("nrc")
# sent <- get_sentiments("afinn")
# sent <- get_sentiments("bing")



df <- report_words_clean %>%
  left_join(sent) %>%
  subset(!is.na(sentiment)) %>%
  group_by(year, sentiment) %>%
  count(year, sentimet) %>%
  mutate(freq = n / sum(n))

df %>% ggplot(aes(x="x", y=freq, fill=sentiment)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, color="black") +
  scale_x_discrete(limits = c("x_empty", "x")) +
  coord_polar(theta = "y", direction = -1, clip = "off") +
  theme_void() +
  theme(plot.title = element_text(vjust = 0.5)) +
  facet_wrap(~year)



# n_words <- report_words_clean %>% count(word)
# report_words_keep <- n_words %>% subset(n > 200)
# 
# df <- report_words_clean %>% 
#   subset(word %in% report_words_keep$word) %>%
#   subset(word %in% sent$word) %>%
#   group_by(year, word) %>%
#   count() %>%
#   as.data.frame() %>%
#   dcast(formula=word~year, value.var="n") %>%
#   column_to_rownames("word") %>%
#   replace(is.na(.), 0)
# 
# 
# 
# pca <- prcomp(df, scale. = TRUE)
# pca_df <- pca$x %>% as.data.frame() %>% rownames_to_column("word")
# 
# # Add additional metadata
# pca_sent <- left_join(pca_df, sent, by="word")
# pca_sent <- left_join(pca_sent, n_words, by="word")
# 
# ggplot(pca_sent,
#        aes(x=PC1, y=PC2, color=sentiment, 
#            size=n, label=word)) +
#   scale_color_manual(values = c("negative"="red", "positive"="forestgreen")) +
#   geom_point() +
#   geom_text_repel() +
#   theme_void() +
#   theme(legend.position="bottom") +
#   labs(x="PC1", y="PC2")
