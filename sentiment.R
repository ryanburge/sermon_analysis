tidy <- sermon %>% 
  unnest_tokens(word, sermon)

tidy <- tidy %>%
  anti_join(stop_words)


## There are 36.71% as many setniment scoring words for females as males, so I need to normalize that. 

men <- tidy %>% 
  filter(gender == "Male") %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment) %>% 
  mutate(n = n * .3671 )

fem <- tidy %>% 
  filter(gender == "Female") %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment) 

## Some interesting stuff here. 

compare <- bind_cols(men, fem) %>% 
  select(-sentiment1) %>% 
  rename(men = n, women = n1) 

compare <- melt(compare, id.vars = c("sentiment"))

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

compare$sentiment <- sapply(compare$sentiment, simpleCap)
compare$variable <- as.character(compare$variable)
compare$variable <- sapply(compare$variable, simpleCap)


ggplot(compare, aes(x=sentiment, y= value, group = variable,  fill = variable)) + 
  geom_col(position = "dodge") + 
  labs(x ="Sentiment", y = "Number of Words", title = "Comparing Sentiment in Sermons Based on Gender")  + 
  theme(legend.position="bottom") + labs(fill="") +
  theme(plot.title = element_text(hjust = 0.5))  + 
  theme(text=element_text(size=28)) + 
  scale_fill_grey()

ggsave(file="sentiment_nrc.png", type = "cairo-png", width = 20, height =12)
