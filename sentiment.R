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

bind_cols(men, fem) %>% 
  select(-sentiment1) %>% 
  rename(men = n, women = n1) %>% 
  mutate(diff = women/men)

