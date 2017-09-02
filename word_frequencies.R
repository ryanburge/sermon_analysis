library(tidytext)
library(tidyverse)

sermon <- read_csv("D:/sermon_analysis/final.csv") %>% mutate(gender = as.factor(gender))

sermon %>% count(gender)
# A tibble: 2 x 2
#gender     n
#<fctr> <int>
#1 Female   151
#2   Male   299


tidy <- sermon %>% 
  unnest_tokens(word, sermon)

tidy %>% 
  group_by(gender) %>% 
  count()

# A tibble: 2 x 2
# Groups:   gender [2]
#gender      n
#<fctr>  <int>
#1 Female 266736
#2   Male 742254

## Average Male Sermon was 2516 words
## Average Female Sermon was was 1767 words 


tidy <- tidy %>%
  anti_join(stop_words)

tidy <- tidy %>% filter(word != "1") %>% filter(word != "2")  %>% filter(word != "3") %>% filter(word != "don")

male <- tidy %>%
  filter(gender == "Male") %>% 
  count(word, sort = TRUE) %>% 
  mutate(pct = n/235694, pct = round(pct, 4)) %>% 
  select(-n) %>% 
  mutate(gender = c("Male")) %>% 
  head(25)

female <- tidy %>%
  filter(gender == "Female") %>% 
  count(word, sort = TRUE) %>% 
  mutate(pct = n/94521, pct = round(pct, 4)) %>% 
  select(-n) %>% 
  mutate(gender = c("Female")) %>% 
  head(25)

ggplot(male, aes(x=reorder(word, pct), y=pct)) + 
  geom_col() + 
  coord_flip() + 
  labs(x= "Word", y= "Frequency", title = "Male Sermons Word Frequency")

ggplot(female, aes(x=reorder(word, pct), y=pct)) + 
  geom_col() + 
  coord_flip() + 
  labs(x= "Word", y= "Frequency", title = "Female Sermons Word Frequency")

male <- tidy %>%
  filter(gender == "Male") %>% 
  count(word, sort = TRUE) %>% 
  mutate(pct = n/238444, pct = round(pct, 4)) %>% 
  select(-n) %>% 
  mutate(gender = c("Male")) %>% 
  mutate(id_male = row_number())

female <- tidy %>%
  filter(gender == "Female") %>% 
  count(word, sort = TRUE) %>% 
  mutate(pct = n/95227, pct = round(pct, 4)) %>% 
  select(-n) %>% 
  mutate(gender = c("Female")) %>% 
  mutate(id_female = row_number())

merge <- merge(male, female, by=c("word"))

merge <- merge %>% select(word, id_male, id_female) %>% mutate(diff = id_male - id_female) %>% filter(id_male < 100 & id_female < 100) %>% arrange(diff)