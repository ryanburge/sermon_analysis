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



g1 <- tibble(type = c("Male", "Female")) 
g1 <- g1 %>% add_column(count = c(2516,1767))

ggplot(g1, aes(x=reorder(type, -count), y=count)) + 
  geom_col(fill = "gray0") + 
  labs(x="Gender of Speaker", y= "Average Number of Words", title = "Which Gender Preaches Longer Sermons?") +
  theme(plot.title = element_text(hjust=0.5,face="bold",size=16))

ggsave(file="word_count_by_gender.png", type = "cairo-png", width = 10, height =12)



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

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

male$word <- sapply(male$word, simpleCap)

ggplot(male, aes(x=reorder(word, pct), y=pct)) + 
  geom_col() + 
  coord_flip() + 
  labs(x= "Word", y= "Frequency", title = "Male Sermons Word Frequency") +
  scale_fill_grey() +
  theme(legend.position="bottom") + labs(fill="") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file="male_frequency.png", type = "cairo-png", width = 20, height =12)


female$word <- sapply(female$word, simpleCap)

ggplot(female, aes(x=reorder(word, pct), y=pct)) + 
  geom_col() + 
  coord_flip() + 
  labs(x= "Word", y= "Frequency", title = "Female Sermons Word Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file="female_frequency.png", type = "cairo-png", width = 20, height =12)


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
merge$class <- ifelse((merge$id_male - merge$id_female) < 0, "red", "green")


p <- ggplot(merge) + geom_segment(aes(x=1, xend=2, y=id_male, yend=id_female, col = class), size =.75, show.legend = FALSE) +  
  geom_vline(xintercept = 1, linetype = "dashed", size =.1) +
  geom_vline(xintercept = 2, linetype = "dashed", size =.1)  +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="grey", "red"="black"))

p <- p + geom_text(label=merge$word, y=merge$id_male, x=rep(1, NROW(merge)), hjust=1.1, size=3.5)
p <- p + geom_text(label=merge$word, y=merge$id_female, x=rep(2, NROW(merge)), hjust=-0.1, size=3.5)

p <- p + geom_text(label="Male Word Rank", x=1.135, y=103, hjust=1.2, size=5) 
p <- p + geom_text(label="Female Word Rank", x=1.85, y=103, hjust=-0.1, size=5)  # title

p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(2,2,2,2), "cm")) + labs(y= "", x ="")

ggsave(file="word_rank.png", type = "cairo-png", width = 15, height = 12)

menmore <- merge %>% head(10)


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

menmore$word <- sapply(menmore$word, simpleCap)

menmore$rightlabel <- paste(menmore$id_female, menmore$word, sep = ". ")
menmore$leftlabel <- paste(menmore$id_male, menmore$word, sep = ". ")

menmore <- menmore %>% mutate(rank_male = id_male - 100) %>% mutate(rank_male =rank_male * -1)
menmore <- menmore %>% mutate(rank_female = id_female - 100) %>% mutate(rank_female =rank_female * -1)


p <- ggplot(menmore) + geom_segment(aes(x=1.5, xend=2, y=rank_male, yend=rank_female, col = class), size =.75, show.legend = FALSE) +  
 # geom_vline(xintercept = 1.5, linetype = "dashed", size =.1) +
 # geom_vline(xintercept = 2, linetype = "dashed", size =.1)  +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="grey", "red"="black"))

p <- p + geom_text(label=menmore$leftlabel, y=menmore$rank_male, x=rep(1.5, NROW(menmore)), hjust=.5, vjust =-.5, size=3.5)
p <- p + geom_text(label=menmore$rightlabel, y=menmore$rank_female, x=rep(2, NROW(menmore)), hjust=.50,  vjust =-.5, size=3.5)

p <- p + geom_text(label="Male Word Rank", x=1.53, y=98, hjust=.2, size=5) 
p <- p + geom_text(label="Female Word Rank", x=1.9, y=98, hjust=-0.1, size=5)  
p <- p +  theme_minimal(base_size = 18)

p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(), 
          plot.margin = unit(c(1,1,1,1), "cm")) + labs(y= "", x ="")

ggsave(file="word_menmore.png", type = "cairo-png", width = 10, height = 12)




