# Differences in sentiment fluctuation between male and female sermons
# --------------------------------------------------------------------
sermons <- read.csv("https://raw.githubusercontent.com/ryanburge/sermon_analysis/master/final.csv")

sermons$Sermon <- as.character(sermons$sermon)

library(tidytext)
library(dplyr)
sermons <- sermons %>% unnest_tokens(word, Sermon, token = "sentences") %>% 
  select(c(word,name,gender,X)) %>% mutate(name = as.character(name)) %>% 
  mutate(gender = as.character(gender))
Cronology <- function(data){
  data$cronology <- NA
  data$cronology[1:(nrow(data)*.2)] <- "()\n
  Beginning"
  data$cronology[-(1:(nrow(data)*.2))] <- "(2)\n
  ..."
  data$cronology[-(1:(nrow(data)*.4))] <- "(3)\n
  Middle"
  data$cronology[-(1:(nrow(data)*.6))] <- "(4)\n
  ..."
  data$cronology[-(1:(nrow(data)*.8))] <- "(5)\n
  End"
  return(data)
}
result <- vector(length=length(unique(sermons$X)))
for(i in sermons$X){
  result[[i]] <- sermons %>% filter(X==i) %>% Cronology() %>% 
    select(cronology)
}
cronology <- unlist(result)
x <- c(1:length(cronology))
sermons$cronology <- data.frame(cronology,x) %>% 
  filter(cronology!="FALSE") %>% select(cronology)
sermons <- sermons %>% mutate(score=syuzhet::get_sentiment(word))

sermons$cronology <- sermons$cronology$cronology
library(Rmisc)
dat <- summarySE(sermons, measurevar="score", groupvars=c("cronology","gender"))

windows()

library(ggplot2)
ggplot(dat,aes(x=cronology, y=score,color=gender, group=gender)) + 
  geom_errorbar(aes(ymin=score-ci,ymax=score+ci),color="black",width=.1, position=position_dodge(0.1)) +
  geom_line(position = position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3, shape=21, fill="white") +
  theme_bw() +
  xlab("") + ylab("Mean Sentiment Score\n(95% Confidence Intervals Shown)") +
  ggtitle("Sentiment in Male vs. Female Sermons") +
  theme(plot.title = element_text(hjust=0.5,face="bold",size=16)) +
  geom_hline(yintercept = 0, color="grey") +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5)) +
  theme(legend.position = c(0.1,0.9)) +
  theme(legend.title = element_blank())

ggsave(file="sentiment_by5.png", type = "cairo-png", width = 15, height = 12)
