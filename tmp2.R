library(tidyverse)
t1<-data.frame(x=c(1,2,3,4,5),y=c("A","B","A","B","A"))
t2<-data.frame(x=c(2,3,5,6))
t2 %>% left_join(t1)
char_vec <- sample(c("a", "b", "c"), 10, replace = TRUE)
char_vec
recode(char_vec, a = "Apple")