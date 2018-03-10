library(tidyverse)

load("~/Documents/Data_Science_Bootcamp/R/Twitter_R/user_search_020618.Rdata")
load("~/Documents/Data_Science_Bootcamp/R/Twitter_R/user_search_021318.Rdata")
load("~/Documents/Data_Science_Bootcamp/R/Twitter_R/user_search_021418.Rdata")
load("~/Documents/Data_Science_Bootcamp/R/Twitter_R/user_search_022518.Rdata")
load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/user_search_full_022518.Rdata")
load("~/Documents/Data_science_Bootcamp/R/Twitter_R/user_search_030616.Rdata")
load("~/Documents/Data_science_Bootcamp/R/Twitter_R/user_search_030918.Rdata")
load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/user_search_full_030618.Rdata")

#df2_unique <- anti_join(user_search_df_2, user_search_df, by = "id")

df2_unique <- anti_join(user_search_df_6, user_search_full_030618)

#user_search_full <- bind_rows(user_search_df, df2_unique, user_search_df_3, user_search_df_4)

user_search_full_030918 <- bind_rows(user_search_full_030618, df2_unique)

save(user_search_full, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/user_search_full_022518.Rdata")

save(user_search_full_030618, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/user_search_full_030618.Rdata")

save(user_search_full_030918, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/user_search_full_030918.Rdata")

#Check how complet the data are. Expect a solid line at 45 degree angle. 
#Breaks mean uncollected data. There are a few spots where thats the case.
library(ggplot2)

user_search_full_030618 %>% 
  ggplot(aes(x = created, y = id)) +
  geom_point(alpha = .01)
