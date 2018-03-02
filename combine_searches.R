load("~/Documents/Data_Science_Bootcamp/R/Twitter_Ruser_search_020618.Rdata")
load("~/Documents/Data_Science_Bootcamp/R/Twitter_Ruser_search_021318.Rdata")
load("~/Documents/Data_Science_Bootcamp/R/Twitter_Ruser_search_021418.Rdata")
load("~/Documents/Data_Science_Bootcamp/R/Twitter_Ruser_search_022518.Rdata")

Reduce(function(x, y) anti_join(x, y, by = "id"), list(user_search_df, user_search_df_2, user_search_df_3, user_search_df_4))

df2_unique <- anti_join(user_search_df_2, user_search_df, by = "id")

user_search_full <- bind_rows(user_search_df, df2_unique, user_search_df_3, user_search_df_4)

save(user_search_full, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/user_search_full_022518.Rdata")

#Check how complet the data are. Expect a solid line at 45 degree angle. 
#Breaks mean uncollected data. There are a few spots where thats the case.
library(ggplot2)

user_search_full %>% 
  ggplot(aes(x = created, y = id)) +
  geom_point(alpha = .01)
