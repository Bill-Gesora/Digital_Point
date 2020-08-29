#Dashboard overview section----
polling_data <- dbGetQuery(con,
                           "SELECT *
                          FROM Geo_Locations")
# polling_data_all <- dbGetQuery(con,
#                                "SELECT *
#                                  FROM Polling_Data")


#######################
#Sentiment analysis using twitter

library("twitteR")
library("rtweet")
library("stringr")
library("tidytext")
library("rgdal")

countiesshapefile <- readOGR("Map_Data/counties.shp")
# countiesshapefile@data <- countiesshapefile@data %>% left_join(polling_data %>% 
#                                                                  group_by(county_name,Status) %>% 
#                                                                  summarize(Count = n(),
#                                                                            max_longitude = max(longitude),
#                                                                            max_latitude = max(latitude),
#                                                                            min_longitude = min(longitude),
#                                                                            min_latitude = min(latitude)) %>% 
#                                                                  arrange(desc(Count)) %>% 
#                                                                  top_n(1) %>% 
#                                                                  select(county_name, Status, max_longitude, max_latitude, min_longitude, min_latitude),
#                                                                by = c("COUNTY_NAM" = "county_name")
# )


constituencyshapefile <- readOGR("Map_Data/constituencies.shp")
# constituencyshapefile@data <- constituencyshapefile@data %>% left_join(polling_data %>% 
#                                                                          group_by(constituency,Status) %>% 
#                                                                          summarize(Count = n(),
#                                                                                    max_longitude = max(longitude),
#                                                                                    max_latitude = max(latitude),
#                                                                                    min_longitude = min(longitude),
#                                                                                    min_latitude = min(latitude)) %>% 
#                                                                          arrange(desc(Count)) %>% 
#                                                                          top_n(1) %>% 
#                                                                          select(constituency, Status, max_longitude, max_latitude, min_longitude, min_latitude),
#                                                                        by = c("CONSTITUEN" = "constituency")
# )

wardshapefile <- readOGR("Map_Data/Kenya wards.shp")
# wardshapefile@data <- wardshapefile@data %>% left_join(polling_data %>% 
#                                                          group_by(caw_name,Status) %>% 
#                                                          summarize(Count = n(),
#                                                                    max_longitude = max(longitude),
#                                                                    max_latitude = max(latitude),
#                                                                    min_longitude = min(longitude),
#                                                                    min_latitude = min(latitude)) %>% 
#                                                          arrange(desc(Count)) %>% 
#                                                          top_n(1) %>% 
#                                                          select(caw_name, Status, max_longitude, max_latitude, min_longitude, min_latitude),
#                                                        by = c("IEBC_WARDS" = "caw_name")
# )
# countycode <- countiesshapefile$COUNTY_COD
# countycode <- as.numeric(countycode)
# constituencycode <- constituencyshapefile$CONST_CODE #COUNTYCODE THAT'S IN
# constituencycode <- as.numeric(constituencycode) 



#Connecting to my twitter account
appname <- "Sentiment_Analysis_Bill"
consumer_key <- "ga4hq1sf7wwRHcYa34hLpQlsv"
consumer_secret <- "afnqV2sL8xV57tVbtnsN68xG5RdYlEcX5XgSvCzQ5iKUWo9uUQ"
access_token <- "49321204-P7FHBFLINswydlXhqTy40TC5WsHkUuWCHhDWIgEsu"
access_secret <- "s0FMDHXG48AkTP6WoxBZz0UihXUEFzu57TgUlZCf1yUUI"

twitter_token <- create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

#Creating my datasets
# busia_sample_politicians <- search_tweets2(
#   c("\"Amos Wako\"",
#     "\"SosPeter Ojamong\"",
#     "\"Moses Mulomi\"",
#     "\"Lucas Meso\"",
#     "\"Mary Oyeyo\"",
#     "\"Florence Mutua\"",
#     "\"Patrobas Odhiambo\"",
#     "\"Nobert Wangalwa\"",
#     "\"Joseph Oyula\""),
#   n = 18000
# )
# 

#saving the file----
# save(busia_sample_politicians, file = "busia_sample_politicians.Rdata")
load(file = "busia_sample_politicians.Rdata")
data("stop_words")


busia_politicians <- rbind(busia_sample_politicians %>%
                             as_tibble() %>%
                             filter(str_detect(text, "Lucas Meso")|
                                      str_detect(quoted_text, "Lucas Meso") |
                                      str_detect(retweet_text, "Lucas Meso")) %>%
                             mutate(politician = "Lucas Meso"),
                           busia_sample_politicians %>%
                             as_tibble() %>%
                             filter(str_detect(text, "SosPeter Ojamong")|
                                      str_detect(quoted_text, "SosPeter Ojamong") |
                                      str_detect(retweet_text, "SosPeter Ojamong")) %>%
                             mutate(politician = "SosPeter Ojamong"),
                           busia_sample_politicians %>%
                             as_tibble() %>%
                             filter(str_detect(text, "Moses Mulomi")|
                                      str_detect(quoted_text, "Moses Mulomi") |
                                      str_detect(retweet_text, "Moses Mulomi")) %>%
                             mutate(politician = "Moses Mulomi"),
                           busia_sample_politicians %>%
                             as_tibble() %>%
                             filter(str_detect(text, "Amos Wako")|
                                      str_detect(quoted_text, "Amos Wako") |
                                      str_detect(retweet_text, "Amos Wako")) %>%
                             mutate(politician = "Amos Wako"),
                           busia_sample_politicians %>%
                             as_tibble() %>%
                             filter(str_detect(text, "Mary Oyeyo")|
                                      str_detect(quoted_text, "Mary Oyeyo") |
                                      str_detect(retweet_text, "Mary Oyeyo")) %>%
                             mutate(politician = "Mary Oyeyo"),
                           busia_sample_politicians %>%
                             as_tibble() %>%
                             filter(str_detect(text, "Florence Mutua")|
                                      str_detect(quoted_text, "Florence Mutua") |
                                      str_detect(retweet_text, "Florence Mutua")) %>%
                             mutate(politician = "Florence Mutua"),
                           busia_sample_politicians %>%
                             as_tibble() %>%
                             filter(str_detect(text, "Patrobas Odhiambo")|
                                      str_detect(quoted_text, "Patrobas Odhiambo") |
                                      str_detect(retweet_text, "Patrobas Odhiambo")) %>%
                             mutate(politician = "Patrobas Odhiambo"),
                           busia_sample_politicians %>%
                             as_tibble() %>%
                             filter(str_detect(text, "Nobert Wangalwa")|
                                      str_detect(quoted_text, "Nobert Wangalwa") |
                                      str_detect(retweet_text, "Nobert Wangalwa")) %>%
                             mutate(politician = "Nobert Wangalwa"),
                           busia_sample_politicians %>%
                             as_tibble() %>%
                             filter(str_detect(text, "Joseph Oyula")|
                                      str_detect(quoted_text, "Joseph Oyula") |
                                      str_detect(retweet_text, "Joseph Oyula")) %>%
                             mutate(politician = "Joseph Oyula"))

#Analyzing the hashtags
busia_politicians_hashtags <- busia_politicians[!is.na(busia_politicians$hashtags),]
busia_politicians_hashtags$hashtags 


#Analyzing the popularity by tweets
# busia_politicians %>%
#   group_by(politician) %>% 
#   summarise(Mentions = n())

#Analyzing top tweets in R
# table(busia_politicians$text) %>% 
#       as.data.frame() %>%
#       arrange(desc(Freq)) %>% head(10) %>% 
#        transmute(Tweet = Var1)

#Sentiment_Analysis----
#Cleaning up the data
text_clean <- busia_sample_politicians %>% 
  mutate(text = gsub("http.*","",text)) %>% 
  mutate(text = gsub("https.*","",text)) %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#10most mentioned words in our data
# text_clean %>%
#   count(word, sort = TRUE) %>%
#   top_n(10) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(x = word, y = n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip() +
#   labs(y = "Frequency of Words",
#        x = "words",
#        title = "Top 10 most used words in tweets")

#Sentiments_tweet
sentiments_tweet <- text_clean %>%  
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  scale_fill_manual(values = c("red2", "green3")) +
  facet_wrap(~sentiment, scales = "free_y") +
  ylim(0, 2500) + 
  labs(y = NULL, x = NULL) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none")

