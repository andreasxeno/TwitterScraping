#Twitter Scrape

    # Preliminaries ============================
    setwd("~/OneDrive - University of New Haven/Spring 2022/BANL 6420-Unsupervised Machine Learning/Week 14 4.25.2022")
    pacman::p_load(rtweet, tidytext, ggplot2, dplyr)
    library(purrr)
    library(tidyverse)
    
    library("twitteR")
    library("openssl")
    library("httpuv")
    library("tm")
    library("stringr")
    library("dplyr")
    library(ROAuth)
    library(openssl)
    library(httpuv)

    options(digits = 3, scipen = 9999)
    remove(list = ls())

# Setup =============================================
# You need to create a developer account - via the Twitter API to create access tokens.
# Resource https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
# Resource: https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
#You will need an app key, consumer key, 
# and access tokens. 


# the name you assigned to your created app
appname = "###############"

## api key 
key = "#################"

## api secret
secret = "################"

access_token = "###############################"
access_secret = "#################################"


# Use create_token() to generate authorization tokens so that you may pull your 
# tweets.
# Here i create token named "twitter_token"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

      # Search Twitter =================================
      ## search for 2500 tweets using the appropriate hashtags
      TAGS = c("#Nets",  "#Celtics", "#NETS")
      # search terms
      SEARCH = paste(TAGS,collapse=" OR ")
      
      ff_tweets <- search_tweets(SEARCH,
                                     n = 2500,
                                     lang = "en", 
                                 include_rts = FALSE) #not including retweets butin the future you might need it
            str(ff_tweets)
            names(ff_tweets)
            unlist(ff_tweets$hashtags)
            
      ff_tweets = ff_tweets %>% filter(hashtags %in% c("Celtics", 
                                                       "celtics",
                                                       "nets",
                                                       "Nets", "NETS")) 

      ff_tweets$hashtags
      
      #saveRDS(ff_tweets, "ff_tweets")
      
  # Class starts Here ==================================================    
      
      ff_tweets = readRDS("ff_tweets")
      
      
# view the first 3 rows of the dataframe
#head(pr_tweets, n = 3)
head(ff_tweets, n = 3)

          # view columns
          head(ff_tweets$text)
          names(ff_tweets)


ff_tweets_df = ff_tweets %>% 
                  select(hashtags, text) %>% 
                      unnest_tokens(word,text) 

  
#creating a tidy text right here 

my_stop_words = stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", 
                                "rt", "amp","4yig9gzh5t",
                                "15", "10th","4",
                                "1","78","3")))

# Word Counts
ff_tweets_df %>% count(word,hashtags, sort=T) %>% 
  #slice(1:50) %>% 
  slice_max(n, n= 100) %>%
  #anti_join(stop_words)%>%
  anti_join(my_stop_words) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = word)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  theme(axis.text.x = element_text(angle = 60,hjust = 1), 
        legend.position = "NULL") + 
  xlab("")


      # Sentiment
      nrc_lex <- get_sentiments("nrc")

      fn_sentiment = ff_tweets_df %>% inner_join(nrc_lex)

      
      fn_sentiment$hashtags = unlist(fn_sentiment$hashtags)        

fn_sentiment %>% 
  filter(!is.na(sentiment)) %>% 
    group_by(sentiment) %>% 
      count(sentiment, hashtags, sort = TRUE) %>%
  ggplot(aes(x = reorder(sentiment, n), n, fill = sentiment)) + 
  geom_bar(stat = "identity") + 
  #theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  xlab("") +
  coord_flip() +
  facet_grid(~hashtags)


            # Create a corpus =============================
            tweetCorpus = quanteda::corpus(ff_tweets$text)
                summary(tweetCorpus)
    
            mywords = c("#", "retweet", "@", "1", "2","3", "7")

          dfmtweet = tweetCorpus %>% 
            char_remove( "^.n.+$", valuetype = "regex") %>%
              tokens(what = "word",
                remove_punct = TRUE, 
                  remove_numbers = TRUE, 
                   remove_symbols = TRUE, 
                    remove_url = TRUE) %>% 
            tokens_remove(stopwords("english")) %>%
            tokens_remove(pattern = phrase(mywords),
                          valuetype = "regex") %>%
            tokens_wordstem() %>%
                dfm() %>% 
                  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*")) %>% 
                    dfm_remove(pattern = stopwords("en"))

ndoc(dfmtweet)

        
        quanteda.textplots::textplot_wordcloud(dfmtweet, max_words = 250, 
                             color = RColorBrewer::brewer.pal(8, "Dark2"))


  # view the first 12 users
  head(ff_tweets$screen_name, n = 12)
  n_distinct(ff_tweets$user_id)
        
        # how many locations are represented
        length(unique(ff_tweets$location))
    
ff_tweets %>% count(location, sort = TRUE)

ff_tweets %>%
  group_by(location) %>%
  count(location, sort = TRUE)%>%
  ungroup() %>%
  slice_max(n, n = 20) %>%
      #slice(-1) %>%
  ggplot(aes(reorder(location, n), n, fill = location)) +
  geom_col() + 
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Twitter users - unique locations ") +
  theme(legend.position = "NULL")



    # Time ==============================
    library(lubridate)

    names(users)
    
        ( ud = na.omit(ff_tweets$quoted_created_at) )
        udate = as_datetime(ud)
            str(udate)
        u_df = cbind.data.frame(udate, index = 1:150)
        
        str(u_df)
        u_df = arrange(u_df, udate) 
            #u_df= u_df %>% dplyr::slice(-c(1:2))
        
        udf = u_df %>% filter(udate > "2022-04-24")
        
        plot(udf$index ~ udf$udate)
        
        # Apply loess function
        x = 1:92
        values <- loess(udf$index ~ x)    
        plot(udf$index ~x)                      
        lines(predict(values),
              col = "blue",
              lwd = 2)
      
    # ==================== #
    # Semantic Network ======================

    dis_tib = tibble(text = ff_tweets$text )
      str(dis_tib)
    
    my_stop_words = rbind(stop_words, tibble(lexicon = "personal", 
                                             word = c("http",
                                                      "https",
                                                      "amp",
                                                      "1",
                                                      "2",
                                                      "3",
                                                      "t.co")))
    tail(my_stop_words)

    remove_reg <- "&amp;|&lt;|&gt;|&?http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    remove_circ = "û"    
    
    #this command below is used to clean the tweets 
    
    dis_tib %>% 
      mutate(text = str_remove_all(text, remove_reg)) %>%
      mutate(text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
      mutate(text = str_remove_all(text, remove_circ)) %>%
      mutate(text = gsub(x = text, pattern = "û", replacement = "")) %>%
      unnest_tokens(word, text) %>%
      anti_join(my_stop_words) %>%
     count(word,sort=T) %>% 
      slice_max(n, n= 20) %>%
      #slice(-1) %>%
      #anti_join(stop_words)%>%
      anti_join(stop_words) %>%
      ggplot(aes(x = reorder(word, n), y = n, fill = word)) + 
      geom_bar(stat = "identity") + 
      coord_flip()+
      theme(axis.text.x = element_text(angle = 60,hjust = 1), 
            legend.position = "NULL") + 
      xlab("")
    
    # Visualizing Semantic Network ========================
    # A visualization broadens the perspective by including information
    # about relationships across the units.
    
    # We deploy a feature co-occurrence matrix
    
      fcmat_news <- fcm(dfmtweet)
    
        dim(fcmat_news)
    
    topfeatures(fcmat_news, 25)
    
    ( feat = names(topfeatures(fcmat_news, 150))  )
    
    fcmat_tweet_select = fcm_select(fcmat_news, 
                                   pattern = feat, 
                                   selection = "keep")
      dim(fcmat_tweet_select)
  
    set.seed(12345)
    quanteda.textplots::textplot_network(fcmat_tweet_select, 
                     min_freq = 0.95,
                     vertex_size = 3,
                     vertex_labelsize = 7,
                     vertex_labelcolor = "navyblue",
                     #edge_alpha = 0.95,
                     edge_color = "darkred")
    
    
    # ==========================  #
    # Package sentimentr
    
    library(sentimentr)
    
    tweet_sentences_data = sentiment(get_sentences(ff_tweets$text)) %>% 
      group_by(element_id) %>% 
      summarize(meanSentiment = mean(sentiment))
    
    head(tweet_sentences_data)
    
    print(paste0("Most negative tweets sentiment: ", min(tweet_sentences_data$meanSentiment)))
    print(paste0("Most positive tweets sentiment: ", max(tweet_sentences_data$meanSentiment)))
    print(paste0("# of Negative Tweets: ", sum(tweet_sentences_data$meanSentiment < 0)))
    print(paste0("# of Neutral Tweets: ", sum(tweet_sentences_data$meanSentiment == 0)))
    print(paste0("# of Positive Tweets: ", sum(tweet_sentences_data$meanSentiment > 0)))

        
    #selecting top 50 tweets by favorites
    ff_tweets$favorite_count
    
    names(ff_tweets)
    
    user_sentiment  = ff_tweets %>% 
      select(user_id, text, favorite_count, hashtags) %>% 
      arrange(desc(favorite_count)) %>% 
      slice(1:50)
    
    head(user_sentiment)
    
    out <- sentiment_by(get_sentences(user_sentiment$text), 
                        list(user_sentiment$user_id))
    plot(out)
    
    out2 <- sentiment_by(get_sentences(user_sentiment$text), 
                        list(user_sentiment$hashtags))
    plot(out2)
    
    
    # QED ==============================