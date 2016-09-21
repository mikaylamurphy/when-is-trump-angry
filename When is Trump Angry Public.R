When_is_Trump_angry <- function(){
# Sentiment analysis of Trump's tweets and speeches.
# Mikayla Murphy Sept 2016
# mikayla [at] mit [dot] edu
  
require(twitteR)
require(dplyr)
require(purrr)
require(tidytext)
require(splitstackshape)

# Import Trump speeches from txt file.  
trump_speeches <- readLines("Trump Speeches.txt", warn = FALSE, encoding = 'latin1')

# Import tweets using twitteR package and convert to dataframes. Add in your own consumer_key and access_token here.
consumer_key <- "redacted"
consumer_secret <- "redacted"
access_token <- "redacted"
access_secret <- "redacted"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

trump_tweets <- userTimeline("realDonaldTrump", n = 3200)
trump_tweets <- tbl_df(map_df(trump_tweets, as.data.frame))

# Importing Clinton tweets.
# clinton_tweets <- userTimeline("HillaryClinton", n = 3200)
# clinton_tweets <- tbl_df(map_df(clinton_tweets, as.data.frame))

# Based off analysis by Variance Explained, removing Trump tweets not from an Android, contain links, or that are direct quotes from other users (as these aren't Trump's words).
trump_tweets <- trump_tweets[grep("android", trump_tweets$'statusSource'), ]
trump_tweets <- trump_tweets[-(grep('^\"', trump_tweets$'text')),]
trump_tweets <- trump_tweets[-(grep('t.co', trump_tweets$'text')),]

# Formatting Trump tweets datatable with one word per line.
trump_tweets$'tweet' <- 'tweet'
trump_tweets <- trump_tweets[ , c('text', 'tweet')]
trump_tweets <- unnest_tokens(trump_tweets, words, text)

# Creating dataframe for Trump speeches.
speech <- rep('speech', length(trump_speeches))
trump_speeches <- data.frame(trump_speeches, speech)
colnames(trump_speeches) <- c('text', 'tweet')

# Removing empty and header rows from Trump speeches dataframe.
trump_speeches[trump_speeches == ""] <- NA
trump_speeches <- na.omit(trump_speeches)
trump_speeches <- trump_speeches[-(grep('SPEECH', trump_speeches$'text')),]

# Formatting Trump speeches datatable with one lowercase word per line and removing punctuation marks.
trump_speeches <- apply(trump_speeches, 2, as.character)
trump_speeches <- cSplit(trump_speeches, "text", " ", "long")
trump_speeches$text <- chartr('.', ' ', trump_speeches$'text')
trump_speeches$text <- chartr(',', ' ', trump_speeches$'text')
trump_speeches$text <- gsub('<e2><80><99>', '\'', trump_speeches$'text')
trump_speeches <- trump_speeches[-(grep('<e2>', trump_speeches$'text')),]
trump_speeches$text <- gsub(' ', '', trump_speeches$'text')
trump_speeches$text <- gsub("?", '', trump_speeches$'text')
trump_speeches$text <- gsub('\"', '', trump_speeches$'text')
trump_speeches$'text' <- tolower(trump_speeches$'text')

# Removing stop words (aka common words without useful content) from all dataframes.
trump_tweets <- anti_join(trump_tweets, stop_words, by = c('words' = 'word'))
trump_speeches <- anti_join(trump_speeches, stop_words, by = c('text' = 'word'))

# Counting how many time each word appears in tweets.
trump_tweets_count <- dplyr::count(trump_tweets, words, sort = TRUE)
trump_speeches_count <- dplyr::count(trump_speeches, text, sort = TRUE)

# Creating dataframe of sentiments.
nrc <- filter(sentiments, lexicon == 'nrc')
nrc <- dplyr::select(nrc, word, sentiment)

# Classifying words from tweets and speeches using sentiments dataframe.
trump_tweets_with_sentiments <- merge(trump_tweets_count, nrc, by.x = c('words'), by.y = c('word'))
trump_speeches_with_sentiments <- merge(trump_speeches_count, nrc, by.x = c('text'), by.y = c('word'))

# Counting number of occurences per sentiment in tweets and speeches.
trump_tweets_with_sentiments_count <- dplyr::count(trump_tweets_with_sentiments, sentiment, sort = TRUE)
trump_speeches_with_sentiments_count <- dplyr::count(trump_speeches_with_sentiments, sentiment, sort = TRUE)

# Assigning tweets to be red and speeches to be blue in plots.
trump_tweets_with_sentiments_count$'colour' <- as.integer(2)
trump_speeches_with_sentiments_count$'colour' <- as.integer(4)

# Calculating total number of sentiments expressed in tweets and speeches.
trump_tweets_sentiments_sum <- sum(trump_tweets_with_sentiments_count$'nn')
trump_speeches_sentiments_sum <- sum(trump_speeches_with_sentiments_count$'nn')
#total_sum <- sum_trump_speeches_sentiments + sum_trump_tweets_sentiments

# Calculating percentage of sentiments relative to total number of sentiments in tweets and total number of sentiments in speeches.
trump_tweets_with_sentiments_count$'percentage' <- trump_tweets_with_sentiments_count$'nn' / trump_tweets_sentiments_sum
trump_speeches_with_sentiments_count$'percentage' <- trump_speeches_with_sentiments_count$'nn' / trump_speeches_sentiments_sum

# Creating and ordering dataframe for plotting absolute numbers of sentiments for tweets and speeches.
trump_sentiment_count <- rbind(trump_tweets_with_sentiments_count, trump_speeches_with_sentiments_count)
trump_sentiment_count <- trump_sentiment_count[order(trump_sentiment_count$sentiment), ]

# Creating dataframe with total number of times sentiment is expressed across speeches and tweets.
total_per_trump_sentiment <- aggregate(trump_sentiment_count[, c(2)], by = list(sentiment = trump_sentiment_count$'sentiment'), FUN = sum)
colnames(total_per_trump_sentiment) <- c('sentiment', 'total sentiment')

# Divding each sentiment by the total number of times sentiment is expressed across speeches and tweets.
trump_sentiment_count <- merge(trump_sentiment_count, total_per_trump_sentiment)
trump_sentiment_count$'percentage of total' <- trump_sentiment_count$'nn' / trump_sentiment_count$'total sentiment'

# Plotting the three dataframes created above. I'd recommend using ggplot2 if you're want these plots to actually be pretty.
barplot(trump_sentiment_count$'nn', names.arg = trump_sentiment_count$'sentiment', col = trump_sentiment_count$'colour', cex.names = .5)
barplot(trump_sentiment_count$'percentage', names.arg = trump_sentiment_count$'sentiment', col = trump_sentiment_count$'colour', cex.names = .5)
barplot(trump_sentiment_count$'percentage of total', names.arg = trump_sentiment_count$'sentiment', col = trump_sentiment_count$'colour', cex.names = .5)

# Writes trump_sentiment_count data frame as a .csv for exporting data to Tableau.
write.csv(trump_sentiment_count, file = "trump_sentiments_count.csv", row.names = TRUE)
}
