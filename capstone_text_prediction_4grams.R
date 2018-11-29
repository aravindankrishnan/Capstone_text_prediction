## Load required packages
library(tidytext) # tokenization
library(tidyr) # working with tidy data
library(stringr)
library(dplyr) # working with tidy data and using pipe operator for ease of coding and understanding
library(tm) # preprocessing of text read into R
library(wordcloud) # visualisation of important N-Grams
library(ggplot2) # frequency plots
library(wordcloud)

########### Read Blogs, News and Twitter Data #####################################################################

## Read Blogs data
Rprof("~/timing.txt")
con_blogs <- "en_US.blogs.txt"
en_us_blogs <- readLines(con_blogs,500000,encoding = "UTF-8") # Use UTF-8 to avoid bad unicode conversion
head(en_us_blogs)
en_us_blogs_clean <- removeNumbers(en_us_blogs)
en_us_blogs_df <- data_frame(document="blogs",line_number=1:length(en_us_blogs_clean),text=en_us_blogs_clean)
en_us_blogs_df

## Read News Data

con_news <- "en_US.news.txt"
en_us_news <- readLines(con_news,600000,encoding = "UTF-8") # Use UTF-8 to avoid bad unicode conversion
en_us_news_clean <- removeNumbers(en_us_news)
en_us_news_df <- data_frame(document="news",line_number=1:length(en_us_news_clean),text=en_us_news_clean)
en_us_news_df

## Read Twitter Data
con_twitter <- "en_US.twitter.txt"
en_us_twitter <- readLines(con_twitter,1000000,encoding = "UTF-8") # Use UTF-8 to avoid bad unicode conversion
en_us_twitter_clean <- removeNumbers(en_us_twitter)
en_us_twitter_df <- data_frame(document="twitter",line_number=1:length(en_us_twitter_clean),text=en_us_twitter_clean)
en_us_twitter_df

# Combine all data into one single dataset
en_us_all_df <- rbind(en_us_blogs_df,en_us_news_df,en_us_twitter_df)
en_us_all_df
summaryRprof("~/timing.txt")
Rprof(NULL)


############# Start of Build 1-gram table ##############

#1.8 Build 1-gram tokens
Rprof("~/timing.txt")
en_us_all_tidy_1 <- en_us_all_df %>% unnest_tokens(word,text) # 34721704
en_us_all_tidy_1

summaryRprof("~/timing.txt")
Rprof(NULL)

#1.10 1-gram frequency table
Rprof("~/timing.txt")
en_us_all_count_1 <- en_us_all_tidy_1 %>% count(word)
en_us_all_count_1
summaryRprof("~/timing.txt")
Rprof(NULL)
rm(en_us_all_tidy_1)

##### Start of 2-gram Table ###########################

#2.1 Build 2-gram table
Rprof("~/timing.txt")
en_us_all_tidy_2 <- en_us_all_df %>% unnest_tokens(word,text,token="ngrams",n=2)
summaryRprof("~/timing.txt")
Rprof(NULL)

object.size(en_us_all_tidy_2)# 1.1 GB
en_us_all_tidy_2

#2.2 separate the bigrams
Rprof("~/timing.txt")
en_us_all_tidy_2<- en_us_all_tidy_2 %>% separate(word,c("word1","word2"),sep=" ")
summaryRprof("~/timing.txt")
Rprof(NULL)
en_us_all_tidy_2

#2.5 Build 2-gram frequency table
Rprof("~/timing.txt")
en_us_all_count_2 <- en_us_all_tidy_2 %>% count(word1,word2)
en_us_all_count_2 <- filter(en_us_all_count_2,en_us_all_count_2$n>2)
summaryRprof("~/timing.txt")
Rprof(NULL)
rm(en_us_all_tidy_2)

##### End of Build 2-gram table #####################
#----------------------------------------------------------------------------#####

#--------------------------------------------------------------------------------#

##### Start of 3-gram Table ###########################

#3.1 Build 3-gram table
Rprof("~/timing.txt")
en_us_all_tidy_3 <- en_us_all_df %>% unnest_tokens(word,text,token="ngrams",n=3)
summaryRprof("~/timing.txt")
Rprof(NULL)

object.size(en_us_all_tidy_3)# 1.1 GB
en_us_all_tidy_3

#3.2 separate the trigrams
Rprof("~/timing.txt")
en_us_all_tidy_3<- en_us_all_tidy_3 %>% separate(word,c("word1","word2","word3"),sep=" ")
summaryRprof("~/timing.txt")
Rprof(NULL)
en_us_all_tidy_3


#3.5 Build 3-gram frequency table
Rprof("~/timing.txt")
en_us_all_count_3 <- en_us_all_tidy_3 %>% count(word1,word2,word3)
en_us_all_count_3 <- filter(en_us_all_count_3, en_us_all_count_3$n>2)
summaryRprof("~/timing.txt")
Rprof(NULL)
rm(en_us_all_tidy_3)
##### End of Build 3-gram table #####################
#----------------------------------------------------------------------------#####


##### Start of 4-gram Table ###########################

#4.1 Build 4-gram table
#Rprof("~/timing.txt")
en_us_all_tidy_4 <- en_us_all_df %>% unnest_tokens(word,text,token="ngrams",n=4)
#summaryRprof("~/timing.txt")
#Rprof(NULL)

object.size(en_us_all_tidy_4)# 1.1 GB
en_us_all_tidy_4

#4.2 separate the 4-grams
#Rprof("~/timing.txt")
en_us_all_tidy_4<- en_us_all_tidy_4 %>% separate(word,c("word1","word2","word3","word4"),sep=" ")
#summaryRprof("~/timing.txt")
#Rprof(NULL)
en_us_all_tidy_4

#4.5 Build 4-gram frequency table
#Rprof("~/timing.txt")
en_us_all_count_4 <- en_us_all_tidy_4 %>% count(word1,word2,word3,word4,sort = TRUE)
en_us_all_count_4 <- filter(en_us_all_count_4,en_us_all_count_4$n>2)
#summaryRprof("~/timing.txt")
#Rprof(NULL)
rm(en_us_all_tidy_4)
##### End of Build 4-gram table #####################
#----------------------------------------------------------------------------#####


##### 5. Start of Text Prediction Algorithm with 3-gram model #########################################

Rprof("~/timing.txt")


## 5.1 Extract last 3 words of input text

quadgram_discount <- 0.5
trigram_discount <- 0.5
bigram_discount <- 0.5

# get the trigram prefix from input text
select_ngram <- function(input_string){
input_split <- unlist(strsplit(input_string," "))
input_split[(length(input_split)-2):length(input_split)]
}

input_string <- "how are you" # hardcoded inout string for testing only, will be made reactive in shiny app
trigram_prefix <- select_ngram(input_string)
#trigram_prefix <- select_ngram(input_string)
## 5.2 Check observed 4-gram counts 

#bigram_prefix <- select_ngram(input_string,3)

obs_quadgram_table <- filter(en_us_all_count_4,word1==trigram_prefix[1]& word2==trigram_prefix[2]& word3==trigram_prefix[3])
obs_quadgram_table

## 5.3 Calculate denominator for probability of 4-grams (count of trigram prefix)

trigram_prefix_count <- filter(en_us_all_count_3,word1==trigram_prefix[1]&word2==trigram_prefix[2]&word3==trigram_prefix[3])$n
trigram_prefix_count
sum(obs_quadgram_table$n) # checking the count from bigram table matches the count from observed trigram table


###### **********************Comments***********************************

## bigram prefix count can be calculated from bigram table or from sum of all counts in observed trigram table 
## based on the bigram prefix. there is a possibility that some trigrams containng the bigram prefix 
## got removed due to the third word being filtered out in a cleanup. hence the two counts may not match.

###### **********************End of Comments*****************************


## 5.4 Calculate probability of observed 4-grams based on trigram prefix

obs_quadgram_table$prob <- (obs_quadgram_table$n-quadgram_discount)/trigram_prefix_count
obs_quadgram_table # observed 4-gram probabilities table - no adjustments needed to the entries in this table.

## 5.5 Calculate left over probability mass for unobserved 4-grams
quad_leftover_prob <- 1-(sum(obs_quadgram_table$prob))
quad_leftover_prob 

# this should be distributed to all trigrams - observed(excluding those in observed quadgram) 
# and unobserved that has been adjusted to 3-gram level.

## look for unobserved 4-grams

## 5.3.1 Look for observed 3-grams

obs_bo_trigrams <- filter(en_us_all_count_3,word1==trigram_prefix[2]&word2==trigram_prefix[3])
obs_bo_trigrams

## calculate probabilities of observed 3-grams.
obs_bo_trigrams$prob <- (obs_bo_trigrams$n-trigram_discount)/sum(obs_bo_trigrams$n)
obs_bo_trigrams
sum(obs_bo_trigrams$prob)

# calculate left over probability from observed trigrams to allocate to unobserved trigrams
tri_leftover_prob <- 1-sum(obs_bo_trigrams$prob)
tri_leftover_prob

# Back off to 2-grams to look for observed 2-grams with THE_WORD4. 
# the probabilities need to be adjusted back to 3-grams and 4-grams

obs_bo_bigrams <- filter(en_us_all_count_2,word1==trigram_prefix[3])
obs_bo_bigrams

# calculate the probabilities of the observed bigrams 
# in order to extract remaining probability mass for unobserved bigrams

obs_bo_bigrams$prob <- (obs_bo_bigrams$n-bigram_discount)/sum(obs_bo_bigrams$n)
obs_bo_bigrams

bi_leftover_prob <- 1-sum(obs_bo_bigrams$prob)
bi_leftover_prob

# Find unobserved unigrams across 2,3 and 4 grams -base it on 2-grams as that will be a superset of 3 and 4-grams as well

unigram_tail <- en_us_all_count_1[!en_us_all_count_1$word %in% obs_bo_bigrams$word2,]
unigram_tail

## distribute the left over bigram probability to these unobserved unigrams in 2,3 and 4 grams

unigram_tail$prob <- bi_leftover_prob*unigram_tail$n/sum(unigram_tail$n)
unigram_tail

#change structure of unigram tail table to match structure of observed 2-gram table

unigram_tail$word1 <- trigram_prefix[3]
unigram_tail
names(unigram_tail)[1] <- "word2"
unigram_tail <- unigram_tail[,c(4,1,2,3)]
unigram_tail

obs_bo_bigrams # 2-gram table structure

# remove tail word in bigrams which was already observed in trigrams (and hence 4-grams as well)
obs_bo_bigrams_unique <- obs_bo_bigrams[!obs_bo_bigrams$word2 %in% obs_bo_trigrams$word3,]
obs_bo_bigrams_unique

# combine unigram tail and unique BO bigram table to get final BO bigram table

total_bo_bigrams <- rbind(obs_bo_bigrams_unique,unigram_tail)
total_bo_bigrams

# Adjust to Trigram level - Allocate trigram left over probability to total BO bigram table - 

total_bo_bigrams$triprob <- tri_leftover_prob*total_bo_bigrams$prob/sum(total_bo_bigrams$prob)
total_bo_bigrams

# Match structure of 3-grams

adj_bo_trigrams <- total_bo_bigrams
names(adj_bo_trigrams)[1:2] <- c("word2","word3") # Rename columns to fit word1,word2,word3
adj_bo_trigrams$word1 <- trigram_prefix[2]
adj_bo_trigrams <- adj_bo_trigrams[,c(6,1,2,3,4,5)]
adj_bo_trigrams <- adj_bo_trigrams[,-5]# remove old probability column
names(adj_bo_trigrams)[5] <- "prob"
adj_bo_trigrams

# check tail word in obs trigram table not in obs 4-gram table

obs_bo_trigrams_unique <- obs_bo_trigrams[!obs_bo_trigrams$word3 %in% obs_quadgram_table$word4,]
obs_bo_trigrams_unique

# Combine adjusted BO trigrams and obs BO trigrams unique from 4-grams

total_bo_trigrams <- rbind(obs_bo_trigrams_unique,adj_bo_trigrams)
total_bo_trigrams

# Allocate 4-gram left over probability to total BO trigrams

total_bo_trigrams$quadprob <- quad_leftover_prob*total_bo_trigrams$prob/sum(total_bo_trigrams$prob)
total_bo_trigrams

# match total BO trigrams structure to 4-grams
names(total_bo_trigrams)[1:3] <- c("word2","word3","word4")
total_bo_trigrams$word1 <- trigram_prefix[1]
total_bo_trigrams <-  total_bo_trigrams[,c(7,1,2,3,4,6)]
names(total_bo_trigrams)[6]<- "prob"

# check observed 4-gram table
obs_quadgram_table

# bind observed and unobserved 4-grams

total_bo_quadgrams <- rbind(obs_quadgram_table,total_bo_trigrams)
arrange(total_bo_quadgrams,desc(prob))[1:20,]

summaryRprof("~/timing.txt")
Rprof(NULL)


sum(total_bo_quadgrams$prob)

object.size(en_us_all_count_1)
object.size(en_us_all_count_2)
object.size(en_us_all_count_3)
object.size(en_us_all_count_4)
