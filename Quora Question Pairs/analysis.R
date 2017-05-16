# Read the data
library(readr)
train2 <- read_csv("~/GitHub/Kaggle-Projects/Quora Question Pairs/train.csv/train.csv")
View(train)

train$id <- as.factor(train$id)
train$qid1 <- as.factor(train$qid1)
train$qid2 <- as.factor(train$qid2)
train$is_duplicate <- as.factor(train$is_duplicate)

str(train)


#Word Count

f.word.count <- function(my.list) { sum(stringr::str_count(my.list, "\\S+")) }

my.list <- list(train = train)

# Random Selection

sample.df <- data.frame(text.source = train,
                       line.count = NA, word.count = NA)

set.seed(324)
percent <- 0.05
randoms <- lapply(my.list, function (x) rbinom(x, 1, percent))

sample.list <- list(train = NA)

for (i in 1:length(my.list)) {
        sample.list[[i]] <- my.list[[i]][randoms[[i]] == 1]
}
# get counts of sample.list
sample.df$line.count <- sapply(sample.list, length)
sample.df$word.count <- sapply(sample.list, f.word.count)

#Helper Functions
removeURL <- function(x) gsub("http:[[:alnum:]]*", "", x)
removeHashTags <- function(x) gsub("#\\S+", "", x)
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)
removeQuestionMarks <- function(x) gsub("?","",x)

# Creating Corpus Class
#Not Required
text.corpus <- tm::Corpus(VectorSource(sample.list))
rm(sample.list)


# Doing N-Grams to find the similarity

qsn1_bigrams <- train %>%
        unnest_tokens(bigram, question1, token = "ngrams", n = 2)

qsn2_bigrams <- train %>%
        unnest_tokens(bigram, question2, token = "ngrams", n = 2)


qsn1_bigrams_separated <- qsn1_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")

qsn2_bigrams_separated <- qsn2_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- qsn1_bigrams_separated %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word)
bigrams_united <- bigrams_filtered %>%
        unite(bigram, word1, word2, sep = " ")
