# This is an example of H2O's recently added word2vec model which is loosely based
# on the script found here: https://github.com/h2oai/h2o-3/blob/master/h2o-r/demos/rdemo.word2vec.craigslistjobtitles.R
library(data.table)
library(h2o)
h2o.init(nthreads = -1)

# Function Declaration

tokenize <- function(sentences, stop.words = STOP_WORDS) {
        tokenized <- h2o.tokenize(sentences, "\\\\W+")
        
        # convert to lower case
        tokenized.lower <- h2o.tolower(tokenized)
        # remove short words (less than 2 characters)
        tokenized.lengths <- h2o.nchar(tokenized.lower)
        tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
        # remove words that contain numbers
        tokenized.words <- tokenized.lower[h2o.grep("[0-9]", tokenized.lower, invert = TRUE, output.logical = TRUE),]
        
        # remove stop words
        #tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
}




ts1 <- fread("C:/Users/vvrsk/Desktop/Kaggle/Quora/input/train.csv", select=c("id","question1","question2","is_duplicate"))
ts2 <- fread("C:/Users/vvrsk/Desktop/Kaggle/Quora/input/train.csv", select=c("test_id","question1","question2"))




print("Some question cleanup")
# It is important to remove "\n" -- it appears to cause a parsing error when converting to an H2OFrame
ts1[,":="(question1=gsub("'|\"|'|“|”|\"|\n|,|\\.|…|\\?|\\+|\\-|\\/|\\=|\\(|\\)|‘", "", question1),
          question2=gsub("'|\"|'|“|”|\"|\n|,|\\.|…|\\?|\\+|\\-|\\/|\\=|\\(|\\)|‘", "", question2))]
ts1[,":="(question1=gsub("  ", " ", question1),
          question2=gsub("  ", " ", question2))]

print("get list of unique questions")
# Using only questions from the training set because the test set has 'questions' that are fake
questions <- as.data.table(rbind(ts1[,.(question=question1)], ts1[,.(question=question2)]))
questions <- unique(questions)
questions.hex <- as.h2o(questions, destination_frame = "questions.hex", col.types=c("String"))

STOP_WORDS = c("ax","i","you","edu","s","t","m","subject","can","lines","re","what",
               "there","all","we","one","the","a","an","of","or","in","for","by","on",
               "but","is","in","a","not","with","as","was","if","they","are","this","and","it","have",
               "from","at","my","be","by","not","that","to","from","com","org","like","likes","so")



print("Break questions into sequence of words")
words <- tokenize(questions.hex$question)


print("Build word2vec model")
vectors <- 15 # Only 10 vectors to save time & memory
w2v.model <- h2o.word2vec(words
                          , model_id = "w2v_model"
                          , vec_size = vectors
                          , min_word_freq = 5
                          , window_size = 5
                          , init_learning_rate = 0.035
                          , sent_sample_rate = 0
                          , epochs = 3) # only a one epoch to save time

h2o.rm('questions.hex') # no longer needed

print("Sanity check - find synonyms for the word 'water'")
print(h2o.findSynonyms(w2v.model, "water", count = 5))
print(h2o.findSynonyms(w2v.model, "where", count = 5))
print(h2o.findSynonyms(w2v.model, "what", count = 5))

print("Get vectors for each question")
question_all.vecs <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")


print("Convert to data.table & merge results")
# Could do the rest of these steps in H2O but I'm a data.table addict
question_all.vecs <- as.data.table(question_all.vecs)
questions_all <- cbind(questions, question_all.vecs)
ts1 <- merge(ts1, questions_all, by.x="question1", by.y="question", all.x=TRUE, sort=FALSE)
ts1 <- merge(ts1, questions_all, by.x="question2", by.y="question", all.x=TRUE, sort=FALSE)
colnames(ts1)[5:ncol(ts1)] <- c(paste0("q1_vec_C", 1:vectors), paste0("q2_vec_C", 1:vectors))

print("output question vectors")
fwrite(ts1, "./h2ow2v_vectors.csv")

a = as.matrix(ts1[,5:14])
b = as.matrix(ts1[,15:24])

temp1 <- as.matrix(rowSums(a * b))

temp12<- as.data.frame(sqrt(rowSums(a*a)))
temp13 <- as.data.frame(sqrt(rowSums(b*b)))

temp <- temp12*temp13

ts1$cosine <- temp1/temp


#temp = temp %>% mutate(Prod = Reduce(`*`, .))
#temp <- subset(temp, select = Prod)
#temp <- as.matrix(temp)

#temp1/ temp

if(FALSE) {

####Test Data

ts2[,":="(question1=gsub("'|\"|'|“|”|\"|\n|,|\\.|…|\\?|\\+|\\-|\\/|\\=|\\(|\\)|‘", "", question1),
          question2=gsub("'|\"|'|“|”|\"|\n|,|\\.|…|\\?|\\+|\\-|\\/|\\=|\\(|\\)|‘", "", question2))]
ts2[,":="(question1=gsub("  ", " ", question1),
          question2=gsub("  ", " ", question2))]

questions2 <- as.data.table(rbind(ts2[,.(question=question1)], ts2[,.(question=question2)]))
questions2 <- unique(questions2)
questions2.hex <- as.h2o(questions2, destination_frame = "questions2.hex", col.types=c("String"))

STOP_WORDS = c("ax","i","you","edu","s","t","m","subject","can","lines","re","what",
               "there","all","we","one","the","a","an","of","or","in","for","by","on",
               "but","is","in","a","not","with","as","was","if","they","are","this","and","it","have",
               "from","at","my","be","by","not","that","to","from","com","org","like","likes","so")

print("Break questions into sequence of words")
words2 <- tokenize(questions2.hex$question)


print("Build word2vec model")
vectors <- 10 # Only 10 vectors to save time & memory
w2v.model2 <- h2o.word2vec(words2
                          , model_id = "w2v_model"
                          , vec_size = vectors
                          , min_word_freq = 5
                          , window_size = 5
                          , init_learning_rate = 0.025
                          , sent_sample_rate = 0
                          , epochs = 1) # only a one epoch to save time

h2o.rm('questions2.hex') # no longer needed

question_all2.vecs <- h2o.transform(w2v.model2, words2, aggregate_method = "AVERAGE")


print("Convert to data.table & merge results")
# Could do the rest of these steps in H2O but I'm a data.table addict
question_all2.vecs <- as.data.table(question_all2.vecs)
questions_all2 <- cbind(questions2, question_all2.vecs)
ts2 <- merge(ts2, questions_all2, by.x="question1", by.y="question", all.x=TRUE, sort=FALSE)
ts2 <- merge(ts2, questions_all2, by.x="question2", by.y="question", all.x=TRUE, sort=FALSE)
colnames(ts2)[5:ncol(ts2)] <- c(paste0("q1_vec_C", 1:vectors), paste0("q2_vec_C", 1:vectors))
ts2$qsn1_avg <- rowMeans(ts2[,5:14])
ts2$qsn2_avg <- rowMeans(ts2[,15:24])

numberofRows <- nrow(ts2)



}
#colnames(ts1)


