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
        tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
}


#predict <- function(job.title, w2v, gbm) {
#        words <- tokenize(as.character(as.h2o(job.title)))
#        job.title.vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
#        h2o.predict(gbm, job.title.vec)
#}


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
               "from","at","my","be","by","not","that","to","from","com","org","like","likes","so","how","where",
               "a","about","above","across","after","again","against","all","almost","alone","along","already","also",
               "although","always","am","among","an","and","another","any","anybody","anyone","anything","anywhere","are",
               "area","areas","aren't","around","as","ask","asked","asking","asks","at","away","b","back","backed",
               "backing","backs","be","became","because","become","becomes","been","before","began","behind","being",
               "beings","below","best","better","between","big","both","but","by","c","came","can","cannot","can't",
               "case","cases","certain","certainly","clear","clearly","come","could","couldn't","d","did","didn't",
               "differ","different","differently","do","does","doesn't","doing","done","don't","down","downed","downing",
               "downs","during","e","each","early","either","end","ended","ending","ends","enough","even","evenly",
               "ever","every","everybody","everyone","everything","everywhere","f","face","faces","fact","facts","far",
               "felt","few","find","finds","first","for","four","from","full","fully","further","furthered","furthering",
               "furthers","g","gave","general","generally","get","gets","give","given","gives","go","going","good","goods",
               "got","great","greater","greatest","group","grouped","grouping","groups","h","had","hadn't","has","hasn't","have",
               "haven't","having","he","he'd","he'll","her","here","here's","hers","herself","he's","high","higher","highest","him",
               "himself","his","how","however","how's","i","i'd","if","i'll","i'm","important","in","interest","interested","interesting",
               "interests","into","is","isn't","it","its","it's","itself","i've","j","just","k","keep","keeps","kind","knew","know","known","knows",
               "l","large","largely","last","later","latest","least","less","let","lets","let's","like","likely","long","longer","longest","m","made",
               "make","making","man","many","may","me","member","members","men","might","more","most","mostly","mr","mrs","much","must","mustn't","my","myself","n",
               "necessary","need","needed","needing","needs","never","new","newer","newest","next","no","nobody","non","noone","nor","not","nothing","now","nowhere",
               "number","numbers","o","of","off","often","old","older","oldest","on","once","one","only","open","opened","opening","opens","or","order","ordered",
               "ordering","orders","other","others","ought","our","ours","ourselves","out","over","own","p","part","parted","parting","parts","per","perhaps",
               "place","places","point","pointed","pointing","points","possible","present","presented","presenting","presents","problem","problems","put","puts",
               "q","quite","r","rather","really","right","room","rooms","s","said","same","saw","say","says","second","seconds","see","seem","seemed","seeming",
               "seems","sees","several","shall","shan't","she","she'd","she'll","she's","should","shouldn't","show","showed","showing","shows","side","sides",
               "since","small","smaller","smallest","so","some","somebody","someone","something","somewhere","state","states","still","such","sure","t","take",
               "taken","than","that","that's","the","their","theirs","them","themselves","then","there","therefore","there's","these","they","they'd","they'll",
               "they're","they've","thing","things","think","thinks","this","those","though","thought","thoughts","three","through","thus","to","today","together",
               "too","took","toward","turn","turned","turning","turns","two","u","under","until","up","upon","us","use","used","uses","v","very","w","want","wanted",
               "wanting","wants","was","wasn't","way","ways","we","we'd","well","we'll","wells","went","were","we're","weren't","we've","will","with","within","without","won't","work","worked",
               "working","works","would","wouldn't","x","y","year","years","yes","yet","you","you'd","you'll","young","younger","youngest","your","you're","yours",
               "yourself","yourselves","you've","z")



print("Break questions into sequence of words")
words <- tokenize(questions.hex$question)


print("Build word2vec model")
vectors <- 30 # Only 10 vectors to save time & memory
w2v.model <- h2o.word2vec(words
                          , model_id = "w2v_model"
                          , vec_size = vectors
                          , min_word_freq = 5
                          , window_size = 5
                          , init_learning_rate = 0.05
                          , sent_sample_rate = 0
                          , epochs = 15) # only a one epoch to save time
h2o.rm('questions.hex') # no longer needed

print("Sanity check - find synonyms for the word 'water'")
print(h2o.findSynonyms(w2v.model, "water", count = 5))
print(h2o.findSynonyms(w2v.model, "where", count = 5))
print(h2o.findSynonyms(w2v.model, "data", count = 5))
print(h2o.findSynonyms(w2v.model, "technology", count = 5))
print(h2o.findSynonyms(w2v.model, "analytics", count = 5))
print(h2o.findSynonyms(w2v.model, "automation", count = 5))
print(h2o.findSynonyms(w2v.model, "country", count = 5))
print(h2o.findSynonyms(w2v.model, "politics", count = 5))
print(h2o.findSynonyms(w2v.model, "bedroom", count = 5))


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

ts1.copy = ts1
# Change this wrt to number of vectors
a = as.data.frame(ts1[,5:34])
b = as.data.frame(ts1[,35:64])

#Commenting for trying somehting new
#temp1 <- as.matrix(rowSums(a * b))

#temp12<- as.data.frame(sqrt(rowSums(a*a)))
#temp13 <- as.data.frame(sqrt(rowSums(b*b)))
#temp <- temp12*temp13


temp1 <- as.data.frame((a * b))

nrow(temp1)

temp12<- as.data.frame(((a*a)))
temp13 <- as.data.frame(((b*b)))

temp14 = temp12+temp13

nrow(temp14)

temp14 = sqrt(temp14)

test23 = temp1/temp14

test23 = cbind(test23,ts1$id,ts1$is_duplicate)
nrow(test23)
nrow(ts1)
ncol(ts1)

rm(ts2)
rm(questions.hex)
rm(a)
rm(b)


ts1.noNA <- ts1[which(!(is.na(ts1$q1_vec_C1)|is.na(ts1$q2_vec_C1))),]
test23.noNA <- test23[which(!is.na(test23$q1_vec_C1)),]
nrow(ts1.noNA)

nrow(test23.noNA)

class(test23.noNA)

class(test23.noNA$`ts1$is_duplicate`)

test23.noNA$`ts1$is_duplicate` = as.numeric(test23.noNA$`ts1$is_duplicate`)
test23.noNA$`ts1$id` = as.numeric(test23.noNA$`ts1$id`)
class(test23.noNA$`ts1$is_duplicate`)
class(test23.noNA$`ts1$id`)

#test23.noNA[which(test23.noNA$`ts1$is_duplicate`==1),]

#ncol(test23.noNA)
# Here we are getting the mean of all teh ectors or which they are duplicates. But would this work ?
test23.col <- as.data.frame(colMeans(test23.noNA[which(test23.noNA$`ts1$is_duplicate`==1),1:30]))

test23.col = data.frame(t(test23.col))
#maintaingina cipy of the data
test23.col.cp = test23.col
#test23.col = test23.col.cp
test23.col = cbind(test23.col,test23.col)


test24.col = subset(ts1, select = -c(question1,question2,is_duplicate))
 
rm(STOP_WORDS)
rm(temp1)
rm(temp12)
rm(temp13)
rm(temp14)
#rm(test23.col)
rm(test23.noNA)
rm(test25.col)
rm(ts1.noNA)
rm(question_all.vecs)
rm(questions_all)
rm(test23)
rm(questions)

test24.col <- test24.col[which(!is.na(test24.col$q1_vec_C1)),]
test24.col <- as.matrix(test24.col)
#rm(test24.col)
test25.col = test24.col

#test24.col = test25.col
nrow(test24.col)
nrow(test23.col)




#test23.col = test23.col.cp
test23.col <- as.matrix(test23.col)
#test24.col <- as.matrix(test24.col)

if(test24.col[1,10] >=test23.col[1,9]){
        print("HEllo") 
        } else{
        print("Hi") 
                }
        


# For all elements that have similarity greater thatn the mean of the similarity of the 
for (j in 1:nrow(test24.col)) {
        for(i in 1:ncol(test23.col)){
                if(test24.col[j,i+1] >=test23.col[1,i]) {
                        test24.col[j,i+1] = 1
                } else {
                        test24.col[j,i+1] = 0
                }
        }
        
}

#Checking the data.
nrow(test23.col)
ncol(test23.col)
ncol(test23.noNA)

class(ts1)

#ts1$cosine_res = rowMeans(test23)


rm(temp12)
rm(temp13)
rm(temp)
rm(temp1)
rm(test23)

ts1 = as.data.frame(ts1)

#ts1$cosine <- temp1/temp
#ts1 = ts1[which(!is.na(ts1$cosine_res)),]

ts1 = ts1[which(!is.na(ts1$q1_vec_C1)),]

nrow(ts1)

#temp_res = ts1[ts1$cosine_res > 0.90,]
#temp_res = subset(temp_res, select = c("is_duplicate","cosine_res"))
#temp_res = temp_res[which(!is.na(temp_res$cosine_res)),]

nrow(ts1)

nrow(temp_res)

nrow(ts1[ts1$is_duplicate == 1,])
nrow(temp_res[temp_res$is_duplicate == 1,])
nrow(temp_res[temp_res$is_duplicate == 0,])


ts1.result <- subset(ts1, select = c(question1,question2,id,is_duplicate,cosine_res))
ts1.res.dup = ts1.result[ts1.result$is_duplicate==1,]
ts1.res.nondup = ts1.result[ts1.result$is_duplicate==0,]
nrow(ts1.res.dup)
nrow(ts1.res.nondup)


mean(ts1.res.dup$cosine_res)
mean(ts1.res.nondup$cosine_res)


names(ts1.res.dup)[5]
colnames(ts1.res.dup)[5] = "cosine"
colnames(ts1.res.nondup)[5] <- "cosine"
View(ts1.res.dup)
View(ts1.res.nondup)



#mean(ts1.res.dup$cosine)

#which(is.na(ts1.res.dup$cosine))
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


