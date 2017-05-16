require("tm")


x=train$question1
x=as.character(x)
x=tolower(x)
x=removePunctuation(x)
x=removeNumbers(x)
stopWords = stopwords("en")
extrastop = c("rt","a","the","is","an","was","can","its","via","will","for")
x=removeWords(x,stopWords)
x=removeWords(x,extrastop)


y=train$question2
y=as.character(y)
y=tolower(y)
y=removePunctuation(y)
y=removeNumbers(y)
stopWords = stopwords("en")
extrastop = c("rt","a","the","is","an","was","can","its","via","will","for")
y=removeWords(y,stopWords)
y=removeWords(y,extrastop)


train$question1 = x
train$question2 = y

tr_qsn1M <- create_matrix(train["question1"],language="english", removeStopwords=FALSE)

