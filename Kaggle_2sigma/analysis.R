<<<<<<< HEAD
library(ggplot2)

packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

train.data <- fromJSON("input/train.json")

vars <- setdiff(names(train.data), c("photos", "features"))
train.data <- map_at(train.data, vars, unlist) %>% tibble::as_tibble(.)

head(train.data,n = 1)
nrow(train.data)
ncol(train.data)
colnames(train.data)
#Getting The Data of interest
train.doi <- as.data.frame(subset(train.data,select = -c(description,photos)))
train.doi$interest_level <- as.factor(train.doi$interest_level)
#train.doi$bathrooms <- as.factor(train.doi$bathrooms)
#train.doi$bedrooms <- as.factor(train.doi$bedrooms)


#Checking the different levels of the data available 
#levels(train.doi$bathrooms)
#levels(train.doi$bedrooms)

View(train.doi)

plot_intrst<- ggplot(train.doi,aes(interest_level,color = interest_level,fill=interest_level ))

pi.hist <- plot_intrst+geom_histogram(stat = "count")

pi.hist
plot_bed <- ggplot(train.doi,aes(x=interest_level, y=bedrooms,color = interest_level))+labs(title = "Interest level vs Bedrooms",x="Interest Level", y = "# of Bedrooms")

p.violin<- plot_bed+geom_violin(trim = FALSE)
p.violin

p.jitter <- plot_bed+geom_jitter(shape=16, position=position_jitter(0.05))
p.jitter

plot_bath <-ggplot(train.doi,aes(x=interest_level, y=bathrooms,color = interest_level))+labs(title = "Interest level vs Bedrooms",x="Interest Level", y = "# of Bedrooms")
plot_bath

pb.violin <- plot_bath+geom_violin(trim = FALSE)
pb.violin

pb.jitter <- plot_bath+geom_jitter(shape=16, position=position_jitter(0.05))
pb.jitter

#Looking at O/p variable
unique(train.data$interest_level)

summary(train.doi)
=======
library(ggplot2)

packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

train.data <- fromJSON("input/train.json")

vars <- setdiff(names(train.data), c("photos", "features"))
train.data <- map_at(train.data, vars, unlist) %>% tibble::as_tibble(.)

head(train.data,n = 1)
nrow(train.data)
ncol(train.data)
colnames(train.data)
#Getting The Data of interest
train.doi <- as.data.frame(subset(train.data,select = -c(description,photos)))
train.doi$interest_level <- as.factor(train.doi$interest_level)
#train.doi$bathrooms <- as.factor(train.doi$bathrooms)
#train.doi$bedrooms <- as.factor(train.doi$bedrooms)


#Checking the different levels of the data available 
#levels(train.doi$bathrooms)
#levels(train.doi$bedrooms)

View(train.doi)

plot_intrst<- ggplot(train.doi,aes(interest_level,color = interest_level,fill=interest_level ))

pi.hist <- plot_intrst+geom_histogram(stat = "count")

pi.hist
plot_bed <- ggplot(train.doi,aes(x=interest_level, y=bedrooms,color = interest_level))+labs(title = "Interest level vs Bedrooms",x="Interest Level", y = "# of Bedrooms")

p.violin<- plot_bed+geom_violin(trim = FALSE)
p.violin

p.jitter <- plot_bed+geom_jitter(shape=16, position=position_jitter(0.05))
p.jitter

plot_bath <-ggplot(train.doi,aes(x=interest_level, y=bathrooms,color = interest_level))+labs(title = "Interest level vs Bedrooms",x="Interest Level", y = "# of Bedrooms")
plot_bath

pb.violin <- plot_bath+geom_violin(trim = FALSE)
pb.violin

pb.jitter <- plot_bath+geom_jitter(shape=16, position=position_jitter(0.05))
pb.jitter

#Looking at O/p variable
unique(train.data$interest_level)

summary(train.doi)
>>>>>>> 3bca7c470f8a032c311d145de8b7548faf42e0ae
