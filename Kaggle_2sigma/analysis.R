
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

## Maps


g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("gray85"),
        subunitwidth = 1,
        countrywidth = 1,
        subunitcolor = toRGB("white"),
        countrycolor = toRGB("white")
)


p <- plot_geo(train.doi, locationmode = 'USA-states', sizes = c(20, 250)) %>%
        add_markers(
                x = ~longitude, y = ~latitude, size = ~price, color = ~interest_level, hoverinfo = "text",
                text = ~paste(train.doi$display_address,",", train.doi$street_address, "<br />", train.doi$bedrooms," bed","<br />",train.doi$bathrooms, " bath")
        ) %>%
        layout(title = 'Distribution of homes wrt interest level<br>(Click legend to toggle)', geo = g)

p 

p4 <- leaflet(train.doi) %>%  addTiles() %>% addMarkers(
        clusterOptions = markerOptions(freezeAtZoom = 5)
        )%>% 
        addMarkers(~longitude, ~latitude, popup = ~paste(train.doi$display_address,",", train.doi$street_address, "<br />", train.doi$bedrooms," bed","<br />",train.doi$bathrooms, " bath") )

p4

train.doi$interest_level
train.doih<- train.doi[train.doi$interest_level =='high',]
train.doil<- train.doi[train.doi$interest_level =='low',]
train.doim<- train.doi[train.doi$interest_level =='medium',]

#Word Cloud with all data
train.features <- unlist(train.doi$features,recursive = TRUE,use.names = FALSE)
train.ft <- as.data.frame(table(train.features))
names(train.ft) <- c("words","freq")
train.ft <- train.ft[with(train.ft, order(-freq)), ]

set.seed(1234)

wrd_cld<- wordcloud(words = train.ft$words, freq = train.ft$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(10,"Dark2"))
wrd_cld

plot_freqwrd<- barplot(train.ft[1:10,]$freq, las = 2, names.arg = train.ft[1:10,]$words,
        col ="lightblue", main ="Most frequent features",
        ylab = "Word frequencies")

plot_freqwrd

#Word cloud with interest level as "High"





summary(train.doi)

summary(train.doi)

