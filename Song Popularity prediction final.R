library(tidyverse)
library(olsrr)
library(dplyr)
library(gains)
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)
library(e1071)
library(FNN)
library(ggplot2)
library(corrplot)
library(DT)
library(highcharter)
library(FNN)


#Importing Dataset
tracks.df <- read.csv("genre_songs.csv", header=T, na.strings=c("","NA"))
#View(tracks.df)
#view(tracks.df[tracks.df$track.id =="5MvCurNMpM3WCCSs40cd37",])



-------------------------------------------------------------------------------------
  
  #DataCleaning
  
  
  
  #head(tracks.df)
  tracks_final.df <- select(tracks.df, -c(track.album.id, track.album.name, playlist_name, playlist_id ))
#view(tracks_final.df)





tracks_final.df = tracks_final.df %>% 
  rename(
    id = track.id,
    popularity = track.popularity,
    genre = playlist_genre,
    sub.genre = playlist_subgenre,
    release.date = track.album.release_date,
    artist = track.artist
  )

view(tracks_final.df)
# track id = 
#5MvCurNMpM3WCCSs40cd37
#577AGkqnLPYPy6AcOnxhfx
#577AGkqnLPYPy6AcOnxhfx
#577AGkqnLPYPy6AcOnxhfx

#loudness more that zero = 0


missing_values = colSums(is.na(tracks_final.df))
missing_values

tracks_final.df <- tracks_final.df[!duplicated(tracks_final.df$id),]
tracks_final.df <- na.omit(tracks_final.df)
tracks_final.df$loudness <- ifelse(tracks_final.df$loudness>0,0,tracks_final.df$loudness)
#tracks_final.df$Bin_pop <- ifelse(tracks_final.df$popularity<=60,0,1)

tracks_final.df$cat_pop <- round(tracks_final.df$popularity/10,digits = 0)
tracks_final.df$duration_min <- tracks_final.df$duration_ms/60000
tracks_final.df$Bin_pop <- ifelse(tracks_final.df$popularity<=60,0,1)

# if(nchar(tracks_final.df$release.date) > 6 & nchar(tracks_final.df$release.date) < 8) {
#   tracks_final.df$release.date_sub <- substr(tracks_final.df$release.date, 1,4)
# }
# 
# date_char8 <- nchar(tracks_final.df$release.date) == 8
# tracks_final.df$release.date[date_char8]
# date_char7 <- nchar(tracks_final.df$release.date) == 7
# tracks_final.df$release.date[date_char7]
# date_char4 <- nchar(tracks_final.df$release.date) == 4
# tracks_final.df$release.date[date_char4]
# tracks_final.df$release.date_sub <- substr(tracks_final.df$release.date[date_char8], 7,8)

#tracks_final.df$release.date_format <- as.Date(tracks_final.df$release.date) #cleaning $release_date
tracks_final.df$artist = as.factor(tracks_final.df$artist)
tracks_final.df$genre = as.factor(tracks_final.df$genre)

#tracks_final1.df <- tracks_final.df[,!c("track.name","popularity", )]

view(tracks_final.df)
summary(tracks_final.df)
str(tracks_final.df)

#count(tracks_final.df[tracks_final.df$popularity <=30,])
count(tracks_final.df[tracks_final.df$Bin_pop == 1,])

#Create a training and validation partition uisng upsampling
numberOfRows <- nrow(tracks_final.df)
tracks_final.df.upsample <- data.frame(upSample(tracks_final.df[,-ncol(tracks_final.df)],as.factor(tracks_final.df$Bin_pop)))
numberOfRows <- nrow(tracks_final.df.upsample)
set.seed(22)
train.index <- sample(numberOfRows, numberOfRows*0.6)
train.index
print(train.index)
train.df_sample <- tracks_final.df.upsample[train.index, ]
valid.df_sample <- tracks_final.df.upsample[-train.index, ]
view(valid.df_sample)
view(valid.df_sample)

ggplot(data = tracks_final.df, mapping = aes(loudness, energy, col = as.factor(Bin_pop))) + geom_point(size = 1, alpha = 0.7)
pairs(tracks_final.df[,c(8,9,11,13)], col = as.factor(tracks_final.df$Bin_pop))
boxplot(tracks_final.df$cat_pop)
hist(tracks_final.df$popularity)
hist(tracks_final.df$Bin_pop)
boxplot(tracks_scaled$tempo)

boxplot(tracks_final.df[,c(10,11,21)])
boxplot(tracks_final.df[,c(4,18)])

boxplot(tracks_final.df[,c(8,9,13,14,15,16)])

temp.df <- data.frame(tracks_final.df$popularity, tracks_final.df.upsample$popularity)

ggplot(mapping=aes(x=tracks_final.df))+
  geom_bar(data=tracks_final.df, aes(x=popularity), fill="red", binwidth=10)+
  geom_bar(data=tracks_final.df.upsample, fill="blue", binwidth=10)

summary(tracks_final.df)

tracks_scaled <- as.data.frame(scale(tracks_final.df[,c(8,9,10,11,12,13,14,15,16,17,18,21)], center = TRUE, scale = TRUE))
view(tracks_scaled)
#Create a training and validation partition without upsampling
numberOfRows <- nrow(tracks_final.df)
tracks_final.df$duration_min <- tracks_final.df$duration_ms/60000
set.seed(22)
train.index <- sample(numberOfRows, numberOfRows*0.6)
train.index
print(train.index)
train.df <- tracks_final.df[train.index, ]
valid.df <- tracks_final.df[-train.index, ]
head(valid.df,10)
view(train.df)


#Model


-----------------------------------------------------------------------
  
  #visualization
  
  
  
  #NUMBER OF SONGS PER YEAR
  year_count_df <- tracks_final.df %>%
  filter(!is.na(release.date)) %>%
  group_by("Year" = substr(tracks_final.df$release.date,1,4)) %>%
  summarise(Freq = n())





class(year_count_df)





highchart() %>%
  hc_add_series(year_count_df,
                hcaes(x = as.numeric(Year),
                      y = Freq,
                      color = Freq),
                type = "line") %>%
  hc_title(text = "Number of songs per year") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Number of Songs")) %>%
  hc_tooltip(useHTML = T,
             borderWidth = 1.5,
             headerFormat = "",
             pointFormat = paste("{point.Year} {point.Freq} songs")) %>%
  hc_legend(enabled = F)





#loudness vs energy
ggplot(data = tracks_final.df, mapping = aes(loudness, energy, col = as.factor(cat_pop))) + geom_point(size = 1, alpha = 0.7)





#correlation matrix
tracks_final_df <- tracks_final.df[,c(8:18,20,21)]
tracks_cor = cor(tracks_final_df)
corrplot(tracks_cor, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)




--------------------------------------------------------------------------
  
  
  #Linear Regression
  
  
  
  tracks.lm <- lm(formula = popularity ~ (genre + duration_min + instrumentalness + energy + loudness +
                                            liveness + acousticness + tempo + speechiness +
                                            danceability), data = train.df)
summary(tracks.lm)

step_tracks.lm <- step(tracks.lm, direction = "both", trace = 1)
step_tracksF.lm <- step(tracks.lm, direction = "forward", trace = 1)
#ols_step_all_possible(tracks.lm)





-----------------------------------------------------------------------
  
  #Logistic Regression
  
  
  
  logitI.reg <- glm(train.df$Bin_pop ~(danceability + speechiness + valence
                                          + duration_min), data = train.df, family = "binomial")
options(scipen=999)
summary(logitI.reg)
confusionMatrix(table(predict(logitI.reg, newdata = valid.df_sample,
                              type="response") >= 0.29, valid.df_sample$Class == 1))





----------------------------------------------------------------------------------------------
  
  
  #DecisionTrees
  
  .ct <- rpart(Bin_pop ~ artist + genre + sub.genre + danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo
               + duration_ms, data = train.df, method = "class", cp = 0, maxdepth = 8, minsplit = 1)



# print tree summary and plot tree. try different values for extra
printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)



ct.pred <- predict(.ct, valid.df_sample, type = "class")
#View(ct.pred)
# generate confusion matrix for training data
confusionMatrix(ct.pred, as.factor(valid.df_sample$Class))





------------------------------------------------------------------
  
  #RandomForest
  
  
  
  
  library(randomForest)
rf <- randomForest(as.factor(train.df$Bin_pop) ~ (genre + sub.genre + danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo
                                                     + duration_min), data = train.df,
                   ntree = 1000, mtry = 6, nodesize = 1, importance = TRUE, sampsize = 1250)
varImpPlot(rf, type = 1)



#create a confusion matrix
valid.df_sample$Bin_pop <- factor(valid.df_sample$Class)
rf.pred <- predict(rf, valid.df_sample)
confusionMatrix(rf.pred, valid.df_sample$Bin_pop)
#####################################################################################
view(train.df)
view(valid.df_sample[valid.df_sample$id =="32OlwWuMpZ6b0aN2RZOeMS",])
train.df$Index <- c(1:17793)

#knn.pred <- knn(train.df[, c(train.df$danceability,train.df$energy,train.df$key,train.df$loudness,train.df$mode,train.df$speechiness,train.df$acousticness,train.df$instrumentalness,train.df$liveness,train.df$valence,train.df$tempo ,train.df$duration_min)], 
#                test = valid.df[, c(valid.df$danceability,valid.df$energy,valid.df$key,valid.df$loudness,valid.df$mode,valid.df$speechiness,valid.df$acousticness,valid.df$instrumentalness,valid.df$liveness,valid.df$valence,valid.df$tempo,valid.df$duration_min
#)], 
#                cl = train.df$Bin_pop, k = 3)

for(i in 1:5) {
  knn.pred <- knn(train.df[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                  test = valid.df_sample[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                  cl = train.df$Bin_pop, k = i)
  conf_mtrx <- confusionMatrix(knn.pred, as.factor(valid.df_sample$Bin_pop))
  acc <- conf_mtrx[[3]][1]
  print(i)
  print(acc)
}

str(knn.pred)
knn.pred[9]

row.names(train.df)[attr(knn.pred, "knn.index")]
#get the IDs of the closest neighbors
row.names(train.df)[attr(knn.pred, "knn.pred.Index")]

# k = 18
# Accuracy 
# 0.7074939 

# k= 47
# Accuracy
# 0.7142375


knn.pred1 <- knn(train.df[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                test = valid.df_sample[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                cl = train.df$Bin_pop, k = 1)
conf_mtrx1 <- confusionMatrix(knn.pred1, as.factor(valid.df_sample$Bin_pop))
conf_mtrx1
knn.pred1[12700]
View(valid.df_sample[12700,])
knn.pred1[12706]
View(valid.df_sample[12706,])
knn.pred1[127]
View(valid.df_sample[127,])
knn.pred1[8972]
View(valid.df_sample[8972,])

row.names(train.df)[attr(knn.pred1, "Index")]

for(i in 1:16921){
  if(valid.df_sample$id[i] == "32OlwWuMpZ6b0aN2RZOeMS"){
    print(i)
  }
}

knn.pred3 <- knn(train.df[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                 test = valid.df_sample[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                 cl = train.df$Bin_pop, k = 3)
conf_mtrx3 <- confusionMatrix(knn.pred3, as.factor(valid.df_sample$Bin_pop))
conf_mtrx3
knn.pred3[12700]


