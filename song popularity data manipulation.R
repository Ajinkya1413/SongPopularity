library(tidyverse)
library(olsrr)
library(dplyr)
library(caret)
library(e1071)
library(FNN)
library(ggplot2)


#Importing Dataset
tracks.df <- read.csv("genre_songs.csv", header=T, na.strings=c("","NA"))
View(tracks.df)
view(tracks.df[tracks.df$track.id =="5MvCurNMpM3WCCSs40cd37",])
head(tracks.df)

tracks_final.df <- select(tracks.df, -c(track.album.id, track.album.name, playlist_name, playlist_id ))
view(tracks_final.df)

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
tracks_final.df$Bin_pop <- ifelse(tracks_final.df$popularity<=60,0,1)

tracks_final.df$cat_pop <- round(tracks_final.df$popularity/10,digits = 0)
tracks_final.df$duration_min <- tracks_final.df$duration_ms/60000

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

ggplot(data = tracks_final.df, mapping = aes(loudness, energy, col = as.factor(Bin_pop))) + geom_point(size = 1, alpha = 0.7)
pairs(tracks_final.df[,c(8,9,11,13)], col = as.factor(tracks_final.df$Bin_pop))
boxplot(tracks_final.df$cat_pop)
hist(tracks_final.df$popularity)
hist(tracks_final.df$Bin_pop)
#8503/29656

#Create a training and validation partition
numberOfRows <- nrow(tracks_final.df)
set.seed(22)
train.index <- sample(numberOfRows, numberOfRows*0.6)
train.index
print(train.index)
train.df <- tracks_final.df[train.index, ]
valid.df <- tracks_final.df[-train.index, ]

#count(train.df[train.df$popularity >= 75,])
View(train.df)
View(valid.df)

#Model

tracks.lm <- lm(formula = cat_pop ~ (id+genre+sub.genre+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_min
), data = train.df)

summary(tracks.lm)

step_tracks.lm <- step(tracks.lm, direction = "both", trace = 1)
step_tracksF.lm <- step(tracks.lm, direction = "forward", trace = 1)
ols_step_all_possible(tracks.lm)



#####################################################################################

train.df$Index <- c(1:17793)

#knn.pred <- knn(train.df[, c(train.df$danceability,train.df$energy,train.df$key,train.df$loudness,train.df$mode,train.df$speechiness,train.df$acousticness,train.df$instrumentalness,train.df$liveness,train.df$valence,train.df$tempo ,train.df$duration_min)], 
#                test = valid.df[, c(valid.df$danceability,valid.df$energy,valid.df$key,valid.df$loudness,valid.df$mode,valid.df$speechiness,valid.df$acousticness,valid.df$instrumentalness,valid.df$liveness,valid.df$valence,valid.df$tempo,valid.df$duration_min
#)], 
#                cl = train.df$Bin_pop, k = 3)

for(i in 1:50) {
knn.pred <- knn(train.df[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                test = valid.df[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                cl = train.df$Bin_pop, k = i)
conf_mtrx <- confusionMatrix(knn.pred, as.factor(valid.df$Bin_pop))
acc <- conf_mtrx[[3]][1]
print(i)
print(acc)
}

str(knn.pred)
knn.pred[9]

row.names(train.df)[attr(knn.pred, "knn.index")]

str(knn.pred)

#get the predicted value for the observation
nn[1]

#get the IDs of the closest neighbors
row.names(train_df)[attr(nn, "nn.index")]

# k = 18
# Accuracy 
# 0.7074939 

# k= 47
# Accuracy
# 0.7142375


knn.pred18 <- knn(train.df[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                test = valid.df[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                cl = train.df$Bin_pop, k = 18, algorithm = c("brute"))
conf_mtrx18 <- confusionMatrix(knn.pred, as.factor(valid.df$Bin_pop))
conf_mtrx18

knn.pred47 <- knn(train.df[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                  test = valid.df[, c(8,9,10,11,12,13,14,15,16,17,18,21)], 
                  cl = train.df$Bin_pop, k = 18, algorithm = c("brute"))
conf_mtrx47 <- confusionMatrix(knn.pred, as.factor(valid.df$Bin_pop))
conf_mtrx47


