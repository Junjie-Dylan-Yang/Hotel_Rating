# set working directory
getwd()
setwd("/Users/Dylan/desktop")
setwd("/home/steve/Dropbox/projects/tolerance-corruption/analysis")
HotelReviews = read.csv('data.csv')

# choose and rename the predictors
HotelR = cbind(HotelReviews[,c(2,3,4,5,8,9,11,12,13,15,16,17)])
names(HotelR) = c("addscore", "date", "score", "hotel", "negative", "review", 
                  "positive", "reviewers", "avescore", "day", "lat", "lng")

# deal with the last predictor to take it numeric without "days"
HotelR$day=as.numeric(HotelR$day)
head(HotelR)

# missing data
missing = is.na(HotelR)
sum(missing)

dim(HotelR)
str(HotelR)

# Average the observations based on the hotel name
Hotel = aggregate(cbind(addscore, score, negative, review, 
                        positive, reviewers, avescore, day, lat, lng) ~ hotel, HotelR, mean)
head(Hotel)

#histogram of reponse
summary(Hotel$score)
hist(Hotel$score)

#k fold cv on whole dataset, k as 5
set.seed(1)
k=5
fold=sample(1:k,nrow(Hotel),replace=TRUE)

kfold.rmse=1:k # we will have 5 RMSEs to fill this with later
for(i in 1:k){
  test=Hotel[fold==i,] # test set is the ith group for the ith iteration
  train=Hotel[fold!=i,]# training set is all the other groups
  
  lmod = lm(score ~ addscore + negative + review + positive + reviewers + day, Hotel)
  pred.time=predict(lmod,test)
  rmse=sqrt(mean((test$score-pred.time)^2))
  
  kfold.rmse[i]=rmse # store current iteration RMSE into ith position of kfold.rmse
}

kfold.rmse # show our RMSEs for each iteration
mean(kfold.rmse) #avg rmse

summary(lmod)

#fwd selection
library(leaps)
model_fwd=regsubsets(score ~ addscore + negative + review + positive + reviewers + day, data=Hotel,nvmax=NULL, method='forward')
summary(model_fwd)
plot(model_fwd, scale='adjr2',main='forward selection')
model_fwd_summary = summary(model_fwd)
which.max(model_fwd_summary$adjr2) #select the best model, which is still the full model)
summary(model_fwd)$which[6,]
best_model_fwd = lm(score~addscore + negative + review + positive + reviewers + day, data=Hotel)
summary(best_model_fwd)


