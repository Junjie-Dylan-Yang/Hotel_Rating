# set working directory
setwd("/Users/huiwang/Downloads")
HotelReviews = read.csv('Hotel_Reviews.csv')

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

# try regression~
lmod = lm(score ~ addscore + negative + review + positive + reviewers + day + lat + lng, Hotel)
summary(lmod)
