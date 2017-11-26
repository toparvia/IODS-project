# Tuure Parviainen
# Wed Nov 22 14:08:55 2017
# title: Chapter 4. Clustering and classification
# empty workspace
rm(list=ls())
# add libary if needed
# .libPaths( c( .libPaths(), "C:/Program Files/R/R-3.4.1/library"))

#Set working directory
#sourceDir <- getSrcDirectory(function(dummy) {dummy})
#setwd(sourceDir)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

require("easypackages") # to download and load packages fast
# install.packages("easypackages")
# install.packages("stingi", dependencies = TRUE) #these are needed for corrplot
# install.packages("broom", dependencies = TRUE) #these are needed for corrplot

packages_to_load <- c("broom", "dplyr", "MASS", "tidyverse", "corrplot", "ggplot2", "GGally", "caret")
packages(packages_to_load, prompt = TRUE) # lock'n'load install/load
libraries(packages_to_load) # load

# load the data
data("Boston")

# explore the dataset
str(      Boston)
summary(  Boston)
dim(      Boston)

# All are numeric variables except chas and rad. 

# plot matrix of the variables
pairs(Boston)

p <-ggpairs(Boston, mapping = aes(alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))


# data has demographical data of boston including tax and other information possibly linked to crime rates within the city.

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
g = ggpairs(Boston,columns = c(1:14), lower = list(continuous = my_fn))
g

# calculate the correlation matrix and round it
cor_matrix<-cor(Boston) %>% round(digits = 2)

# print the correlation matrix
cor_matrix

# visualize the correlation matrix
corrplot(cor_matrix, method="circle", type = "upper", cl.pos = "b", tl.pos = "d", tl.cex = 0.6)

# It seems that indus, nox, rad, tax and lstat have positive correlation with crime, and dis, black and medv have negative correlation with crime.


# Scaling the variables with mean and standard deviation with scale()

Boston_scaled <- as.data.frame(scale(Boston))

# crim (per capita crime rate by town)

# summary of the scaled crime rate
summary(Boston_scaled$crim)

# create a quantile vector of crime and print it
bins <- quantile(Boston_scaled$crim)
bins

# create a categorical variable 'crim'
crime <- cut(Boston_scaled$crim, breaks = bins, include.lowest= TRUE, label = c("low","med_low","med_high", "high"))

# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
Boston_scaled <- dplyr::select(Boston_scaled, -crim)

# add the new categorical value to scaled data
Boston_scaled <- data.frame(Boston_scaled, crime)

# number of rows in the Boston dataset
n <-nrow(Boston)

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- Boston_scaled[ind,]

# create test set
test <- Boston_scaled[-ind,]

# save the correct classes from test data
correct_classes <-test$crime

# remove the crime variable from test data
test <- dplyr::select(test, -crime)

# linear discriminant analysis
lda.fit <- lda(crime ~., data = train)

# print the lda.fit object
lda.fit

# Here we see how the first linear discriminant explain 94.72 % of the between-group variance in the Boston dataset.
# Also the rad = index of accessibility to radial highways, which had before negative correlation with crime, has relatively big parameter in the first discriminant.



# the function for lda biplot arrows, I couldn't get this to work
# lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
#   heads <- coef(x)
#   arrows(x0 = 0, y0 = 0, 
#          x1 = myscale * heads[,choices[1]], 
#          y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
#   text(myscale * heads[,choices], labels = row.names(heads), 
#        cex = tex, col=color, pos=3)
# }
# 
# # target classes as numeric
# classes <- as.numeric(train$crime)
# 
# # plot the lda results
# plot(classes, dimen = 2, col = classes, pch = classes)
# lda.arrows(lda.fit, myscale = 2)

# I used another package I found in Github

install.packages("devtools")
library(devtools)
install_github("fawda123/ggord")
library(ggord)
# Example
# ord <- lda(Species ~ ., iris, prior = rep(1, 3)/3)
# ggord(ord, iris$Species)

# own data plotted
library("ggthemes")
library("scales")

g <- ggord(lda.fit, train$crime, ellipse_pro = 0.95, vec_ext = 2, alpha_el = 0.3, size = 2)
g  + theme_tufte()  + geom_rangeframe()
# We get the points only seperated in by the 1st LD and rad parameter in the high group.

# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

# in High crime quartile, we could predict all cases, but for other classes we didn't do so well.

# euclidean distance matrix
dist_eu <- dist(Boston)

# look at the summary of the distances
summary(dist_eu)

# manhattan distance matrix
dist_man <- dist(Boston, method="manhattan")

# look at the summary of the distances
summary(dist_man)

# k-means clustering
km <-kmeans(Boston, centers = 4)

# plot the Boston dataset with clusters
pairs(Boston[6:10], col = km$cluster)

# Rad and tax, provide good seperation

set.seed(123)

# determine the number of clusters
k_max <- 4

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(Boston, k)$tot.withinss})

# visualize the results
qplot(x = 1:k_max, y = twcss, geom = 'line')

# k-means clustering
km <-kmeans(Boston, centers = 3)

# plot the Boston dataset with clusters
pairs(Boston[6:10], col = km$cluster)


# For comparison using Caret - package including also centering

fit <- sbf(
  form = Boston$crim ~ .,
  data = Boston[c(1:14)], method = "glmnet", # Dalc and Walc are dropped as they are the parameters high_use is based on, D1:D3, dropped as well, since grades are known only after students are done with the studies (especially final exam G3)
  tuneGrid=expand.grid(.alpha = .01, .lambda = .1),
  preProc = c("center", "scale"),
  trControl = trainControl(method = "none"),
  sbfControl = sbfControl(functions = caretSBF, method = 'cv', number = 10)
)
fit

# During resampling, the top 5 selected variables (out of a possible 12):
#   age (100%), black (100%), dis (100%), indus (100%), lstat (100%)



m <- lm(crim ~ . , data=Boston)
m2 <- lm(crim ~ age + black + dis +indus + lstat, data=Boston)
m3 <- lm(crim ~ dis +tax, data=Boston)
m4 <- lm(crim ~ zn + nox + dis + rad + ptratio + black + 
           lstat + medv, data = Boston)
AIC(m,m2,m3, m4)
par(mfrow=c(2,2))
plot(m4)
plot(m)
### Boston Data set information

# This data frame contains the following columns:
# 
#   crim
# per capita crime rate by town.
# 
# zn
# proportion of residential land zoned for lots over 25,000 sq.ft.
# 
# indus
# proportion of non-retail business acres per town.
# 
# chas
# Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# 
# nox
# nitrogen oxides concentration (parts per 10 million).
# 
# rm
# average number of rooms per dwelling.
# 
# age
# proportion of owner-occupied units built prior to 1940.
# 
# dis
# weighted mean of distances to five Boston employment centres.
# 
# rad
# index of accessibility to radial highways.
# 
# tax
# full-value property-tax rate per \$10,000.
# 
# ptratio
# pupil-teacher ratio by town.
# 
# black
# 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# 
# lstat
# lower status of the population (percent).
# 
# medv
# median value of owner-occupied homes in \$1000s.
