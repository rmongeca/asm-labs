# Data Exploration
library(corrplot)
library(effects)
library(car)

# read data
data <- read.table(file="IMDB.csv", header = T, sep = ";")
# what data do we have
dim(data)
summary(data)

# no NAs in summary, but print check as well
sum(is.na(data))

# keep data in working df and let initial values untouched
df <- data.frame(data)

# values in gross and budget are too big. Divide by 1 million
df$gross <-df$gross/1000000
df$budget <-df$budget/1000000

# transform titleyear to factor yearcat
df$yearcat <- cut(df$titleyear, 
                    breaks=c(1999, 2005, 2010, 2016))
# all factors of the data are:
df$yearcat <- factor(df$yearcat, labels = c("1", "2", "3"))
df$genre <- factor(df$genre)

# drop titleyear, and I guess we also don't need the name
df <- df[,-c(1,5)]

summary(df)

# for now let's leave gross as is, and compare it against the categorical variables
par(mfrow=c(1,1))
plot(gross~yearcat, df) # same distribution for the three categories
plot(gross~genre, df) # here comedy, drama and terror have similar dists. Only Action differentiate

par(mfrow=c(1,2))
summary(df$genre)
barplot(table(df$genre), main = paste("Genre Barplot"))

summary(df$yearcat)
barplot(table(df$yearcat), main = paste("Year Category Barplot"))

# check the univariate relationship of gross with the rest
pairs(~.,df[,-c(10,11)])

# in the plot we can see that there is a kind of linear relatioship with budget and duration. Also, actor1fl 
# with actor2fl and castfl seem to have a linear correlation which is worth investigating for
# multicoliniarity


## Boxplots for gross compare with a log transformation
par(mfrow=c(1,2))
boxplot(df$gross, main = paste("Boxplot for gross"))
boxplot(log(df$gross + 1), main = paste("Boxplot for log gross"))
# ??????????? #

# variables correlations
data.cor <- cor(df[,-c(10,11)])
corrplot(data.cor)
# the suspicions of correlation between actor1fl and castfl are confirmed here

# Question 2 -> fit model 

# total model with interaction between numerical-categorical and categorical-categorical
# is this correct?

summary(mt<-lm(gross~(.-genre-yearcat)*(genre+yearcat)+genre:yearcat,df))

par(mfrow = c(1,1))
plot(mt)

# Question 3
# I'm lost regarding the bitwise function
summary(m1<-step(mt,direction="both",k=log(nrow(df)))) # taken from S2_crimerateUSA
residualPlots(m1)

# Question 4
# as commented before, we suspect a multicolliniarity between acto1fl and castfl
# did we see any test to test if the correlation is significant?

# Question 5

# we need to check for the normality of the residuals
# the plot of the residuals doesn't look very normal
# there is clearly a shape and the residuals are not scattered around
qqnorm(resid(mt))
plot(mt)
scatter.smooth(sqrt(abs(res))~predict(mt),lpars=list(col=2))
# plot residuals
res = resid(mt)
plot(res~predict(mt))
abline(h=0)

scatterplot(predict(mt),res,smooth=FALSE)

summary(mt<-update(mt,.~.*castfl))
residualPlots(mt)

# and the check the independence of the residuals
plot(res ~ df$gross, ylab = "residuals", xlab = "gross")  # dear lord


# Question 6
# Assuming that we have a final model








