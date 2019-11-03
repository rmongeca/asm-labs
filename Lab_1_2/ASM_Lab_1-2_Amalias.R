### Lab 1-2 ###

# libraries
library(ggplot2)
library(glmnet)
library(MASS)

# set a seed for reproducibility
set.seed(1234)

# set working directory
setwd("~/Documents/courses/ASM/asm-labs/Lab_1_2")

# read data
jyb <- read.csv("JYB.csv", sep=";")
dim(jyb)
names(jyb)
summary(jyb)


# check for missing stuff
wherenan <- colnames(jyb)[apply(jyb, 2, function(n) any(is.na(n)))]
whereempty <- colnames(jyb)[apply(jyb, 2, function(n) any(n==''))]
# nada

# check the levels
levels(jyb$job)
levels(jyb$marital)
levels(jyb$education)
levels(jyb$default)
levels(jyb$housing)
levels(jyb$loan)
levels(jyb$contact)

# histogram, boxplot for age, campaign, pdays, previous
hist(jyb$age)
hist(jyb$campaign)
hist(jyb$pdays) # ???
hist(jyb$previous)

# barplot for job, marital, education, default, loan, contact, month, day, putcome
barplot(table(jyb$job))
barplot(table(jyb$marital))
barplot(table(jyb$education))
barplot(table(jyb$default))
barplot(table(jyb$loan))


# check correlations for the numeric
numeric_cols <- names(which(sapply(jyb, is.numeric)))
jyb %>%
    select(numeric_cols) %>%
    cor() %>%
    corrplot::corrplot(method = "number", type = "upper", tl.cex = 0.8, tl.offset = 0.5, tl.srt = 45)


### complete model
modelfull <- glm(y ~ ., data = jyb, family = binomial)
summary(modelfull)                  
plot(modelfull)
alias(modelfull)   # loan unknown is correlated with housinguknown?
stepAIC(modelfull)
# and now BIC
# stepAIC(logit.2,k=log()))


# try fancy boxTidwell?



