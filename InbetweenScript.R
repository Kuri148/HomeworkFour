
install.packages("MASS")
library(MASS)
Sys.setenv(LANG = "en")

ordinalData2 <- df.final
ordinalData2
#ordinalData$Course <-  df.final$Course / 100

# In this data set, the rating variable has already been labeled into numbers
#Poor = 1, Fair = 2, Good = 3, Excellent = 4
#if your raw data only has characters, you can use this code to generate response variable with ordered levels

#my.data$rating = factor(my.data$rating, levels = 1:4, labels = c("Poor", "Fair", "Good", "Excellent"))



#The 'rating' variable must be a factor with ordered levels, in order to run the model.

ordinalData2$Course = as.factor(ordinalData2$Course)


# Fit the ordinal logistic regression model

my.model <- polr(Course ~ TC1, data = ordinalData2)

my.model.1 <- polr(Course ~ TC1 + TC2, data = ordinalData2)

my.model.2 <- polr(Course ~ TC1 + TC2 + TC3, data = ordinalData2)

#compare models

anova(my.model, my.model.1, my.model.2)

# Model summary
summary(my.model.2)

# Model coefficients
coef(my.model.2)

# Odds ratios and confidence intervals
exp(coef(my.model.2))
exp(confint(my.model.2))

#we can also form a table of the coefficients.
output.my = cbind(OR=exp(coef(my.model.2)), exp(confint(my.model.2)))

output.my
