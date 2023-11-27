# Let's use this dataset in the "lavaan" package.
# The classic Holzinger and Swineford (1939) dataset consists of mental ability test scores
# of seventh- and eighth-grade children from two different schools (Pasteur and Grant-White).
# In the original dataset (available in the MBESS package), there are scores for 26 tests.
# However, a smaller subset with 9 variables is more widely used in the literature
# (for example in Joreskog's 1969 paper, which also uses the 145 subjects from the
# Grant-White school only).


######
#EFA #
######

#install packages for EFA

install.packages('psych')
install.packages('GPArotation')
install.packages('lavaan')

library("lavaan")
library('psych')
library('GPArotation')
library('tidyverse')

# Load the data and check the variables in the dataset.

df0 <- read.csv(file = "D:\\R\\HomeworkFour\\HW4_WinkeZhang_data.csv", header = T)
str(df0)

# Factors are currently integers so turn them into numers (meaning they can have a decimal)
#has issues do it in a later step when they are all alone.



# Remove variables not included in the factor analysis

removeThese <- names(df0) %in% c("StudentID", "Gender", "Course", "OPICA", "Level")
df1 <- df0[!removeThese]
str(df1)
df2 <- lapply(df1,as.numeric)
df2 <- as.data.frame(df2)
str(df2)

# Then, it is best practice to scale all variables by changing all variales into z scores.

df.1 = as.data.frame(scale(df2))
df <- as.data.frame(scale(df2))

# Check correlation matrix to see if too many variables are too strongly correlated
# or not correlated at all. Either situation will be bad news for factor analysis

cor.matrix <- cor(df, method = "pearson", use = "complete.obs")

# KMO Kaiser-Meyer-Olkin (KMO) index--sampling accuracy, should be > .6

KMO(cor.matrix)

# Bartlett's Test of Sphericity, we need to use the correlation matrix from the previous step.
# Then indicate the sample size through n= , and hope to find significant difference.

cortest.bartlett(cor.matrix, n=377)

# Find out how many factors are appropriate (scree plot and eigenvalue)

EFA_1 <- fa.parallel(df, fm = 'ml', fa = 'fa')
print(EFA_1)

#rerun the factor analysis with a specified number of factors, get factor loadings

EFA_2 <- fa(df, nfactors = 3, rotate = "varimax", fm="ml")
EFA_2 <- fa(df, nfactors = 3, rotate = "oblimin", fm="ml") #you can also choose orthoganal rotation by changing rotate =. e.g., rotate = varimax

print(EFA_2$loadings)
print(EFA_2$loadings, digits = 2, cutoff = .3, sort = TRUE)

#You can also get the communality for each variable
EFA_2$communalities


#get a visual representation of the factor structure

fa.diagram(EFA_2)

#extracting factor scores for each observation

jerry <- factor.scores(df,EFA_2)
mary <- as.data.frame(jerry$scores)

#adding factor scores back into the dataset  #bind in properly, check bindee before binding.
dfpca <- cbind(df0, mary)
