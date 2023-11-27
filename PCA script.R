#running PCA using the principal() function

pc1 <- principal(df, rotate="none")

#The eigenvalues are stored in a variable called pc1$values, so we can create the scree plot

plot(pc1$values, type="b") # type="b" will show both the line and the points

#From the scree plot, it seems 3 is a good number of factors.
#So we redo the principal(), but add nfactors = 3.

pc2 <- principal(df, nfactors=3, rotate="none")

#We can set rotate="varimax" in the principal() function. But there are too many things to see.
#print.psych() command prints the factor loading matrix associated with the model pc3,
#but displaying only loadings above .3 (cut = 0.3)
#and sorting items by the size of their loadings (sort = TRUE).

pc3 <- principal(df, nfactors=3, rotate="varimax")
pc3 <- principal(df, nfactors=3, rotate="oblimin")

print.psych(pc3, cut = 0.3, sort = TRUE)

#You can also calculate component scores using scores = TRUE.

pc4 <- principal(df, nfactors = 3, rotate = "oblimin", scores = TRUE)
# head(pc4$scores)    # access scores by pc5$scores

df.final <- cbind(df0, pc4$scores)
# bind the factor scores to raqData dataframe for other

df.finalcleanup <- na.omit(df.final)
colMeans(df.finalcleanup)
sd(df.finalcleanup$TC1)
mean(df.finalcleanup$TC1)
sd(df.finalcleanup$TC2)
mean(df.finalcleanup$TC2)
sd(df.finalcleanup$TC3)
mean(df.finalcleanup$TC3)

table(df.final["Course"])
