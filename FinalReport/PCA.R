dta = na.omit(read.csv('https://raw.githubusercontent.com/asafgibor/404finalproject/master/FinalReport/hof_all.csv'))
pca = princomp(dta[,c(5,9,11,13:34,38)]) #Select numeric columns
summary(pca)
#The first PC explains an astonishing 98 % of the variance

#Coefficients for each variable in first PC
loadings(pca)[,1]
#In the first PC, AB's are weighted much more heavily than any others, almost 8 times as much as the next highest

#Coefficients for each variable in second PC
loadings(pca)[,2]
#In the second PC, SO is by far the highest, more than 3 times as high as the next main group of AB, HR, and BB 

#First two PC's
plot(loadings(pca))
#Most coefficients are very close to 0, interestingly, there is no extreme outlier in the top left, meaning that extreme outliers for one PC 
# are not extreme outliers for the other.

#Side by side plot of player's PC scores
par(mfrow = c(1,2))
plot(pca$scores[,1], main = "PC 1")
plot(pca$scores[,2], main = 'PC 2')
#The first PC's scores are skewed to the negative end, while the second PC's scores are evenly spread around 0
# There is a lot more information from the first PC, and it probably would be more useful in categorizing HOF type players
