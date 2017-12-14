dta = na.omit(read.csv('https://raw.githubusercontent.com/asafgibor/404finalproject/master/FinalReport/hof_all.csv'))
pca = princomp(dta[,c(5,9,11,13:34,38)]) #Select numeric columns
summary(pca)

#Coefficients for each variable in first PC
loadings(pca)[,1]

#Coefficients for each variable in second PC
loadings(pca)[,2]

#First two PC's
plot(loadings(pca))

#Side by side plot of player's PC scores
par(mfrow = c(1,2))
plot(pca$scores[,1], main = "PC 1")
plot(pca$scores[,2], main = 'PC 2')
