dta <- read.csv("hof_all.csv")
library(MASS)

## Extract a few offensive statistics (numerical variables).
#num_vars <- c("AB", "OBP", "SF_Nornm", "SLG", "SB_Norm", "SH_Norm")
#X <- as.matrix(DTA[, num_vars])
dta_st <- DTA[, c("HOF", "OBP", "AB", "SF","SLG","SB","SH")]
#DTA_num2 <- DTA[, c("HOF","H","TP","SB","HBP")] #other option

## Variable declarations
sens = NULL # sensitivity 
spec = NULL # specificity
acc = NULL # accuracy
test = NULL # test data
train = NULL # training data
thresh_seq = seq(from = .05, to = .95, by = .05) # list of threshold values
n <- nrow(DTA) # stores number of rows in the data set
pred = matrix(0, nrow = n, ncol = 19) # matrix for storing predictions
check = matrix(0, nrow = n, ncol = 19) # matrix for storing the results of LOOCV

fit <- glm(HOF ~., data = train, family = binomial())
fit_pred <- predict(fit, newdata = test, type = 'response')	

sens_glm <- spec_glm <- bacc_glm <- ppv_glm <- npv_glm <- sens_glm2 <-NULL
for(i in 1:length(thresh_seq)) {
  class_glm <- fit_pred$posterior[,2] > thresh_seq[i]
  sens_glm[i] <- mean(class_glm[dta_st$HOF == 'Y'] == TRUE)
  spec_glm[i] <- mean(class_glm[dta_st$HOF == 'N'] == FALSE)
  ppv_glm[i] <- mean(dta_st$HOF[class_glm == TRUE] == 'Y')
  npv_glm[i] <- mean(dta_st$HOF[class_glm == FALSE] == 'N')
  bacc_glm[i] <- (sens_glm[i] + 3*spec_glm[i]) / 4
  df1 <- as.data.frame(cbind(sens_glm, spec_glm, ppv_glm, npv_glm, bacc_glm))
}