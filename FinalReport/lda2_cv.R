

dta <- read.csv("hof_all_cp.csv")
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

#dta_st <- data.frame(dta$HOF, DTA_num)

lda_out <- lda(HOF ~., data=dta_st, CV = TRUE)
sens_lda <- spec_lda <- bacc_lda <- ppv_lda <- npv_lda <- sens_lda2 <-NULL
for(i in 1:length(thresh_seq)) {
  class_lda <- lda_out$posterior[,2] > thresh_seq[i]
  sens_lda[i] <- mean(class_lda[dta_st$HOF == 'Y'] == TRUE)
  spec_lda[i] <- mean(class_lda[dta_st$HOF == 'N'] == FALSE)
  ppv_lda[i] <- mean(dta_st$HOF[class_lda == TRUE] == 'Y')
  npv_lda[i] <- mean(dta_st$HOF[class_lda == FALSE] == 'N')
  bacc_lda[i] <- (sens_lda[i] + 3*spec_lda[i]) / 4
  df1 <- as.data.frame(cbind(sens_lda, spec_lda, ppv_lda, npv_lda, bacc_lda))
}