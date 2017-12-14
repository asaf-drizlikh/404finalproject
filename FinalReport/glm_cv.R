dta <- read.csv("hof_all.csv")

## Extract a few offensive statistics (numerical variables).
dta_st <- DTA[, c("HOF", "OBP", "AB","SLG","SB","SH")]

## Variable declarations
thresh_seq = seq(from = .05, to = .95, by = .05) # list of threshold values
n <- nrow(DTA) # stores number of rows in the data set
fit_pred <- matrix(rep(0,n), nrow=n, ncol=1)

for(i in 1:n) {
  fit <- glm(HOF ~., data = dta_st[-i,], family = binomial()) #uses all values but one as training data
  fit_pred[i] <- predict(fit, newdata = dta_st[i,], type = 'response') # uses remaining value to predict
}

sens_glm <- spec_glm <- bacc_glm <-NULL
for(i in 1:length(thresh_seq)) {
  class_glm <- fit_pred > thresh_seq[i]
  # filters results by players who are actually in the HOF (Y), then checks if TRUE was predicted. The average of the correct YES predictions defines sensitivity
  sens_glm[i] <- mean(class_glm[dta_st$HOF == 'Y'] == TRUE)
  # filters results by players who are actually NOT in the HOF (N), then checks if FALSE was predicted. The average of the correct YES predictions defines sensitivity
  spec_glm[i] <- mean(class_glm[dta_st$HOF == 'N'] == FALSE)
  bacc_glm[i] <- (sens_glm[i] + 3*spec_glm[i]) / 4 
  df1 <- as.data.frame(cbind(sens_glm, spec_glm, bacc_glm))
}

spec2 <- 1-spec_glm
roc_dat2<-data.frame(x=spec2,y=sens_glm) # data for roc curve

roc2 <- ggplot(roc_dat2) + geom_point(aes(roc_dat2$x,roc_dat2$y),size=2) + labs(title= "GLM ROC Curve", 
                                                         x = "False Positive Rate (1-Specificity)", 
                                                         y = "True Positive Rate (Sensitivity)")
