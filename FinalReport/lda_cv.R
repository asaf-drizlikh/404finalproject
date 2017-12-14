dta <- read.csv("hof_all.csv")
library(MASS)

## Extract a few offensive statistics (numerical variables).
dta_st <- DTA[, c("HOF", "OBP", "SF","SLG","SB","SH")]

## Variable declarations
thresh_seq = seq(from = .05, to = .95, by = .05) # list of threshold values
n <- nrow(DTA) # stores number of rows in the data set

lda_out <- lda(HOF ~., data=dta_st, CV = TRUE)
sens_lda <- spec_lda <- bacc_lda <-NULL
for(i in 1:length(thresh_seq)) {
  class_lda <- lda_out$posterior[,2] > thresh_seq[i]
  # filters results by players who are actually in the HOF (Y), then checks if TRUE was predicted. The average of the correct YES predictions defines sensitivity
  sens_lda[i] <- mean(class_lda[dta_st$HOF == 'Y'] == TRUE) 
  # filters results by players who are actually NOT in the HOF (N), then checks if FALSE was predicted. The average of the correct YES predictions defines sensitivity
  spec_lda[i] <- mean(class_lda[dta_st$HOF == 'N'] == FALSE)
  bacc_lda[i] <- (sens_lda[i] + 3*spec_lda[i]) / 4 # balance accuracy calculation is weighted
  df1 <- as.data.frame(cbind(sens_lda, spec_lda, bacc_lda))
}

spec2 <- 1-spec_lda
roc_dat1<-data.frame(x=spec2,y=sens_lda)

roc1 <- ggplot(roc_dat1) + geom_point(aes(roc_dat1$x,roc_dat1$y),size=2) + labs(title= "LDA ROC Curve", 
                                                        x = "False Positive Rate (1-Specificity)", 
                                                        y = "True Positive Rate (Sensitivity)")
