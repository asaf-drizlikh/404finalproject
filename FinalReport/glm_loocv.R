#imports library for LDA/QDA
library(ggplot2)

## Load data
DTA <- read.csv("hof_all.csv")

## Extract a few offensive statistics (numerical variables).
DTA_num <- DTA[, c("HOF", "OBP", "AB", "SF","SLG","SB","SH")]



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


for (j in 1:length(thresh_seq)) { # makes a prediction for each threshold value
  # cycles through all data, using excluding one player each time to be used later for the accuracy check
  for (i in 1:n) {
    
    test <- DTA_num[i,] # uses one element as a test
    train <- DTA_num[-i,] # uses remainder of data elements as training
    
    # glm fit
    fit <- glm(HOF ~., data = train, family = binomial())
    fit_pred <- predict(fit, newdata = test, type = 'response')	
    
    # checks whether posterior probability from glm is higher
    # than the current threshold value, then makes prediction
    if (fit_pred > thresh_seq[j]) { 
      pred[i,j] = 1 # predicts as yes for HOF
    }
    else {
      pred[i,j] = 0 # predicts as no for HOF
    }
    
  }
}

## Performs LOOCV
## Checks the pred matrix to see which values
## were correctly predicted	for each threshold value
for (j in 1:length(thresh_seq)) {
  predYesRight = 0 # reset number of HOF yes predicted correctly
  predYesWrong = 0 # reset number of HOF yes predicted incorrectly
  predNoRight = 0 # reset number of HOF no predicted correctly
  predNoWrong = 0 # reset number of HOF no predicted incorrectly
  
  for (i in 1:n) {
    if (pred[i,j] == 1) { # if yes was predicted
      if (DTA$HOF[i] == 'Y') { # compare to test case
        check[i,j] = 0 # marks prediction as correct
        predYesRight <- predYesRight + 1  # increase count
      }
      else {
        check[i,j] = 1 # marks prediction as incorrect
        predYesWrong <- predYesWrong + 1  
      }
    }
    else if(pred[i,j] == 0) { # if no was predicted
      if (DTA$HOF[i] == 'N') { # compare to test case
        check[i,j] = 0 # marks prediction as correct
        predNoRight <- predNoRight + 1 # increase count
      }
      else {
        check[i,j] = 1 # marks prediction as incorrect		
        predNoWrong <- predNoWrong + 1  				
      }
    }
  }
  
  sens[j] = predYesRight / (predYesRight + predNoWrong) # calculation for sensitivity 
  spec[j] = predNoRight / (predYesWrong + predNoRight) # calculation for specificity 
  acc[j] = (sens[j] + spec[j]) / 2 # calculation for balanced accuracy
}

spec2 <- 1-spec
dta<-data.frame(x=spec2,y=sens)

roc <- ggplot(dta) + geom_line(aes(dta$x,dta$y)) + labs(title= "ROC curve", 
                                                        x = "False Positive Rate (1-Specificity)", 
                                                        y = "True Positive Rate (Sensitivity)")
