election_data <- read.csv("A1/ElectionDataAlone.csv")
summary(election_data)

impute_data <- function(vec, mn) {
  ifelse(is.na(vec),mn,vec)
}

data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)
(data_mean)

for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
summary(election_data)


election_data$Region <- as.factor(election_data$Region)
election_data$ElectionDate <- as.Date(election_data$ElectionDate)




election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]
names(election_data_train)

election_data_train$Other_Candidates <- election_data_train$TotalVote - election_data_train$Clinton - election_data_train$Obama
election_data_train$Other_Candidates_Percent <- election_data_train$Other_Candidates/election_data_train$TotalVote

election_data_train_obama_clinton <- election_data_train[which(election_data_train$Other_Candidates_Percent < 0.2),]
election_data_train <- election_data_train_obama_clinton


election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)


nrow_train <- nrow(election_data_train)
nrow_small_train <- round(nrow(election_data_train)*0.75)
nrow_validation <- nrow_train - nrow_small_train

row_indices_smaller_train <- sample(1:nrow_train, size=nrow_small_train, replace=FALSE)
election_data_smaller_train <- election_data_train[row_indices_smaller_train,]
election_data_validation <- election_data_train[-row_indices_smaller_train,]

install.packages("corrplot")
library("corrplot")

data <- election_data_train[10:46]
corrplot(cor(data,method="spearman"))


election_linear_model <- lm(Obama_margin_percent ~ .-ï..County - State, data = election_data_smaller_train)
summary(election_linear_model)

election_linear_model_stepped <-  step(election_linear_model, direction="backward")
summary(election_linear_model_stepped)

election_linear_model_2 <- lm(Obama_margin_percent ~ .-ï..County - State - FIPS - ElectionDate - ElectionType - TotalVote - Clinton - Obama - Other_Candidates_Percent - Obama_wins - Obama_margin - Other_Candidates, data = election_data_smaller_train)
summary(election_linear_model)

election_linear_model_stepped_2 <- step(election_linear_model_2, direction="backward")
summary(election_linear_model_stepped_2)

install.packages("car")
library(car)

vif(election_linear_model_stepped_2)

election_linear_model_3 <- lm(Obama_margin_percent ~ Region + White + Black + AmericanIndian + Hawaiian + Bachelors + HighSchool + Poverty + MedianIncome + AverageIncome +  UnemployRate + MedicareRate + SocialSecurity + SocialSecurityRate + Disabilities + DisabilitiesRate + Homeowner + PopDensity + LandArea + FarmArea, data = election_data_smaller_train)
summary(election_linear_model_3)

vif(election_linear_model_3)

election_linear_model_4 <- lm(Obama_margin_percent ~ Region + Black + Bachelors + HighSchool + Poverty + MedianIncome + UnemployRate + SocialSecurity + SocialSecurityRate + Disabilities + DisabilitiesRate + Homeowner + PopDensity + LandArea + FarmArea, data = election_data_smaller_train)
summary(election_linear_model_4)

vif(election_linear_model_4)

cut <- 0
LogitTrainPrediction_4 <- election_linear_model_4$fitted.values>cut
LogitTrainPrediction_4[1:5]
train_compare_4 <- election_data_smaller_train$Obama_margin_percent>cut
train_compare_4[1:5]
summary(LogitTrainPrediction_4)
LogitTrainError_4 <- mean(LogitTrainPrediction_4!=train_compare_4)


LogitTestPrediction_4 <- predict(election_linear_model_4, election_data_validation, type = "response")>cut
LogitTestPrediction_4[1:5]
summary(LogitTestPrediction_4)
test_compare_4 <- election_data_validation$Obama_margin_percent>cut
LogitTestError_4 <- mean(LogitTestPrediction_4!=test_compare_4)


X <- c(seq(-1,1,0.01))
ErrorVector <- c(0:200)
Y=0
for (cut in X)
{
  LogitTrainPrediction_4 <- election_linear_model_4$fitted.values>cut
  LogitTrainError_4 <- mean(LogitTrainPrediction_4!=train_compare_4)
  Y=Y+1
  ErrorVector[Y] <- LogitTrainError_4
}

plot(X,ErrorVector,main="Error versus cut election_linear_model_4")

election_linear_model_final <- lm(Obama_margin_percent ~ Region + Black + HighSchool + Poverty, data = election_data_smaller_train)
summary(election_linear_model_final)

vif(election_linear_model_final)


election_linear_model_final$fitted.values[1:5]

cut <- 0
LogitTrainPrediction <- election_linear_model_final$fitted.values>cut
LogitTrainPrediction[1:5]
test <- election_data_smaller_train$Obama_margin_percent>0
test[1:5]
summary(LogitTrainPrediction)
LogitTrainError <- mean(LogitTrainPrediction!=test)


LogitTestPrediction <- predict(election_linear_model_final, election_data_validation, type = "response")>cut
LogitTestPrediction[1:5]
summary(LogitTestPrediction)
test2 <- election_data_validation$Obama_margin_percent>0
LogitTestError <- mean(LogitTestPrediction!=test2)

X <- c(seq(-1,1,0.01))
ErrorVector <- c(0:200)
Y=0
for (cut in X)
{
  LogitTrainPrediction <- election_linear_model_final$fitted.values>cut
  LogitTrainError <- mean(LogitTrainPrediction!=test)
  Y=Y+1
  ErrorVector[Y] <- LogitTrainError
}

plot(X,ErrorVector,main="Error versus cut election_linear_model_final")

plot(election_linear_model_final)


election_glm <- glm(Obama_wins ~ Region + Black + HighSchool + Poverty, data = election_data_smaller_train, family="binomial")
summary(election_glm)


LogitTrainPrediction <- election_glm$fitted>0.5
test <- election_data_smaller_train$Obama_wins>0.5
test[1:5]
summary(LogitTrainPrediction)
LogitTrainError <- mean(LogitTrainPrediction!=test)


LogitTestPrediction <- predict(election_glm, election_data_validation, type = "response")>0.5
summary(LogitTestPrediction)
test2 <- election_data_validation$Obama_wins>0.5
LogitTestError <- mean(LogitTestPrediction!=test2)


X <- c(seq(0,1,0.01))
ErrorVector <- c(0:100)
Y=0
for (cut in X)
{
  LogitTrainPrediction <- election_glm$fitted.values>cut
  LogitTrainError <- mean(LogitTrainPrediction!=test)
  Y=Y+1
  ErrorVector[Y] <- LogitTrainError
}

plot(election_glm)
plot(election_linear_model_final)

plot(X,ErrorVector,main="Error versus cut")




install.packages("rpart")
library(rpart)
election_reg_tree <- rpart(Obama_margin_percent ~ Region + Black + HighSchool + Poverty + PopDensity, data=election_data_smaller_train)
printcp(election_reg_tree)
plotcp(election_reg_tree)

summary(election_reg_tree)
print(election_reg_tree)
plot(election_reg_tree)
text(election_reg_tree, use.n=TRUE, all=TRUE, cex=0.5)

install.packages("partykit")
install.packages("party")
library(partykit)
library(party)
plot(as.party(election_reg_tree), type="extended")


TreeTrainPrediction <- predict(election_reg_tree,election_data_smaller_train)
TreeTrainPrediction <- TreeTrainPrediction>0
test <- election_data_smaller_train$Obama_margin_percent>0
test[1:5]
summary(TreeTrainPrediction)
TreeTrainError <- mean(TreeTrainPrediction!=test)

TreeTestPrediction <- predict(election_reg_tree,election_data_validation)
TreeTestPrediction <- TreeTestPrediction>0
test1 <- election_data_validation$Obama_margin_percent>0
test1[1:5]
summary(TreeTestPrediction)
TreeTestError <- mean(TreeTestPrediction!=test1)


install.packages("forecast")
library(forecast)

final_prediction_train <- predict(election_linear_model_final, election_data_smaller_train)
summary(final_prediction_train)
accuracy(final_prediction_train,election_data_smaller_train$Obama_margin_percent)
summary(final_prediction_train>0)

final_prediction_test <- predict(election_linear_model_final, election_data_validation)
summary(final_prediction_test)
accuracy(final_prediction_test,election_data_validation$Obama_margin_percent)
summary(final_prediction_test>0)




final_prediction_forecast <- predict(election_linear_model_final, election_data_test)
summary(final_prediction_forecast)
summary(final_prediction_forecast>0)




1-mean(election_data_train$Obama_wins)

mean(election_data_test$Black)
mean(election_data_train$Black)
mean(election_data_test$Poverty)
mean(election_data_train$Poverty)
mean(election_data_test$HighSchool)
mean(election_data_train$HighSchool)


install.packages("dplyr")
library("dplyr")
dplyr::count(election_data_test, Region)
dplyr::count(election_data_train, Region)


election_speech_test <- election_data_smaller_train


election_speech_test$FarmbyLand <- (election_speech_test$FarmArea / election_speech_test$LandArea)
election_speech_test_sub <- subset(election_speech_test, election_speech_test$FarmArea < 60) 

farmer_model <- lm(Obama_margin_percent ~ FarmbyLand, data = election_speech_test_sub)
summary(farmer_model)
plot(farmer_model)
pairs(~FarmbyLand + Obama_margin_percent, data=election_speech_test_sub)

blue_collar_model <- lm(Obama_margin_percent ~ Bachelors + ManfEmploy, data = election_data_smaller_train)
summary(blue_collar_model)
plot(blue_collar_model)
vif(blue_collar_model)


mean(election_data_test$ManfEmploy)
mean(election_data_train$ManfEmploy)
mean(election_data_test$Bachelors)
mean(election_data_train$Bachelors)

mean(election_data_test$PopDensity)
mean(election_data_train$PopDensity)
mean(election_data_test$FarmArea)
mean(election_data_train$FarmArea)
mean(election_data_test$LandArea)
mean(election_data_train$LandArea)

pairs(~Obama_margin_percent + FarmArea, data=election_data_smaller_train)

