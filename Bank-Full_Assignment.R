#Installing the required packages
install.packages("fastDummies")
library("fastDummies")

#Taking the File as input
bank <- read.csv(file.choose())
data <- bank

#Exploring the dataset
View(bank)
head(bank)
colnames(bank)
str(bank)
unique(bank$job)
unique(bank$marital)
unique(bank$education)
unique(bank$contact)
unique(bank$month)
unique(bank$poutcome)
unique(bank$default)
unique(bank$housing)

#Initialization of Dummy Variable
bank <- fastDummies::dummy_cols(bank,select_columns = c("job","marital","education",
                                                        "contact","month","poutcome")
                                ,remove_selected_columns=TRUE)
bank$default <- ifelse(bank$default=='yes',1,0)
bank$housing <- ifelse(bank$housing=='yes',1,0)
bank$loan <- ifelse(bank$loan=='yes',1,0)
bank$Target <- ifelse(bank$Target=='no',0,1)

#Adding Dummy Variables to the new data table :


#Model Creation
model <- glm(Target~.,data = bank,family = "binomial")
summary(model)

#Finding Probability
prob <- predict(model,type=c("response"),bank)
prob

#Creating the confusion Matrix
confusion <- table(prob>=0.5,bank$Target)
confusion
acc <- sum(diag(confusion)/sum(confusion))
acc #Accuracy= 0.90548

#Plotting ROC Curve
install.packages("ROCR",dependencies = T)
library("ROCR")
propred <- prediction(prob,bank$Target)
properf <- performance(propred,'tpr','fpr')
plot(properf,colorize=T,text.adj=c(0.5,0.5))


#Making up the Final Dataset
Final <- bank
Final[,"Probability"] <- prob
Final <- cbind(Final,Predicted_value)
View(Final)

write.csv(Final,file = "Assignment_Final.csv")
