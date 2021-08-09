#INSTALL PACKAGES:
  library(C50)
  library(tree)
  library(gmodels)
  library(party)
  library(knitr)
  library(png)
  
#IMPORT  DATASETS: 
FraudCheck <- read.csv("C:/Users/Rafiya/Downloads/FraudCheck.csv")
View(FraudCheck)
hist(FraudCheck$Taxable.Income)
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)  
View(FC)
#TEST AND TRAIN MODEL:
FC_train <- FC[1:300,]

# View(CD_train)
FC_test <- FC[301:600,]

# View(CD_test)

###Using Party Function 

png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
plot(opall_tree)
# using the training Data 

png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)
plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good)
CrossTable(FC_test$Risky_Good,pred_test_df)
confusionMatrix(FC_test$Risky_Good,pred_test_df)

