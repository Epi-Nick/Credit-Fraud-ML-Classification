########################################
#Setting working directory instructions#
########################################
#Before running the following code it is important to first set the working directory to the folder where the original
#data file is stored. This can be done in the following ways:
#Navigate to: Session > Set Working Directory > Choose Directory
#Alternatively you can press: Ctrl + Shift + H
#Or you can type the code: setwd("") and type the folder directory into the quotation marks
setwd("C:\\Users\\EpiNick\\Desktop\\Data Analytics\\MAST5957")

##############################################
#Installing and calling all relevant packages#
############################################## 
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse")}
library(tidyverse)
if (!("dplyr" %in% installed.packages())) {
  install.packages("dplyr")}
library(dplyr)
if (!("grid" %in% installed.packages())) {
  install.packages("grid")}
library(grid)
if (!("gridExtra" %in% installed.packages())) {
  install.packages("gridExtra")}
library(gridExtra)
if (!("corrplot" %in% installed.packages())) {
  install.packages("corrplot")}
library(corrplot)
if (!("tidyr" %in% installed.packages())) {
  install.packages("tidyr")}
library(tidyr)
if (!("ggplot2" %in% installed.packages())) {
  install.packages("ggplot2")}
library(ggplot2)
if (!("RColorBrewer" %in% installed.packages())) {
  install.packages("RColorBrewer")}
library(RColorBrewer)
if (!("factoextra" %in% installed.packages())) {
  install.packages("factoextra")}
library(factoextra)
if (!("cluster" %in% installed.packages())) {
  install.packages("cluster")}
library(cluster)
if (!("dendextend" %in% installed.packages())) {
  install.packages("dendextend")}
library(dendextend)
if (!("ranger" %in% installed.packages())) {
  install.packages("ranger")}
library(ranger)
if (!("caret" %in% installed.packages())) {
  install.packages("caret")}
library(caret)
if (!("data.table" %in% installed.packages())) {
  install.packages("data.table")}
library(data.table)
if (!("scales" %in% installed.packages())) {
  install.packages("scales")}
library(scales)
if (!("rpart" %in% installed.packages())) {
  install.packages("rpart")}
library(rpart)
if (!("rpart.plot" %in% installed.packages())) {
  install.packages("rpart.plot")}
library(rpart.plot)
if (!("caTools" %in% installed.packages())) {
  install.packages("caTools")}
library(caTools)
if (!("Metrics" %in% installed.packages())) {
  install.packages("Metrics")}
library(Metrics)
if (!("gbm" %in% installed.packages())) {
  install.packages("gbm")}
library(gbm)
if (!("pROC" %in% installed.packages())) {
  install.packages("pROC")}
library(pROC)
if (!("xgboost" %in% installed.packages())) {
  install.packages("xgboost")}
library(xgboost)
if (!("cvms" %in% installed.packages())) {
  install.packages("cvms")}
library(cvms)
if (!("randomForest" %in% installed.packages())) {
  install.packages("randomForest")}
library(randomForest)
if (!("ggpubr" %in% installed.packages())) {
  install.packages("ggpubr")}
library(ggpubr)
#Dependencies for DMwR package:
if (!("xts" %in% installed.packages())) {
  install.packages("xts")}
library(xts)
if (!("quantmod" %in% installed.packages())) {
  install.packages("quantmod")}
library(quantmod)
if (!("zoo" %in% installed.packages())) {
  install.packages("zoo")}
library(zoo)
if (!("ROCR" %in% installed.packages())) {
  install.packages("ROCR")}
library(ROCR)


#Package DMwR which contains the SMOTE function has been removed from the CRAN library.
#However, it can be installed from the CRAN archives from the following link:
#https://cran.r-project.org/src/contrib/Archive/DMwR/
#After downloading the latest version you will need to provide the directory path to it
#in the following code:
if (!("DMwR" %in% installed.packages())) {
  install.packages( "C:\\Users\\EpiNick\\Desktop\\Data Analytics\\MAST5957\\DMwR_0.4.1.tar.gz", repos=NULL, type="source" )} #Change this line to make it function
library(DMwR)

###############
#Data Cleaning#
###############
data <- read.csv("creditcard.csv")           #Import Data Set

#Exploratory Analysis:
head(data)                #Show's variable names as well as the first 6 data entries for said variables.
sum(is.na(data))          #Check for missing values
table(data$Class)         #Compares how many variables fall into is fraud and is not fraud categories.
#Interestingly, only around 0.13% of transactions are fraudulent.

#######################
#Bar Chart for isFraud#
#######################
#Create Data frame contaning count stats for instances of Fraud and Non-Fraud:
df_Class <- data.frame(TransactionType  = c("Fraud", "Not_Fraud"), Count = c(492, 284315))

#Formats the axis for all upcoming line graphs. Makes all axis text grey so that axis and plot titles can stand out:
theme_set(theme(
  axis.line = element_line(colour = "grey87"),
  axis.ticks = element_line(colour = "grey87"),
  axis.text = element_text(colour = "grey40", size = 12),
))  
#Bar chart for fraudelent vs non-fraudelent transactions:
ggplot(data=df_Class, aes(x = TransactionType, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +                     #Formats the bars.
  scale_y_continuous("Count", labels = scales::comma) +                 #Scales the Y-axis.
  geom_text(aes(label = Count), vjust = -0.2) +                         #Adds data labels just above each bar.
  labs(
    title = "Number of fraudulent vs non-fraudulent transactions",      #Sets a plot title.
    Count = "Count",                                                    #Changes y-axis title.
    TransactionType = "Type of Transaction"                             #Changes x-axis title.
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),                       #Formats plot title.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  ) 

#Box Plot for Amount by Class
Box_plot1 <- ggplot(data, aes(y = Amount, x = factor(Class))) + geom_boxplot() +
  labs(
    title = "Distribution of Transaction Amount by Class",              #Sets a plot title.
    y = "Amount",                                                        #Changes y-axis title.
    x = "Class"                                           #Changes x-axis title.
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),                       #Formats plot title.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  ) 

#Histogram for Amount
Histogram1 <- ggplot(data, aes(x = Amount)) + 
  geom_histogram(fill = "steelblue", binwidth = 1000) +                 #Formats the bars.
  scale_y_continuous("Count", labels = scales::comma) +                 #Scales the Y-axis.
  labs(
    title = "Number of Transactions by Amount",                         #Sets a plot title.
    y = "Count",                                                        #Changes y-axis title.
    x = "Type of Transaction"                                           #Changes x-axis title.
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),                       #Formats plot title.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  ) 

grid.arrange(Box_plot1, Histogram1) #Arrange Histogram and Box plot for Amount on the Same plot

attach(data) #Attach data to create the following data frame

#Create Data from for the Ranges and mean values of all PCA variables:
df_PCA <- data.frame (Names = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", 
                                "V9", "V10", "V11", "V12", "V13", "V14", "V15",
                                "V16", "V17", "V18", "V19", "V20", "V21", "V22",
                                "V23", "V24", "V25", "V26", "V27", "V28"),
                      Start = c(min(V1), min(V2), min(V3), min(V4), min(V5), 
                                min(V6), min(V7), min(V8), min(V9), min(V10),
                                min(V11), min(V12), min(V13), min(V14), min(V15), 
                                min(V16), min(V17), min(V18), min(V19), min(V20),
                                min(V21), min(V22), min(V23), min(V24), min(V25), 
                                min(V26), min(V27), min(V28)),
                      End = c(max(V1), max(V2), max(V3), max(V4), max(V5), 
                              max(V6), max(V7), max(V8), max(V9), max(V10),
                              max(V11), max(V12), max(V13), max(V14), max(V15), 
                              max(V16), max(V17), max(V18), max(V19), max(V20),
                              max(V21), max(V22), max(V23), max(V24), max(V25), 
                              max(V26), max(V27), max(V28)),
                      Mean = c(mean(V1), mean(V2), mean(V3), mean(V4), mean(V5), 
                               mean(V6), mean(V7), mean(V8), mean(V9), mean(V10),
                               mean(V11), mean(V12), mean(V13), mean(V14), mean(V15), 
                               mean(V16), mean(V17), mean(V18), mean(V19), mean(V20),
                               mean(V21), mean(V22), mean(V23), mean(V24), mean(V25), 
                               mean(V26), mean(V27), mean(V28))
)

positions <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8",  #Creates data frame which will be used to order variables on the
               "V9", "V10", "V11", "V12", "V13", "V14", "V15",  #cross-bar plot
               "V16", "V17", "V18", "V19", "V20", "V21", "V22",
               "V23", "V24", "V25", "V26", "V27", "V28")

ggplot(data = df_PCA, aes(x = Names, y = Mean)) +                                 #GGplot setup
  geom_crossbar(aes(ymin = Start, ymax = End), width = 0.5, fill = "skyblue") +   #Create Cross-bar plot for PCA variable Ranges
  scale_x_discrete(limits = positions) +                                          #Ensures Bars are ordered by their names
  coord_flip() +
  labs(
    title = "Range and Mean of PCA Variables",            #Sets a plot title.
    y = "Value",                                          #Changes y-axis title.
    x = "Variable Name"                                   #Changes x-axis title.
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),                       #Formats plot title.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  ) 

#Creating denisty plots for select PCA variables to test for normality:
V1_Density <- ggdensity(data$V1, 
                        main = "Density plot of V1",
                        xlab = "V1 Value")

V7_Density <- ggdensity(data$V7, 
                        main = "Density plot of V7",
                        xlab = "V7 Value")

V21_Density <- ggdensity(data$V21, 
                         main = "Density plot of V21",
                         xlab = "V21 Value")

V28_Density <- ggdensity(data$V28, 
                         main = "Density plot of V28",
                         xlab = "V28 Value")

grid.arrange(V1_Density, V7_Density, V21_Density, V28_Density)

df <- data[,-1]                      #Create data frame to preserve original data set. Time variable was removed as it is only used to order the data and messes with some of modelling techniques due to it's large values
df[,-30] <- scale(df[,-30])          #All PCA numeric variables were scaled to ensure their magnitudes do not skew the detection algorithms importance of them

#Correlations
correlation <- cor(df , method = "pearson")                                                                    #Runs correlations for the data set variables
corrplot(correlation, number.cex = 1.5, method = "square", type = "full", tl.cex = 0.8, tl.col = "black",      #Plots correlation matrix
         col = colorRampPalette(c("azure3", "dodgerblue", "dodgerblue4","navyblue"))(100), cl.lim = c(0, 1))   #Sets custom colour scheme for the correlation matrix

##############################
#Creating Test and Train data#
##############################
df <- data[,-1]                                #Create data frame to preserve original data set. Time variable was removed as it is only used to order the data and messes with some of modelling techniques due to it's large values
df[,-30] <- scale(df[,-30])                    #All PCA numeric variables were scaled to ensure their magnitudes do not skew the detection algorithms importance of them
df$Class <- as.factor(df$Class)                #Convert Class variable into factor to allow for up and down sampling

set.seed(120)                                  #Ensures all following random procedures can be replicated when code is re-run 
sample<-sample.split(df, SplitRatio = 0.8)     #This first code is used to split the data into 80% and 20%
train <- subset(df, sample== TRUE)             #80% is put into the train data to test and run
test <- subset(df, sample== FALSE)             #20% is put into the test data to predict later on
dim(train)                                     #Returns the number of elements in the data
dim(test)                                      #Returns the number of elements in the data

#Create under-sampled train model
train_down <- downSample(x = train [, -ncol(train)], y = train$Class)
table(train_down$Class)

#Create Up-sampled train data
train_up <- upSample(x = train[, -ncol(train)], y = train$Class)
table(train_up$Class)

#Create synthetically over sampled minority class train data (using SMOTE):
train_smote <- SMOTE(Class ~ ., data  = train)
table(train_smote$Class)

###############
#Decision Tree#
###############
#Annotation of code for all models was limited to the first instance where the model was run
#with a normal sample as every all other training code was identical to this original.
set.seed(120)
#Decision Tree Normal Sample
decisionTree_model_norm <- rpart(Class ~ . , train, method = 'class')                      #Creates regression decision tree model. Method "class" is suitable for binary classification 
rpart.plot(decisionTree_model_norm)                                                        #Plots decision tree model
tree_norm_pred <- predict(decisionTree_model_norm, newdata = test, type = 'class')         #Predicts values using the decision tree model with test data
table(predicted = tree_norm_pred, actual = test$Class)                                     #Create a Punnet Square for Decision Tree prediction results
tree_norm_prob <- predict(decisionTree_model_norm, newdata = test, type = "prob")          #Predicts the probabilities of cases being Fraud or not Fraud using decision tree model
roc(test$Class, tree_norm_prob[, 2], plot = TRUE, print.auc = TRUE,                        #Creates and stores ROC Curve for our Decision Tree model prediction
    main = "Decision Tree Normal Sample")                  

#Decision Tree Down Sample
decisionTree_model_down <- rpart(Class ~ . , train_down, method = 'class')          
tree_down_pred <- predict(decisionTree_model_down, newdata = test, type = 'class')  
table(predicted = tree_down_pred, actual = test$Class)                              
tree_down_prob <- predict(decisionTree_model_down, newdata = test, type = "prob")
roc(test$Class, tree_down_prob[, 2], plot = TRUE,           
    print.auc = TRUE, main = "Decision Tree Under Sample")   

#Decision Tree Up Sample
decisionTree_model_up <- rpart(Class ~ . , train_up, method = 'class')              
tree_up_pred <- predict(decisionTree_model_up, newdata = test, type = 'class')      
table(predicted = tree_up_pred, actual = test$Class)                                
tree_up_prob <- predict(decisionTree_model_up, newdata = test, type = "prob")
roc_tree_up <- roc(test$Class,tree_up_prob[,2], plot = TRUE,                
                   print.auc = TRUE, main = "Decision Tree Up Sample")                                                        

#Decision Tree SMOTE Sample
decisionTree_model_smote <- rpart(Class ~ . , train_smote, method = 'class')        
tree_smote_pred <- predict(decisionTree_model_smote, newdata = test, type = 'class')
table(predicted = tree_smote_pred, actual = test$Class)                             
tree_smote_prob <- predict(decisionTree_model_smote, newdata = test, type = "prob")
roc_tree_smote <- roc(test$Class, tree_smote_prob[, 2], plot = TRUE,              
                      print.auc = TRUE, main = "Decision Tree SMOTE Sample")   

#Create Data Frame for Decision Tree Sensitivity Bar Chart:
df_Tree <- data.frame (Sample  = c("Normal","Under", "Up", "SMOTE"),
                       Sensitivity = c(76.53, 84.69, 82.65, 81.63)
)

order_tree <- c("Under", "Up", "SMOTE", "Normal")                      #Create variable to order bars

ggplot(data=df_Tree, aes(x = Sample, y = Sensitivity)) +
  geom_bar(stat="identity") +                                          #Plots Bar Chart
  geom_col() +
  geom_text(aes(label = Sensitivity), vjust = 1.5, colour = "white") + #Adds data labels to each bar
  scale_x_discrete(limits = order_tree) +                              #Orders bars by height instead of in alphabetical order of sample type
  labs(                                                               
    title = "Decision Tree Sensitivity as Percentage for each Sample", #Sets a plot title.
    x = "Sample Type",                                                 #Changes x-axis title.
    y = "Sensitivity %"                                                #Changes y-axis title.
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),                       #Formats plot title.
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),                    #Formats plot subtitle.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  )

###############
#Random Forest#
###############
set.seed(120)

#Random Forest Normal Sample
randomForest_norm <- randomForest(Class~.,                        #Creates random forest model
                                  data = train,                   #Selects data set which is used to train the model
                                  minNode = 20,                   #Sets the minimum number of nodes contained within each decision tree
                                  maxLeaf = 13,                   #Sets the maximum number of leaves (outputs) each tree can have
                                  ntree=500)                      #Tells the model how many trees to use to train the model.
forest_norm_pred <- predict(randomForest_norm, test)              #Predicts values using the random forest model with test data
table(forest_norm_pred, test$Class)                               #Creates Punnet Square for Random Forest predictions.
forest_norm_prob <- predict(randomForest_norm, newdata = test, type = "prob") #Calculated prediction probabilities to be used in ROC curve creation
roc_tree_norm <- roc(test$Class, forest_norm_prob[, 2], plot = TRUE,          #Creates and stores ROC curve for the model    
                      print.auc = TRUE, main = "Random Forest Normal Sample")   

#Random Forest Down Sample
randomForest_down <- randomForest(Class~.,                       
                                  data = train_down, 
                                  minNode = 20, 
                                  maxLeaf = 13, 
                                  ntree=500) 
forest_down_pred <- predict(randomForest_down, test)              
table(forest_down_pred, test$Class)
forest_down_prob <- predict(randomForest_down, newdata = test, type = "prob")
roc_tree_down <- roc(test$Class, forest_down_prob[, 2], plot = TRUE,              
                      print.auc = TRUE, main = "Random Forest Under Sample")  

#Random Forest Up Sample
randomForest_up <- randomForest(Class~.,                   
                                data = train_up, 
                                minNode = 20, 
                                maxLeaf = 13, 
                                ntree=500) 
forest_up_pred <- predict(randomForest_up, test)            
table(forest_up_pred, test$Class)
forest_up_prob <- predict(randomForest_up, newdata = test, type = "prob")
roc_tree_up <- roc(test$Class, forest_up_prob[, 2], plot = TRUE,              
                     print.auc = TRUE, main = "Random Forest Up Sample") 

#Random Forest SMOTE Sample
randomForest_smote <- randomForest(Class~.,                   
                                   data = train_smote, 
                                   minNode = 20, 
                                   maxLeaf = 13, 
                                   ntree=500) 
forest_smote_pred <- predict(randomForest_smote, test)            
table(forest_smote_pred, test$Class)
forest_smote_prob <- predict(randomForest_smote, newdata = test, type = "prob")
roc_tree_smote <- roc(test$Class, forest_smote_prob[, 2], plot = TRUE,              
                   print.auc = TRUE, main = "Random Forest SMOTE Sample") 

#Create Data Frame for Random Forest Sensitivity Bar Chart:
df_forest <- data.frame (Sample  = c("Normal","Under", "Up", "SMOTE"),
                         Sensitivity = c(75.51, 90.82, 73.47, 88.78)
)

order_forest <- c("Under", "SMOTE", "Normal", "Up")                    #Create variable to order bars

ggplot(data=df_forest, aes(x = Sample, y = Sensitivity)) +
  geom_bar(stat="identity") +                                          #Plots Bar Chart
  geom_col() +
  geom_text(aes(label = Sensitivity), vjust = 1.5, colour = "white") + #Adds data labels to each bar
  scale_x_discrete(limits = order_forest) +                            #Orders bars by height instead of in alphabetical order of sample type
  labs(                                                               
    title = "Random Forest Sensitivity as Percentage for each Sample", #Sets a plot title.
    x = "Sample Type",                                                 #Changes x-axis title.
    y = "Sensitivity %"                                                #Changes y-axis title.
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),                       #Formats plot title.
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),                    #Formats plot subtitle.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  )

#####
#GBM#
#####
set.seed(120)

#GBM Normal Sample
gbm_norm <- gbm(as.character(Class) ~ .,                                   #Creates GBM model with Class as the dependent variable
                distribution = "bernoulli",                                #Sets the statistical distribution. Bernoulli is used for logistic regression
                data = rbind(train, test),                                 #Lists the data set used to train and test the model
                n.trees = 500,                                             #Sets number of decision trees used to train the model at 500
                interaction.depth = 3,                                     #Sets maximum depth of each decision tree which effects how it interacts with other trees through the additive model
                n.minobsinnode = 100,                                      #Sets the minimum number of observations within each decision tree
                shrinkage = 0.01,                                          #Sets the learning rate for each tree. Smaller learning rates require more trees
                bag.fraction = 0.5,                                        #Fraction of training set observations randomly selected to propose the next tree in the expansion. This is not random in this case as we used set.seed
                train.fraction = nrow(train) / (nrow(train) + nrow(test))  #Signals training set to be used to fit the GBM model and the combination of training and test data
)                                                                          #to be used for computing out-of-sample estimates of the loss function.


gbm_norm_iter = gbm.perf(gbm_norm, method = "test")                                  #Calculates the optimal number of trees needed to train the GBM which will be used to make predcitions
gbm_norm_pred <- predict(object = gbm_norm, newdata = test, n.trees = gbm_norm_iter) #Makes predictions using the GBM model
table(as.factor(gbm_norm_pred > 0.50), test$Class)                                   #Prints out Punnet Square for GBM predictions
gbm_norm_prob <- predict(gbm_norm, newdata = test, type = "response")                #Calculates probabilities for GBM predictions used to plot ROC curves
roc_gbm_norm <- roc(test$Class, gbm_norm_prob, plot = TRUE,                          #Creates and stores ROC curve
                     print.auc = TRUE, main = "GBM Normal Sample") 

#GBM Down Sample
gbm_down <- gbm(as.character(Class) ~ ., 
                distribution = "bernoulli", 
                data = rbind(train_down, test), 
                n.trees = 500, 
                interaction.depth = 3, 
                n.minobsinnode = 100, 
                shrinkage = 0.01, 
                bag.fraction = 0.5, 
                train.fraction = nrow(train_down) / (nrow(train_down) + nrow(test))
)


gbm_down_iter = gbm.perf(gbm_down, method = "test")    
gbm_down_pred <- predict(object = gbm_down, newdata = test, n.trees = gbm_down_iter)
table(as.factor(gbm_down_pred > 0.50), test$Class)
gbm_down_prob <- predict(gbm_down, newdata = test, type = "response")
roc_gbm_down <- roc(test$Class, gbm_down_prob, plot = TRUE,              
                    print.auc = TRUE, main = "GBM Under Sample") 

#GBM Up Sample
gbm_up <- gbm(as.character(Class) ~ ., 
                distribution = "bernoulli", 
                data = rbind(train_up, test), 
                n.trees = 500, 
                interaction.depth = 3, 
                n.minobsinnode = 100, 
                shrinkage = 0.01, 
                bag.fraction = 0.5, 
                train.fraction = nrow(train_up) / (nrow(train_up) + nrow(test))
)


gbm_up_iter = gbm.perf(gbm_up, method = "test")    
gbm_up_pred <- predict(object = gbm_up, newdata = test, n.trees = gbm_up_iter)
table(as.factor(gbm_up_pred > 0.50), test$Class)
gbm_up_prob <- predict(gbm_up, newdata = test, type = "response")
roc_gbm_up <- roc(test$Class, gbm_up_prob, plot = TRUE,              
                    print.auc = TRUE, main = "GBM Up Sample") 

#GBM SMOTE Sample
gbm_smote <- gbm(as.character(Class) ~ ., 
                distribution = "bernoulli", 
                data = rbind(train_smote, test), 
                n.trees = 500, 
                interaction.depth = 3, 
                n.minobsinnode = 100, 
                shrinkage = 0.01, 
                bag.fraction = 0.5, 
                train.fraction = nrow(train_smote) / (nrow(train_smote) + nrow(test))
)


gbm_smote_iter = gbm.perf(gbm_smote, method = "test")    
gbm_smote_pred <- predict(object = gbm_smote, newdata = test, n.trees = gbm_smote_iter)
table(as.factor(gbm_smote_pred > 0.50), test$Class)
gbm_smote_prob <- predict(gbm_smote, newdata = test, type = "response")
roc_smote_up <- roc(test$Class, gbm_smote_prob, plot = TRUE,              
                  print.auc = TRUE, main = "GBM SMOTE Sample") 

#Create Data Frame for GBM Sensitivity Bar Chart:
df_gbm <- data.frame (Sample  = c("Normal","Under", "Up", "SMOTE"),
                      Sensitivity = c(71.43, 87.76, 86.73, 87.76)
)

order_gbm <- c("Under", "SMOTE", "Up", "Normal")                       #Create variable to order bars

ggplot(data=df_gbm, aes(x = Sample, y = Sensitivity)) +
  geom_bar(stat="identity") +                                          #Plots Bar Chart
  geom_col() +
  geom_text(aes(label = Sensitivity), vjust = 1.5, colour = "white") + #Adds data labels to each bar
  scale_x_discrete(limits = order_gbm) +                               #Orders bars by height instead of in alphabetical order of sample type
  labs(                                                               
    title = "GBM Sensitivity as Percentage for each Sample",           #Sets a plot title.
    x = "Sample Type",                                                 #Changes x-axis title.
    y = "Sensitivity %"                                                #Changes y-axis title.
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),                       #Formats plot title.
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),                    #Formats plot subtitle.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  )

#########
#xGboost#
#########
#Setting Labels for xGboost:
train_x <- train
test_x <- test
train_x_down <- train_down
train_x_up <- train_up
train_x_smote <- train_smote
levels(train_x$Class) <- c("Not_Fraud", "Fraud")
levels(test_x$Class) <- c("Not_Fraud", "Fraud")  
levels(train_x_down$Class) <- c("Not_Fraud", "Fraud")  
levels(train_x_up$Class) <- c("Not_Fraud", "Fraud")  
levels(train_x_smote$Class) <- c("Not_Fraud", "Fraud")  
labels_norm <- train_x$Class
labels_down <- train_x_down$Class
labels_up <- train_x_up$Class
labels_smote <- train_x_smote$Class
x_norm <- recode(labels_norm, 'Not_Fraud' = 0, "Fraud" = 1)
x_down <- recode(labels_down, 'Not_Fraud' = 0, "Fraud" = 1)
x_up <- recode(labels_up, 'Not_Fraud' = 0, "Fraud" = 1)
x_smote <- recode(labels_smote, 'Not_Fraud' = 0, "Fraud" = 1)

set.seed(120)
#Note: Number of trees was reduced to 300 to avoid over fitting.
#xGboost Normal Sample
xgboost_norm <- xgboost(data = data.matrix(train_x[, -30]), #Sets data set used to train the model. Outcome has to be removed to test and train xGboost models
                        label = x_norm,                     #Sets the labels for training the model. This information contains the outcomes for each record
                        eta = 0.1,                          #Sets the learning rate for the model.
                        gamma = 0.1,                        #Tells the model the minimum loss reduction required to split decision tree nodes
                        max_depth = 10,                     #Sets maximum depth of each decision tree
                        nrounds = 300,                      #Tells the model to use 300 weak learners for training
                        objective = "binary:logistic",      #Informs the model that it is solving a binary classification problem
                        colsample_bytree = 0.6,             #Sub sample ratio of columns to be used when creating each tree
                        verbose = 0,                        #Tells the model not to print performance stats
                        nthread = 7,                        #Sets number of threads that can be used for model training (processor threads)
)
xgb_norm_pred <- predict(xgboost_norm, newdata = data.matrix(test_x[, -30])) #Predicts results for test data set
table(as.numeric(xgb_norm_pred > 0.50), test_x$Class)                        #Calculates probabilities for model predictions
roc_xgb_norm <- roc(test$Class, xgb_norm_pred, plot = TRUE,                  #Creates and stores ROC curve for model
                    print.auc = TRUE, main = "xGboost Normal Sample") 

#xGboost Down Sample
xgboost_down <- xgboost(data = data.matrix(train_x_down[, -30]), 
                      label = x_down,
                      eta = 0.1,
                      gamma = 0.1,
                      max_depth = 10, 
                      nrounds = 300, 
                      objective = "binary:logistic",
                      colsample_bytree = 0.6,
                      verbose = 0,
                      nthread = 7,
)
xgb_down_pred <- predict(xgboost_down, newdata = data.matrix(test_x[, -30]))
table(as.numeric(xgb_down_pred > 0.50), test_x$Class)
roc_xgb_down <- roc(test$Class, xgb_down_pred, plot = TRUE,              
                   print.auc = TRUE, main = "xGboost Under Sample") 

#xGboost Up Sample
xgboost_up <- xgboost(data = data.matrix(train_x_up[, -30]), 
                    label = x_up,
                    eta = 0.1,
                    gamma = 0.1,
                    max_depth = 10, 
                    nrounds = 300, 
                    objective = "binary:logistic",
                    colsample_bytree = 0.6,
                    verbose = 0,
                    nthread = 7,
)
xgb_up_pred <- predict(xgboost_up, newdata = data.matrix(test_x[, -30]))
table(as.numeric(xgb_up_pred > 0.50), test_x$Class)
roc_xgb_up <- roc(test$Class, xgb_norm_pred, plot = TRUE,              
                   print.auc = TRUE, main = "xGboost Up Sample") 

#xGboost Smote Sample
xgboost_smote <- xgboost(data = data.matrix(train_x_smote[, -30]), 
                        label = x_smote,
                        eta = 0.1,
                        gamma = 0.1,
                        max_depth = 10, 
                        nrounds = 300, 
                        objective = "binary:logistic",
                        colsample_bytree = 0.6,
                        verbose = 0,
                        nthread = 7,
)
xgb_smote_pred <- predict(xgboost_smote, newdata = data.matrix(test_x[, -30]))
table(as.numeric(xgb_smote_pred > 0.50), test_x$Class)
roc_xgb_smote <- roc(test$Class, xgb_smote_pred, plot = TRUE,              
                  print.auc = TRUE, main = "xGboost SMOTE Sample") 

#Create Data Frame for xGboost Sensitivity Bar Chart:
df_xgb <- data.frame (Sample  = c("Normal","Under", "Up", "SMOTE"),
                      Sensitivity = c(76.53, 91.84, 78.57, 86.73)
)

order_xgb <- c("Under", "SMOTE", "Up", "Normal")                       #Create variable to order bars

ggplot(data=df_xgb, aes(x = Sample, y = Sensitivity)) +
  geom_bar(stat="identity") +                                          #Plots Bar Chart
  geom_col() +
  geom_text(aes(label = Sensitivity), vjust = 1.5, colour = "white") + #Adds data labels to each bar
  scale_x_discrete(limits = order_xgb) +                               #Orders bars by height instead of in alphabetical order of sample type
  labs(                                                               
    title = "xGboost Sensitivity as Percentage for each Sample",       #Sets a plot title.
    x = "Sample Type",                                                 #Changes x-axis title.
    y = "Sensitivity %"                                                #Changes y-axis title.
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),                       #Formats plot title.
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),                    #Formats plot subtitle.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  )

#Create Data Frame for Sensitivities of best models and sample combos Bar Chart:
df_best <- data.frame (Name  = c("Tree_SMOTE","Forest_Under", "GBM_SMOTE", "xGboost_Under"),
                      Sensitivity = c(81.63, 90.82, 87.76, 91.84)
)

order_best <- c("xGboost_Under", "Forest_Under", "GBM_SMOTE", "Tree_SMOTE") #Create variable to order bars

ggplot(data=df_best, aes(x = Name, y = Sensitivity)) +
  geom_bar(stat="identity") +                                          #Plots Bar Chart
  geom_col() +
  geom_text(aes(label = Sensitivity), vjust = 1.5, colour = "white") + #Adds data labels to each bar
  scale_x_discrete(limits = order_best) +                              #Orders bars by height instead of in alphabetical order of sample type
  labs(                                                               
    title = "Best Model and Sample Combination Sensitivity Comparison",#Sets a plot title.
    x = "Sample Type",                                                 #Changes x-axis title.
    y = "Sensitivity %"                                                #Changes y-axis title.
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),                       #Formats plot title.
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),                    #Formats plot subtitle.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  )

#Create Data Frame for AUC's of best models and sample combos Bar Chart:
df_bauc <- data.frame (Name  = c("Tree_SMOTE","Forest_Under", "GBM_SMOTE", "xGboost_Under"),
                       AUC = c(0.836, 0.974, 0.978, 0.969)
)

order_bauc <- c("GBM_SMOTE", "Forest_Under", "xGboost_Under", "Tree_SMOTE") #Create variable to order bars

ggplot(data=df_bauc, aes(x = Name, y = AUC)) +
  geom_bar(stat="identity") +                                          #Plots Bar Chart
  geom_col() +
  geom_text(aes(label = AUC), vjust = 1.5, colour = "white") +         #Adds data labels to each bar
  scale_x_discrete(limits = order_bauc) +                              #Orders bars by height instead of in alphabetical order of sample type
  labs(                                                               
    title = "Best Model and Sample Combination AUC Comparison",        #Sets a plot title.
    x = "Sample Type",                                                 #Changes x-axis title.
    y = "Sensitivity %"                                                #Changes y-axis title.
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),                       #Formats plot title.
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),                    #Formats plot subtitle.
    axis.title.x = element_text(size = 12, face = "bold", colour = "grey40"),               #Formats x-axis title.  
    axis.title.y = element_text(angle = 90, size = 12, face = "bold", colour = "grey40")    #Formats y-axis title.
  )