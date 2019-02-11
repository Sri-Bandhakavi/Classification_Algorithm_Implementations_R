
setwd("C:\\Users\\Sri Bandhakavi\\Desktop\\MPSA\\ALY6040DataMiningApplications\\Project")
getwd()

##############################################################################
####AirBnB Satisfaction Score Predictions for Boston properties###############
##############################################################################

# AIM - evaluate performance of different classifier algorithms to predict for a listing
# of airbnb properties in Boston whether or not they will have a overall satisfaction score of 5 
# (highest rating = 5) versus not.

# July data for Boston airbnb listings downloaded from http://tomslee.net/airbnb-data-collection-get-the-data/airbnb
# (direct link - http://tomslee.net/airbnb-data-collection-get-the-data ) and used for this exercise


# Into above data set also incorporate prior overall satisfaction rating (if available) for use as an 
# additional predictor variable in modeling.

library(dplyr)
library(tidyr)

#-------------------------1. Read in dataset, set up dummy dependent variable, clean extraneous columns-----------------------------------#

boston_17_Jul2<-read.csv("tomslee_airbnb_boston_1429_2017-07-10.csv")

str(boston_17_Jul2)
summary(boston_17_Jul2)
head(boston_17_Jul2)
colSums(is.na(boston_17_Jul2)) # country is only column with NAs


#-----------------------------------2.Feature engineering (part 1) -----------------------------------------------------------------------#

#set dependent variable (highest satisfaction score of 5 to 1) to be a factor dummy variable
boston_17_Jul2$overall_satisfaction <- as.factor(ifelse(boston_17_Jul2$overall_satisfaction == 5.0, 1, 0))
table(boston_17_Jul2$overall_satisfaction)
#remove columns with all NAs or non relevant ID information or overlapping information or variables that are inconsistently collected

colSums(is.na(boston_17_Jul2)) # country is only column with NAs

boston_17_Jul2<-boston_17_Jul2%>% select(-country, -survey_id, -host_id, -location, -latitude, -longitude, -last_modified)

str(boston_17_Jul2) # name column is a unique "ad" associated with each property but as-is has little value in ascertaining its importance in generating customer demand (which may be linked to overall satisfaction score)
#in next step process name column to generate a numeric sentiment score
table(boston_17_Jul2$overall_satisfaction)
#----------------------------------3.Feature engineering (part 2) - generate sentiment scores from "name" column -------------------------##
#Plan - sentiment scores generated from "name" column will be appended back to boston_17_july prior to EDA/data modeling

idname<-boston_17_Jul2[,c(1,10)]

#A. Build a Text Corpus
library(tm)
airbnb.corpus<-Corpus(VectorSource(idname$name))
summary(airbnb.corpus)
inspect(airbnb.corpus[1:5]) #Inspecting elements in Corpus

#B.Data Transformations -Cleaning
airbnb.corpus<-tm_map(airbnb.corpus,tolower) #Converting to lower case
airbnb.corpus<-tm_map(airbnb.corpus,stripWhitespace) #Removing extra white space
airbnb.corpus<-tm_map(airbnb.corpus,removePunctuation) #Removing punctuations
airbnb.corpus<-tm_map(airbnb.corpus,removeNumbers) #Removing numbers
my_stopwords<-c(stopwords('english'),'available') #Can add more words apart from standard list
airbnb.corpus<-tm_map(airbnb.corpus,removeWords,my_stopwords)

#C. Tag the sentiments
airbnb.text<-idname$name
airbnb.text

#D.Read in word dictionaries (BING lexicon- https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/master/data/opinion-lexicon-English)
pos = scan('positive-words.txt',what='character',comment.char=';')
pos
neg = scan('negative-words.txt',what='character',comment.char=';')
neg
#E.Adding words to dictionaries

pos[2007:2015]<-c("spectacular","everyday","better","top","thumbs","four","five", "nine", "ten")
neg[4784:4789]<-c("one","two","careful","sync","Beware","suck")

#F. Jeffrey Breen Function to "Tag" sentiments to sentences (https://datamatters.blog/tag/sentiment-analysis/)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  #we got a vector of sentences. plyr will handle a list
  #or a vector as an "l" for us
  #we want a simple array of scores back, so we use
  #"l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    #clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence) #removes punctuations
    sentence = gsub('[[:cntrl:]]', '', sentence) #removes control characters
    sentence = gsub('\\d+', '', sentence) #removes digits
    
    #and convert to lower case:
    sentence = tolower(sentence)
    
    #split sentences into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    
    #sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    #compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    #match() returns the position of the matched term or NA
    #we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    #and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#G. pass airbnb.text to Jeffrey Breen function from above to generate numerical sentiment score for each property
analysis<-score.sentiment(airbnb.text, pos, neg, .progress="text")

analysis$text<-as.character(analysis$text)

#H. Rank each property as postive, negative, or neutral based on sentiment score
analysis$sentiment<-as.factor(ifelse(analysis$score>0,"positive",
                                     ifelse(analysis$score<0,"negative","neutral")))

#I. Cleaning the data again
analysis$text = gsub('[[:punct:]]', '', analysis$text)
str(analysis)


#J. Append score/rank from sentiment analysis to boston_17_jul dataset
boston_17_Jul2<-cbind(boston_17_Jul2, analysis[, c(1,3)])
str(boston_17_Jul2)
boston_17_Jul2$sentiment<-as.factor(boston_17_Jul2$sentiment)
colnames(boston_17_Jul2)[11]<-"sentiment_score"

table(boston_17_Jul2$overall_satisfaction)
#----------------------------------4.Feature engineering (part 3) - append overall satisfaction scores from prior month(s)-------------------------##
#Plan - retrieve overall_satisfaction score for same properties from prior month data (June) and append as new column in current data -------------##
# For NA values, impute mean of column

July<- read.csv("tomslee_airbnb_boston_1429_2017-07-10.csv", stringsAsFactors = FALSE)
July<-July[, c(1,9)]

June<-read.csv("tomslee_airbnb_boston_1309_2017-06-10.csv")
June<-June[, c(1,10)]
colnames(June)[2]<-"Prev_1_Mnth_SatisfacnScore"

JulyJune<-merge(July, June, by="room_id", all.x = TRUE)

colSums(is.na(JulyJune)) # 506/4705 (~11% have NA values)

#impute column mean for these 506 entries and store imputed values as new column
JulyJune$imp.prior.sat_score <- ifelse(is.na(JulyJune$Prev_1_Mnth_SatisfacnScore), mean(JulyJune$Prev_1_Mnth_SatisfacnScore, na.rm=TRUE), JulyJune$Prev_1_Mnth_SatisfacnScore)

# Append prior month satisfaction score to boston_17_jul dataset
boston_17_Jul2<-cbind(boston_17_Jul2, JulyJune$imp.prior.sat_score)
str(boston_17_Jul2)
colnames(boston_17_Jul2)[13]<-"prior_satisfaction_score"
table(boston_17_Jul2$overall_satisfaction)

#convert prior month's overall_satisfaction score as a two-level dependent variable prior to EDA/modeling
boston_17_Jul2$prior_satisfaction_score<-as.factor(ifelse(boston_17_Jul2$prior_satisfaction_score == 5, "fivestar", "notfivestar"))
table(boston_17_Jul2$overall_satisfaction)

#-------------------------4. Exploratory data analysis-----------------------------------#


#(a1)distribution of factor variables (except "name" variable) by themselves
prop.table(table(boston_17_Jul2$room_type))
prop.table(table(boston_17_Jul2$neighborhood))
prop.table(table(boston_17_Jul2$sentiment))
prop.table(table(boston_17_Jul2$prior_satisfaction_score)) #31% have five star ratings in prior month
prop.table(table(boston_17_Jul2$overall_satisfaction)) #34% have five star rating in current month

#(a2) rename levels of neighborhood factor (some neighborhood names are rather long)
summary(boston_17_Jul2$neighborhood) # list level names
levels(boston_17_Jul2$neighborhood) <- c("Allston", "Back Bay", "Bay Village", "Beacon Hill", "Brighton", 
                        "Charlestown", "Chinatown", "Dorchester", "Downtown", "East Boston", "Fenway",
                        "Hyde Park", "Jamaica Plain", "Leather Dist", "Lwood Med Area", "Mattapan",
                        "Mission Hill", "North End", "Roslindale", "Roxbury", "South Boston",
                        "S.Bos Waterfront", "South End", "West End", "West Roxbury") 
summary(boston_17_Jul2$neighborhood) # List new level names

#(b)distribution of factor variables (except "name" variable) by satisfaction score - tabular result and visual result

#room type by satisfaction score
rtss<-table(boston_17_Jul2$overall_satisfaction, boston_17_Jul2$room_type)
round(100*prop.table(rtss,2), digits = 1) #"2" indicates column-wise proportions selected
    
    #room type by satisfaction score (counts)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(rtss, xlab='Class',ylab='Frequency',main="Satisfaction score by room type",
        col=c("steelblue","lightcyan")
        ,legend=rownames(rtss), args.legend = list(x = "topright", inset = c(-.25, 0)))

    #room type by satisfaction score (proportions)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot((prop.table(rtss, 2)), xlab='Class',ylab='Frequency',main="Room type by satisfaction score",
        col=c("steelblue","lightcyan")
        ,legend=rownames(prop.table(rtss,2)), args.legend = list(x = "topright", inset = c(-.25,0)))

#neighborhood by satisfaction score
nbss<-table(boston_17_Jul2$overall_satisfaction, boston_17_Jul2$neighborhood)

    #neighborhood by satisfaction score (counts)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(nbss,ylab='Frequency',main="satisfaction score by room type",
        col=c("steelblue","lightcyan")
        ,legend=rownames(nbss), las=2,cex.names=0.55,args.legend = list(x = "topright", inset = c(-.25, 0)))

    #neighborhood by satisfaction score (proportions)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(prop.table(nbss,2),ylab='Frequency',main="satisfaction score by neighborhood",
        col=c("steelblue","lightcyan")
        ,legend=rownames(prop.table(nbss,2)), las=2, cex.names=0.55,args.legend = list(x = "topright",inset=c(-0.25,0)))

#sentiment score for name column by satisfaction score
sentss<-table(boston_17_Jul2$overall_satisfaction, boston_17_Jul2$sentiment)

    #sentiment score by satisfaction score (counts)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(sentss,ylab='Frequency',main="satisfaction score by 'ad (name)' sentiment",
        col=c("steelblue","lightcyan")
        ,legend=rownames(sentss), las=2,cex.names=0.8, args.legend = list(x = "topright", inset = c(-.25, 0)))

    #sentiment score by satisfaction score (proportions)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(prop.table(sentss,2),ylab='Frequency',main="satisfaction score by '(name)' sentiment",
        col=c("steelblue","lightcyan")
        ,legend=rownames(prop.table(sentss,2)), las=2, cex.names=0.8,args.legend = list(x = "topright",inset=c(-0.25,0)))

#prior month rating by satisfaction score
pmss<-table(boston_17_Jul2$overall_satisfaction, boston_17_Jul2$prior_satisfaction_score)

    #prior month rating by satisfaction score (counts)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(prop.table(pmss,2),ylab='Frequency',main="current satisfaction score  by prior rating",
        col=c("steelblue","lightcyan")
        ,legend=rownames(prop.table(pmss,2)), las=2, cex.names=0.8,args.legend = list(x = "topright",inset=c(-0.25,0)))

#referred to link for plotting categorical variables https://www.sheffield.ac.uk/polopoly_fs/1.714591!/file/stcp-karadimitriou-categoricalR.pdf

#(c)distribution of numeric variables

#generate list of names for numeric variables
library(purrr)
numerics<-boston_17_Jul2[, -c(1)] %>%
  keep(is.numeric) %>% names

list<-names(boston_17_Jul2[numerics])
View(list)
#generate boxplots of numeric columns by iterating through list of names
par(mfrow=c(3,2)) #plot in a 2X2 matrix

for(i in 1:(length(list))) {
  boxplot(boston_17_Jul2[,list[i]],main=list[i])
}
# 3/4 columns have high # of outliers (reviewers, accomodates, price)
dev.off()

#density plot(https://drsimonj.svbtle.com/quick-plot-of-all-variables)
library(purrr)
library(ggplot2)
boston_17_Jul2[, -c(1)] %>%
  keep(is.numeric) %>% # Keep only numeric 
  gather() %>% # Convert to key-value
  ggplot(aes(value)) + # Plot the values
  theme_classic()+
  facet_wrap(~ key, scales = "free") + # In separate panels
  geom_density(aes(fill=factor(key)))+# as density
  xlab("")+
  theme(legend.position="none")


#(d)distribution of numeric variables by satisfaction score  
#numeric variables by overall satisfaction score
library(ggplot2)
for (i in 1:(length(list))) {
  g<-(ggplot(boston_17_Jul2) + theme_classic()+ theme(legend.position = "none")+geom_boxplot(aes(x=boston_17_Jul2$overall_satisfaction, y=boston_17_Jul2[,list[i]], fill = boston_17_Jul2$overall_satisfaction)))
  print(g + ggtitle(list[i]) + ylab(list[i]) + xlab("overall satisfaction score"))
}           #accomodates, bedrooms, and price do not seem to affect overall satisfaction score

#thus far we have identified few variables - #reviews, neighborhoods, and perhaps accomodates, as likely significant in receiving highest rating

##############################################################################################################
#--------------------------Prepare data for modeling ------------------------------------------------------#
#--------------------------Partition data into test and train datasets-------------------------------------#
#--------------------------check for ~ similar proportions in each set-------------------------------------#
##############################################################################################################
set.seed(12)

indexes<-sample(0.75*nrow(boston_17_Jul2))

train<-boston_17_Jul2[indexes,]
test<-boston_17_Jul2[-indexes,]

prop.table(table(train$overall_satisfaction)) #~0.34 have highest overall satisfaction score in training data

prop.table(table(test$overall_satisfaction)) # similar proportions with highest overall satisfaction score (~0.34) in testing data

#########################################################################################################################
########################MODELING BY DECISION TREE########################################################################
#########################################################################################################################

library(rpart) #classification and regression trees
library(partykit) #treeplots
library(rattle) #used along with rpart.plot and Rcolor brewer for fancy decision tree plots
library(rpart.plot)
library(RColorBrewer)

#----------------------------------------1. initial tree----------------------------------------------------------------#
tree1 <- rpart(overall_satisfaction~., data=train[, -c(1,3,10)])

#--------------------2. determine optimal value of cost parameter and optimize model (if possible)----------------------#
print(tree1$cptable)
plotcp(tree1)
cp = min(tree1$cptable[5,])

set.seed(234)
DecisionTree <- prune(tree1, cp = cp)

#---------------------------3. variable importance in decision tree model-----------------------------------------------#
DecisionTree$variable.importance
barplot(DecisionTree$variable.importance, main = "Variable importance in decision tree model",
        ylab="Goodness of split (sum decrease GINI)")


#-------------------------4. Append predicted labels/probabilities as columns within test data set----------------------# 
test$DTPred <- predict(DecisionTree, newdata = test, type="class") #Returns the predicted class
test$DTProb <- predict(DecisionTree, newdata = test, type="prob") #Returns a matrix of predicted probabilities

#-------------------------5. Evaluate model performance-----------------------------------------------------------------# 
mean(test$DTPred == test$overall_satisfaction) 
library(caret)
confusionMatrix(test$DTPred,test$overall_satisfaction,positive="1")

#--------------------------6. ROC curve --------------------------------------------------------------------------------#
library(ROCR)
#convert labels to numeric format for ROC curve
DTpred<-as.numeric(as.character(test$DTPred))
actual<-as.numeric(as.character(test$overall_satisfaction))

DT_pred<-prediction(DTpred, actual)
DT_perf<-ROCR::performance(DT_pred,"tpr","fpr")
plot(DT_perf,col="red", lwd=2, main="Decision tree ROC")
abline(0,1, lty = 8, lwd=2, col = "grey")

DT_auc<-performance(DT_pred, "auc")
unlist(DT_auc@y.values)

##########################################################################################################################
########################MODELING BY RANDOM FOREST#########################################################################
##########################################################################################################################
library(randomForest)

#----------------------------------------1. initial Random Forest-------------------------------------------------------#
#generate random forest using default parameters  
set.seed(14)
Random_Forest <- randomForest(overall_satisfaction ~ ., data = train[, -c(1,3,10)], importance = TRUE, ntree=500) 
# default ntrees=500
Random_Forest$confusion

#--------------------2. determine optimal value of cost parameter and optimize model (if possible)----------------------#
#tuning initial model
set.seed(3)
tune.rf <- tuneRF(train[, -c(1,3,6,10)],train$overall_satisfaction, stepFactor=0.5)
print(tune.rf) #mtry = 3 results in lowest OOB errors

Random_Forest <- randomForest(overall_satisfaction ~ ., data = train[, -c(1,3,10)], importance = TRUE, ntree=500, mtry=3) 

#---------------------------3. variable importance in model-------------------------------------------------------------#
#variable importance
RFI<-as.data.frame(Random_Forest$importance)
class(RFI)

library(data.table)
setDT(RFI, keep.rownames = TRUE) #save output of Random_Forest$importance in dataframe with rownames as first column
RFI <- RFI[order(-MeanDecreaseGini,MeanDecreaseAccuracy),]

barplot(RFI$MeanDecreaseGini, names.arg=RFI$rn, main="Variable importance for Random Forest model", ylab = "Mean decrease GINI")
barplot(RFI$MeanDecreaseAccuracy, names.arg=RFI$rn, main="Variable importance for Random Forest model", ylab = "Mean decrease Accuracy")


#-------------------------4. Append predicted labels/probabilities as columns within test data set----------------------# 
#a retrieve predicted labels/probabilites
test$RFPred <- predict(Random_Forest, newdata = test, type="class") #Returns the predicted class
test$RFProb <- predict(Random_Forest, newdata = test, type="prob") #Returns a matrix of predicted probabilities


#-------------------------5. Evaluate model performance-----------------------------------------------------------------#
#evaluate RF model performance against test set

mean(test$RFPred == test$overall_satisfaction) 
library(caret)
confusionMatrix(test$RFPred,test$overall_satisfaction,positive="1")

#--------------------------6. ROC curve --------------------------------------------------------------------------------#
library(ROCR)
#convert labels to numeric format for confusion matric/ROC generation
RFpred<-as.numeric(as.character(test$RFPred))
actual<-as.numeric(as.character(test$overall_satisfaction))

#ROC plot
RF_pred<-prediction(RFpred, actual)
RF_perf<-ROCR::performance(RF_pred,"tpr","fpr")
plot(RF_perf,col="green", lwd=2, main="Random Forest ROC")
abline(0,1, lty = 8, lwd=2, col = "grey")


RF_auc<-performance(RF_pred, "auc")
unlist(RF_auc@y.values)

##########################################################################################################################
########################MODELING BY LINEAR SVM ###########################################################################
##########################################################################################################################

testing <-test
training <-train
#----------------------------------------1. initial linear SVM-------------------------------------------------------#
#generate using initial settings and 10-fold validation 3 times 

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
svm_Linear <- train(overall_satisfaction ~., data = training[, -c(1,3,10)], method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear$results
svm_Linear

#--------------------2. determine optimal value of Cost parameter ("C") in SVM_linear model and optimize model (if possible)----------------------#
#tuning initial model

grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 2, 3))
set.seed(3233)
svm_Linear_Grid <- train(overall_satisfaction ~., data = training[, -c(1,3,10)], method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)

svm_Linear_Grid
plot(svm_Linear_Grid)

#---------------------------3. variable importance in optimized SVM model-------------------------------------------------------------#
#variable importance
SVML_roc_imp <- varImp(svm_Linear_Grid, scale = FALSE)
SVML_roc_imp
str(SVML_roc_imp)

SVMLI<-(as.data.frame(SVML_roc_imp$importance))
SVMLI <- SVMLI[order(-X0, -X1),]
library(data.table)
setDT(SVMLI, keep.rownames = TRUE)

barplot(SVMLI$X0, names.arg=SVMLI$rn, main="Variable importance for SVM linear model", 
        ylab = "Area under ROC curve")


#-------------------------4. Append predicted labels/probabilities as columns within test data set----------------------# 
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
testing$SVMLPred <- predict(svm_Linear_Grid, newdata = testing, type="raw") #Returns the predicted class
str(test_pred_grid)


#-------------------------5. Evaluate model performance-----------------------------------------------------------------#
mean(test_pred_grid == testing$overall_satisfaction) 

confusionMatrix(testing$overall_satisfaction, testing$SVMLPred, positive = "1")

#--------------------------6. ROC curve --------------------------------------------------------------------------------#
library(ROCR)
#convert labels to numeric format for confusion matric/ROC generation
SVMLPred<-as.numeric(as.character(testing$SVMLPred))
actual<-as.numeric(as.character(testing$overall_satisfaction))

#ROC plot
SVLpred<-prediction(SVMLPred, actual)
SVLperf<-ROCR::performance(SVLpred,"tpr","fpr")
plot(SVLperf,col="pink", lwd=2, main="SVM linear model ROC")
abline(0,1, lty = 8, lwd=2, col = "grey")

SVL_auc<-performance(SVLpred, "auc")
unlist(SVL_auc@y.values)

##########################################################################################################################
########################MODELING BY RADIAL SVM ###########################################################################
##########################################################################################################################

#----------------------------------------1. initial radial SVM-------------------------------------------------------#
#generate using initial settings and 10-fold validation 3 times 

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(341)
svm_Radial <- train(overall_satisfaction ~., data = training[, -c(1,3,10)], method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Radial$results

svm_Radial
#--------------------2. tune initial SVM radial model by optimizing Cost parameter ("C") and Sigma values------------#
#tuning initial model
grid_radial <- expand.grid(sigma = c(0.01, 0.02, 0.025, 0.03, 0.04,
                                     0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5,0.9),
                           C = c(0.25, 1, 10, 50, 100, 128, 150, 200,300))
set.seed(326)
str(training)
svm_Radial_Grid <- train(overall_satisfaction ~., data = training[, -c(1,3,10)], method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_radial,
                         tuneLength = 10)


plot(svm_Radial_Grid)
svm_Radial_Grid
###
#---------------------------3. variable importance in optimized Radial SVM model-------------------------------------------------------------#
#variable importance
SVMR_roc_imp <- varImp(svm_Radial_Grid, scale = FALSE)
SVMR_roc_imp
str(SVMR_roc_imp)

SVMRI<-(as.data.frame(SVMR_roc_imp$importance))
library(data.table)
setDT(SVMRI, keep.rownames = TRUE)

SVMRI <- SVMRI[order(-X0, -X1),]
barplot(SVMRI$X0, names.arg=SVMLI$rn, main="Variable importance for radial SVM model", 
        ylab = "Area under ROC curve")

#-------------------------4. Append predicted labels/probabilities as columns within test data set----------------------# 
#test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
testing$SVMRPred <- predict(svm_Radial_Grid, newdata = testing, type="raw") #Returns the predicted class

#-------------------------5. Evaluate model performance-----------------------------------------------------------------#
mean(testing$SVMRPred == testing$overall_satisfaction) 

confusionMatrix(testing$overall_satisfaction, testing$SVMRPred, positive = "1")


##--------------------------6. ROC curve --------------------------------------------------------------------------------#
library(ROCR)
#convert labels to numeric format for confusion matric/ROC generation
SVMRPred<-as.numeric(as.character(testing$SVMRPred))
actual<-as.numeric(as.character(testing$overall_satisfaction))

#ROC plot
SVRpred<-prediction(SVMRPred, actual)
SVRperf<-ROCR::performance(SVRpred,"tpr","fpr")
plot(SVRperf,col="orange", lwd=2, main="Radial SVM model ROC")
abline(0,1, lty = 8, lwd=2, col = "grey")

SVR_auc<-performance(SVRpred, "auc")
unlist(SVR_auc@y.values)

##########################################################################################################################
########################MODELING BY KNN ###########################################################################
##########################################################################################################################

#----------------------------------------1. initial knn model-------------------------------------------------------#
#generate using initial settings and 10-fold validation 3 times 

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(347)
knn_fit <- train(overall_satisfaction ~., data = training[, -c(1,3,10)], method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit$results

plot(knn_fit) #k=15 is most accurate model 
#---------------------------2. tuning initial model------------------------------------------------------------------------------------------#
#automatcially selects best k parameter in knn_fit

#---------------------------3. variable importance in optimized Radial SVM model-------------------------------------------------------------#
#variable importance
knn_fit_roc_imp <- varImp(knn_fit, scale = FALSE)
knn_fit_roc_imp
str(knn_fit_roc_imp)

knn_fit_roc_imp<-(as.data.frame(knn_fit_roc_imp$importance))
library(data.table)
setDT(knn_fit_roc_imp, keep.rownames = TRUE)

knn_fit_roc_imp <- knn_fit_roc_imp[order(-X0, -X1),]
barplot(knn_fit_roc_imp$X0, names.arg=knn_fit_roc_imp$rn, main="Variable importance for knn model", 
        ylab = "Area under ROC curve")


#-------------------------4. Append predicted labels/probabilities as columns within test data set----------------------# 
testing$knnPred <- predict(knn_fit, newdata = testing, type="raw") #Returns the predicted class

#-------------------------5. Evaluate model performance-----------------------------------------------------------------#
mean(testing$knnPred == testing$overall_satisfaction) 

confusionMatrix(testing$overall_satisfaction, testing$knnPred, positive = "1")

##--------------------------6. ROC curve --------------------------------------------------------------------------------#
library(ROCR)
#convert labels to numeric format for confusion matric/ROC generation
knnPred<-as.numeric(as.character(testing$knnPred))
actual<-as.numeric(as.character(testing$overall_satisfaction))

#ROC plot
KNN_pred<-prediction(knnPred, actual)
KNN_perf<-ROCR::performance(KNN_pred,"tpr","fpr")
plot(KNN_perf,col="brown", lwd=2, main="knn model ROC")
abline(0,1, lty = 8, lwd=2, col = "grey")

KNN_auc<-performance(KNN_pred, "auc")
unlist(KNN_auc@y.values)

write.csv(testing, "airbnbtestingresults.csv")

##########################################################################################################################
########################MODELING BY LOGISTIC REGRESSION ##################################################################
##########################################################################################################################

#----------------------------------------1. initial logistic model-------------------------------------------------------#
#generate using initial settings and 10-fold validation 3 times 

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(547)
logistic_fit <- train(overall_satisfaction ~., data = training[, -c(1,3,10)], method = "glm",
                 family = "binomial",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

logistic_fit$results



#---------------------------2. tuning initial model------------------------------------------------------------------------------------------#
#no tuning parameter for "glm" hence logistic_fit is the final model per below code output 
logistic_fit$bestTune
#see also https://stackoverflow.com/questions/47822694/logistic-regression-tuning-parameter-grid-in-r-caret-package

#---------------------------3. variable importance in logistic model-------------------------------------------------------------#
#variable importance
logistic_fit_roc_imp <- varImp(logistic_fit, scale = FALSE)
logistic_fit_roc_imp
str(logistic_fit_roc_imp)

logistic_fit_roc_imp<-(as.data.frame(logistic_fit_roc_imp$importance))
library(data.table)
setDT(logistic_fit_roc_imp, keep.rownames = TRUE)

logistic_fit_roc_imp <- logistic_fit_roc_imp[order(Overall),]
barplot(logistic_fit_roc_imp$Overall, names.arg=logistic_fit_roc_imp$rn, main="Variable importance for logistic model", 
        ylab = "Area under ROC curve")

#-------------------------4. Append predicted labels/probabilities as columns within test data set----------------------# 
testing$logisticPred <- predict(logistic_fit, newdata = testing, type="raw") #Returns the predicted class

#-------------------------5. Evaluate model performance-----------------------------------------------------------------#
mean(testing$logisticPred == testing$overall_satisfaction) 

confusionMatrix(testing$overall_satisfaction, testing$logisticPred, positive = "1")

##--------------------------6. ROC curve --------------------------------------------------------------------------------#
library(ROCR)
#convert labels to numeric format for confusion matric/ROC generation
logisticPred<-as.numeric(as.character(testing$logisticPred))
actual<-as.numeric(as.character(testing$overall_satisfaction))

#ROC plot
logistic_pred<-prediction(logisticPred, actual)
logistic_perf<-ROCR::performance(logistic_pred,"tpr","fpr")
plot(logistic_perf,col="brown", lwd=2, main="logistic model ROC")
abline(0,1, lty = 8, lwd=2, col = "grey")

logistic_auc<-performance(logistic_pred, "auc")
unlist(logistic_auc@y.values)  #worst performer after linear SVM and knn (both of which are quite poor)

# for model comparison plots below, logistic regression not included 
# due to its relatively poor performance (i.e., did not add anything significant) AND
# different formatting of most important variables in logistic regression compared to all other models

########################################################################################################################
#-------------------------------------MODEL PERFORMANCE COMPARISON-----------------------------------------------------#
########################################################################################################################

#compare models by ROC curve

plot(RF_perf,col=1, lwd=1, lty=2, main="ROC comparison of classifiers ")
plot(DT_perf,col=2, lwd=1, lty=2, add = TRUE)
plot(SVRperf,col=3, lwd=1, lty=2, add=TRUE)
plot(KNN_perf,col=4, lwd=1, lty=2, add=TRUE)
plot(SVLperf,col=5, lwd=1, lty=2, add=TRUE)
abline(0,1, lty = 1, lwd=1, col = "grey")
legend(0.8, 0.45, c("RandomForest (AUC = 0.72)","DecisionTree (AUC=0.65)", "RadialSVM (AUC = 0.6)","KNN (AUC = 0.58)" ,"LinearSVM (AUC = 0.55)"), 1:5,bty="n", pt.cex=1, cex=0.9)

#########################################################################
##################Combined figure of variable importance from each model
#########################################################################
par(mfrow=c(3,3))

barplot(DecisionTree$variable.importance, col="cyan",las=2,adj=0,main = "Decision tree model",
        ylab="Goodness of split (sum decrease GINI)")

barplot(RFI$MeanDecreaseGini, names.arg=RFI$rn,col="steelblue",las=2,adj=0, main="Random Forest model", ylab = "Mean decrease GINI")
barplot(RFI$MeanDecreaseAccuracy, names.arg=RFI$rn, col="steelblue",las=2,adj=0, main="Random Forest model", ylab = "Mean decrease Accuracy")

barplot(SVMLI$X0, names.arg=SVMLI$rn, las=2, adj=0, col="light cyan",main="Linear SVM model", 
        ylab = "Area under ROC curve")

barplot(SVMRI$X0, names.arg=SVMLI$rn, las=2, adj=0, col="light blue",main="Radial SVM model", 
        ylab = "Area under ROC curve")

barplot(knn_fit_roc_imp$X0, names.arg=knn_fit_roc_imp$rn, col="cornflowerblue",las=2,adj=0, main="knn model", 
        ylab = "Area under ROC curve")

dev.off()

##########################################################################################################
##############combined figure for distribution of all factor variables by dependent variable (proportions)
##########################################################################################################
par(mfrow=c(3,2))
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot((prop.table(rtss, 2)),ylab='Frequency',las=1, adj=0, main="Satisfaction score by room type",
        col=c("steelblue","lightcyan"))

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(prop.table(nbss,2),ylab='Frequency',las=2, adj=0, main="Satisfaction score by neighborhood",
        col=c("steelblue","lightcyan"))

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(prop.table(sentss,2),ylab='Frequency',las=1, adj=0, main="Satisfaction score by 'name' sentiment",
        col=c("steelblue","lightcyan"))

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(prop.table(pmss,2),ylab='Frequency',las=1, adj=0, main="Current satisfaction score  by prior month rating",
        col=c("steelblue","lightcyan"))

dev.off()

#####################################################################################
##Combined figure for distribution of FACTOR VARIABLES BY SATISFACTION SCORE(COUNTS)
#####################################################################################
par(mfrow=c(2,2))

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(rtss, xlab='Class',ylab='Frequency',main="Satisfaction score by room type",
        col=c("steelblue","lightcyan")
        , las=1, cex.names=0.8)

#neighborhood by satisfaction score (counts)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(nbss,ylab='Frequency',main="Satisfaction score by neighborhood",
        col=c("steelblue","lightcyan")
        , las=2,cex.names=0.8)


#sentiment score by satisfaction score (counts)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(sentss,ylab='Frequency',main="Satisfaction score by 'ad (name)' sentiment",
        col=c("steelblue","lightcyan")
        , las=1,cex.names=0.8)


#prior month rating by satisfaction score (counts)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(prop.table(pmss,2),ylab='Frequency',main="Satisfaction score (current)  by prior month rating",
        col=c("steelblue","lightcyan")
        ,las=1, cex.names=0.8)

dev.off()


###------------------NEXT STEPS: FOCUS ON DRIVERS OF REVIEWS AS THIS IS MOST IMP PREDICTOR-------------##

###------------------PRELIM ANALYSIS - PART 1- REGRESSION PREDICTION OF REVIEWS -----------------------##

### Given the importance of # of reviews in overall_satisfaction ratings, I sought to model 
### the # of reviews based on available data using a random forest regression model.
### However, the performance of this model is quite poor indicating either that 
### random forest is a poor choice or that available data is insufficient for modeling # of reviews 

### To rule out effect of a poor model choice, I also did correlation analysis in PART 2 to determine
### if any variables in existing data set that correlate significantly (i.e., r>0.5)  with # of reviews

#initial random forest model
set.seed(51)
reviewsRFM <- randomForest(reviews~.,
                     data = training[, -c(1,3,10)],importance=TRUE, ntree=500)
#tune initial random forest model
set.seed(3)
tune.rf <- tuneRF(training[, -c(1,3,10)],training$reviews, stepFactor=0.5)
print(tune.rf) #mtry = 6 results in lowest OOB errors

reviewsRFM <- randomForest(reviews~.,data = training[, -c(1,3,10)],importance=TRUE, ntree=500, mtry=6) 

#variable importance in final random forest model
varImpPlot(reviewsRFM, cex=1.6, pch=18)

#model performance
rfprediction<-predict(reviewsRFM, testing[, -c(1,3,10, 14:20)],type="response" )
rfprediction

boxplot(rfprediction, testing$reviews, ylim=c(0,100),
        names=c("predicted", "actual"),col = c("light blue", "grey"),
        ylab="# of reviews",
        main = "Random Forest model predictions for predicted reviews vs actual reviews")

plot(testing$reviews,rfprediction, ylim=c(0,150), xlim=c(0,150), 
     xlab= "Total reviews 'actuals'", ylab="Total reviews 'Predictions'",
     main= "Regression plot of 'actuals' vs 'predicted' # of reviews using Random Forest model")

summary(lm(rfprediction~testing$reviews)) # R-squared value of only 0.065 (poor model performance)
##-------------------PRELIM ANALYSIS - PART 2 - RANGE OF REVIEWS/LISTINGS BY NEIGHBORHO-----------##

bstn<-boston_17_Jul2[ , -c(1,3,10)]

str(bstn)


#convert all factor variables to "dummy" variables using "one-hot encoding"
library(dummies)

ohe_bstn<- dummy.data.frame(bstn, names=c("room_type", "neighborhood", "sentiment", "prior_satisfaction_score", "verification_status"), sep="_")
ohe_bstn

#convert all factors to numeric (required for scaling all variables below)

numeric_ohe_bstn <- data.frame(lapply(ohe_bstn, function(x) as.numeric(as.character(x))))

#write a scaling function to normalize all values in ohe_bstn to scale of 0 to 1
data_scale<- function(x){
  ((x-min(x))/(max(x)-min(x)))
}

scaled_ohe_bstn<-as.data.frame(lapply(numeric_ohe_bstn, data_scale))

#
corr<-round(cor(scaled_ohe_bstn), 1)

library(ggplot2)
library(ggcorrplot)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower",  
           lab = TRUE, 
           lab_size = 2, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of boston airbnb listings", 
           ggtheme=theme_bw)


##conclusion: # no significant correlations (i.e., r > 0.5) found for any variables with # of reviews
## When this result is combined with the poor performance of random forest model, it suggests the notion that:
## Available data is inadequate/insufficient to predict # of reviews. 
## Focused surveys recommended for identifying factors affecting # of reviews

##------------------------------------------------------------------------------------------------##
#########################################THE END####################################################
##------------------------------------------------------------------------------------------------##


