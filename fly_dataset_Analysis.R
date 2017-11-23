#### FLYING ETIQUETTES
install.packages("tidyverse")
install.packages("plotly")
install.packages("ggplot2")
install.packages("caTools")
install.packages("corrplot")
install.packages("randomForest")
install.packages("plotrix")
install.packages("descr")
install.packages("VIM")
install.packages("ineq")
install.packages("arm")
library(arm)
library(ineq)
library(lattice)
library("caret")
library("e1071")
library(VIM)
library(descr)
library(plotly)
library(tidyr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caTools)
library(e1071)
library(plotrix)
library(rpart)
library(rpart.plot)
library(tree)
library(rattle)
library(RColorBrewer)
library(randomForest)
setwd("C:/Users/Abhishek Singh/Desktop/538/fly")
getwd()
raw_data <- read.csv("flying-etiquette.csv", header = T,na.strings=c("","NA")) #na.strings will treat the empty cells as NA
#raw_data[raw_data== ""] <- NA
head(raw_data)
##Renaming of the column names
#attach(raw_data)
raw_data = rename(raw_data,
               Travel_freq = How.often.do.you.travel.by.plane.,
                Recline_freq = Do.you.ever.recline.your.seat.when.you.fly.,
                Height = How.tall.are.you.,
                Children_under_18 = Do.you.have.any.children.under.18.,
                Arm_rest_3 = In.a.row.of.three.seats..who.should.get.to.use.the.two.arm.rests.,
                Arm_rest_2 = In.a.row.of.two.seats..who.should.get.to.use.the.middle.arm.rest.,
                Window_shade_control = Who.should.have.control.over.the.window.shade.,
                Rude_unsold_seat = Is.itrude.to.move.to.an.unsold.seat.on.a.plane.,
                Rude_talk_stranger = Generally.speaking..is.it.rude.to.say.more.than.a.few.words.tothe.stranger.sitting.next.to.you.on.a.plane.,
                get_up = On.a.6.hour.flight.from.NYC.to.LA..how.many.times.is.it.acceptable.to.get.up.if.you.re.not.in.an.aisle.seat.,
                Recline_obligation = Under.normal.circumstances..does.a.person.who.reclines.their.seat.during.a.flight.have.any.obligation.to.the.person.sitting.behind.them.,
                Recline_rude = Is.itrude.to.recline.your.seat.on.a.plane.,
                Recline_eliminate= Given.the.opportunity..would.you.eliminate.the.possibility.of.reclining.seats.on.planes.entirely.,
                Rude_Switch_friend = Is.it.rude.to.ask.someone.to.switch.seats.with.you.in.order.to.be.closer.to.friends.,
                Rude_Switch_family = Is.itrude.to.ask.someone.to.switch.seats.with.you.in.order.to.be.closer.to.family.,
                Rude_wake_bathroom = Is.it.rude.to.wake.a.passenger.up.if.you.are.trying.to.go.to.the.bathroom.,
                Rude_wake_walk = Is.itrude.to.wake.a.passenger.up.if.you.are.trying.to.walk.around.,
                Rude_bring_baby = In.general..is.itrude.to.bring.a.baby.on.a.plane.,
                Rude_bring_unruly_child = In.general..is.it.rude.to.knowingly.bring.unruly.children.on.a.plane.,
                Electronics = Have.you.ever.used.personal.electronics.during.take.off.or.landing.in.violation.of.a.flight.attendant.s.direction.,
                Smoked = Have.you.ever.smoked.a.cigarette.in.an.airplane.bathroom.when.it.was.against.the.rules.,
                Household_Income = Household.Income,
                Region = Location..Census.Region.)

#missing Imputation:
#imputing missing Height by mode of height 
Mode_Height <-names(table(raw_data$Height))[which(table(raw_data$Height)==max(table(raw_data$Height)))]
raw_data$Height[which(is.na(raw_data$Height))]<-Mode_Height

#imputing missing Age by mode of Age 
Mode_Age <-names(table(raw_data$Age))[which(table(raw_data$Age)==max(table(raw_data$Age)))]
raw_data$Age[which(is.na(raw_data$Age))]<-Mode_Age

##Height conversion from ft to Inches
h <- as.factor(raw_data$Height)
levels(h) <- sub("Under 5 ft.", "4'11\"", levels(h)) #converting under 5ft into numerical and a category of 5ft
levels(h) <- sub("6'6\" and above", "6'7\"", levels(h)) #converting above 6ft6inc as 6'7"
table(h)
##converting height category into numerical height(inch)
raw_data$Height_in_inches<-sapply(strsplit(as.character(h),"'|\""),function(x){12*as.numeric(x[1]) + as.numeric(x[2])})

names(raw_data)
raw_data <- raw_data[,c(1,23,24,26,4,28,25,27,2,3,5:8,11,12,14,9,10,13,15:22)] #areanging the  columns according to demographic and rudeness 
names(raw_data)

## creating new data frame raw_Data1 in which removed those rows which has missing values in all the columns
#raw_data1 <- raw_data
raw_data$na_count <- apply(is.na(raw_data),1,sum) #counting the missing values in each row
raw_data1<- raw_data[which(raw_data$na_count <4),]
raw_data1 <- raw_data1[,-29] #removing the na_count column
## Writing the CSV file with the updated dataset:
write.csv(raw_data1,"data_flying_etiquette.csv",row.names = F)
#------------------------------------------------------------------------------------------

raw_data1$RespondentID = as.factor(raw_data1$RespondentID) #converting RespondentID into factor from double int

##############################################################
###---------- Univariate Analysis--------------
##Histogram of missing values 
# missing count for raw data
missing_count <- data.frame(colSums(is.na(raw_data[,-29]))) #counting the missing values in each columns of RAW DATA
missing_count1 <- data.frame(colSums(is.na(raw_data1))) #counting the missing values in each column or REFINED Data
miss_data <- as.data.frame(cbind(missing_count,missing_count1))

plot_miss_data<- plot_ly(miss_data,x = rownames(miss_data), y = miss_data$colSums.is.na.raw_data....29..., type = 'bar', name = 'Raw Data', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = miss_data$colSums.is.na.raw_data1.., name = 'Refined Data', marker = list(color = 'rgb(204,204,204)')) %>%
  layout(xaxis = list(title = "Raw data Missing Values compared to refined data", tickangle = -45,list(b = 160)), yaxis = list(title = ""),barmode = 'group',margin=list(b = 160))
plot_miss_data

missing_percent <- data.frame(colMeans(is.na(raw_data))) # percent of missing values in each columns
colMeans(data.frame(colMeans(is.na(raw_data)))) # mean of % of Missing value throughout the dataset

#******************
# Pie Charts
# Plot the chart.
#gender Pie
d_gender <- table(raw_data1$Gender)
d_gender_per <- paste((round(100*d_gender/sum(d_gender), 1)),"%",sep="")
pie_gender<- pie(d_gender, main = "Gender Chart",
                 labels = paste(names(d_gender),d_gender_per), col = rainbow(length(d_gender)))

#age Pie
d_age <- table(na.omit(raw_data1$Age))
d_age_per <- paste((round(100*d_age/sum(d_age), 1)),"%",sep="")
lbls_age<- paste(names(d_age),d_age_per)
pie_age<- pie3D(d_age, labels = lbls_age, main = "Age Chart",col = rainbow(length(d_age)),explode =0.1)
#equally Distributed

#Education Pie
d_edu <- table(na.omit(raw_data1$Education))
d_edu_per <- paste((round(100*d_edu/sum(d_edu), 1)),"%",sep="")
pie_edu<- pie(d_edu, labels = d_edu_per, main = "Education Distribution", col = rainbow(length(d_age)))
legend(-1.43,1.08, c("Bachelor","Graduate","High School","less high school", "Some College"),
       cex = 0.55, fill = rainbow(length(d_gender)))

################################################# Scoring ##############

### Creating binary response of Recline_frequency
df1<-raw_data1
#converting Recline frequency into NO for never and Yes  for the levels of any angle of reclination.
levels(df1$Recline_freq)[levels(df1$Recline_freq)== "Never"]  <- "No"
levels(df1$Recline_freq)[levels(df1$Recline_freq)=="About half the time" | levels(df1$Recline_freq)=="Usually"
                         | levels(df1$Recline_freq)=="Always" | levels(df1$Recline_freq)=="Once in a while"] <- "Yes"
df1$Recline_freq <- factor(df1$Recline_freq, levels = c("No","Yes"))

#encodinga and transformation of data
# Travel_ Frequency - Never = 1; once a yr or less= 2; once a month =3; few times in month = 4; few times a week=5; daily =6
levels(df1$Travel_freq) # shows how the factor levels  are arranged in the Travel_freq
lev=levels(df1$Travel_freq)[c(4,6,5,1,2,3)] ## reordering the levels according to the requirement
df1$Travel_freq <- as.numeric(factor(df1$Travel_freq,levels=lev)) # giving numeric values to the categories

#Encoding Recline column
df1$Recline_freq <- as.numeric(factor(df1$Recline_freq))
#children under 18 binary
## ArmRest on 3 seat: other-1, shared-5, corner gets both the armrest -3 , middle seat person -4 , first take away-2
df1$Arm_rest_3 <- as.numeric(factor(df1$Arm_rest_3,levels= levels(df1$Arm_rest_3)[c(1,5,3,4,2)]))
## ArmRest on 2 seat: other-1, shared-5, aisle seater -3 , windows seater -4 , first take away-2
df1$Arm_rest_2 <- as.numeric(factor(df1$Arm_rest_2,levels= levels(df1$Arm_rest_2)[c(1,5,3,4,2)]))
#AGE group encoding lowest to highest
df1$Age <- as.numeric(factor(df1$Age,levels= levels(df1$Age)[c(2,3,4,1)]))
#Education lowest to highest
df1$Education <- as.numeric(factor(df1$Education,levels= levels(df1$Education)[c(4,3,5,1,2)]))
#household income lowest to highest
df1$Household_Income <- as.numeric(factor(df1$Household_Income,levels= levels(df1$Household_Income)[c(1,3,4,2,5)]))

#Window shade control
df1$Window_shade_control <- as.numeric(factor(df1$Window_shade_control,levels= levels(df1$Window_shade_control)[c(2,1)]))
#Movement of aisle seater during flight
df1$get_up <- as.numeric(factor(df1$get_up,levels= levels(df1$get_up)[c(2,4,6,5,1,3)]))

##Yes/NO encoding
df1$Smoked <- as.numeric(df1$Smoked)
df1$Electronics <- as.numeric(df1$Electronics)
df1$Recline_obligation <- as.numeric(df1$Recline_obligation)
df1$Recline_eliminate <-as.numeric(df1$Recline_eliminate)
df1$Children_under_18 <- as.numeric(df1$Children_under_18)
df1$Gender <- as.numeric(df1$Gender)

#**************Rudeness encoding *******************
#                   Not rude-> 1; Somewhat rude ->2; Very Rude -> 3
df1$Rude_bring_baby <- as.numeric(factor(df1$Rude_bring_baby))
df1$Rude_unsold_seat <- as.numeric(factor(df1$Rude_unsold_seat))
df1$Rude_talk_stranger <- as.numeric(factor(df1$Rude_talk_stranger))
df1$Recline_rude <- as.numeric(factor(df1$Recline_rude))
df1$Rude_wake_bathroom <- as.numeric(factor(df1$Rude_wake_bathroom))
df1$Rude_wake_walk <- as.numeric(factor(df1$Rude_wake_walk))
df1$Rude_bring_unruly_child <- as.numeric(factor(df1$Rude_bring_unruly_child))
df1$Rude_Switch_family <- as.numeric(factor(df1$Rude_Switch_family))
df1$Rude_Switch_friend <- as.numeric(factor(df1$Rude_Switch_friend))
##writing score files
write.csv(df1,"score_flying_etiquette.csv")

#***************************************************************************************************************#

##Correlation among rude data
df2<-df1
#replace missing numerical values with 0
df2[is.na(df2)]<- 0
corr_df<- cor(df2[,c(17:25)])
corrplot(corr_df,type = "upper",method ="pie")  #Correlation plot of rudiness
corrplot(corr_df,method ="number") # Correlation matrix graphic

#######Scorings Rules
df_score <-df2
df_score$score <-rowSums(df_score[,c(18:26)])
summary(df_score$score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.00   11.00   13.00   13.44   15.00   27.00

#converting Score into binary (threshold: 13, mean=13, median=13.44)
df_score<-mutate(df_score,binary_score = ifelse(df_score$score <=13,"satisfied","Not Satisfied"))
df_score$binary_score <- factor(df_score$binary_score)
attach(df_score)

df_tree <-cbind(raw_data1,df_score$binary_score)
names(df_tree)[names(df_tree) == 'df_score$binary_score'] <- 'Score'#renaming binary score in df_tree as Score
######    Imputation of Education, Household_income and region on the basis of K nearest neighbour by selection 
######    Gender, heigh and Age variable for nearest neighbour
df_tree<- kNN(df_tree,variable = colnames(df_tree)[c(4,7,8)] ,k=4, dist_var = c("Gender", "Age","Height"))
df_tree <- df_tree[,-c(30,31,32)]
attach(df_tree)


#********* Training and test data for Decision Tree
# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(df_tree$Score, SplitRatio = 0.7)
train_DT = subset(df_tree[,c(-1)], split == TRUE)
test_DT = subset(df_tree[,c(-1)], split == FALSE)

##############################       Modeling    ----------------------
##-------------------------------------------- Decision Tree on scoring------------------------------  ######
r.ctrl = rpart.control(minsplit=20, minbucket = 5, cp = 0, xval = 10)
modelCT = rpart(train_DT$Score ~ .,method = 'class',data=train_DT,control = r.ctrl )

#Plotting the tree
prp(modelCT)
fancyRpartPlot(modelCT)

#Evaluating the value of complexity parameter to prune tree.
printcp(modelCT)
plotcp(modelCT) # finding optimal depth of a decision tree model
#prune tree
mytree<-prune(modelCT,cp=0.030,"CP")
fancyRpartPlot(mytree, uniform=TRUE)


train_DT$predictCT.Class<-predict(mytree,train_DT, type="class")
train_DT$predictCT.Score<-predict(mytree,train_DT)
library(ROCR)
pred <- prediction(train_DT$predictCT.Score[,2], train_DT$Score)
perf <- performance(pred, "tpr", "fpr")
plot(perf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
gini = ineq(train_DT$predictCT.Score[,2], type="Gini")
######### Confusion matrix and Accuracy on Train Data Set
with(train_DT, table(Score, predictCT.Class))
confusionMatrix(train_DT$predictCT.Class,train_DT$Score)
auc
KS
gini

#### Validating the decision tree model on test data
test_DT$predictCT.Class<-predict(mytree,test_DT, type="class")
test_DT$predictCT.Score<-predict(mytree,test_DT)
#confusion matrix with accuracy for test data
with(test_DT,table(Score,predictCT.Class))  
confusionMatrix(test_DT$predictCT.Class,test_DT$Score)

#### -----------------------------------------------------------------------------------------------------##

####-------------------------------- logistic regression model-----------------------------------------#####
model_glm <- bayesglm (Score ~ Age+Gender+Height_in_inches+Education+Household_Income+Region+Travel_freq+Rude_bring_baby+
                     Rude_wake_bathroom, 
              data = train_DT, family = binomial)
summary(model_glm)
predict <- predict(model_glm, type = 'response')
#confusion matrix
table(train_DT$Score, predict > 0.5)

library(ROCR)
ROCRpred <- prediction(predict, train_DT$Score)
ROCRperf <- performance(ROCRpred, 'tpr','fpr') 
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7)) #plotting roc curve for logistic regression
auc_glm <- performance(ROCRpred,"auc")
auc_glm <- as.numeric(auc_glm@y.values)
auc_glm ## Area under curve for logistic regression

###----------------------------------------------------------------------------------------------------####

###------------------------------------- Random Forest --------------------------------------------#####

library(randomForest)
rf <- randomForest(as.factor(Score) ~ Gender+Age+Household_Income+Education+Height+Region+Travel_freq
                   +Recline_freq+Arm_rest_2+Arm_rest_3+Window_shade_control+get_up+Recline_obligation+
                     Recline_eliminate+Rude_unsold_seat+Rude_talk_stranger+Rude_Switch_family+Rude_Switch_friend+
                     Rude_wake_bathroom+Rude_wake_walk+Rude_bring_baby+Rude_bring_unruly_child+
                     Electronics+Smoked,
                   data=train_DT, importance=TRUE, ntree=200)

varImpPlot(rf) #plotting the mean decrease Accuracy and mean decrease gini for significant variables

predict_rf <- predict(rf, type = 'response')
#############################################################################################################