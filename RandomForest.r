setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df <- read.csv("C:/Users/User/Documents/Comp/Models/Entity/Data/EntityData_Grades.csv", stringsAsFactors = F)

gradeLabs <- c("AAA", "AA+", "AA","AA-","A+", "A","A-", "BBB+","BBB","BBB-","BB+",
          "BB","B")
GradesCnt <- table(factor(df$Grade, levels = gradeLabs))
barplot(GradesCnt, main = "Grades Distribution", xlab = "Grade")

#begin by narrowing down some issues which should be excluded
table(df$Corp.Acceptable16) # should remove the 331 which are not Corp Acceptable
#should also remove the Corp-1 labeled Grades
dfExpl <- df[df$G34Cash16 == 0,]
table(dfExpl$Grade)
sum(is.na(dfExpl$G34Cash16)) #keep zeros, remove NAs

library(sqldf)
dfClean <- sqldf("
                 SELECT * FROM df
                 WHERE 1 = 1
                    AND Grade != 'Corp-1+'
                    AND G34Cash16 IS NOT NULL
                 ")
table(dfClean$Corp.Acceptable16)

dfClean <- dfClean[dfClean$Corp.Acceptable16 == "Yes",]

write.csv(dfClean, "dfGOSchCleanRows.csv", row.names = F)

##########
#begin here
library(sqldf)
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df1 <- read.csv("dfGOSchCleanRows.csv", stringsAsFactors = F, na.strings = c("","NA"), strip.white = T)

#now clean columns for modeling purposes
dfModel <- df[c(1,12,5,9,19:159)] #eliminated some of the intro descriptive columns
rmCols <- colnames(dfModel)[colSums(is.na(dfModel)) > 1000] #now look for columns with large amounts of NA (start with 1k, easier decisions)
dfModel <- dfModel[ , !(names(dfModel) %in% rmCols)] 

#add a column for Investment Grade or Junk (only 5 Grades are junk, only 56 are BBB or lower)
table(dfModel$Grade)
gradeLabs <- c("AAA", "AA+", "AA","AA-","A+", "A","A-", "BBB+","BBB","BBB-","BB+",
              "BB","B")
gradeLabs
IG <- gradeLabs[1:10]
Junk <- gradeLabs[11:13]

"AAA" %in% IG
dfModel$Grade <- ifelse(dfModel$Grade %in% IG == TRUE, "IG","Junk")
table(dfModel$Grade)

dfInCorpect <- sqldf("SELECT Grade, Grade FROM dfModel")

###(only 5 Grades are junk, only 56 are BBB or lower....)
###begin by modeling as factors, then come back to break into Upper/Lower divisions

###continue cleaning
#possRmCols <- colnames(dfModel)[colSums(is.na(dfModel)) > 1000] #only one other

###correlation analysis
library(mlbench)
library(caret)
library(purrr)
library(dplyr)
library(corrplot)

#41, 47  = identify numeric columns to use for correlation matrix
#begin at col 5:79, exclude 41, 47
dfNumeric <- dfModel[c(5:40,42:46,48:79)]

#commas are throwing everything off, remove them
for (i in 1:nrow(dfNumeric)){
  for (j in 1:ncol(dfNumeric)){
    dfNumeric[i,j] <- gsub(",", "", dfNumeric[i,j])
  }
}

dfNumeric %>% map_if(is.character, as.numeric) %>% as_data_frame -> dfNumCl
str(dfNumCl)
colnames(dfNumCl)[colSums(is.na(dfNumCl)) == 0] #only g34cash and triagevarmissing have 0 NAs

dfNumZero <- dfNumCl #create new df replacing NA with 0
dfNumZero[is.na(dfNumZero)] <- 0
colnames(dfNumZero)[colSums(is.na(dfNumZero)) > 0] #converted correctly

###Now move to correlation matrix and removing variables w/ high correlaiton
corrMatr <- cor(dfNumZero)
corrplot(corrMatr, tl.pos = "n", type="lower")
#begin by removing variables highly correlated w/ g34cash
corrMatrDF <- as.data.frame(corrMatr)
rmColsMatr <- rownames(corrMatrDF)[corrMatrDF$G34Cash16 > 0.7]
rmColsMatr <- rmColsMatr[-1] #remove g34cash from the list

###remove columns w/ correlation > 70% and create new df called dfSlim, repeat.
dfSlim <- dfNumZero[ , !(names(dfNumZero) %in% rmColsMatr)] #removed 23 features, down to 50

corrMatrSlim <- cor(dfSlim)
corrplot(corrMatrSlim, tl.pos = "n", type="lower")
corrMatrSlimDF <- as.data.frame(corrMatrSlim)

rmColsMatrSlim <- rownames(corrMatrSlim)[corrMatrSlimDF$G34Invest16 > 0.7] #remove invest16
rmColsMatrSlim <- rownames(corrMatrSlim)[corrMatrSlimDF$TotAsts16 > 0.7]
rmColsMatrSlim <- rmColsMatrSlim[-1]
rmColsMatrSlim <- c(rmColsMatrSlim, "Invest16")

###remove columns w/ correlation > 70% and create new df called dfSlim, repeat.
dfSlim <- dfSlim[ , !(names(dfSlim) %in% rmColsMatrSlim)] #removed 9 variables, 41 remain

corrMatrSlim <- cor(dfSlim)
corrplot(corrMatrSlim, tl.pos = "n", type="lower")
corrMatrSlimDF <- as.data.frame(corrMatrSlim)

rmColsMatrSlim <- rownames(corrMatrSlim)[corrMatrSlimDF$Population16. > 0.7]
rmColsMatrSlim <- rmColsMatrSlim[-1]

###remove columns w/ correlation > 70% and create new df called dfSlim, repeat.
dfSlim <- dfSlim[ , !(names(dfSlim) %in% rmColsMatrSlim)] #removed 3 variables, 38 remain

corrMatrSlim <- cor(dfSlim)
corrplot(corrMatrSlim, tl.pos = "n", type="lower")
corrMatrSlimDF <- as.data.frame(corrMatrSlim)

rmColsMatrSlim <- rownames(corrMatrSlim)[corrMatrSlimDF$UnassignFdBal16. > 0.7]
rmColsMatrSlim <- rmColsMatrSlim[-1]

###remove columns w/ correlation > 70% and create new df called dfSlim, repeat.
dfSlim <- dfSlim[ , !(names(dfSlim) %in% rmColsMatrSlim)] #removed 2 variables, 36 remain

corrMatrSlim <- cor(dfSlim)
corrplot(corrMatrSlim, tl.pos = "n", type="lower")
corrMatrSlimDF <- as.data.frame(corrMatrSlim)

rmColsMatrSlim <- rownames(corrMatrSlim)[corrMatrSlimDF$TotFdBal.Exp16. > 0.7]
rmColsMatrSlim <- rmColsMatrSlim[-1]

###remove columns w/ correlation > 70% and create new df called dfSlim, repeat.
dfSlim <- dfSlim[ , !(names(dfSlim) %in% rmColsMatrSlim)] #removed 4 variables, 32 remain

corrMatrSlim <- cor(dfSlim)
corrplot(corrMatrSlim, tl.pos = "n", type="lower")
corrMatrSlimDF <- as.data.frame(corrMatrSlim)

rmColsMatrSlim <- rownames(corrMatrSlim)[corrMatrSlimDF$NetResult16 > 0.7] #remove NetResult16.
rmColsMatrSlim <- rownames(corrMatrSlim)[corrMatrSlimDF$HouseEBI16. > 0.7] #remove HouseEBIStAvg16. & PerCapEBI16.
rmColsMatrSlim <- rmColsMatrSlim[-1]
rmColsMatrSlim <- c(rmColsMatrSlim, "NetResult16.")


###remove columns w/ correlation > 70% and create new df called dfSlim, repeat.
dfSlim <- dfSlim[ , !(names(dfSlim) %in% rmColsMatrSlim)] #removed 3 variables, 29 remain

corrMatrSlim <- cor(dfSlim)
corrplot(corrMatrSlim, tl.pos = "n", type="lower")
corrMatrSlimDF <- as.data.frame(corrMatrSlim)

######dfSlim is ready now, these are the colulmns we want to use (29 total)
write.csv(dfSlim, "finalCols.csv", row.names = F)
featNames <- colnames(dfSlim)

dfDesc <- dfModel[c(1:4,82,83)]
colKeepDesc <- colnames(dfDesc)


dfSlim <- read.csv("finalCols.csv", stringsAsFactors = F)
colKeep <- colnames(dfSlim)

colAll <- c(colKeepDesc, colKeep)

dfFinal <- dfModel[ , (names(dfModel) %in% colAll)] 

for (i in 1:nrow(dfFinal)){
  for (j in 1:ncol(dfFinal)){
    dfFinal[i,j] <- gsub(",", "", dfFinal[i,j])
  }
}

dfFinal[is.na(dfFinal)] <- 0
colnames(dfFinal)[colSums(is.na(dfFinal)) > 0] #converted correctly

#now convert columns to numeric
dfFinal[5:33] %>% map_if(is.character, as.numeric) %>% as_data_frame -> dfNumCl
str(dfFinal)

dfFinalTest <- dfFinal
dfFinalTest[colKeep] <- sapply(dfFinalTest[colKeep], as.numeric)
str(dfFinalTest)
dfFinalTest[is.na(dfFinalTest)] <- 0
colnames(dfFinalTest)[colSums(is.na(dfFinalTest)) > 0] #converted correctly

write.csv(dfFinalTest, "dfModelClean.csv", row.names = F)



####Now we have a clean data frame to move forward with modeling
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df <- read.csv("dfModelClean.csv", stringsAsFactors = F)

gradeLabs <- c("AAA", "AA+", "AA","AA-","A+", "A","A-", "BBB+","BBB","BBB-","BB+",
              "BB","B")
Junk <- gradeLabs[11:13]
df$Grade <- ifelse(df$Grade %in% Junk == TRUE, "Junk", df$Grade)


df[29:35] <- lapply(df[29:35], factor)


###rank features by importance
set.seed(15)
library(mlbench)
library(caret)

dfModel <- df[c(34, 5:33)]
colnames(dfModel)[colSums(is.na(dfModel)) > 0]

###begin random forests
library(randomForest)
set.seed(15)

trainIndex <- createDataPartition(dfModel$Grade, p = 0.7, list = F, times = 1)
dfTrain <- dfModel[trainIndex,]
dfTest <- dfModel[-trainIndex,]

table(dfTrain$Grade)/nrow(dfTrain)
table(dfTest$Grade)/nrow(dfTest) #validate maintain ratios

colnames(dfTrain)[colSums(is.na(dfTrain)) > 0]

fit <- randomForest(Grade ~ ., data=dfTrain, importance=T, ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, dfTest)
dfTest$Result <- Prediction

dfExamine <- dfTest[c(1,31)]
#holy shit. it actually looks like it did surprisingly well. Not all of the values are right on
#but the misses are either right above or right below where they should be


###Now try for Grades as integers
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df <- read.csv("dfModelClean.csv", stringsAsFactors = F)

gradeLabs <- c("AAA", "AA+", "AA","AA-","A+", "A","A-", "BBB+","BBB","BBB-","BB+",
              "BB","B")
Junk <- gradeLabs[11:13]
df$Grade <- ifelse(df$Grade %in% Junk == TRUE, "Junk", df$Grade)


df[29:35] <- lapply(df[29:35], factor)

#more cleaning
library(mlbench)
library(caret)

dfModel <- df[c(34, 5:33)]
table(dfModel$Grade)
#convert Grades to integers
library(plyr)
gradeLabs
dfModel$iGrade <- revalue(dfModel$Grade, 
                           c("AAA"=10, "AA+"=9, "AA"=8, "AA-"=7,
                             "A+"=6, "A"=5, "A-"=4, "BBB+"=3,"BBB"=2,
                             "BBB-"=1,"Junk"=0))
dfModel$iGrade <- as.integer(as.character(dfModel$iGrade))
dfModel <- dfModel[c(31,2:30)]
str(dfModel$iGrade)

###begin random forest
library(ggplot2)
library(randomForest)
set.seed(15)

trainIndex <- createDataPartition(dfModel$iGrade, p = 0.7, list = F, times = 1)
dfTrain <- dfModel[trainIndex,]
dfTest <- dfModel[-trainIndex,]

table(dfTrain$iGrade)/nrow(dfTrain)
table(dfTest$iGrade)/nrow(dfTest) #validate maintain ratios

colnames(dfTrain)[colSums(is.na(dfTrain)) > 0]

fit <- randomForest(iGrade ~ ., data=dfTrain, importance=T, ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, dfTest)
dfTest$Result <- Prediction

dfExamine <- dfTest[c(1,31)]
dfExamine$Residual <- abs(dfExamine$iGrade - dfExamine$Result)
summary(dfExamine$Residual)
qplot(y=dfExamine$Residual, x = "Residual", geom = "boxplot")
sum(dfExamine$Residual > 2) #321/1445 with a residual over 1
                            #69 with a residual over 2
#upon further examination, it looks like the lower values were harder to predict, 
#possibly because there were so few examples to train from
###Next, only use Grades for which we have observations > 100?
###remove < BBB+ and try integer:
library(sqldf)
dfModelA <- sqldf("SELECT * FROM dfModel WHERE iGrade NOT IN (0,1,2,3)")

#begin randomforest
library(randomForest)
set.seed(15)

table(dfModelA$iGrade)
dfModelA$iGrade <- as.numeric(revalue(as.character(dfModelA$iGrade), 
                           c("10"=7, "9"=6, "8"=5, "7"=4,
                             "6"=3, "5"=2, "4"=1)))

trainIndex <- createDataPartition(dfModelA$iGrade, p = 0.7, list = F, times = 1)
dfTrain <- dfModelA[trainIndex,]
dfTest <- dfModelA[-trainIndex,]

table(dfTrain$iGrade)/nrow(dfTrain)
table(dfTest$iGrade)/nrow(dfTest) #validate maintain ratios

colnames(dfTrain)[colSums(is.na(dfTrain)) > 0]

fit <- randomForest(iGrade ~ ., data=dfTrain, importance=T, ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, dfTest)
dfTest$Result <- Prediction

dfExamine <- dfTest[c(1,31)]
dfExamine$Residual <- abs(dfExamine$iGrade - dfExamine$Result)
summary(dfExamine$Residual)
qplot(y=dfExamine$Residual, x = "Residual", geom = "boxplot")
sum(dfExamine$Residual > 1) #351 > 1 residual, 57 > 2
#about the same as before. some of the miscalculations are all over the place, 
#probably missing certain features important to determining Grade

###also try grouping the AAA, AA, A, etc.
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df <- read.csv("dfModelClean.csv", stringsAsFactors = F)

gradeLabs <- c("AAA", "AA+", "AA","AA-","A+", "A","A-", "BBB+","BBB","BBB-","BB+",
              "BB","B")
Junk <- gradeLabs[11:13]
df$Grade <- ifelse(df$Grade %in% Junk == TRUE, "Junk", df$Grade)
table(df$Grade)
BBB <- gradeLabs[8:10]
df$Grade <- ifelse(df$Grade %in% BBB == TRUE, "BBB", df$Grade)
table(df$Grade)
A <- gradeLabs[5:7]
df$Grade <- ifelse(df$Grade %in% A == TRUE, "A", df$Grade)
table(df$Grade)
AA <- gradeLabs[2:4]
df$Grade <- ifelse(df$Grade %in% AA == TRUE, "AA", df$Grade)
table(df$Grade) #conveting Junk to BBB, too few to have alone
df$Grade <- ifelse(df$Grade == "Junk", "BBB", df$Grade)
#now only 4 classes, save this file jic
write.csv(df, "dfModel4Class", row.names = F)

###begin grouped modeling
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df <- read.csv("dfModel4Class.csv", stringsAsFactors = F)

df[29:35] <- lapply(df[29:35], factor)

#more cleaning
library(mlbench)
library(caret)

dfModel <- df[c(34, 5:33)]
table(dfModel$Grade)
#convert Grades to integers
library(plyr)
dfModel$iGrade <- revalue(dfModel$Grade, 
                           c("AAA"=3, "AA"=2, "A"=1, "BBB"=0))
dfModel$iGrade <- as.integer(as.character(dfModel$iGrade))
dfModel <- dfModel[c(31,2:30)]
str(dfModel$iGrade)

###begin random forest
library(ggplot2)
library(randomForest)
set.seed(15)

trainIndex <- createDataPartition(dfModel$iGrade, p = 0.7, list = F, times = 1)
dfTrain <- dfModel[trainIndex,]
dfTest <- dfModel[-trainIndex,]

table(dfTrain$iGrade)/nrow(dfTrain)
table(dfTest$iGrade)/nrow(dfTest) #validate maintain ratios

colnames(dfTrain)[colSums(is.na(dfTrain)) > 0]

fit <- randomForest(iGrade ~ ., data=dfTrain, importance=T, ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, dfTest)
dfTest$Result <- Prediction

dfExamine <- dfTest[c(1,31)]
dfExamine$Residual <- abs(dfExamine$iGrade - dfExamine$Result)
summary(dfExamine$Residual)
qplot(y=dfExamine$Residual, x = "Residual", geom = "boxplot")
sum(dfExamine$Residual > 1) #only 17 are off by more than 1


###now try w/o integers
#begin grouped modeling
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df <- read.csv("dfModel4Class.csv", stringsAsFactors = F)

df[29:35] <- lapply(df[29:35], factor)

#more cleaning
library(mlbench)
library(caret)

dfModel <- df[c(34, 5:33)]
table(dfModel$Grade)

###begin random forest
library(ggplot2)
library(randomForest)
library(sqldf)
set.seed(15)

trainIndex <- createDataPartition(dfModel$Grade, p = 0.7, list = F, times = 1)
dfTrain <- dfModel[trainIndex,]
dfTest <- dfModel[-trainIndex,]

table(dfTrain$Grade)/nrow(dfTrain)
table(dfTest$Grade)/nrow(dfTest) #validate maintain ratios

colnames(dfTrain)[colSums(is.na(dfTrain)) > 0]

fit <- randomForest(Grade ~ ., data=dfTrain, importance=T, ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, dfTest)
dfTest$Result <- Prediction

dfExamine <- dfTest[c(1,31)]
dfExamine$Accuracy <- ifelse(dfExamine$Grade == dfExamine$Result, 1,0)
table(dfExamine$Accuracy)
print(paste0("Accuracy: ", sum(dfExamine$Accuracy), "/", nrow(dfExamine), " = ",
            round(sum(dfExamine$Accuracy)/nrow(dfExamine) * 100, digits = 3), "%"))
#76.51% Accuracy
#zoom in on incorrect predictions
table(dfExamine)
table(dfExamine$Grade)
#Accuracies per Grade:
print(paste0("AAA: ",sum(dfExamine$Accuracy[dfExamine$Grade=="AAA"]), 
             "/",length(which(dfExamine$Grade == "AAA")), " = ", 
             round(sum(dfExamine$Accuracy[dfExamine$Grade=="AAA"])/
               length(which(dfExamine$Grade == "AAA")) * 100, digits = 3), "%"))

print(paste0("AA: ",sum(dfExamine$Accuracy[dfExamine$Grade=="AA"]), 
             "/",length(which(dfExamine$Grade == "AA")), " = ", 
             round(sum(dfExamine$Accuracy[dfExamine$Grade=="AA"])/
                     length(which(dfExamine$Grade == "AA")) * 100, digits = 3), "%"))

print(paste0("A: ",sum(dfExamine$Accuracy[dfExamine$Grade=="A"]), 
             "/",length(which(dfExamine$Grade == "A")), " = ", 
             round(sum(dfExamine$Accuracy[dfExamine$Grade=="A"])/
                     length(which(dfExamine$Grade == "A")) * 100, digits = 3), "%"))

print(paste0("BBB: ",sum(dfExamine$Accuracy[dfExamine$Grade=="BBB"]), 
             "/",length(which(dfExamine$Grade == "BBB")), " = ", 
             round(sum(dfExamine$Accuracy[dfExamine$Grade=="BBB"])/
                     length(which(dfExamine$Grade == "BBB")) * 100, digits = 3), "%"))

#AAA: 3/25 = 12%
#AA: 320/504 = 63.492%
#A: 781/898 = 86.971%
#BBB: 0/16 = 0%
#We can see that the lower frequency Grades are harder to predict, would be better with more data

##Random Forests w/o Slim 
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df <- read.csv("dfGOSchCleanRows.csv", stringsAsFactors = F, na.strings = c("","NA"), strip.white = T)

dfModel <- df[c(1,12,5,9,19:159)] #eliminated some of the intro descriptive columns
rmCols <- colnames(dfModel)[colSums(is.na(dfModel)) > 1000] #now look for columns with large amounts of NA (start with 1k, easier decisions)
dfModel <- dfModel[ , !(names(dfModel) %in% rmCols)] 

#remove columns features for modeling (keeping descriptive columns to relate back to)
dfModel <- dfModel[,-c(80,81)]

#commas are throwing everything off, remove them
for (i in 1:nrow(dfModel)){
  for (j in 1:ncol(dfModel)){
    dfModel[i,j] <- gsub(",", "", dfModel[i,j])
  }
}

#subset numeric columns, convert them, then combine them back in
library(purrr)
library(dplyr)
dfNumeric <- dfModel[,c(1,5:40,42:46,48:79)]

dfNumeric %>% map_if(is.character, as.numeric) %>% as_data_frame -> dfNumCl
str(dfNumCl)
colnames(dfNumCl)[colSums(is.na(dfNumCl)) == 0] #only g34cash and triagevarmissing have 0 NAs

#Handle NAs
dfNumZero <- dfNumCl #create new df replacing NA with 0
dfNumZero[is.na(dfNumZero)] <- 0
colnames(dfNumZero)[colSums(is.na(dfNumZero)) > 0] #converted correctly

#now consolidate data frame
dfFresh <- dfModel[,c(1:4,41,47,80)]
dfWhole <- merge(dfFresh, dfNumZero, by = "Security.ID") #lost some values w/o a sec id

#handle NA for strings
df$PensionDataStatus16.[is.na(df$PensionDataStatus16.)] <- "Blank"
df$OPEBDataStatus16.[is.na(df$OPEBDataStatus16.)] <- "Blank"
colnames(df)[colSums(is.na(df)) > 0] #converted correctly (only org id)

write.csv(dfWhole, "dfWhole.csv", row.names = F)

#begin modeling:
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df <- read.csv("dfWhole.csv", stringsAsFactors = F)

#Convert string features to factor
df$PensionDataStatus16. <- as.factor(df$PensionDataStatus16.)
df$OPEBDataStatus16. <- as.factor(df$OPEBDataStatus16.)
df$Grade <- as.factor(df$Grade)

df[c(77:80)] <- lapply(df[77:80], factor)
sapply(df[c(77:80)], class)

#begin model dataframe
dfModel <- df[,c(7:80, 5, 6)]

table(dfModel$Grade)
gradeLabs <- c("AAA", "AA+", "AA","AA-","A+", "A","A-", "BBB+","BBB","BBB-","BB+",
              "BB","B")
Junk <- gradeLabs[8:13]
dfModel$Grade <- ifelse(dfModel$Grade %in% Junk == TRUE, "Junk", dfModel$Grade)
table(dfModel$Grade) #only grouping all of the BBB+ and lower, later will group more if need

#Begin Random Forest
library(ggplot2)
library(randomForest)
library(caret)
library(sqldf)
set.seed(15)

trainIndex <- createDataPartition(dfModel$Grade, p = 0.7, list = F, times = 1)
dfTrain <- dfModel[trainIndex,]
dfTest <- dfModel[-trainIndex,]

table(dfTrain$Grade)/nrow(dfTrain)
table(dfTest$Grade)/nrow(dfTest) #validate maintain ratios

colnames(dfTrain)[colSums(is.na(dfTrain)) > 0]

fit <- randomForest(Grade ~ ., data=dfTrain, importance=T, ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, dfTest)
dfTest$Result <- Prediction

dfExamine <- dfTest[c(1,77)]
dfExamine$Accuracy <- ifelse(dfExamine$Grade == dfExamine$Result, 1,0)
table(dfExamine$Accuracy)
print(paste0("Accuracy: ", sum(dfExamine$Accuracy), "/", nrow(dfExamine), " = ",
             round(sum(dfExamine$Accuracy)/nrow(dfExamine) * 100, digits = 3), "%"))
#51.76% Accuracy

table(dfExamine) ###very interesting to note that the misses seem to be very close

###Try adding Outlook code and Creditwatch
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
df <- read.csv("dfWhole.csv", stringsAsFactors = F)
dfAddl <- read.csv("GoSchAddl.csv")
colnames(dfAddl)[1] <- "Security.ID"

dfTot <- merge(x=df, y=dfAddl, by.x="Security.ID", by.y="Security.ID", all.x=T )
str(dfTot$Grade_TYPE_CODE)
str(dfTot$OUTLOOK_CODE)
sum(is.na(dfTot$Grade_TYPE_CODE)) #394 w/o Grade type code or outlook code 
#replace NA w/ "None"
colnames(dfTot)[colSums(is.na(dfTot)) > 1]
#first convert to string:
dfTot$Grade_TYPE_CODE <- as.character(dfTot$Grade_TYPE_CODE)
dfTot$OUTLOOK_CODE <- as.character(dfTot$OUTLOOK_CODE)
dfTot[is.na(dfTot)] <- "None"

#convert back to factor
dfTot$Grade_TYPE_CODE <- as.factor(dfTot$Grade_TYPE_CODE)
dfTot$OUTLOOK_CODE <- as.factor(dfTot$OUTLOOK_CODE)

write.csv(dfTot, "dfAddlCols.csv", row.names = F)

##Now move to RF
setwd("C:/Users/User/Documents/Comp/Models/Entity/Data")
dfTot <- read.csv("dfAddlCols.csv", stringsAsFactors = F)
##convert relevant strings to factors
str(dfTot[4:7])
dfTot[4:7] <- lapply(dfTot[4:7], factor)
str(dfTot[76:80])
dfTot[76:80] <- lapply(dfTot[76:80], factor)
str(dfTot)


###Now try for Grades as integers
gradeLabs <- c("AAA", "AA+", "AA","AA-","A+", "A","A-", "BBB+","BBB","BBB-","BB+",
              "BB","B")
Junk <- gradeLabs[11:13]
str(dfTot$Grade)
dfTot$Grade <- as.character(dfTot$Grade)
dfTot$Grade <- ifelse(dfTot$Grade %in% Junk == TRUE, "Junk", dfTot$Grade)


#convert Grades to integers
dfModel <- dfTot[,c(1,7,4:6,8:82)]
library(plyr)
gradeLabs <- c("AAA", "AA+", "AA","AA-","A+", "A","A-", "BBB+","BBB","BBB-","Junk")
dfModel$iGrade <- revalue(dfModel$Grade, 
                           c("AAA"=10, "AA+"=9, "AA"=8, "AA-"=7,
                             "A+"=6, "A"=5, "A-"=4, "BBB+"=3,"BBB"=2,
                             "BBB-"=1,"Junk"=0))
dfModel$iGrade <- as.integer(as.character(dfModel$iGrade))
dfModel <- dfModel[c(81,3:80)]
str(dfModel$iGrade)

###begin random forest
library(ggplot2)
library(randomForest)
library(caret)
set.seed(15)

trainIndex <- createDataPartition(dfModel$iGrade, p = 0.7, list = F, times = 1)
dfTrain <- dfModel[trainIndex,]
dfTest <- dfModel[-trainIndex,]

table(dfTrain$iGrade)/nrow(dfTrain)
table(dfTest$iGrade)/nrow(dfTest) #validate maintain ratios

colnames(dfTrain)[colSums(is.na(dfTrain)) > 0]

fit <- randomForest(iGrade ~ ., data=dfTrain, importance=T, ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, dfTest)
dfTest$Result <- Prediction

dfExamine <- dfTest[c(1,80)]
dfExamine$Residual <- (dfExamine$iGrade - dfExamine$Result)
dfExamine$AbsResid <- abs(dfExamine$iGrade - dfExamine$Result)
summary(dfExamine$Residual)
summary(dfExamine$AbsResid)
qplot(y=dfExamine$Residual, x = "Residual", geom = "boxplot")
sum(dfExamine$AbsResid > 1) #80.5% within 1

##Entity2 has 10k observations, maybe look into including these
##Also looks like they use most of the same variables
##maybe run the model on this new set and then bring in the Grades for Muni Cnty and evaluate performance that way


###now try PCA w/ RandomForests, then try NN
