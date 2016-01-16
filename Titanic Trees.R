library(reshape)

dftable <- as.data.frame(Titanic)
df <- untable(dftable,dftable$Freq)
df <- within(df,rm(Freq))

#Create indicator variable for GBM
df$SurInd <- df$Survived
df <- within(df, levels(SurInd)[levels(SurInd) == "Yes"] <- 1)
df <- within(df, levels(SurInd)[levels(SurInd) == "No"] <- 0)

attach(df)

###                             CART model                      ###
library(rpart)

#Fit the CART model
CARTfit <- rpart(Survived ~ Class + Sex + Age, data = df,
        control = rpart.control(minsplit = 10, xval = 10))
#Plot some output
par(lwd=1, font=1)
plotcp(CARTfit)
barplot(CARTfit$variable.importance/max(CARTfit$variable.importance)*100, axisnames = TRUE, horiz = TRUE, xlab = "Variable", ylab = "Relative variable importance", main = "Variable importance", sub = "Titanic survival CART model", col = "Blue")
plot(CARTfit)
#Prune the tree using complexity cost pruning
CARTpfit<- prune(CARTfit, cp=CARTfit$cptable[which.min(CARTfit$cptable[,"xerror"]),"CP"])

#Plot some output of pruned model
par(lwd=1, font=1)
plotcp(CARTpfit)
barplot(CARTfit$variable.importance/max(CARTfit$variable.importance)*100, axisnames = TRUE, horiz = TRUE, xlab = "Variable", ylab = "Relative variable importance", main = "Variable importance", sub = "Titanic survival CART model", col = "Blue")

library(rattle)
par(font = 4)
fancyRpartPlot(CARTpfit, sub = "Titanic survival CART model")

set.seed(2)
###                             Bagged CART model                      ###
library(randomForest)

#Fit the bagged tree m=number of variables
baggedfit <- randomForest(Survived ~ Class + Sex + Age, data = df,
                          mtry=3, ntree = 100)
par(lwd=1, font=1)
barplot(apply(importance(baggedfit),1,as.numeric)/max(apply(importance(baggedfit),1,as.numeric))*100, axisnames = TRUE, horiz = TRUE, ylab = "Variable", xlab = "Relative variable importance", main = "Variable importance", sub = "Titanic survival bagged CART model", col = "Blue")

###                             Random Forest CART model                      ###
#Fit the bagged tree m=sqrt(p) of variables
rffit <- randomForest(Survived ~ Class + Sex + Age, data = df,
                      mtry=2, ntree = 100)
par(lwd=1, font=1)
barplot(apply(varImpPlot(rffit),1,as.numeric)/max(apply(varImpPlot(rffit),1,as.numeric))*100, axisnames = TRUE, horiz = TRUE, ylab = "Variable", xlab = "Relative variable importance", main = "Variable importance", sub = "Titanic survival Random Forest model", col = "Blue")

#Compare the OOB error of Bagging vs. Random forests
par(lwd=4, font=4)
plot(baggedfit$err.rate[,1],type = "l", col="red",xlim = c(0,100), ylim = c(0.208,0.232), ylab = "OOB Error", xlab = "Number of trees")
par(new=T)
plot(rffit$err.rate[,1],,type = "l", col="blue",xlim = c(0,100), ylim = c(0.208,0.232), ylab="", xlab="")
legend('topright', c("Bagged CART", "Random Forest"),lty=1, col=c('red', 'blue'), bty='n', cex=.75)
par(new=F)

table(df, df$Survived)
getwd()
setwd("d:/code/")
write.csv(df,"Titanic.csv")


                   