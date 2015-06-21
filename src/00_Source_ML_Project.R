#  install.packages ("caret", dependencies=TRUE)
#  install.packages ("randomForest")
#  install.packages ("e1071", dependencies=TRUE)
#  install.packages("pRF", dependencies=TRUE)

library(caret)

library(randomForest)

library(e1071)

library(pRF)

options(stringsAsFactors = FALSE)

# Training filename and dataframe contains training data
trainingFn <- "./data/pml-training.csv"
trainingDf<- data.frame(read.csv(trainingFn,na.strings=c("NA",""),header=TRUE))

# Testing filenmae and dataframe contains testing data
testingFn <- "./data/pml-testing.csv"
testingDf<- data.frame(read.csv(testingFn,header=TRUE))


trCleanDf.wk   <- trainingDf[, colMeans(is.na(trainingDf))   < 0.96]
colnames(trainingDf)[1:7]

## trainingDf contains all columns with numeric and classes
trCleanDf.wk<- trCleanDf.wk[,8:dim(trCleanDf.wk)[2]]


tsCleanDf.wk   <- testingDf[, colMeans(is.na(testingDf))   < 0.96]
## numeric data from column 8 to the next to last column
tsCleanDf.wk <- tsCleanDf.wk[,8:dim(tsCleanDf.wk)[2]-1]


## Build model
set.seed(20150619)

classe.rf.model <- randomForest (as.factor(trCleanDf.wk$classe) ~ .
                                 , trCleanDf.wk
                                 , ntree=10
                                 , importance= TRUE
                                 , norm.votes=FALSE)
# prediction
pred <- predict (classe.rf.model, tsCleanDf.wk );

print(pred)

par(mar=c(1,1,1,1))
plot(classe.rf.model
     ,log="y"
     , main="Fitting training data with Random forest (10 trees)")
     )

## Add legend
legend("top", cex =0.5, legend=colnames(classe.rf.model$err.rate)
       , lty=c(1,2,3,4,5,6), col=c(1,2,3,4,5,6), horiz=T)
## end add legend

## ---------------------------------------------------
## Increase the number of trees to test the OOB
## ---------------------------------------------------
classe.rf.model <- randomForest (as.factor(trCleanDf.wk$classe) ~ .
                                 , trCleanDf.wk
                                 , ntree=1000
                                 , importance= TRUE
                                 , norm.votes=FALSE)
par(mar=c(1,1,1,1))
plot(classe.rf.model
     ,log="y"
     , main="Fitting training data with Random forest (1000 trees)")
legend("top", cex =0.5, legend=colnames(classe.rf.model$err.rate)
       , lty=c(1,2,3,4,5,6), col=c(1,2,3,4,5,6), horiz=T)


classe.rf.importance <- importance(classe.rf.model);
classe.rf.impvar  <- rownames(classe.rf.importance)[order(classe.rf.importance[, 1], decreasing=TRUE)]



## variable importance
## Class A
classe.rf.model <- randomForest (as.factor(trCleanDf.wk$classe) ~ .
                                 , trCleanDf.wk
                                 , ntree= 900
                                 , importance=TRUE
                                 , proximity=TRUE
                                 , norm.votes=FALSE
)

## Class A
classA <- data.frame(importance(classe.rf.model, type=1, class="A"))
t1 <- data.frame(varName=rownames(classA), meanDecreaseAccuracy=round(classA[,"A"],1))
t2 <- t1[order(-t1$meanDecreaseAccuracy, t1$varName),]
sortData <- transform(t1, varName=reorder(varName, -meanDecreaseAccuracy) ) 
pA<- ggplot(sortData, aes( as.factor(varName), meanDecreaseAccuracy))+
  ggtitle("Class A: Variable importance that decreases Accuracy")+
  geom_point(colour="red2", shape=17) +
  coord_flip() 



classB <- data.frame(importance(classe.rf.model, type=1, class="B"))
t1 <- data.frame(varName=rownames(classB), meanDecreaseAccuracy=round(classB[,"B"],1))
t2 <- t1[order(-t1$meanDecreaseAccuracy, t1$varName),]
sortData <- transform(t1, varName=reorder(varName, -meanDecreaseAccuracy) ) 
pB<- ggplot(sortData, aes( as.factor(varName), meanDecreaseAccuracy))+
  ggtitle("Class B: Variable importance that decreases Accuracy")+
  geom_point(colour="lawngreen", shape=17) +
  coord_flip()


classC <- data.frame(importance(classe.rf.model, type=1, class="C"))
t1 <- data.frame(varName=rownames(classC), meanDecreaseAccuracy=round(classC[,"C"],1))
t2 <- t1[order(-t1$meanDecreaseAccuracy, t1$varName),]
sortData <- transform(t1, varName=reorder(varName, -meanDecreaseAccuracy) ) 
pC<- ggplot(sortData, aes( as.factor(varName), meanDecreaseAccuracy))+
  ggtitle("Class C: Variable importance that decreases Accuracy")+
  geom_point(colour="royalblue3", shape=17) +
  coord_flip()


classD <- data.frame(importance(classe.rf.model, type=1, class="D"))
t1 <- data.frame(varName=rownames(classD), meanDecreaseAccuracy=round(classD[,"D"],1))
t2 <- t1[order(-t1$meanDecreaseAccuracy, t1$varName),]
sortData <- transform(t1, varName=reorder(varName, -meanDecreaseAccuracy) ) 
pD<- ggplot(sortData, aes( as.factor(varName), meanDecreaseAccuracy))+
  ggtitle("Class D: Variable importance that decreases Accuracy")+
  geom_point(colour="cyan2", shape=17) +
  coord_flip()

classE <- data.frame(importance(classe.rf.model, type=1, class="E"))
t1 <- data.frame(varName=rownames(classE), meanDecreaseAccuracy=round(classE[,"E"],1))
t2 <- t1[order(-t1$meanDecreaseAccuracy, t1$varName),]
sortData <- transform(t1, varName=reorder(varName, -meanDecreaseAccuracy) ) 
pE<- ggplot(sortData, aes( as.factor(varName), meanDecreaseAccuracy))+
  ggtitle("Class E: Variable importance that decreases Accuracy")+
  geom_point(colour="hotpink2", shape=17) +
  coord_flip()


pA

pB

pC

pD

pE

varImp <- data.frame(importance(classe.rf.model, type=2))
t1 <- data.frame(varName=rownames(varImp), meanDecreaseGini=round(varImp[,"MeanDecreaseGini"],1))
t2 <- t1[order(-t1$mmeanDecreaseGini, t1$varName),]
sortData <- transform(t1, varName=reorder(varName, -meanDecreaseGini) ) 
pI<- ggplot(sortData, aes( as.factor(varName), meanDecreaseGini))+
  ggtitle("Variable importance that decreases purity")+
  geom_point(colour="purple2", shape=18) +
  coord_flip()


## MDSplot
### This does not come back
MDSplot(classe.rf.model,as.factor(trCleanDf.wk$classe), k=2)







