# Data Input


library(ElemStatLearn)
data(zip.test)
data(zip.train)

summary(zip.test)
summary(zip.train)

zip.train <- as.data.frame(zip.train)

test.0 <- zip.test[which(zip.test[,1] == 0 ),]
train.0 <- zip.train[which(zip.train[,1] == 0 ),]

test.1 <- zip.test[which(zip.test[,1] == 1 ),]
train.1 <- zip.train[which(zip.train[,1] == 1 ),]

test.2 <- zip.test[which(zip.test[,1] == 2 ),]
train.2 <- zip.train[which(zip.train[,1] == 2 ),]

test.3 <- zip.test[which(zip.test[,1] == 3 ),]
train.3 <- zip.train[which(zip.train[,1] == 3 ),]

test.4 <- zip.test[which(zip.test[,1] == 4 ),]
train.4 <- zip.train[which(zip.train[,1] == 4 ),]

test.5 <- zip.test[which(zip.test[,1] == 5 ),]
train.5 <- zip.train[which(zip.train[,1] == 5 ),]

test.6 <- zip.test[which(zip.test[,1] == 6 ),]
train.6 <- zip.train[which(zip.train[,1] == 6 ),]

test.7 <- zip.test[which(zip.test[,1] == 7 ),]
train.7 <- zip.train[which(zip.train[,1] == 7 ),]

test.8 <- zip.test[which(zip.test[,1] == 8 ),]
train.8 <- zip.train[which(zip.train[,1] == 8 ),]

test.9 <- zip.test[which(zip.test[,1] == 9 ),]
train.9 <- zip.train[which(zip.train[,1] == 9 ),]

####################################### LR

res <- zip.train[,1]
pred <- zip.train[,-1]
B = as.vector(solve(t(pred)%*%pred)%*%t(pred)%*%res)

predicted.0 = round(test.0[,-1]%*%B)
predicted.1 = round(test.1[,-1]%*%B)
predicted.2 = round(test.2[,-1]%*%B)
predicted.3 = round(test.3[,-1]%*%B)
predicted.4 = round(test.4[,-1]%*%B)
predicted.5 = round(test.5[,-1]%*%B)
predicted.6 = round(test.6[,-1]%*%B)
predicted.7 = round(test.7[,-1]%*%B)
predicted.8 = round(test.8[,-1]%*%B)
predicted.9 = round(test.9[,-1]%*%B)

acc.0 <- length(which(predicted.0 < 1))/nrow(test.0)
acc.1 <- length(which(predicted.0 == 1))/nrow(test.1)
acc.2 <- length(which(predicted.2 == 2))/nrow(test.2)
acc.3 <- length(which(predicted.3 == 3))/nrow(test.3)
acc.4 <- length(which(predicted.4 == 4))/nrow(test.4)
acc.5 <- length(which(predicted.5 == 5))/nrow(test.5)
acc.6 <- length(which(predicted.6 == 6))/nrow(test.6)
acc.7 <- length(which(predicted.7 == 7))/nrow(test.7)
acc.8 <- length(which(predicted.8 == 8))/nrow(test.8)
acc.9 <- length(which(predicted.9 >= 9))/nrow(test.9)


###################################### KNN
k = 3
labels = 0;
temp.KNN <- test.9
for (i in 1:length(temp.KNN[,1])){
  
  x = temp.KNN[i,-1]
  xx = matrix(rep(x,each=nrow(zip.train)),nrow=nrow(zip.train))
  
  D = rowSums((xx-zip.train[,-1])^2)
  
  S = sort(D, index.return=1)
  
  N = S$ix[1:k]
  neigh_labels = zip.train[N,1]
  labels[i] = round(mean(neigh_labels))
}
lab.knn <- labels
nums.knn <- which(labels == 9)
length(nums.knn)/nrow(temp.KNN)
lab.knn[nums.knn] <- 1
lab.knn[-nums.knn] <- 0

###################################### Logistic Regression
library(nnet)
zip_train_dframe = data.frame(zip.train)
xnam <- paste("X", 2:257, sep="")
(fmla <- as.formula(paste("X1 ~ ", paste(xnam, collapse= "+"))))
mod <- multinom(fmla, zip_train_dframe, MaxNWts = 3000)

temp.LoR <- test.0
nd <- data.frame(temp.LoR)
p <- predict(mod, type="probs", newdata=nd)
lab.LoR <- 0
num_test_observations = length(temp.LoR[,1])
count = 0
for (i in 1:num_test_observations){
  temp.LoR[i,1] == which.max(t(p[i,]))-1
  if (temp.LoR[i,1] == which.max(t(p[i,]))-1){
  count = count + 1
  lab.LoR[i] <- 1
  }else{
    lab.LoR[i] <- 0
  }
}
accuracy = count/num_test_observations
print(accuracy)



###################################### LDA
library(MASS)
lda.zip <- qda(V1 ~ ., data = as.data.frame(zip.train))

temp.LDA <- as.data.frame(test.0)
temp.LDA <- temp.LDA[,-1]
lda.pred <- predict(lda.zip, temp.LDA)
length(which(lda.pred$class == temp.LDA[1,1]))/nrow(temp.LDA)

lab.lda <- as.numeric(lda.pred$class)
nums.lda <- which(lda.pred$class == 0)
lab.lda[nums.lda] <- 1
lab.lda[-nums.lda] <- 0

#################################### Voting
votdat <- cbind(lab.LoR,lab.knn,lab.lda)
vots <- rowSums(votdat)
length(which(vots > 0))/length(vots)


###################################### ANN
library("neuralnet")
temp <- as.data.frame(zip.train)
colnames(temp) <- sprintf("Var%d", 1:257)
n <- colnames(temp)
f <- as.formula(paste("Var1 ~", paste(n[!n %in% "Var1"], collapse = " + ")))
ann.zip <- neuralnet(f ,temp, hidden=c(100,50,25,10),likelihood=TRUE)

temp.ann <- test.5
res.ann <- round(compute(ann.zip, temp.ann[,-1])$net.result)
length(which(res.ann == 5))/nrow(temp.ann)