# loading the train data
train<-read.csv(file.choose())

# loading the test data
test<-read.csv(file.choose())

#Combining the train and test data
test$loss<-"None"
combi<-rbind(train,test)
combi$loss<-as.numeric(combi$loss)

sum(is.na(combi))

str(combi)

str (combi, list.len = ncol(combi))
# it will now show the entire columns
names(combi)

#Removing the id column
combi<-combi[,-1]

#treating the  continuous variables first
combin<-combi[,117:130]
str(combin)

# finding the correlation on numeric data
corn = cor(combin, method = c('pearson'))
corn = as.data.frame(as.table(corn))
corn<-corn[(which(corn$Freq>0.8)),]

#?hetcor
#corr<-hetcor(combi[,-131], ML = FALSE, std.err = TRUE, use=c("complete.obs", "pairwise.complete.obs"), bins=4, pd=TRUE)

# removing the variables with correlation > 80%
names(combin)
combin<-combin[,-c(9,10,12,13)]

#?prcomp
# running the prcomp on numerical variables
pcanum<-prcomp(combin, scale. = TRUE, center = TRUE)
summary(pcanum)

# comparing the eigen values >1
ign<-pcanum$sdev^2

# so, the eigen values are
#[1] 3.3672922 1.7871456 1.4962727 0.9932480 0.8356763 0.5156215 0.4162672 0.3041772 0.1744456 0.1098537

#                         PC1    PC2    PC3     PC4     PC5     PC6     PC7     PC8     PC9    PC10
#Standard deviation     1.8350 1.3368 1.2232 0.99662 0.91415 0.71807 0.64519 0.55152 0.41767 0.33144
#Proportion of Variance 0.3367 0.1787 0.1496 0.09932 0.08357 0.05156 0.04163 0.03042 0.01744 0.01099
#Cumulative Proportion  0.3367 0.5154 0.6651 0.76440 0.84796 0.89953 0.94115 0.97157 0.98901 1.00000

# considering the fact that eigen values should be >=1 and we should justify the variance, PC1 to PC6 will consider
str(combin)
combin<-combin[,1:6]

str(combin, list.len=ncol(combin))

#treating the categorical variables now
combif1<-combi[,1:88] # factor variables upto 4
combif2<-combi[,89:116] # factor variables > 4
str(combif1)

# converting combif1 into numerical variables
combif1<-sapply(combif1, as.numeric)
unique(combif1[,88])

# running the prcomp on numerical variables
pcacombif1<-prcomp(combif1, scale. = FALSE, center = FALSE)

pcacombif1$sdev
summary(pcacombif1)


# comparing the eigen values >1
ign2<-pcacombif1$sdev^2
which(ign2>1)
combif1final<-combif1[,c(1:2)]

# converting combif1 into numerical variables
combif2<-sapply(combif2, as.numeric)

names(combif2)
unique(combif2[,28])
pcacombif2<-prcomp(combif2, scale. = FALSE, center = FALSE)
summary(pcacombif2)

# comparing the eigen values >1
ign3<-pcacombif2$sdev^2
which(ign3>1)
combif2final<-combif2[,c(1:18)]

# combining al the data
finald2<-cbind(combif1final,combif2final)
finald1<-cbind(finald2,combin)

# linking the loss variable
finald1<-cbind(finald1,combi[131])

# splitting the data into train and test

finaltrain<-finald1[1:188318,]
finaltrain$loss<-as.numeric(finaltrain$loss)
finaltest<-finald1[188319:313864,]

str(finaltrain)
library(randomForest)


# RF model on train data
rftrain <- randomForest(finaltrain[,-27], finaltrain[,27], importance = TRUE, ntree = 500)
?randomForest

#Predicting the missing values on test file
pred<-predict(rftrain,finaltest)


submit<-data.frame(id = test[,1], loss = finalPred)

# writing the data to Submit file
write.csv(submit, file = "Submit.csv",row.names=FALSE)


############ or else as an alternative
###### MCA from the FactoMineR library could be used for categorical variable, rest will remain same

library(FactoMineR)

combi99<-read.table("combi.csv", header = TRUE, sep = ",", row.names = 1)

?MFA()
mfafct<- MFA(combi[,1:116],group = c(72, 4, 12, 3, 3, 1, 4, 1, 1, 1, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1),
             type = rep("n",length(group)),
             graph = FALSE)

M1<-MFA(combi, group = c(72, 4, 12), type = rep("n", "n", "n"), graph = FALSE)
################################################################################
