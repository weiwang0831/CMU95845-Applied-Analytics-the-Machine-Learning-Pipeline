#set environment
loadlibs = function(libs) {
  for(lib in libs) {
    class(lib)
    if(!do.call(require,as.list(lib))) {install.packages(lib)}
    do.call(require,as.list(lib))
  }
}
libs = c("dplyr","mice","randomForest","adabag","gbm","pROC","gsubfn","chron")
loadlibs(libs)

#read dataset
stroke=read.csv("IST_corrected.csv",header = TRUE)
aggregate( . ~ RASP3, data=stroke[c("AGE","RCONSC","RSBP","RASP3")], FUN=range)
aggregate( . ~ RASP3, data=stroke[c("AGE","RSBP","RASP3")], FUN=mean)
stroke %>% 
  group_by(RASP3,SEX) %>%
  summarise(Freq = n()) %>%
  filter(RASP3!="")
stroke %>% 
  group_by(RASP3,RCONSC) %>%
  summarise(Freq = n()) %>%
  filter(RASP3!="")

stroke=stroke[stroke$DASP14!="n",]
stroke=stroke[stroke$DASP14!="y",]


# split the dataset into 50-50
smp_size <- floor(0.5 * nrow(stroke))
## set the seed to make your partition reproducible
set.seed(123)
train_stroke <- sample(seq_len(nrow(stroke)), size = smp_size)
train <- stroke[train_stroke, ]
test <- stroke[-train_stroke, ]

print("train percentage: ")
length(train[train$FDEAD=="Y",])/length(train$FDEAD)
print("test percentage: ")
length(test[test$FDEAD=="Y",])/length(test$FDEAD)

delete=c("FDEADX","DDEAD", "DDEADD", "DDEADC", "DDEADX", "DALIVE", "DALIVED", "DPLACE","COUNTRY",
         "CNTRYNUM","NCCODE","RDATE","DSIDEX","DNOSTRKX","DMAJNCHX",
         "HOURLOCAL","MINLOCAL","DAYLOCAL","FSOURCE","ID","EXPDD","EXPD14","EXPD6","SET14D",
         "ID14","OCCODE","SETASPLT","ID","DIED","DEAD1","DEAD2","DEAD3","DEAD4","DEAD5","DEAD6","DEAD7","DEAD8",
         "FDEADC","FDENNIS","FPLACE",
         "RDEF1","RDEF2","RDEF3","RDEF4","RDEF5","RDEF6","RDEF7","RDEF8")

train=train[, !(colnames(train) %in% delete), drop=FALSE] 
test=test[, !(colnames(test) %in% delete), drop=FALSE]

null=sapply(train, function(x) sum(is.na(x)))
null=null[null>0]
null

delete2=c("DMAJNCHD", "DSIDED","DRSISCD","DRSHD","DRSUNKD","DPE","DPED","FLASTD","FDEADD","DRSUNK",
          "FAP","FAP","FRECOVER","FOAC","CMPLHEP","DRSH","DNOSTRKX","TD")
train=train[, !(colnames(train) %in% delete2), drop=FALSE] 
test=test[, !(colnames(test) %in% delete2), drop=FALSE] 
train$FDEADC <- replace(train$FDEADC,is.na(train$FDEADC),0)
test$FDEADC <- replace(test$FDEADC,is.na(test$FDEADC),0)
train$ONDRUG <- replace(train$ONDRUG,is.na(train$ONDRUG),0)
test$ONDRUG <- replace(test$ONDRUG,is.na(test$ONDRUG),0)

#imputate train data
mtrain = mice(train %>% 
              select(c("FU1_RECD","FU2_DONE","FU1_COMP")) %>% 
              mutate_if(is.character, as.factor),m=1,maxit = 1) 
train_tmp = mice::complete(mtrain) %>% as_tibble()
# Rename columns to indicate imputation
names(train_tmp) = lapply(names(train_tmp), paste0, "_imputed")

#imputate test data
mtest = mice(test %>% 
                select(c("FU1_RECD","FU2_DONE","FU1_COMP")) %>% 
                mutate_if(is.character, as.factor),m=1,maxit = 1) 
test_tmp = mice::complete(mtest) %>% as_tibble()
# Rename columns to indicate imputation
names(test_tmp) = lapply(names(test_tmp), paste0, "_imputed")

#reform the two dataset
delete3=c("FU1_RECD","FU2_DONE","FU1_COMP","TD","ONDRUG")
train=train[, !(colnames(train) %in% delete3), drop=FALSE] 
test=test[, !(colnames(test) %in% delete3), drop=FALSE]
train=cbind(train,train_tmp)
test=cbind(test,test_tmp)

#clean dependent variable
train=train[train$FDEAD!="U",]
train=train[train$FDEAD!="",]
train$FDEAD=droplevels(train$FDEAD)

test=test[test$FDEAD!="U",]
test=test[test$FDEAD!="",]
test$FDEAD=droplevels(test$FDEAD)


mylogit <- glm(FDEAD ~., data = train,family = "binomial")
summary(mylogit)

rf <- randomForest(FDEAD ~ ., data = train, ntree = 500, mtry = 6, importance = TRUE)
summary(rf)

boost <- boosting(FDEAD~., data=train, boos=TRUE, mfinal=3)
summary(boost)

gbm <- gbm(FDEAD~., data=train,distribution = "gaussian")
#n.trees = 10000,shrinkage = 0.01, interaction.depth = 4
summary(gbm) #Summary gives a table of Variable Importance and a plot of Variable Importance

#ROC for multiple model
rf_pred=data.frame(predict(rf, test, type="prob"))
lg_pre=predict(mylogit,newdata=test,type=c("response"))
boost_pred=predict(boost,test,type=c("response"))
boost_pred=data.frame(boost_pred$prob)
boost_pred=boost_pred$X2
gbm_pred=predict(gbm,test,n.trees = 100,type = c("response"))

roc_rose <- plot(roc(test$FDEAD,lg_pre), print.auc = TRUE, col = "blue")
roc_rose <- plot(roc(test$FDEAD, rf_pred$Y), print.auc = TRUE, 
                 col = "green", print.auc.y = .4, add = TRUE)
roc_rose <- plot(roc(test$FDEAD,boost_pred), print.auc = TRUE, 
                 col = "red", print.auc.y = .3, add = TRUE)
roc_rose <- plot(roc(test$FDEAD,gbm_pred), print.auc = TRUE, 
                 col = "yellow", print.auc.y = .2, add = TRUE)

# variable importance
importance(rf)
library(caret)
varImpPlot(rf,type=2)

##Part2
#read data
item=read.csv("d_labitems.csv",header = TRUE)
events=read.csv("labevents.csv",header = TRUE)
data=events[events$subject_id=="13033",]
data=data[data$itemid=="50112",]

summary(data$valuenum)
boxplot(data$valuenum)

library(ggplot2)
library(scales)
library(lubridate)
data$Date <- as.Date(data$charttime)
data$time <- times(strftime(data$charttime,"%H:%M:%S"))
ggplot(data, aes(charttime, valuenum)) + 
  geom_point() + 
  #scale_x_datetime(breaks=date_breaks("2 month"), labels="%b-%d-%Y") + 
  #theme(axis.text.x=element_text(angle=90))+
  geom_hline(yintercept = mean(data$valuenum), color="blue")+
  geom_hline(yintercept = 72, color="red")+
  geom_hline(yintercept = 140, color="red")

#split data
bld_test=data[data$Date>"3417-5-1",]
bld_train=data[data$Date<="3417-5-1",]

#sine consine basic function
period = 0.5
K = 5

# Function for basis expansion of {sin(kx),cos(kx)} for i = 1 to K
sincos = function(dat, variable="time", period=2*pi, K=10) {
  data = dat
  for(i in 1:K) {
    data[[paste0("sin_",i)]] = sin(data[[variable]]*i*2*pi/period)
  }
  for(i in 1:K) {
    data[[paste0("cos_",i)]] = cos(data[[variable]]*i*2*pi/period)
  }
  # data$intercept = 1  # not necessary if using with models that use intercepts
  data
}

bld_train1 = sincos(bld_train, period=period, K=K)
bld_train1 = bld_train1 %>% dplyr::select(-charttime)%>% dplyr::select(-time)%>% dplyr::select(-Date)

# Learn a linear model, regularized
library(glmnet)  # Does regularization with (generalized) linear models
lasso = cv.glmnet(x = bld_train1 %>% select(-c(subject_id,hadm_id,icustay_id,itemid,value,valuenum,valueuom,flag)) 
                  %>% as.matrix(),
                  y=bld_train1$valuenum, family="gaussian")
lambda = lasso$lambda.min
lambda
plot(lasso$glmnet.fit, "lambda")


#predict
bld_test1 = sincos(bld_test, period=period, K=K)
bld_test2 = bld_test1 %>% dplyr::select(-charttime)%>% dplyr::select(-time)%>% dplyr::select(-Date)

self_data=data.frame(seq(ISOdate(2000,1,31), by = "min", length.out = 1440))
self_data$time=times(strftime(self_data$seq.ISOdate.2000..1..31...by....min...length.out...1440.,"%H:%M:%S"))
self_data1=sincos(self_data, period=period, K=K)
self_data2=self_data1%>% dplyr::select(-time)%>% dplyr::select(-seq.ISOdate.2000..1..31...by....min...length.out...1440.)

self_data1[["ylasso"]] = predict(lasso,
                                 self_data2
                                #%>% select(-c(subject_id,hadm_id,icustay_id,itemid,value,valuenum,valueuom,flag)) 
                                %>% as.matrix(),
                              s = c(lasso$lambda.min))


# Plot
ggplot(data=bld_test1, aes(time,valuenum)) + geom_point()+
  geom_line(data = self_data1, aes(x=time,y=ylasso,color="red"))

train_mean=mean(bld_train$valuenum)
#sum of square error
ssq=sum((bld_test$valuenum-train_mean)^2)
rsq=sum((bld_test1$valuenum-bld_test1$ylasso)^2)
r2=rsq/ssq
r2
