### Load helper packages ###
loadlibs = function(libs) {
  for(lib in libs) {
    class(lib)
    if(!do.call(require,as.list(lib))) {install.packages(lib)}
    do.call(require,as.list(lib))
  }
}
libs = c("tidyr","magrittr","purrr","dplyr","stringr","readr","data.table", "lubridate","crossval","eeptools","rpart","rpart.plot","caret","plotROC","pROC","ROCR","data.table")
loadlibs(libs)

#################################################################3
icustay=read.csv("icustay_detail.csv",header = TRUE)
demo=read.csv("demographic_detail.csv",header = TRUE)
icd=read.csv("icd9.csv",header = TRUE)

icustay=icustay[c("icustay_id","subject_id","icustay_admit_age","gender","hadm_id","icustay_total_num","hospital_expire_flg","dob","dod")]
demo=demo[c("subject_id","ethnicity_descr","overall_payor_group_descr","admission_type_descr","admission_source_descr")]

new_table=icustay %>% left_join(demo, by="subject_id")

icd = icd %>%
  group_by(subject_id,hadm_id) %>%
  summarise(ttlCode=length(code)) %>%
  arrange(subject_id,hadm_id) %>% as.data.frame()
new_table = left_join(new_table,icd,by=c("subject_id","hadm_id"))
new_table = new_table %>% 
  group_by(subject_id) %>%
  mutate(number_hadm = seq(1,length(hadm_id)),
         number_code = cumsum(ttlCode)) %>% as.data.frame()

newdata=unique(new_table)
newdata$age_death=age_calc(as.Date(newdata$dob),as.Date(newdata$dod),units = "years",precise = FALSE)


#seperate train and test data
newdata = newdata[sample(1:nrow(newdata)),]
lr_train = newdata[1:8000,]
lr_train=na.omit(lr_train)
lr_test = newdata[-(1:8000),]
lr_test = na.omit(lr_test)




#logistic regression
##cross validation, k=5
ctrl = trainControl(method = 'cv',number = 5)
mylogit3 = train(hospital_expire_flg~icustay_admit_age+gender+admission_type_descr+overall_payor_group_descr+ttlCode,
                 data=lr_train,method='glm',family="binomial",trControl=ctrl)
lr_prediction=data.frame(preds=(mylogit3 %>% predict(lr_test, type="raw")))
lr_prediction$preds2=!as.numeric(lr_prediction$preds)==1
lr_prediction=data.frame(lr_prediction$preds2)
#ROC Curve
columns_for_ROC =  # columns: predictions, labels
  lr_test %>%
  select(hospital_expire_flg) %>%
  bind_cols(lr_prediction) %>% mutate(type=hospital_expire_flg=="Y")
# plot it:
prediction(predictions=as.numeric(columns_for_ROC$lr_prediction.preds2),
           labels=columns_for_ROC$type) %>%
  performance("tpr", "fpr") %>% plot()

lr_pred = ROCR::prediction(as.numeric(columns_for_ROC$lr_prediction.preds2),columns_for_ROC$type)
lr_auc = mean(ROCR::performance(lr_pred,"auc")@y.values %>% unlist())
lr_auc

####decision tree
newdata=na.omit(newdata)
fit <- train(hospital_expire_flg ~ icustay_admit_age+gender+admission_type_descr+overall_payor_group_descr+ttlCode, data = newdata , method = "ctree",
             trControl = trainControl(method = "cv", number = 5),
             tuneLength=5) 
fit    
fit$finalModel
tree_prediction <- predict(fit, newdata = lr_test,type='raw') # prediction probabilities
tree_prediction=data.frame(preds=(fit %>% predict(lr_test, type="raw")))
tree_prediction$preds2=!as.numeric(tree_prediction$preds)==1
tree_prediction=data.frame(tree_prediction$preds2)
#ROC Curve
columns_for_ROC =  # columns: predictions, labels
  lr_test %>%
  select(hospital_expire_flg) %>%
  bind_cols(tree_prediction) %>% mutate(type=hospital_expire_flg=="Y")

# plot it:
prediction(predictions=as.numeric(columns_for_ROC$tree_prediction.preds2),
           labels=columns_for_ROC$type) %>%
  performance("tpr", "fpr") %>% plot()

tree_pred = ROCR::prediction(as.numeric(columns_for_ROC$tree_prediction.preds2),columns_for_ROC$type)
tree_auc = mean(ROCR::performance(tree_pred,"auc")@y.values %>% unlist())
tree_auc

###learning curve

lrn_crv=data.table(size=numeric(),accuracy=double(),desc=character())

for (i in seq(3000,nrow(newdata),200)) {
  #have a new sample set
  print(i)
  newdata = newdata[sample(1:nrow(newdata)),]
  lr_train = newdata[1:i,]
  lr_train=na.omit(lr_train)
  lr_test = newdata[-(1:i),]
  lr_test = na.omit(lr_test)
  #conduct model
  mlr <- glm(hospital_expire_flg ~ icustay_admit_age+gender+admission_type_descr+overall_payor_group_descr+ttlCode, family = binomial("logit"), data = lr_train)
  mtree <- rpart(hospital_expire_flg ~ icustay_admit_age+gender+admission_type_descr+overall_payor_group_descr+ttlCode, data = lr_train, method = 'class')
  
  model_prob <- predict(mlr, lr_test, type = "response")
  lr_test <- lr_test  %>% mutate(model_pred = 1*(model_prob > .5) + 0,
                           hospital_expire_flg = 1*(hospital_expire_flg == "Y") + 0)
  lr_test <- lr_test %>% mutate(accurate = 1*(model_pred == hospital_expire_flg))
  acc=sum(lr_test$accurate)/nrow(lr_test)
  
  PreditionsWithClass <- predict(mtree, lr_test, type = "class")
  t <- table(predictions = PreditionsWithClass, actual = lr_test$hospital_expire_flg)
  tree_acc<-sum(diag(t))/sum(t)
  
  
  #append result into lrn_crv table
  tmp=data.table(size=i,accuracy=acc,desc="logit")
  tmp2=data.table(size=i,accuracy=tree_acc,desc="tree")
  lrn_crv=rbind(lrn_crv,tmp)
  lrn_crv=rbind(lrn_crv, tmp2)
}

summary(lrn_crv)

p<-ggplot(lrn_crv, aes(x=size, y=accuracy, group=desc)) +
  #geom_line(aes(color=desc))+
  geom_point(aes(color=desc))+
  geom_smooth(span = 0.2,aes(color=desc))
p


case = data.frame(icustay_admit_age=58, gender='M', admission_type_descr='URGENT', ttlCode=11)
predict(mylogit,case, type = "response")

coef=mylogit$coefficients
m=min(abs(mylogit$coefficients))
points=signif(coef/m,digits = 2)
coef=points
points
1/m

for(i in 1:length(coef)){
  if(coef[i]>100){
    coef[i]=signif(coef[i],digits = 2)
  }else{
    coef[i]=round(coef[i],0)
  }
}
coef

#points for this patient
points.patient = 76.5 * 1 + 1*17 + 3*(-60) + 14*2
odds = exp(points.patient*m)
new.prob = odds/(1+odds)
new.prob

#construct points system
points=seq(-450,450,50)
prob = list()
for(i in 1:length(points)){
  p = points[i]*m
  odds = exp(p)
  prob[[i]] = odds/(1+odds)
}

score = data.frame(cbind(prob,points))

plot(score$points,score$prob)
lines(score$points,score$prob)

plot <-  prob %>% 
  unlist() %>% 
  cbind(points) %>%
  as_data_frame()

colnames(plot) <- c('prob.hospital.death','number.of.points')
plot %>%
  ggplot()+
  geom_line(aes( x= number.of.points, y = prob.hospital.death))

