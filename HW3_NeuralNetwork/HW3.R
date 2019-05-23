#set environment
loadlibs = function(libs) {
  for(lib in libs) {
    class(lib)
    if(!do.call(require,as.list(lib))) {install.packages(lib)}
    do.call(require,as.list(lib))
  }
}
libs = c("dplyr","tidyr")
loadlibs(libs)

drgs=read.csv("drgevents.csv",header = TRUE)
procedure=read.csv("procedureevents.csv",header = TRUE)
codeitem=read.csv("d_codeditems.csv",header = TRUE)
lab=read.csv("labevents.csv",header = TRUE)
labitem=read.csv("d_labitems.csv",header = TRUE)

drg_count=left_join(drgs,codeitem,by=c("itemid"))
procedure_count=left_join(procedure,codeitem,by=c("itemid"))

##1b
drg_event=drg_count %>%
  group_by(itemid,description) %>%
  summarise(ttlCount=length(itemid)) %>%
  arrange(itemid,description) %>% as.data.frame()
  
drg_event2=drg_event[,c(2,3)] %>% 
  #filter(rank(desc(ttlCount))<=10) %>%
  arrange(description,ttlCount)

prc_event=procedure_count %>%
  group_by(itemid,description) %>%
  summarise(ttlCount=length(itemid)) %>%
  arrange(itemid,description) %>% as.data.frame()

prc_event2=prc_event[,c(2,3)] %>% 
  #filter(rank(desc(ttlCount))<=10) %>%
  arrange(description,ttlCount)

drg_event2%>%filter(rank(desc(ttlCount))<=10)
prc_event2%>%filter(rank(desc(ttlCount))<=10)

##1c
lab_event=lab[,c(2,4,8)]
lab_event=left_join(lab_event,labitem,by=c("itemid"))
lab_event$flag2[lab_event$flag=="abnormal"]<-"abnormal"
#lab_event$flag2[lab_event$flag=="normal"]<-"normal"
lab_event$flag2[is.na(lab_event$flag)]<-"normal"
lab_event$flag2[lab_event$flag=="delta"]<-"delta"

#replace na in description
levels=levels(lab_event$loinc_description)
levels[length(levels) + 1] <- "None"
lab_event$loinc_description=factor(lab_event$loinc_description,levels = levels)
lab_event$loinc_description[is.na(lab_event$loinc_description)]<-"None"

#lab_event2=lab_event[which(c(lab_event$flag2!="other")),]
#lab_event=lab_event[!is.na(lab_event$hadm_id),]
lab_event$tuple=paste(lab_event$itemid,lab_event$flag2,sep="_")
lab_event$tuple_desc=paste(lab_event$loinc_description,lab_event$flag2,sep="_")


lab_count=lab_event %>%
  group_by(tuple,tuple_desc) %>%
  summarise(ttlCount=length(tuple)) %>%
  arrange(tuple_desc,ttlCount) %>% as.data.frame()

lab_count%>%filter(rank(desc(ttlCount))<=10)

#1d
drg_filter=drg_event%>%filter(rank(desc(ttlCount))<=2000)
prc_filter=prc_event%>%filter(rank(desc(ttlCount))<=2000)
lab_filter=lab_count%>%filter(rank(desc(ttlCount))<=2000)

drg1d=inner_join(drg_count[,c(2,3,9)],drg_filter,by=c("itemid"))[,c(1,2,5)]
prc1d=inner_join(procedure_count[,c(2,3,10)],prc_filter,by=c("itemid"))[,c(1,2,5)]
lab1d=inner_join(lab_event[,c(1,10,11)],lab_filter,by=c("tuple"))[,c(1,2,5)]
names(lab1d) <- c("hadm_id", "itemid","ttlCount")
drg1d$itemid=as.character(drg1d$itemid)
prc1d$itemid=as.character(prc1d$itemid)

#long format
long_data=bind_rows(drg1d,prc1d,lab1d)
long_data=unique(long_data)
long_data$count=log10(1+long_data$ttlCount)
long_data=long_data[,c(1,2,4)]
wide_data=long_data%>%spread(key=itemid,value=count)
wide_data=inner_join(wide_data,drgs[,c(2,4)],by=c("hadm_id"))
wide_data[is.na(wide_data)]=0
wide_data[,c(1000)]

## 75% of the sample size
smp_size <- floor(0.5 * nrow(wide_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(wide_data)), size = smp_size)

train <- wide_data[train_ind, ]
test <- wide_data[-train_ind, ]
xtrain=train%>% select(-cost_weight) %>% as.matrix()
ytrain=train%>% select(cost_weight) %>% as.matrix()
xtest=test%>% select(-cost_weight) %>% as.matrix()
ytest=test%>% select(cost_weight) %>% as.matrix()
dim(train)

#modeling
#specify the architecture
library(keras)
model = keras_model_sequential() 
model %>%
  layer_dense(units = 32,
              activation = 'relu',
              input_shape = c(ncol(xtrain))) %>% 
  #layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = 'sigmoid') %>%
  layer_dense(units =32, activation = 'relu') %>%
  layer_dense(units = 32, activation = 'linear',kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dense(units = 1, activation = 'linear')

summary(model)

model %>% compile(
  # loss = 'categorical_crossentropy',
  loss = c('mse'),
  optimizer = optimizer_nadam(clipnorm = 10),
  metrics = c('mse')
)

#train model
inner_epochs = 10
early_stopping = callback_early_stopping(monitor = "val_loss",
                                         patience = inner_epochs/2)
bestLoss = 1e10
for(i in 1:20) {
  history = model %>% fit(xtrain, ytrain,
                          epochs = inner_epochs,
                          callbacks = c(early_stopping),
                          batch_size = 100,
                          validation_split = 0.2, shuffle=T
  )
  loss = history$metrics$val_loss[length(history$metrics$val_loss)]
  if(loss < bestLoss) {
    bestLoss = loss
    model %>% save_model_weights_hdf5("my_model_weights.h5")
  }
  if(length(history$metrics$val_loss) < inner_epochs)
    break
}
plot(history, metrics = "loss")

### Load the early-stopping model
bestModel = model %>% load_model_weights_hdf5('my_model_weights.h5')
bestModel %>% compile(
  # loss = 'categorical_crossentropy',
  loss = 'mse',
  optimizer = optimizer_nadam(),
  metrics = c('mse')
)

### Make predictions
bestModel %>% evaluate(xtest, ytest)
bestModel %>% evaluate(xtrain, ytrain)
bestModel %>% predict_on_batch(xtest) %>% head()
summary(bestModel)

plot(ytest,bestModel %>% predict_on_batch(xtest))
#interpretation
#

#part2
library(keras)
library(abind)
# create the base pre-trained model
base_model <- application_inception_v3(weights = 'imagenet', include_top = FALSE)

# image data, from: http://orbit.dtu.dk/files/2528109/oersted-dtu2886.pdf
image_normal_dir = "./normals/"
image_cancer_dir = "./carcinoma_in_situ/"
load_images = function(image_dir, dims=c(224,224,3), verbose=F) {
  imgs = array(0, dim=c(length(list.files(image_dir)),dims))
  fi=0
  for (f in list.files(image_dir)) {
    if(verbose && fi%%10 == 0) {
      print(paste0(fi, "/", length(list.files(image_dir)))) }
    fi = fi + 1
    img <- image_load(paste0(image_dir,f), target_size = dims[1:2])
    x <- image_to_array(img)
    # ensure we have a 4d tensor with single element in the batch dimension,
    # the preprocess the input for prediction using resnet50
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x, mode="tf")
    imgs[fi,,,] = x
  }
  return(imgs)
}
normals = load_images(image_normal_dir, verbose=T)
cancers = load_images(image_cancer_dir, verbose=T)
alldata = abind(normals,cancers, along = 1)
labels = 
  data.frame(label=c(rep("normal", dim(normals)[1]),
                     rep("cancer", dim(cancers)[1]))) %>%
  as_tibble()
rm(normals,cancers)

# Create train test split
set.seed(1e8)
traini = sample.int(n=dim(alldata)[1], size=200)
train_data = alldata[traini,,,]
test_data = alldata[-traini,,,]
train_labels = labels[traini,][[1]]
test_labels = labels[-traini,][[1]]
rm(alldata)

# plot a few examples
par(mfrow=c(5,4), oma = c(2, 2, 0.2, 0.2), mar=c(0,1,1,1))
for(i in 1:20) {
  ((train_data[i,,,] - min(train_data))/(max(train_data)-min(train_data))) %>% 
    as.raster() %>% plot()
}
##I cannot distinguish normal and cancerous cells, they are under similary colors, and cells are presenting with different structures, while I have few domain knowledge here, therefore I cannot distinguish them

#2b train network layers
# add our custom layers
predictions = base_model$output %>% 
  layer_global_average_pooling_2d() %>% 
  #layer_flatten(input_shape=c(224,224,3)) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'tanh') %>% 
  layer_dense(units = 2, activation = 'sigmoid')  # there are 2 classes

model = keras_model(inputs = base_model$input, outputs = predictions)
freeze_weights(base_model)
model %>% 
  compile(optimizer = optimizer_adam(), loss = loss_categorical_crossentropy,
                  metrics = "accuracy")

# train the model on the new data for a few epochs
test_labels=array(test_labels)
train_labels=array(train_labels)
train_labels[train_labels=="cancer"]=1
train_labels[train_labels=="normal"]=0
test_labels[test_labels=="cancer"]=1
test_labels[test_labels=="normal"]=0
history_part2=model %>% fit(x=train_data, y=train_labels, epochs=5, shuffle=T)
plot(history_part2)
#print the summary of the model
summary(model)
#evaluate the original model
eval_1=model %>% evaluate(x=test_data, y=test_labels)
preds = model %>% predict(test_data, verbose=1)

#2c train with fine-tuning
freeze_weights(base_model, from = 1, to = 172)
unfreeze_weights(base_model, from = 173)

# We need to recompile the model for these modifications to take effect
model_fine=model
model_fine=model_fine%>% compile(
  optimizer = optimizer_adam(lr = 0.0001), 
  loss = loss_categorical_crossentropy,
  metrics = "accuracy"
)
history_fine_tuning = model_fine %>% fit(x=train_data, y=train_labels, epochs=5, shuffle=T)
summary(model_fine)

#2d-compare-ROC-AUC
eval_2=model_fine%>% evaluate(x=test_data,y=test_labels)
preds_fine = model_fine %>% predict(test_data, verbose=1)

#first model
eval_1
#fine-tuning model
eval_2

library(pROC)
roc_rose <- plot(roc(test_labels,preds[,2]), print.auc = TRUE, col = "red")
roc_rose <- plot(roc(test_labels,preds_fine[,2]), print.auc = TRUE, print.auc.y = .4,col = "blue",add=TRUE)
