currentpath <- "E:\\Rcode\\Rcode\\Finalmodel\\RF_optimize"
setwd(currentpath)
palette <- c("#313695", "#D73027")

#install.packages("skimr")
#install.packages("DataExplorer")
#install.packages('dplyr')
windowsFonts(TNM = windowsFont("Times New Roman"))

library(doParallel) 
cl <- makeCluster(2)  # 设置并行核数
registerDoParallel(cl) # 注册并行

#stopCluster(cl)
library(openxlsx)
library(skimr)
library(DataExplorer)
library(caret)
library(randomForest)
library(pROC)
library(dplyr)
library(ROSE)
############################### function to change label to Y/N#####################################
change2Level <- function(y){
  y <- factor(y, levels=c(0, 1, "No", "Yes"))
  y[y == '0'] <- 'No'
  y[y == '1'] <- 'Yes'
  y <- factor(y, levels=c("No", "Yes"))
}
#########################read input data ###########################################
acne_num_train<-readRDS('E:\\Rcode\\Rcode\\Finalmodel\\data\\acne_num_train.rds')
acne_num_test<-readRDS('E:\\Rcode\\Rcode\\Finalmodel\\data\\acne_num_test.rds')

#################################build rf model##############################################
x_names<-colnames(acne_num_train[,-60])
form_cls<-as.formula(paste0("Acne~",paste(x_names,collapse = "+")))
form_cls

###########################grid search#####################
hyper_grid <- expand.grid(
  ntree =(2:10)*250) 

for (i in 1:nrow( hyper_grid)){
  set.seed(100)
  currentmodel <- i
  print(paste0("current running model is ",i,sep="")) 

  cmodel<-randomForest(
  form_cls,
  data=acne_num_train,
  ntree=hyper_grid$ntree[i],    
  mtry=9,         #depending on the final values in adaptive boosting process
  importance=T   
  )
  cname <- paste("acne_tuneRF.",i,".rds",sep = "")
  saveRDS(cmodel,cname, )
}

###########################RF performance compare#####################

fit_rf_cls1<-readRDS("acne_tuneRF.1.rds")  
testpredprop1<-predict(fit_rf_cls1,newdata = acne_num_test,type = 'prob')
rfPred_1<-predict(fit_rf_cls1, acne_num_test,type="response")
rfperf_1<-confusionMatrix(acne_num_test$Acne,rfPred_1,positive='Yes', mode="everything")
rocrf_1<-roc(acne_num_test$Acne,testpredprop1[,2],aur=TRUE,ci=TRUE)


overall_1 <- data.frame(rfperf_1$overall)
overall_1$names<-rownames(overall_1) 
overall_1$performance<-overall_1 $rfperf_1.overall
overall_1 $rfperf_1.overall<-NULL

byClass_1<-data.frame(rfperf_1$byClass)
byClass_1$names<-rownames(byClass_1)
byClass_1$performance<-byClass_1$rfperf_1.byClass
byClass_1$rfperf_1.byClass<-NULL


aucci_1<-data.frame(rocrf_1$ci)
aucci_1$names<-c("aucciLower","auccimedian","aucciupper")
aucci_1$performance<-aucci_1$rocrf_1.ci
aucci_1$rocrf_1.ci<-NULL

auc_1<-data.frame(rocrf_1$auc)
auc_1$names<-c("auc")
auc_1$performance<-auc_1$rocrf_1.auc
auc_1$rocrf_1.auc<-NULL

perRF1<-rbind.data.frame(overall_1,byClass_1,auc_1,aucci_1)
perRF1$performance<-round(perRF1$performance,5)



fit_rf_cls2<-readRDS("acne_tuneRF.2.rds")  
testpredprop2<-predict(fit_rf_cls2,newdata = acne_num_test,type = 'prob')
rfPred_2<-predict(fit_rf_cls2, acne_num_test,type="response")
rfperf_2<-confusionMatrix(acne_num_test$Acne,rfPred_2,positive='Yes', mode="everything")
rocrf_2<-roc(acne_num_test$Acne,testpredprop2[,2],aur=TRUE,ci=TRUE)


overall_2 <- data.frame(rfperf_2$overall)
overall_2$names<-rownames(overall_2) 
overall_2$performance<-overall_2 $rfperf_2.overall
overall_2 $rfperf_2.overall<-NULL

byClass_2<-data.frame(rfperf_2$byClass)
byClass_2$names<-rownames(byClass_2)
byClass_2$performance<-byClass_2$rfperf_2.byClass
byClass_2$rfperf_2.byClass<-NULL


aucci_2<-data.frame(rocrf_2$ci)
aucci_2$names<-c("aucciLower","auccimedian","aucciupper")
aucci_2$performance<-aucci_2$rocrf_2.ci
aucci_2$rocrf_2.ci<-NULL

auc_2<-data.frame(rocrf_2$auc)
auc_2$names<-c("auc")
auc_2$performance<-auc_2$rocrf_2.auc
auc_2$rocrf_2.auc<-NULL

perRF2<-rbind.data.frame(overall_2,byClass_2,auc_2,aucci_2)
perRF2$performance<-round(perRF2$performance,5)


fit_rf_cls3<-readRDS("acne_tuneRF.3.rds")  
testpredprop3<-predict(fit_rf_cls3,newdata = acne_num_test,type = 'prob')
rfPred_3<-predict(fit_rf_cls3, acne_num_test,type="response")
rfperf_3<-confusionMatrix(acne_num_test$Acne,rfPred_3,positive='Yes', mode="everything")
rocrf_3<-roc(acne_num_test$Acne,testpredprop3[,2],aur=TRUE,ci=TRUE)


overall_3 <- data.frame(rfperf_3$overall)
overall_3$names<-rownames(overall_3) 
overall_3$performance<-overall_3 $rfperf_3.overall
overall_3 $rfperf_3.overall<-NULL

byClass_3<-data.frame(rfperf_3$byClass)
byClass_3$names<-rownames(byClass_3)
byClass_3$performance<-byClass_3$rfperf_3.byClass
byClass_3$rfperf_3.byClass<-NULL


aucci_3<-data.frame(rocrf_3$ci)
aucci_3$names<-c("aucciLower","auccimedian","aucciupper")
aucci_3$performance<-aucci_3$rocrf_3.ci
aucci_3$rocrf_3.ci<-NULL

auc_3<-data.frame(rocrf_3$auc)
auc_3$names<-c("auc")
auc_3$performance<-auc_3$rocrf_3.auc
auc_3$rocrf_3.auc<-NULL

perRF3<-rbind.data.frame(overall_3,byClass_3,auc_3,aucci_3)
perRF3$performance<-round(perRF3$performance,5)


fit_rf_cls4<-readRDS("acne_tuneRF.4.rds")  
testpredprop4<-predict(fit_rf_cls4,newdata = acne_num_test,type = 'prob')
rfPred_4<-predict(fit_rf_cls4, acne_num_test,type="response")
rfperf_4<-confusionMatrix(acne_num_test$Acne,rfPred_4,positive='Yes', mode="everything")
rocrf_4<-roc(acne_num_test$Acne,testpredprop4[,2],aur=TRUE,ci=TRUE)


overall_4 <- data.frame(rfperf_4$overall)
overall_4$names<-rownames(overall_4) 
overall_4$performance<-overall_4 $rfperf_4.overall
overall_4 $rfperf_4.overall<-NULL

byClass_4<-data.frame(rfperf_4$byClass)
byClass_4$names<-rownames(byClass_4)
byClass_4$performance<-byClass_4$rfperf_4.byClass
byClass_4$rfperf_4.byClass<-NULL


aucci_4<-data.frame(rocrf_4$ci)
aucci_4$names<-c("aucciLower","auccimedian","aucciupper")
aucci_4$performance<-aucci_4$rocrf_4.ci
aucci_4$rocrf_4.ci<-NULL

auc_4<-data.frame(rocrf_4$auc)
auc_4$names<-c("auc")
auc_4$performance<-auc_4$rocrf_4.auc
auc_4$rocrf_4.auc<-NULL

perRF4<-rbind.data.frame(overall_4,byClass_4,auc_4,aucci_4)
perRF4$performance<-round(perRF4$performance,5)

fit_rf_cls5<-readRDS("acne_tuneRF.5.rds")  
testpredprop5<-predict(fit_rf_cls5,newdata = acne_num_test,type = 'prob')
rfPred_5<-predict(fit_rf_cls5, acne_num_test,type="response")
rfperf_5<-confusionMatrix(acne_num_test$Acne,rfPred_5,positive='Yes', mode="everything")
rocrf_5<-roc(acne_num_test$Acne,testpredprop5[,2],aur=TRUE,ci=TRUE)


overall_5 <- data.frame(rfperf_5$overall)
overall_5$names<-rownames(overall_5) 
overall_5$performance<-overall_5 $rfperf_5.overall
overall_5 $rfperf_5.overall<-NULL

byClass_5<-data.frame(rfperf_5$byClass)
byClass_5$names<-rownames(byClass_5)
byClass_5$performance<-byClass_5$rfperf_5.byClass
byClass_5$rfperf_5.byClass<-NULL


aucci_5<-data.frame(rocrf_5$ci)
aucci_5$names<-c("aucciLower","auccimedian","aucciupper")
aucci_5$performance<-aucci_5$rocrf_5.ci
aucci_5$rocrf_5.ci<-NULL

auc_5<-data.frame(rocrf_5$auc)
auc_5$names<-c("auc")
auc_5$performance<-auc_5$rocrf_5.auc
auc_5$rocrf_5.auc<-NULL

perRF5<-rbind.data.frame(overall_5,byClass_5,auc_5,aucci_5)
perRF5$performance<-round(perRF5$performance,5)

fit_rf_cls6<-readRDS("acne_tuneRF.6.rds")  
testpredprop6<-predict(fit_rf_cls6,newdata = acne_num_test,type = 'prob')
rfPred_6<-predict(fit_rf_cls6, acne_num_test,type="response")
rfperf_6<-confusionMatrix(acne_num_test$Acne,rfPred_6,positive='Yes', mode="everything")
rocrf_6<-roc(acne_num_test$Acne,testpredprop6[,2],aur=TRUE,ci=TRUE)


overall_6 <- data.frame(rfperf_6$overall)
overall_6$names<-rownames(overall_6) 
overall_6$performance<-overall_6 $rfperf_6.overall
overall_6 $rfperf_6.overall<-NULL

byClass_6<-data.frame(rfperf_6$byClass)
byClass_6$names<-rownames(byClass_6)
byClass_6$performance<-byClass_6$rfperf_6.byClass
byClass_6$rfperf_6.byClass<-NULL


aucci_6<-data.frame(rocrf_6$ci)
aucci_6$names<-c("aucciLower","auccimedian","aucciupper")
aucci_6$performance<-aucci_6$rocrf_6.ci
aucci_6$rocrf_6.ci<-NULL

auc_6<-data.frame(rocrf_6$auc)
auc_6$names<-c("auc")
auc_6$performance<-auc_6$rocrf_6.auc
auc_6$rocrf_6.auc<-NULL

perRF6<-rbind.data.frame(overall_6,byClass_6,auc_6,aucci_6)
perRF6$performance<-round(perRF6$performance,5)

fit_rf_cls7<-readRDS("acne_tuneRF.7.rds")  
testpredprop7<-predict(fit_rf_cls7,newdata = acne_num_test,type = 'prob')
rfPred_7<-predict(fit_rf_cls7, acne_num_test,type="response")
rfperf_7<-confusionMatrix(acne_num_test$Acne,rfPred_7,positive='Yes', mode="everything")
rocrf_7<-roc(acne_num_test$Acne,testpredprop7[,2],aur=TRUE,ci=TRUE)


overall_7 <- data.frame(rfperf_7$overall)
overall_7$names<-rownames(overall_7) 
overall_7$performance<-overall_7 $rfperf_7.overall
overall_7 $rfperf_7.overall<-NULL

byClass_7<-data.frame(rfperf_7$byClass)
byClass_7$names<-rownames(byClass_7)
byClass_7$performance<-byClass_7$rfperf_7.byClass
byClass_7$rfperf_7.byClass<-NULL


aucci_7<-data.frame(rocrf_7$ci)
aucci_7$names<-c("aucciLower","auccimedian","aucciupper")
aucci_7$performance<-aucci_7$rocrf_7.ci
aucci_7$rocrf_7.ci<-NULL

auc_7<-data.frame(rocrf_7$auc)
auc_7$names<-c("auc")
auc_7$performance<-auc_7$rocrf_7.auc
auc_7$rocrf_7.auc<-NULL

perRF7<-rbind.data.frame(overall_7,byClass_7,auc_7,aucci_7)
perRF7$performance<-round(perRF7$performance,5)

fit_rf_cls8<-readRDS("acne_tuneRF.8.rds")  
testpredprop8<-predict(fit_rf_cls8,newdata = acne_num_test,type = 'prob')
rfPred_8<-predict(fit_rf_cls8, acne_num_test,type="response")
rfperf_8<-confusionMatrix(acne_num_test$Acne,rfPred_8,positive='Yes', mode="everything")
rocrf_8<-roc(acne_num_test$Acne,testpredprop8[,2],aur=TRUE,ci=TRUE)


overall_8 <- data.frame(rfperf_8$overall)
overall_8$names<-rownames(overall_8) 
overall_8$performance<-overall_8 $rfperf_8.overall
overall_8 $rfperf_8.overall<-NULL

byClass_8<-data.frame(rfperf_8$byClass)
byClass_8$names<-rownames(byClass_8)
byClass_8$performance<-byClass_8$rfperf_8.byClass
byClass_8$rfperf_8.byClass<-NULL


aucci_8<-data.frame(rocrf_8$ci)
aucci_8$names<-c("aucciLower","auccimedian","aucciupper")
aucci_8$performance<-aucci_8$rocrf_8.ci
aucci_8$rocrf_8.ci<-NULL

auc_8<-data.frame(rocrf_8$auc)
auc_8$names<-c("auc")
auc_8$performance<-auc_8$rocrf_8.auc
auc_8$rocrf_8.auc<-NULL

perRF8<-rbind.data.frame(overall_8,byClass_8,auc_8,aucci_8)
perRF8$performance<-round(perRF8$performance,5)


fit_rf_cls9<-readRDS("acne_tuneRF.9.rds")  
testpredprop9<-predict(fit_rf_cls9,newdata = acne_num_test,type = 'prob')
rfPred_9<-predict(fit_rf_cls9, acne_num_test,type="response")
rfperf_9<-confusionMatrix(acne_num_test$Acne,rfPred_9,positive='Yes', mode="everything")
rocrf_9<-roc(acne_num_test$Acne,testpredprop9[,2],aur=TRUE,ci=TRUE)

overall_9 <- data.frame(rfperf_9$overall)
overall_9$names<-rownames(overall_9) 
overall_9$performance<-overall_9 $rfperf_9.overall
overall_9 $rfperf_9.overall<-NULL

byClass_9<-data.frame(rfperf_9$byClass)
byClass_9$names<-rownames(byClass_9)
byClass_9$performance<-byClass_9$rfperf_9.byClass
byClass_9$rfperf_9.byClass<-NULL
 

aucci_9<-data.frame(rocrf_9$ci)
aucci_9$names<-c("aucciLower","auccimedian","aucciupper")
aucci_9$performance<-aucci_9$rocrf_9.ci
aucci_9$rocrf_9.ci<-NULL

auc_9<-data.frame(rocrf_9$auc)
auc_9$names<-c("auc")
auc_9$performance<-auc_9$rocrf_9.auc
auc_9$rocrf_9.auc<-NULL

perRF9<-rbind.data.frame(overall_9,byClass_9,auc_9,aucci_9)
perRF9$performance<-round(perRF9$performance,5)

per_acne<-cbind.data.frame(perRF1,perRF2,perRF3,perRF4,perRF5,perRF6,perRF7,perRF8,perRF9)
per_acne<-per_acne[,c(1,2,4,6,8,10,12,14,16,18)]
per_acne<-data.frame(t(per_acne))
per_acne<-per_acne[2:10,]
per_acne$AUC<- per_acne$X1
per_acne$AUCLower<- per_acne$X11
per_acne$AUCUpper<- per_acne$X3
per_acne$X1<-NULL
per_acne$X11<-NULL
per_acne$X2<-NULL
per_acne$X3<-NULL
per_acne1<-as.data.frame(lapply(per_acne,as.numeric))
per_acne1$Model<-c("RF1","RF2","RF3","RF4","RF5","RF6","RF7","RF8","RF9")

optimalRF<-per_acne1$Model[which(per_acne1$AUC==max(per_acne1$AUC))] # selecting the optimal RF using largest AUC
optimalRF
write.csv(per_acne1, 'E:\\Rcode\\Rcode\\Finalmodel\\RFresults\\RFperformance_Acne.csv')

################################# feature importance  ##############################################
rfbest<-fit_rf_cls7
acnerf_importance_scale<-data.frame(importance(rfbest,scale = TRUE), check.names = FALSE)
acnerf_importance<-data.frame(importance(rfbest), check.names = FALSE)
acnerf_importance_sort<- data.frame(acnerf_importance[order(acnerf_importance$MeanDecreaseAccuracy,decreasing = TRUE),])
acnerf_importance_sort$features=rownames(acnerf_importance_sort)
acnerf_importance_sort$relativeinf<-sprintf("%0.2f", acnerf_importance_sort$MeanDecreaseAccuracy/sum(acnerf_importance_sort$MeanDecreaseAccuracy)*100)

acnerf_importance_sort$group<-ifelse(acnerf_importance_sort$features%in%c("Gender","Age","Ethnicity","Household_income","Grade"),"DSCs",
                                    ifelse(acnerf_importance_sort$features%in%c("Skin_type","Sensitive_skin","Genetic","Menstrual_blood_volume",
                                                                        "Menstrual_colic","Menstrual_cycle","","PHQ9","BMI"),"BMAs",
                                           ifelse(acnerf_importance_sort$features%in%c("Den_barbecue_shop","Den_busstop","Den_cafeterias_shop","Den_fruit_shop",
                                                                               "Den_hotpot_restaurant","Den_intersection","Den_KFC_McDold.s",
                                                                               "Den_milktea_shop","Den_roadlength","Denpop","NDVI"),"BEs",
                                                  ifelse(acnerf_importance_sort$features%in%c("Daylight","Temperature","Relative_humidity",
                                                                                    "CO","NO2","O3","PM2.5","SO2"),"NEs","LFs"))))
write.csv(acnerf_importance_sort,'E:\\Rcode\\Rcode\\Finalmodel\\RFresults\\acnerf_importance.csv')
