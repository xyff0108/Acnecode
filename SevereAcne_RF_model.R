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
change2Level <- function(y){y <- factor(y, levels=c(0, 1, "No", "Yes"))
  y[y == '0'] <- 'No'
  y[y == '1'] <- 'Yes'
  y <- factor(y, levels=c("No", "Yes"))}
#########################read input data ###########################################
acnerank_num_train<-readRDS('E:\\Rcode\\Rcode\\Finalmodel\\data\\acnerank_num_train.rds')
acnerank_num_test<-readRDS('E:\\Rcode\\Rcode\\Finalmodel\\data\\acnerank_num_test.rds')
acnerank_num_test$Acne_rank<-NULL

acnerank_num_train$Acnerank<-change2Level(acnerank_num_train$Acnerank)
acnerank_num_test$Acnerank<-change2Level(acnerank_num_test$Acnerank)
acnerank_num_train_rose<-ROSE(Acnerank~., data =acnerank_num_train[,-1], seed = 1)$data

#################################build rf model##############################################
x_names<-colnames(acnerank_num_train_rose[,-60])
form_cls<-as.formula(paste0("Acnerank~",paste(x_names,collapse = "+")))
form_cls
 
###########################grid research#####################
hyper_grid <- expand.grid(
  ntree =(2:10)*250) 

for (i in 5:nrow( hyper_grid)){
  set.seed(100)
  currentmodel <- i
  print(paste0("current running model is ",i,sep="")) 

  cmodel<-randomForest(
  form_cls,
  data=acnerank_num_train_rose,
  ntree=hyper_grid$ntree[i], 
  mtry=5,         #depending on the final values in adaptive boosting process
  importance=T   
  )
  cname <- paste("SevereAcne_tuneRF.",i,".rds",sep = "")
  saveRDS(cmodel,cname)
}

##################modelling performance compare#######################################

rf1_acnerank<-readRDS("SevereAcne_tuneRF.1.rds")  
testpredprop1_acnerank<-predict(rf1_acnerank,newdata = acnerank_num_test,type = 'prob')
rfPred1_acnerank<-predict(rf1_acnerank,acnerank_num_test,type="response")
rfperf1_acnerank<-confusionMatrix(acnerank_num_test$Acnerank,rfPred1_acnerank,positive='Yes', mode = 'everything')
rocrf1_acnerank<-roc(acnerank_num_test$Acnerank,testpredprop1_acnerank[,2],aur=TRUE,ci=TRUE)

overall_1 <- data.frame(rfperf1_acnerank$overall)
overall_1$names<-rownames(overall_1) 
overall_1$performance<-overall_1 $rfperf1_acnerank.overall
overall_1 $rfperf1_acnerank.overall<-NULL

byClass_1<-data.frame(rfperf1_acnerank$byClass)
byClass_1$names<-rownames(byClass_1)
byClass_1$performance<-byClass_1$rfperf1_acnerank.byClass
byClass_1$rfperf1_acnerank.byClass<-NULL


aucci_1<-data.frame(rocrf1_acnerank$ci)
aucci_1$names<-c("aucciLower","auccimedian","aucciupper")
aucci_1$performance<-aucci_1$rocrf1_acnerank.ci
aucci_1$rocrf1_acnerank.ci<-NULL

auc_1<-data.frame(rocrf1_acnerank$auc)
auc_1$names<-c("auc")
auc_1$performance<-auc_1$rocrf1_acnerank.auc
auc_1$rocrf1_acnerank.auc<-NULL

perRF1<-rbind.data.frame(overall_1,byClass_1,auc_1,aucci_1)
perRF1$performance<-round(perRF1$performance,5)



rf2_acnerank<-readRDS("SevereAcne_tuneRF.2.rds")  
testpredprop2_acnerank<-predict(rf2_acnerank,newdata = acnerank_num_test,type = 'prob')
rfPred2_acnerank<-predict(rf2_acnerank,acnerank_num_test,type="response")
rfperf2_acnerank<-confusionMatrix(acnerank_num_test$Acnerank,rfPred2_acnerank,positive='Yes',mode="everything")
rocrf2_acnerank<-roc(acnerank_num_test$Acnerank,testpredprop2_acnerank[,2],aur=TRUE,ci=TRUE)

overall_2 <- data.frame(rfperf2_acnerank$overall)
overall_2$names<-rownames(overall_2) 
overall_2$performance<-overall_2 $rfperf2_acnerank.overall
overall_2 $rfperf2_acnerank.overall<-NULL

byClass_2<-data.frame(rfperf2_acnerank$byClass)
byClass_2$names<-rownames(byClass_2)
byClass_2$performance<-byClass_2$rfperf2_acnerank.byClass
byClass_2$rfperf2_acnerank.byClass<-NULL


aucci_2<-data.frame(rocrf2_acnerank$ci)
aucci_2$names<-c("aucciLower","auccimedian","aucciupper")
aucci_2$performance<-aucci_2$rocrf2_acnerank.ci
aucci_2$rocrf2_acnerank.ci<-NULL

auc_2<-data.frame(rocrf2_acnerank$auc)
auc_2$names<-c("auc")
auc_2$performance<-auc_2$rocrf2_acnerank.auc
auc_2$rocrf2_acnerank.auc<-NULL

perRF2<-rbind.data.frame(overall_2,byClass_2,auc_2,aucci_2)
perRF2$performance<-round(perRF2$performance,5)

rf3_acnerank<-readRDS("SevereAcne_tuneRF.3.rds")  
testpredprop3_acnerank<-predict(rf3_acnerank,newdata = acnerank_num_test,type = 'prob')
rocrf_test3_acnerank<-roc.curve(acnerank_num_test$Acnerank,testpredprop3_acnerank[,2])

rfPred3_acnerank<-predict(rf3_acnerank,acnerank_num_test,type="response")
rfperf3_acnerank<-confusionMatrix(acnerank_num_test$Acnerank,rfPred3_acnerank,positive='Yes', 
                                  mode="everything")
rocrf3_acnerank<-roc(acnerank_num_test$Acnerank,testpredprop3_acnerank[,2],aur=TRUE,ci=TRUE)

overall_3 <- data.frame(rfperf3_acnerank$overall)
overall_3$names<-rownames(overall_3) 
overall_3$performance<-overall_3 $rfperf3_acnerank.overall
overall_3 $rfperf3_acnerank.overall<-NULL

byClass_3<-data.frame(rfperf3_acnerank$byClass)
byClass_3$names<-rownames(byClass_3)
byClass_3$performance<-byClass_3$rfperf3_acnerank.byClass
byClass_3$rfperf3_acnerank.byClass<-NULL


aucci_3<-data.frame(rocrf3_acnerank$ci)
aucci_3$names<-c("aucciLower","auccimedian","aucciupper")
aucci_3$performance<-aucci_3$rocrf3_acnerank.ci
aucci_3$rocrf3_acnerank.ci<-NULL

auc_3<-data.frame(rocrf3_acnerank$auc)
auc_3$names<-c("auc")
auc_3$performance<-auc_3$rocrf3_acnerank.auc
auc_3$rocrf3_acnerank.auc<-NULL

perRF3<-rbind.data.frame(overall_3,byClass_3,auc_3,aucci_3)
perRF3$performance<-round(perRF3$performance,5)

rf4_acnerank<-readRDS("SevereAcne_tuneRF.4.rds")  
testpredprop4_acnerank<-predict(rf4_acnerank,newdata = acnerank_num_test,type = 'prob')
rocrf_test4_acnerank<-roc.curve(acnerank_num_test$Acnerank,testpredprop4_acnerank[,2])

rfPred4_acnerank<-predict(rf4_acnerank,acnerank_num_test,type="response")
rfperf4_acnerank<-confusionMatrix(acnerank_num_test$Acnerank,rfPred4_acnerank,positive='Yes', mode = 'everything')
rocrf4_acnerank<-roc(acnerank_num_test$Acnerank,testpredprop4_acnerank[,2],aur=TRUE,ci=TRUE)

overall_4 <- data.frame(rfperf4_acnerank$overall)
overall_4$names<-rownames(overall_4) 
overall_4$performance<-overall_4 $rfperf4_acnerank.overall
overall_4 $rfperf4_acnerank.overall<-NULL

byClass_4<-data.frame(rfperf4_acnerank$byClass)
byClass_4$names<-rownames(byClass_4)
byClass_4$performance<-byClass_4$rfperf4_acnerank.byClass
byClass_4$rfperf4_acnerank.byClass<-NULL


aucci_4<-data.frame(rocrf4_acnerank$ci)
aucci_4$names<-c("aucciLower","auccimedian","aucciupper")
aucci_4$performance<-aucci_4$rocrf4_acnerank.ci
aucci_4$rocrf4_acnerank.ci<-NULL

auc_4<-data.frame(rocrf4_acnerank$auc)
auc_4$names<-c("auc")
auc_4$performance<-auc_4$rocrf4_acnerank.auc
auc_4$rocrf4_acnerank.auc<-NULL

perRF4<-rbind.data.frame(overall_4,byClass_4,auc_4,aucci_4)
perRF4$performance<-round(perRF4$performance,5)

rf5_acnerank<-readRDS("SevereAcne_tuneRF.5.rds")  
testpredprop5_acnerank<-predict(rf5_acnerank,newdata = acnerank_num_test,type = 'prob')
rocrf_test5_acnerank<-roc.curve(acnerank_num_test$Acnerank,testpredprop5_acnerank[,2])

rfPred5_acnerank<-predict(rf5_acnerank,acnerank_num_test,type="response")
rfperf5_acnerank<-confusionMatrix(acnerank_num_test$Acnerank,rfPred5_acnerank,positive='Yes', mode = 'everything')
rocrf5_acnerank<-roc(acnerank_num_test$Acnerank,testpredprop5_acnerank[,2],aur=TRUE,ci=TRUE)

overall_5 <- data.frame(rfperf5_acnerank$overall)
overall_5$names<-rownames(overall_5) 
overall_5$performance<-overall_5 $rfperf5_acnerank.overall
overall_5 $rfperf5_acnerank.overall<-NULL

byClass_5<-data.frame(rfperf5_acnerank$byClass)
byClass_5$names<-rownames(byClass_5)
byClass_5$performance<-byClass_5$rfperf5_acnerank.byClass
byClass_5$rfperf5_acnerank.byClass<-NULL


aucci_5<-data.frame(rocrf5_acnerank$ci)
aucci_5$names<-c("aucciLower","auccimedian","aucciupper")
aucci_5$performance<-aucci_5$rocrf5_acnerank.ci
aucci_5$rocrf5_acnerank.ci<-NULL

auc_5<-data.frame(rocrf5_acnerank$auc)
auc_5$names<-c("auc")
auc_5$performance<-auc_5$rocrf5_acnerank.auc
auc_5$rocrf5_acnerank.auc<-NULL

perRF5<-rbind.data.frame(overall_5,byClass_5,auc_5,aucci_5)
perRF5$performance<-round(perRF5$performance,5)


rf6_acnerank<-readRDS("SevereAcne_tuneRF.6.rds")  
testpredprop6_acnerank<-predict(rf6_acnerank,newdata = acnerank_num_test,type = 'prob')
rocrf_test6_acnerank<-roc.curve(acnerank_num_test$Acnerank,testpredprop6_acnerank[,2])

rfPred6_acnerank<-predict(rf6_acnerank,acnerank_num_test,type="response")
rfperf6_acnerank<-confusionMatrix(acnerank_num_test$Acnerank,rfPred6_acnerank,positive='Yes',mode="everything")
rocrf6_acnerank<-roc(acnerank_num_test$Acnerank,testpredprop6_acnerank[,2],aur=TRUE,ci=TRUE)


overall_6 <- data.frame(rfperf6_acnerank$overall)
overall_6$names<-rownames(overall_6) 
overall_6$performance<-overall_6 $rfperf6_acnerank.overall
overall_6 $rfperf6_acnerank.overall<-NULL

byClass_6<-data.frame(rfperf6_acnerank$byClass)
byClass_6$names<-rownames(byClass_6)
byClass_6$performance<-byClass_6$rfperf6_acnerank.byClass
byClass_6$rfperf6_acnerank.byClass<-NULL


aucci_6<-data.frame(rocrf6_acnerank$ci)
aucci_6$names<-c("aucciLower","auccimedian","aucciupper")
aucci_6$performance<-aucci_6$rocrf6_acnerank.ci
aucci_6$rocrf6_acnerank.ci<-NULL

auc_6<-data.frame(rocrf6_acnerank$auc)
auc_6$names<-c("auc")
auc_6$performance<-auc_6$rocrf6_acnerank.auc
auc_6$rocrf6_acnerank.auc<-NULL

perRF6<-rbind.data.frame(overall_6,byClass_6,auc_6,aucci_6)
perRF6$performance<-round(perRF6$performance,5)


rf7_acnerank<-readRDS("SevereAcne_tuneRF.7.rds")  
testpredprop7_acnerank<-predict(rf7_acnerank,newdata = acnerank_num_test,type = 'prob')
rocrf_test7_acnerank<-roc.curve(acnerank_num_test$Acnerank,testpredprop7_acnerank[,2])

rfPred7_acnerank<-predict(rf7_acnerank,acnerank_num_test,type="response")
rfperf7_acnerank<-confusionMatrix(acnerank_num_test$Acnerank,rfPred7_acnerank,positive='Yes', mode = 'everything')
rocrf7_acnerank<-roc(acnerank_num_test$Acnerank,testpredprop7_acnerank[,2],aur=TRUE,ci=TRUE)


overall_7 <- data.frame(rfperf7_acnerank$overall)
overall_7$names<-rownames(overall_7) 
overall_7$performance<-overall_7 $rfperf7_acnerank.overall
overall_7 $rfperf7_acnerank.overall<-NULL

byClass_7<-data.frame(rfperf7_acnerank$byClass)
byClass_7$names<-rownames(byClass_7)
byClass_7$performance<-byClass_7$rfperf7_acnerank.byClass
byClass_7$rfperf7_acnerank.byClass<-NULL


aucci_7<-data.frame(rocrf7_acnerank$ci)
aucci_7$names<-c("aucciLower","auccimedian","aucciupper")
aucci_7$performance<-aucci_7$rocrf7_acnerank.ci
aucci_7$rocrf7_acnerank.ci<-NULL

auc_7<-data.frame(rocrf7_acnerank$auc)
auc_7$names<-c("auc")
auc_7$performance<-auc_7$rocrf7_acnerank.auc
auc_7$rocrf7_acnerank.auc<-NULL

perRF7<-rbind.data.frame(overall_7,byClass_7,auc_7,aucci_7)
perRF7$performance<-round(perRF7$performance,5)



rf8_acnerank<-readRDS("SevereAcne_tuneRF.8.rds")  
testpredprop8_acnerank<-predict(rf8_acnerank,newdata = acnerank_num_test,type = 'prob')
rocrf_test8_acnerank<-roc.curve(acnerank_num_test$Acnerank,testpredprop8_acnerank[,2])

rfPred8_acnerank<-predict(rf8_acnerank,acnerank_num_test,type="response")
rfperf8_acnerank<-confusionMatrix(acnerank_num_test$Acnerank,rfPred8_acnerank,positive='Yes',  mode="everything")
rocrf8_acnerank<-roc(acnerank_num_test$Acnerank,testpredprop8_acnerank[,2],aur=TRUE,ci=TRUE)



overall_8 <-data.frame(rfperf8_acnerank$overall)
overall_8$names<-rownames(overall_8) 
overall_8$performance<-overall_8 $rfperf8_acnerank.overall
overall_8 $rfperf8_acnerank.overall<-NULL

byClass_8<-data.frame(rfperf8_acnerank$byClass)
byClass_8$names<-rownames(byClass_8)
byClass_8$performance<-byClass_8$rfperf8_acnerank.byClass
byClass_8$rfperf8_acnerank.byClass<-NULL


aucci_8<-data.frame(rocrf8_acnerank$ci)
aucci_8$names<-c("aucciLower","auccimedian","aucciupper")
aucci_8$performance<-aucci_8$rocrf8_acnerank.ci
aucci_8$rocrf8_acnerank.ci<-NULL

auc_8<-data.frame(rocrf8_acnerank$auc)
auc_8$names<-c("auc")
auc_8$performance<-auc_8$rocrf8_acnerank.auc
auc_8$rocrf8_acnerank.auc<-NULL

perRF8<-rbind.data.frame(overall_8,byClass_8,auc_8,aucci_8)
perRF8$performance<-round(perRF8$performance,5)

rf9_acnerank<-readRDS("SevereAcne_tuneRF.9.rds")  
testpredprop9_acnerank<-predict(rf9_acnerank,newdata = acnerank_num_test,type = 'prob')
rocrf_test9_acnerank<-roc.curve(acnerank_num_test$Acnerank,testpredprop9_acnerank[,2])

rfPred9_acnerank<-predict(rf9_acnerank,acnerank_num_test,type="response")
rfperf9_acnerank<-confusionMatrix(acnerank_num_test$Acnerank,rfPred9_acnerank,positive='Yes',  mode="everything")
rocrf9_acnerank<-roc(acnerank_num_test$Acnerank,testpredprop9_acnerank[,2],aur=TRUE,ci=TRUE)


overall_9 <- data.frame(rfperf9_acnerank$overall)
overall_9$names<-rownames(overall_9) 
overall_9$performance<-overall_9 $rfperf9_acnerank.overall
overall_9 $rfperf9_acnerank.overall<-NULL

byClass_9<-data.frame(rfperf9_acnerank$byClass)
byClass_9$names<-rownames(byClass_9)
byClass_9$performance<-byClass_9$rfperf9_acnerank.byClass
byClass_9$rfperf9_acnerank.byClass<-NULL


aucci_9<-data.frame(rocrf9_acnerank$ci)
aucci_9$names<-c("aucciLower","auccimedian","aucciupper")
aucci_9$performance<-aucci_9$rocrf9_acnerank.ci
aucci_9$rocrf9_acnerank.ci<-NULL

auc_9<-data.frame(rocrf9_acnerank$auc)
auc_9$names<-c("auc")
auc_9$performance<-auc_9$rocrf9_acnerank.auc
auc_9$rocrf9_acnerank.auc<-NULL

perRF9<-rbind.data.frame(overall_9,byClass_9,auc_9,aucci_9)
perRF9$performance<-round(perRF9$performance,5)

per_acnerank<-cbind.data.frame(perRF1,perRF2,perRF3,perRF4,perRF5,perRF6,perRF7,perRF8,perRF9)
per_acnerank<-per_acnerank[,c(1,2,4,6,8,10,12,14,16,18)]
per_acnerank<-data.frame(t(per_acnerank))
per_acnerank<-per_acnerank[2:10,]
per_acnerank$AUC<- per_acnerank$X1
per_acnerank$AUCLower<- per_acnerank$X11
per_acnerank$AUCUpper<- per_acnerank$X3
per_acnerank$X1<-NULL
per_acnerank$X11<-NULL
per_acnerank$X2<-NULL
per_acnerank$X3<-NULL
per_acnerank1<-as.data.frame(lapply(per_acnerank,as.numeric))
per_acnerank1$Model<-c("RF1","RF2","RF3","RF4","RF5","RF6","RF7","RF8","RF9")

optimalRF_acnerank<-per_acnerank1$Model[which(per_acnerank1$F1==max(per_acnerank1$F1))] # selecting the optimal RF using largest AUC
optimalRF_acnerank

write.csv(per_acnerank1, 'E:\\Rcode\\Rcode\\Finalmodel\\RFresults\\RFperformance_Acnerank.csv')

################################# feature importance unsing the optimal RF  ##############################################
rfbest_acnerank<-rf3_acnerank
acnerankrf_importance_scale<-data.frame(importance(rfbest_acnerank,scale = TRUE), check.names = FALSE)
acnerankrf_importance<-data.frame(importance(rfbest_acnerank), check.names = FALSE)
acnerankrf_importance_sort<- data.frame(acnerankrf_importance[order(acnerankrf_importance$MeanDecreaseAccuracy,decreasing = TRUE),])
acnerankrf_importance_sort$features=rownames(acnerankrf_importance_sort)
acnerankrf_importance_sort$relativeinf<-round(acnerankrf_importance_sort$MeanDecreaseAccuracy/sum(acnerankrf_importance_sort$MeanDecreaseAccuracy)*100,2)

acnerankrf_importance_sort$group<-ifelse(acnerankrf_importance_sort$features%in%c("Gender","Age","Ethnicity","Household_income","Grade"),"DSCs",
                                     ifelse(acnerankrf_importance_sort$features%in%c("Skin_type","Sensitive_skin","Genetic","Menstrual_blood_volume",
                                                                                 "Menstrual_colic","Menstrual_cycle","","PHQ9","BMI"),"BMAs",
                                            ifelse(acnerankrf_importance_sort$features%in%c("Den_barbecue_shop","Den_busstop","Den_cafeterias_shop","Den_fruit_shop",
                                                                                        "Den_hotpot_restaurant","Den_intersection","Den_KFC_McDonald.s",
                                                                                        "Den_milktea_shop","Den_roadlength","Denpop","NDVI"),"BEs",
                                                   ifelse(acnerankrf_importance_sort$features%in%c("Daylight","Temperature","Relative_humidity",
                                                                                               "CO","NO2","O3","PM2.5","SO2"),"NEs","LFs"))))
write.csv(acnerankrf_importance_sort,'E:\\Rcode\\Rcode\\Finalmodel\\RFresults\\acnerankrf_importance.csv')
