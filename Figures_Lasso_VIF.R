currentpath <- "E:\\Rcode\\Rcode\\Finalmodel"
setwd(currentpath)
palette <- c("#313695", "#D73027")

#install.packages("skimr")
#install.packages("DataExplorer")
#install.packages('dplyr')
#install.packages('patchwork')
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


acne_num_train<-readRDS('data/acne_num_train.rds')
acne_num_test<-readRDS('data/acne_num_test.rds')
acne_num_train$Acne<-as.numeric((acne_num_train$Acne))

acnerank_num_train<-readRDS('data/acnerank_num_train.rds')
acnerank_num_test<-readRDS('data/acnerank_num_test.rds')
acnerank_num_train$Den_KFC_McDold.s<-acnerank_num_train$Den_KFC_McDonald.s
acnerank_num_train$Den_KFC_McDonald.s<-NULL
acnerank_num_train$Acne_rank<-NULL
acnerank_num_train$Acnerank<-as.numeric((acnerank_num_train$Acnerank))


library(car)
lm_acne<-lm(acne_num_train$Acne~., data=acne_num_train)
summary(vif(lm_acne))
vif_acne<-data.frame(vif(lm_acne))
vif_acne$Features<-rownames(vif_acne)

lm_acnerank<-lm(acnerank_num_train$Acnerank~., data=acnerank_num_train)
vif_acnerank<-data.frame(vif(lm_acnerank))
vif_acnerank$Features<-rownames(vif_acnerank)
summary(vif(lm_acnerank))

features<-read.csv('E:\\Rcode\\Rcode\\Finalmodel\\Lasso\\lasso_coef.csv')
features$Features<-features$X
features$coef<-features$x
features1<-features[,3:4]
features1$class<-ifelse(features1$coef > 0,"postive",
                       ifelse(features1$coef<0,"negative","redundant"))

merge_data<-left_join(features1,vif_acne, by='Features')
merge_data1<-left_join(merge_data,vif_acnerank, by='Features')

write.csv(merge_data1,'E:\\Rcode\\Rcode\\Finalmodel\\Featureplotdata.csv')
Featureplotdata<-read.csv('E:\\Rcode\\Rcode\\Finalmodel\\Featureplotdata.csv')

library('ggplot2')
library(patchwork)

p_LASSO<-ggplot(Featureplotdata, aes(x=reorder(Featurename,coef),y=coef, fill=class))+  
  geom_bar(stat="identity")+
  labs(title = paste("(a) LASSO regression"))+
  geom_text(aes(label=sprintf("%0.4f", coef)),size=2.5,position = position_dodge(width =0.9),hjust=-0.5)+
  theme(plot.margin = unit(c(0.5,0.5,0.1,0.1),"cm"))+coord_flip()+labs(x="Features", y='Coeffiecnts')+ ylim(-1.5,1.5)+
  scale_fill_manual(name=NULL, values =c("dodgerblue4","red","grey" ))+theme(legend.position = c(0.8,-1))+
  theme(plot.title = element_text(face='bold',size =9),axis.text = element_text(size=8),axis.title=element_text(face='bold',size=9))


p_Acnerankvif<-ggplot(Featureplotdata, aes(x=reorder(Featurename,coef),y=vif.lm_acnerank.))+  
  geom_bar(stat="identity",fill ='dodgerblue4')+
  labs(title = paste("(b) Acne prevalence"))+
  geom_text(aes(label=sprintf("%0.1f", vif.lm_acnerank.)),size=2.5,hjust=-0.1)+
  theme(plot.margin = unit(c(0.5,0.5,0.1,0.1),"cm"))+coord_flip()+labs(x=NULL, y='VIFs')+ ylim(0,8)+
  theme(axis.ticks.y= element_blank())+
  theme(plot.title = element_text(face='bold',size = 9), axis.text.y=element_blank(), axis.text = element_text(size=8),axis.title=element_text(face='bold',size=9))

p_Acnevif<-ggplot(Featureplotdata, aes(x=reorder(Featurename,coef),y=vif.lm_acne.))+  
  geom_bar(stat="identity",fill ='dodgerblue4')+
  labs(title = paste("(c) Acne severity"))+
  geom_text(aes(label=sprintf("%0.1f", vif.lm_acne.)),size=2.5,hjust=-0.1)+
  theme(plot.margin = unit(c(0.5,0.5,0.1,0.1),"cm"))+coord_flip()+labs(x=NULL, y='VIFs')+ ylim(0,8)+
  theme(axis.ticks.y= element_blank())+
    theme(plot.title = element_text(face='bold', size =9), axis.text.y=element_blank(), axis.text = element_text(size=8),axis.title=element_text(face='bold',size=9))

p<-p_LASSO|(p_Acnerankvif|p_Acnevif)
p
ggsave('Lasso and VIFs.tiff', units = 'cm', dpi=400, width = 17.5, height =22, device = "tiff" )