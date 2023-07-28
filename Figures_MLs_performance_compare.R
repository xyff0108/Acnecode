currentpath <- "E:\\Rcode\\Rcode\\Finalmodel"
setwd(currentpath)
palette <- c("#313695", "#D73027")

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


performance_Acne<-read.csv("MLresults/performance_Acne.csv")
performance_Acnerank<-read.csv("MLresults/performance_Acnerank.csv")
performance_Acne<-na.omit(performance_Acne)
performance_Acnerank<-na.omit(performance_Acnerank)
performance_Acnerank$Model<-as.factor(performance_Acnerank$Model)
performance_Acne$Model<-as.factor(performance_Acne$Model)

acne_meds<-ddply(performance_Acne,.(Model), summarise, med=median(ROC))
acnerank_meds<-ddply(performance_Acnerank,.(Model), summarise, med=median(ROC))


library(ggplot2)
library(cowplot)
library(patchwork)
library(PMCMRplus)

# Krustal-wallis test


Acnerank_fit<-kruskal.test(ROC~Model, data =performance_Acnerank)
Acnerank_fit
Acnerank_res1<-kwAllPairsDunnTest(Acnerank_fit)
Acnerank_res1<-kwAllPairsDunnTest(ROC~Model, data = performance_Acnerank)
summary(Acnerank_res1)



Acne_fit<-kruskal.test(ROC~Model, data =performance_Acne)
Acne_fit
Acne_res1<-kwAllPairsDunnTest(Acne_fit)
Acne_res1<-kwAllPairsDunnTest(ROC~Model, data = performance_Acne)
summary(Acne_res1)



textsize=2
level_order<-c("RF","GBDT", "NN", "SVM","KNN")

p_acnerank<-ggplot(data =performance_Acnerank, aes(x=factor(Model,levels= level_order), y=ROC, fill=Model))+
  geom_boxplot(outlier.colour = 'red',outlier.shape = 8, outlier.size = 1)+
  theme(legend.position = "none")+
  #geom_text(data =acnerank_meds, aes(x=Model, y=med, label=sprintf("%0.3f",med),  vjust=-0.6))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'),
        legend.title = element_blank(), legend.key = element_blank(), plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face='bold',size =8),axis.text = element_text(size=8),axis.title=element_text(size=8))+
  labs(x = '', y = '', title = '(b) Classfication of severe acne\n')+
  annotate('segment', x = 1, xend = 5, y = 0.975, yend = 0.975) +
  annotate('segment', x = 1, xend = 4, y = 0.965, yend = 0.965) +
  annotate('segment', x = 1, xend = 3, y = 0.955, yend = 0.955) +
  annotate('segment', x = 1, xend = 2, y = 0.945, yend = 0.945) +
  annotate('segment', x = 2, xend = 5, y = 0.935, yend = 0.935) +
  annotate('segment', x = 2, xend = 4, y = 0.925, yend = 0.925) +
  annotate('segment', x = 2, xend = 3, y = 0.915, yend = 0.915) +
  annotate('segment', x = 3, xend = 5, y = 0.905, yend = 0.905) +
  annotate('segment', x = 3, xend = 4, y = 0.895, yend = 0.895) +
  annotate('segment', x = 4, xend = 5, y = 0.885, yend = 0.885) +
  annotate('text', x = 2.6, y = 0.98, label = 'p < 0.001 ***', size=textsize) +
  annotate('text', x = 2.3, y = 0.97, label = 'p < 0.001 ***', size=textsize) +
  annotate('text', x = 2, y = 0.96, label = 'p < 0.001 ***',size=textsize)+
  annotate('text', x = 1.7, y = 0.950, label = 'p = 0.610',size=textsize) +
  annotate('text', x = 3.6, y = 0.940, label = 'p < 0.001 ***',size=textsize) +
  annotate('text', x = 3, y = 0.930, label = 'p < 0.001',size=textsize)+
  annotate('text', x = 2.6, y = 0.920, label = 'p < 0.001 ',size=textsize) +
  annotate('text', x = 4.3, y =0.910, label = 'p = 0.045 **',size=textsize) +
  annotate('text', x = 3.6, y = 0.9, label = 'p = 0.955',size=textsize)+
  annotate('text', x = 4.6, y = 0.89, label = 'p = 0.045 **',size=textsize) 




p_acne<-ggplot(data =performance_Acne, aes(x=factor(Model,levels= level_order), y=ROC, fill=Model))+
  geom_boxplot(outlier.colour = 'red',outlier.shape = 8, outlier.size = 1)+
  theme(legend.position = "none")+
  # geom_text(data =acne_meds, aes(x=Model, y=med, label=sprintf("%0.3f",med), size=, vjust=-0.6))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'),
        legend.title = element_blank(), legend.key = element_blank(), plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face='bold',size =8),axis.text = element_text(size=8),axis.title=element_text(size=8))+
  labs(x = '', y = '', title = '(a) Classfication of acne\n')+
  annotate('segment', x = 1, xend = 5, y = 0.90, yend = 0.90) +
  annotate('segment', x = 1, xend = 4, y = 0.89, yend = 0.89) +
  annotate('segment', x = 1, xend = 3, y = 0.88, yend = 0.88) +
  annotate('segment', x = 1, xend = 2, y = 0.87, yend = 0.87) +
  annotate('segment', x = 2, xend = 5, y = 0.86, yend = 0.86) +
  annotate('segment', x = 2, xend = 4, y = 0.85, yend = 0.85) +
  annotate('segment', x = 2, xend = 3, y = 0.84, yend = 0.84) +
  annotate('segment', x = 3, xend = 5, y = 0.83, yend = 0.83) +
  annotate('segment', x = 3, xend = 4, y = 0.82, yend = 0.82) +
  annotate('segment', x = 4, xend = 5, y = 0.81, yend = 0.81) +
  annotate('text', x = 2.6, y = 0.905, label = 'p <  0.001 ***', size=textsize) +
  annotate('text', x = 2.3, y = 0.895, label = 'p < 0.001 ***', size=textsize) +
  annotate('text', x = 2, y = 0.885, label = 'p = 0.022 **',size=textsize)+
  annotate('text', x = 1.7, y = 0.875, label = 'p = 0.022 **',size=textsize) +
  annotate('text', x = 3.6, y = 0.865, label = 'p < 0.001 ***',size=textsize) +
  annotate('text', x = 3, y = 0.855, label = 'p = 0.543',size=textsize)+
  annotate('text', x = 2.6, y = 0.845, label = 'p = 0.807',size=textsize) +
  annotate('text', x = 4.3, y =0.835, label = 'p < 0.001 ***',size=textsize) +
  annotate('text', x = 3.6, y = 0.825, label = 'p =0.641',size=textsize)+
  annotate('text', x = 4.6, y = 0.815, label = 'p < 0.001 ***',size=textsize) 

p<- p_acne|p_acnerank
p
ggsave('MLresults/MLs_performance.tiff', units = 'cm', dpi=400, width = 17, height =16, device = "tiff" ) 


