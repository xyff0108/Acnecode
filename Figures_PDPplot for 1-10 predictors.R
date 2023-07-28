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


change2Level <- function(y){
  y <- factor(y, levels=c(0, 1, "No", "Yes"))
  y[y == '0'] <- 'No'
  y[y == '1'] <- 'Yes'
  y <- factor(y, levels=c("No", "Yes"))}

#########################read testing  data and optimal RF###########################################
acne_num_test<-readRDS('data/acne_num_test.rds')
acnerank_num_test<-readRDS('data/acnerank_num_test.rds')
acnerank_num_test$Acne_rank<-NULL

rfbest_acne<-readRDS("RF_optimize/acne_tuneRF.7.rds")  
rfbest_acnerank<-readRDS("RF_optimize/SevereAcne_tuneRF.3.rds") 

########################################################PDP plots for top ten features and acne#####################################
par(mfrow = c(5,2), mar = c(4,6,2,2) + 0.1)

pacne.Skin_type=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Skin_type,
                                            rug=FALSE, col='red', which.class= "Yes", 
                                            xlim=c(0,3), ylim=c(-0.55,0.25), 
                                            ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,3,1), seq(0,3, 1),labels=c("Dry","Normal","Mixed","Oily"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.55,0.25,0.2), sprintf("%0.2f",seq(-0.55,0.25,0.2)), las = 1, cex.axis =1.5)
title(main= NULL, xlab= '(a) Skin_type ',ylab="",  cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Skin_type,amount = 0.1))


pacne.genetic=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Genetic,
                                          rug=FALSE, col='red', which.class= "Yes", main='' , 
                                          xlim=c(0,3), ylim=c(-0.55,0.25),
                                          ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,3,1), seq(0,3, 1),labels=c("None","Two","One"," Both"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.55,0.25,0.2), sprintf("%0.2f",seq(-0.55,0.25,0.2)), las = 1,cex.axis = 1.5)
title(xlab= '(b) Genetic', ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Genetic,amount = 0.1))



pacne.Sensitive_skin=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Sensitive_skin,
                                        rug=FALSE, which.class= "Yes", main='' , 
                                         ylim=c(-0.55,0.1),
                                        ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,2,1), seq(1,2,1),labels=c("No","Yes"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.55,0.1,0.1), sprintf("%0.2f",seq(-0.55,0.1,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(c) Sensitive_skin', ylab="", cex.lab = 1.5, font.lab = 2)
#rug(jitter(acnerank_num_test$Sensitive_skin,amount = 0.1))



pacne.Den_busstop=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_busstop, 
                                            rug=TRUE, col='red', which.class= "Yes", main='' , 
                                            xlim=c(0,25), ylim=c(-0.55,0.1),
                                            ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,25,5), seq(0,25, 5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.55,0.1,0.1), sprintf("%0.2f",seq(-0.55,0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(d) Den_busstop', ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Den_busstop,amount = 0.05))


pacne.Den_milktea_shop=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_milktea_shop, 
                                               rug=TRUE, col='red', which.class= "Yes", main='' , 
                                               xlim=c(0,25), ylim=c(-0.2,0),
                                               ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,25,5), seq(0,25, 5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.2,0,0.05), sprintf("%0.2f",seq(-0.2,0,0.05)), las = 1,cex.axis = 1.5)
title( xlab='(e) Den_milktea_shop', ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Den_milktea_shop,amount = 0.05))



pacne.Den_roadlength=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_roadlength, 
                                            rug=TRUE, col='red', which.class= "Yes", main='' , 
                                            xlim=c(0,12), ylim=c(-0.2,0),
                                            ann = F, xaxt = "n", yaxt = "n") 
axis(1,seq(0,12,2), seq(0,12, 2),las = 1,cex.axis = 1.5)
axis(2,seq(-0.2,0,0.05), sprintf("%0.2f",seq(-0.2,0,0.05)), las = 1,cex.axis = 1.5)
title( xlab='(f) Den_roadlength', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Den_roadlength,amount = 0.05))


pacne.Den_barbecue_shop=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_barbecue_shop, 
                                               rug=TRUE, col='red', which.class= "Yes", main='' , 
                                               xlim=c(0,25), ylim=c(-0.2,0),
                                               ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,25,5), seq(0,25, 5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.2,0,0.05), sprintf("%0.2f",seq(-0.2,0,0.05)), las = 1,cex.axis = 1.5)
title( xlab='(g) Den_barbecue_shop', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Den_barbecue_shop,amount = 0.05))


pacne.Relative_humidity=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Relative_humidity, 
                                                  rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                  xlim=c(45,85), ylim=c(-0.2,0),
                                                  ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(45,85,5), seq(45,85,5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.2,0,0.05), sprintf("%0.2f",seq(-0.2,0,0.05)), las = 1,cex.axis = 1.5)
title( xlab='(h) Relative_humidity', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Relative_humidity,amount = 0.05))


pacne.Den_intersection=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_intersection, 
                                                  rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                  xlim=c(0,85), ylim=c(-0.2,0),
                                                  ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,85,10), seq(0,85, 10),las = 1,cex.axis = 1.5)
axis(2,seq(-0.2,0,0.05), sprintf("%0.2f",seq(-0.2,0,0.05)), las = 1,cex.axis = 1.5)
title( xlab='(i) Den_intersection', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Den_intersection,amount = 0.05))

pacne.Den_fruit_shop=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_fruit_shop, 
                                                 rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                 xlim=c(0,50), ylim=c(-0.2,0),
                                                 ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,50,10), seq(0,50, 10),las = 1,cex.axis = 1.5)
axis(2,seq(-0.2,0,0.05), sprintf("%0.2f",seq(-0.2,0,0.05)), las = 1,cex.axis = 1.5)
title( xlab='(j) Den_fruit_shop', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Den_fruit_shop,amount = 0.05))



########################################################PDP plots for top ten features and severe acne#####################################
par(mfrow = c(5,2), mar = c(4,6,2,2) + 0.1)

pacnerank.NDVI=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=NDVI,
                                          rug=FALSE, col='red', which.class= "Yes", 
                                          xlim=c(0,0.6), ylim=c(-0.8,0.1), 
                                          ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,0.6,0.1), seq(0,0.6, 0.1),las = 1,cex.axis = 1.5)
axis(2,seq(-0.8,0.1,0.1), sprintf("%0.2f",seq(-0.8,0.1,0.1)), las = 1, cex.axis =1.5)
title(main= NULL, xlab= '(A) NDVI',ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$NDVI,amount = 0.1))
mtext("Severe acne occurrence", side = 4, line = -65, las = 1)



pacnerank.Skin_type=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Skin_type,
                                          rug=FALSE, col='red', which.class= "Yes", 
                                          xlim=c(0,3), ylim=c(-0.8,-0.2), 
                                          ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,3,1), seq(0,3, 1),labels=c("Dry","Normal","Mixed","Oily"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.8,-0.2,0.1), sprintf("%0.2f",seq(-0.8,-0.2,0.1)), las = 1, cex.axis =1.5)
title(main= NULL, xlab= '(B) Skin_type ',ylab="",  cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Skin_type,amount = 0.1))


pacnerank.Den_intersection=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Den_intersection, 
                                                 rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                 xlim=c(0,85), ylim=c(-0.8,-0.2),
                                                 ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,85,10), seq(0,85, 10),las = 1,cex.axis = 1.5)
axis(2,seq(-0.8,-0.2,0.1), sprintf("%0.2f",seq(-0.8,-0.2,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(C) Den_intersection', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Den_intersection,amount = 0.05))


pacnerank.Den_busstop=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Den_busstop, 
                                                     rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                     xlim=c(0,25), ylim=c(-0.8,-0.2),
                                                     ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,25,5), seq(0,25, 5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.8,-0.2,0.1), sprintf("%0.2f",seq(-0.8,-0.2,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(D) Den_busstop', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Den_busstop,amount = 0.05))




pacnerank.IR_lightfood=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=IR_lightfood, 
                                                rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                xlim=c(0,5), ylim=c(-0.8,-0.2),
                                                ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,5,1), seq(0,5, 1),labels=c("None","Few","Less","Medium"," More","Many"),las = 1,cex.axis = 1.4)
axis(2,seq(-0.8,-0.2,0.1), sprintf("%0.2f",seq(-0.8,-0.2,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(E) IR_lightfood', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$IR_lightfood,amount = 0.05))



pacnerank.genetic=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Genetic,
                                        rug=FALSE, col='red', which.class= "Yes", main='' , 
                                        xlim=c(0,3), ylim=c(-0.8,-0.2),
                                        ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,3,1), seq(0,3, 1),labels=c("None","Two","One"," Both"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.8,-0.2,0.1), sprintf("%0.2f",seq(-0.8,-0.2,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(F) Genetic', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Genetic,amount = 0.1))



pacnerank.IR_milkfood=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=IR_milkfood, 
                                                 rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                 xlim=c(0,5), ylim=c(-0.8,-0.2),
                                                 ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,5,1), seq(0,5, 1),labels=c("None","Few","Less","Medium"," More","Many"),las = 1,cex.axis = 1.4)
axis(2,seq(-0.8,-0.2,0.1), sprintf("%0.2f",seq(-0.8,-0.2,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(G) IR_milkfood', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$IR_milkfood,amount = 0.05))


pacnerank.Using_SCP_sunscreen=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Using_SCP_sunscreen, 
                                                rug=TRUE,  which.class= "Yes", main='' , 
                                                xlim=c(0,2.5), ylim=c(-0.8,0),
                                                ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,2,1), seq(1,2,1),labels=c("No","Yes"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.8,0,0.1), sprintf("%0.2f",seq(-0.8,0,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(H) Using_SCP_sunscreen', ylab="",cex.lab = 1.5, font.lab = 2)

pacnerank.Gender=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Gender, 
                                                        rug=TRUE,  which.class= "Yes", main='' , 
                                                        xlim=c(0,2.5), ylim=c(-0.8,0),
                                                        ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,2,1), seq(1,2,1),labels=c("Female","Male"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.8,0,0.1), sprintf("%0.2f",seq(-0.8,0,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(I) Gender', ylab="",cex.lab = 1.5, font.lab = 2)


pacnerank.Den_hotpot_restaurant=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Den_hotpot_restaurant, 
                                                rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                xlim=c(0,32), ylim=c(-0.5,-0.3),
                                                ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,32,4), seq(0,32, 4),las = 1,cex.axis = 1.5)
axis(2,seq(-0.5,-0.3,0.05), sprintf("%0.2f",seq(-0.5,-0.3,0.05)), las = 1,cex.axis = 1.5)
title( xlab='(J) Den_hotpot_restaurant', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Den_hotpot_restaurant,amount = 0.05))

