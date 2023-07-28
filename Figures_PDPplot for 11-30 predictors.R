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
par(mfrow = c(5,4), mar = c(4,6,2,2) + 0.1)



pacne.NDVI=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=NDVI,
                                          rug=FALSE, col='red', which.class= "Yes", main='' , 
                                          ylim=c(-0.35,0.05),
                                          ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,0.6,0.1), seq(0,0.6,0.1),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(a1) NDVI,11#', ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$NDVI,amount = 0.1))


pacne.Den_hotpot_restaurant=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_hotpot_restaurant,
                                     rug=FALSE, col='red', which.class= "Yes", main='' , 
                                     ylim=c(-0.35,0.05),
                                     ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,32,2), seq(0,32, 2),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(a2) Den_hotpot_restaurant,12#', ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Den_hotpot_restaurant,amount = 0.1))



pacne.NO2=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=NO2,
                                        rug=FALSE, col='red',which.class= "Yes", main='' , 
                                       xlim=c(10,40),ylim=c(-0.35,0.05),
                                        ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(10,40,5), seq(10,40,5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(a3) NO2,13#', ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$NO2,amount = 0.1))



pacne.Den_fruit_shop=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_fruit_shop, 
                                            rug=TRUE, col='red', which.class= "Yes", main='' , 
                                            xlim=c(0,55), ylim=c(-0.35,0.05),
                                            ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,55,5), seq(0,55, 5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a4) Den_fruit_shop,14#', ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Den_fruit_shop,amount = 0.1))


pacne.Daylight=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Daylight, 
                                               rug=TRUE, col='red', which.class= "Yes", main='' , 
                                               xlim=c(4,9), ylim=c(-0.35,0.05),
                                               ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(4,9,1), seq(4,9,1),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a5) Daylight,15#', ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Daylight,amount = 0.1))



pacne.Grade=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Grade, 
                                            rug=TRUE, col='red', which.class= "Yes", main='' , 
                                            xlim=c(1,5), ylim=c(-0.35,0.05),
                                            ann = F, xaxt = "n", yaxt = "n") 
axis(1,seq(1,5,1), seq(1,5, 1),labels=c("1st","2nd","3rd","4th","5th"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a6) Grade ,16#', ylab="",cex.lab = 1.5, font.lab = 2)
#rug(jitter(acne_num_test$Grade,amount = 0.05))


pacne.O3=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=O3, 
                                               rug=TRUE, col='red', which.class= "Yes", main='' , 
                                               ylim=c(-0.35,0.05),
                                               ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(35,120,15), seq(35,120,15),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a7) Ozone,17#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$O3,amount = 0.05))


pacne.CO=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Relative_humidity, 
                                                  rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                  xlim=c(45,85), ylim=c(-0.35,0.05),
                                                  ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(45,85,5), seq(45,85,5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a8) CO,18#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$CO,amount = 0.05))


pacne.Temperature=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Temperature, 
                                                  rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                  xlim=c(7.5,25.5), ylim=c(-0.35,0.05),
                                                  ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(7.5,25.5,3), seq(7,28, 4),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a9) Temperature,19#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Temperature,amount = 0.05))


pacne.Den_cafeterias_shop=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_cafeterias_shop, 
                                                 rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                 xlim=c(0,6), ylim=c(-0.35,0.05),
                                                 ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,6,1), seq(0,6, 1),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a10) Den_cafeterias_shop,20#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Den_cafeterias_shop,amount = 0.05))



pacne.PM2.5=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=PM2.5, 
                                                    rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                    xlim=c(10,45), ylim=c(-0.35,0.05),
                                                    ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(10,45,5), seq(10,45,5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a11)PM2.5,21#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$PM2.5,amount = 0.05))




pacne.SO2=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=SO2, 
                                                    rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                    xlim=c(2,22), ylim=c(-0.35,0.05),
                                                    ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(2,22,2), seq(2,22,2),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a12) SO2,22#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$SO2,amount = 0.05))




pacne.Age=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Age, 
                                                    rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                    xlim=c(15,30), ylim=c(-0.35,0.05),
                                                    ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(15,30,2), seq(15,30, 2),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a13) Age,23#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Age,amount = 0.05))


pacne.Den_KFC_McDold.s=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Den_KFC_McDold.s, 
                                    rug=TRUE, col='red', which.class= "Yes", main='' , 
                                    xlim=c(0,0.5), ylim=c(-0.35,0.05),
                                    ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,2.5,0.5), seq(0,2.5,0.5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a14) Den_KFC_McDold.s,24#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Den_KFC_McDold.s,amount = 0.05))


pacne.Using_SCP_oil_control=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Using_SCP_oil_control, 
                                    rug=TRUE,  which.class= "Yes", main='' , 
                                    xlim=c(0,2.5), ylim=c(-0.35,0.05),
                                    ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,2,1), seq(1,2,1),labels=c("No","Yes"), las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a15) Using_SCP_oil_control,25#', ylab="",cex.lab = 1.5, font.lab = 2)
#rug(jitter(acne_num_test$Using_SCP_oil_control,amount = 0.05))


pacne.PHQ9=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=PHQ9, 
                                    rug=TRUE, col='red', which.class= "Yes", main='' , 
                                    xlim=c(0,27), ylim=c(-0.35,0.05),
                                    ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,27,3), seq(0,27,3),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a16) PHQ9,26#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$PHQ9,amount = 0.05))



pacne.Menstrual_cycle=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Menstrual_cycle, 
                                    rug=TRUE,col='red', which.class= "Yes", main='' , 
                                    xlim=c(1,3), ylim=c(-0.35,0.05),
                                    ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,3,1), seq(1,3,1),labels=c("Normal","Early","Delay"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a17) Menstrual_cycle,27#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Menstrual_cycle,amount = 0.1))


pacne.Menstrual_blood_volume=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Menstrual_blood_volume, 
                                    rug=TRUE, col='red', which.class= "Yes", main='' , 
                                    xlim=c(1,3), ylim=c(-0.35,0.05),
                                    ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,3,1), seq(1,3,1),labels=c("Normal","Less","More"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a18) Menstrual_blood_volume,28#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acne_num_test$Menstrual_blood_volume,amount = 0.1))


pacne.Gender=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Gender, 
                                    rug=TRUE,  which.class= "Yes", main='' , 
                                    xlim=c(0,2.5), ylim=c(-0.35,0.05),
                                    ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,2,1), seq(1,2,1),labels=c("Female","Male"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a19) Gender,29#', ylab="",cex.lab = 1.5, font.lab = 2)
#rug(jitter(acne_num_test$Age,amount = 0.05))


pacne.Using_SCP_sunscreen=randomForest::partialPlot(rfbest_acne, acne_num_test, x.var=Using_SCP_sunscreen, 
                                       rug=TRUE,  which.class= "Yes", main='' , 
                                       xlim=c(0,2.5), ylim=c(-0.35,0.05),
                                       ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,2,1), seq(1,2,1),labels=c("No","Yes"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.35,0.05,0.1), sprintf("%0.2f",seq(-0.35,0.05,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(a20) Using_sunscreen,30#', ylab="",cex.lab = 1.5, font.lab = 2)
#rug(jitter(acne_num_test$Age,amount = 0.05))

########################################################PDP plots for top ten features and severe acne#####################################
par(mfrow = c(5,4), mar = c(4,6,2,2) + 0.1)

pacnerank.CO=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=CO,
                                          rug=FALSE, col='red', which.class= "Yes", 
                                          xlim=c(0.5,1.2), ylim=c(-0.6,-0.1), 
                                          ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0.5,1.2,0.1), seq(0.5,1.2,0.1),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1, cex.axis =1.5)
title(main= NULL, xlab= '(A1) CO,11#',ylab="", cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$CO,amount = 0.1))
#mtext("Severe acne occurrence", side = 4, line = -65, las = 1)



pacnerank.Den_roadlength=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Den_roadlength,
                                          rug=FALSE, col='red', which.class= "Yes", 
                                         ylim=c(-0.6,-0.1), 
                                          ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,13,1), seq(0,13, 1),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1, cex.axis =1.5)
title(main= NULL, xlab= '(A2)Den_roadlength,12# ',ylab="",  cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Den_roadlength,amount = 0.1))


pacnerank.O3=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=O3, 
                                                 rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                 ylim=c(-0.6,-0.1),
                                                 ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(35,120,15), seq(35,120,15),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A3) O3, 13#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$O3,amount = 0.1))


pacnerank.PHQ9=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=PHQ9, 
                                                     rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                     xlim=c(0,27), ylim=c(-0.6,-0.1),
                                                     ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,27,3), seq(0,27,3),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A4) PHQ9, 14#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$PHQ9,amount = 0.1))




pacnerank.Temperature=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Temperature, 
                                                rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                xlim=c(7.5,25.5), ylim=c(-0.6,-0.1),
                                                ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(7.5,25.5,3), seq(7.5,25.5,3),las = 1,cex.axis = 1.4)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A5) Temperature,15#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Temperature,amount = 0.1))



pacnerank.Den_barbecue_shop=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Den_barbecue_shop,
                                        rug=FALSE, col='red', which.class= "Yes", main='' , 
                                        xlim=c(0,21), ylim=c(-0.6,-0.1),
                                        ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,21,3), seq(0,21, 3),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(A6) Den_barbecue_shop,16#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Den_barbecue_shop,amount = 0.1))



pacnerank.IR_spicyfood=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=IR_spicyfood, 
                                                 rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                 xlim=c(0,5), ylim=c(-0.6,-0.1),
                                                 ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,5,1), seq(0,5, 1),labels=c("None","Few","Less","Medium"," More","Many"),las = 1,cex.axis = 1.4)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A7) IR_spicyfood,17#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$IR_spicyfood,amount = 0.1))


pacnerank.Household_income=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Household_income, 
                                                rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                xlim=c(1,5), ylim=c(-0.8,0),
                                                ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,5,1), seq(1,5,1),labels=c("<500","501-1000","1001-2000","2001-3000",">3000"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.8,0,0.1), sprintf("%0.2f",seq(-0.8,0,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A8) Household_income,18#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Household_income,amount = 0.1))


pacnerank.Sensitive_skin=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Sensitive_skin, 
                                                        rug=TRUE,  which.class= "Yes", main='' , 
                                                        xlim=c(0,2.5), ylim=c(-0.8,0),
                                                        ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,2,1), seq(1,2,1),labels=c("No","Yes"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.8,0,0.1), sprintf("%0.2f",seq(-0.8,0,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A9) Sensitive_skin,19#', ylab="",cex.lab = 1.5, font.lab = 2)


pacnerank.Water_drinking=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Water_drinking, 
                                                rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                xlim=c(0,3), ylim=c(-0.6,-0.1),
                                                ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,3,1), seq(0,3,1),labels=c("Untreated","Unboiled","Boiled","Bottled"),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A10) Water_drinking,20#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Water_drinking,amount = 0.1))



pacnerank.IR_oilyfood=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=IR_oilyfood, 
                                                 rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                 xlim=c(0,5), ylim=c(-0.6,-0.1),
                                                 ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,5,1), seq(0,5, 1),labels=c("None","Few","Less","Medium"," More","Many"),las = 1,cex.axis = 1.4)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A11) IR_oilyfood,21#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$IR_spicyfood,amount = 0.1))


pacnerank.Age=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Age,
                                                      rug=FALSE, col='red', which.class= "Yes", main='' , 
                                                      xlim=c(15,30), ylim=c(-0.6,-0.1),
                                                      ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(15,30,2), seq(15,30,2),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(A12) Age,22#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Age,amount = 0.1))


pacnerank.Menstrual_blood_volume=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Menstrual_blood_volume, 
                                                rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                xlim=c(1,3), ylim=c(-0.6,-0.1),
                                                ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,3,1), seq(1,3, 1),labels=c("Normal","Less","More"),las = 1,cex.axis = 1.4)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A13) Menstrual_blood_volume,23#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Menstrual_blood_volume,amount = 0.1))


pacnerank.SO2=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=SO2,
                                        rug=FALSE, col='red', which.class= "Yes", main='' , 
                                        xlim=c(3,24), ylim=c(-0.6,-0.1),
                                        ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(3,24,3), seq(3,24,3),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(A14) SO2,24#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$SO2,amount = 0.1))


pacnerank.Menstrual_cycle=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Menstrual_cycle, 
                                                           rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                           xlim=c(1,3), ylim=c(-0.6,-0.1),
                                                           ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1,3,1), seq(1,3, 1),labels=c("Normal","Early","Delay"),las = 1,cex.axis = 1.4)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A15) Menstrual_cycle,25#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Menstrual_cycle,amount = 0.1))


pacnerank.Den_fruit_shop=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Den_fruit_shop,
                                        rug=FALSE, col='red', which.class= "Yes", main='' , 
                                        xlim=c(0,51), ylim=c(-0.6,-0.1),
                                        ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,51,3), seq(0,51,3),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(A16) Den_fruit_shop,26#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Den_fruit_shop,amount = 0.1))


pacnerank.NO2=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=NO2,
                                                   rug=FALSE, col='red', which.class= "Yes", main='' , 
                                                   xlim=c(13,39), ylim=c(-0.6,-0.1),
                                                   ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(13,39,3), seq(13,39,3),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(A17) NO2,27#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$NO2,amount = 0.1))



pacnerank.Daylight=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Daylight,
                                        rug=FALSE, col='red', which.class= "Yes", main='' , 
                                        xlim=c(4,9), ylim=c(-0.6,-0.1),
                                        ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(4,9,1), seq(4,9,1),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(A18) Daylight,28#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Daylight,amount = 0.1))


pacnerank.Den_KFC_McDonald.s=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=Den_KFC_McDonald.s,
                                             rug=FALSE, col='red', which.class= "Yes", main='' , 
                                             xlim=c(0,2.5), ylim=c(-0.6,-0.1),
                                             ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,2.5,0.5), seq(0,2.5,0.5),las = 1,cex.axis = 1.5)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title(xlab= '(A19) Den_KFC_MacDonalds,29#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$Den_KFC_McDonald.s,amount = 0.1))


pacnerank.IR_sweetfood=randomForest::partialPlot(rfbest_acnerank, acnerank_num_test, x.var=IR_sweetfood, 
                                                rug=TRUE, col='red', which.class= "Yes", main='' , 
                                                xlim=c(0,5), ylim=c(-0.6,-0.1),
                                                ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(0,5,1), seq(0,5, 1),labels=c("None","Few","Less","Medium"," More","Many"),las = 1,cex.axis = 1.4)
axis(2,seq(-0.6,-0.1,0.1), sprintf("%0.2f",seq(-0.6,-0.1,0.1)), las = 1,cex.axis = 1.5)
title( xlab='(A20) IR_sweetfood,30#', ylab="",cex.lab = 1.5, font.lab = 2)
rug(jitter(acnerank_num_test$IR_sweetfood,amount = 0.1))
