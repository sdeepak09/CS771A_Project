#\\172.17.127.16 address for printer
## Tonnage data removing error
tonnage_prob_no_error_data=subset(tonnage,tonnage$TOT_CAR_EAST>tonnage$TOT_TRN_EAST & tonnage$TOT_CAR_WEST>tonnage$TOT_TRN_WEST)
View(tonnage_prob_no_error_data)
any(tonnage_prob_no_error_data$TOT_CAR_EAST< tonnage_prob_no_error_data$TOT_TRN_EAST)
any(tonnage_prob_no_error_data$TOT_CAR_WEST< tonnage_prob_no_error_data$TOT_TRN_WEST)
any(tonnage_prob_no_error_data$TOT_CAR_WEST< tonnage_prob_no_error_data$TOT_TRN_WEST)
any(tonnage_prob_no_error_data$TOT_CAR_EAST==0 | tonnage_prob_no_error_data$TOT_CAR_WEST==0 | tonnage_prob_no_error_data$TOT_TRN_EAST==0 | tonnage_prob_no_error_data$TOT_TRN_WEST==0)
which(tonnage_prob_no_error_data$TOT_CAR_EAST==0 | tonnage_prob_no_error_data$TOT_CAR_WEST==0 | tonnage_prob_no_error_data$TOT_TRN_EAST==0 | tonnage_prob_no_error_data$TOT_TRN_WEST==0)
# To remove the data points having 0 in the any of Total cars/train from east/west to west/east
tonnage_prob_no_error_data=subset(tonnage_prob_no_error_data,tonnage_prob_no_error_data$TOT_CAR_EAST!=0 & tonnage_prob_no_error_data$TOT_CAR_WEST!=0 & tonnage_prob_no_error_data$TOT_TRN_EAST!=0 & tonnage_prob_no_error_data$TOT_TRN_WEST!=0)
any(tonnage_prob_no_error_data$TOT_CAR_EAST==0 | tonnage_prob_no_error_data$TOT_CAR_WEST==0 | tonnage_prob_no_error_data$TOT_TRN_EAST==0 | tonnage_prob_no_error_data$TOT_TRN_WEST==0)
# Spiliting the data into two parts training data and validation data
d=sample(1:23299,5000,F)
tonnage_prob_train_data=tonnage_prob_no_error_data[-d,]
tonnage_prob_test_data=tonnage_prob_no_error_data[d,]
dim(tonnage_prob_test_data)
dim(tonnage_prob_train_data)
# to keep the only last 5 columns of above dataframe
tonnage_prob_test_data=tonnage_prob_test_data[,-c(1,2,3,4,5,6)]
dim(tonnage_prob_test_data)
tonnage_prob_train_data=tonnage_prob_train_data[,-c(1,2,3,4,5,6)]
dim(tonnage_prob_train_data)
# Fitting a regression line for the purpose of prediction of missing values in tonnage prob dataset
library(corrplot)
corrplot(cor(tonnage_prob_train_data))
cor(tonnage_prob_train_data)
# Here we can see there is a high correlation in between TOT_CAR_EAST AND TOT_CAR_WEST SO WE MAY USE ONLY ONE in our model
fit=lm(tonnage_prob_train_data$TOT_DFLT_MGT~tonnage_prob_train_data$TOT_CAR_EAST+tonnage_prob_train_data$TOT_TRN_EAST+tonnage_prob_train_data$TOT_TRN_WEST)
summary(fit)
plot(fit$residuals) # Here 3 points are such that they have residual greater than 15 so we can say them outlier
# outlier detection and removing
which(fit$residuals>15)
tonnage_prob_train_data=tonnage_prob_train_data[-c(2354,2636,2638),]
sum(is.na(tonnage_prob_train_data)) # Returns 0 meaning no NAs are there
fit1=lm(tonnage_prob_train_data$TOT_DFLT_MGT~tonnage_prob_train_data$TOT_CAR_EAST+tonnage_prob_train_data$TOT_TRN_EAST+tonnage_prob_train_data$TOT_TRN_WEST)
summary(fit1) # adjusted R-Square 0.8901
plot(fit1$residuals)
predicted_Tot_dflt_mgt=tonnage_prob_test(tonnage_prob_test_data)
head(predicted_Tot_dflt_mgt)
head(tonnage_prob_test_data$TOT_DFLT_MGT)
resi_tonnage_prob_test_data=predicted_Tot_dflt_mgt- tonnage_prob_test_data$TOT_DFLT_MGT
sum2_resi_tonnage_prob_test_data=sum(resi_tonnage_prob_test_data*resi_tonnage_prob_test_data)
sum2_resi_tonnage_prob_test_data
anova(fit1)
9603.4+15415.5+19807.4+5535.8
1-1998.566/50362.1
plot(resi_tonnage_prob_test_data)
which(resi_tonnage_prob_test_data<(-18))
resi_tonnage_prob_test_data[4547]
resi_tonnage_prob_test_data[4547]
tonnage_prob_test_data[4547,]
tonnage_prob_test_data=tonnage_prob_test_data[-4547,]
predicted_Tot_dflt_mgt=tonnage_prob_test(tonnage_prob_test_data)
resi_tonnage_prob_test_data=predicted_Tot_dflt_mgt- tonnage_prob_test_data$TOT_DFLT_MGT
sum2_resi_tonnage_prob_test_data=sum(resi_tonnage_prob_test_data*resi_tonnage_prob_test_data)
sum2_resi_tonnage_prob_test_data=sum(resi_tonnage_prob_test_data*resi_tonnage_prob_test_data)
sum2_resi_tonnage_prob_test_data
plot(resi_tonnage_prob_test_data)
# tonnage problem data having Tot_dflt_mgt in between 0 and 2
tonnage_prob_test_data_0_2=subset(tonnage_prob_test_data,tonnage_prob_test_data$TOT_DFLT_MGT>0 & tonnage_prob_test_data$TOT_DFLT_MGT<2)
r1=sample(tonnage_prob_test_data_0_2$TOT_DFLT_MGT,100000,T)
r1=sample(tonnage_prob_test_data_0_2$TOT_CAR_EAST,100000,T)
r2=sample(tonnage_prob_test_data_0_2$TOT_CAR_WEST,100000,T)
r3=sample(tonnage_prob_test_data_0_2$TOT_CAR_EAST,100000,T)
r4=sample(tonnage_prob_test_data_0_2$TOT_CAR_WEST,100000,T)
r_sample_0_2=cbind(r1,r2,r3,r4)

### Date 23/05/2016
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TRACK_SDTK_NBR,tonnage_prob_no_error_data_removing_outliers$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
cor(tonnage_prob_no_error_data_removing_outliers$TOT_TRN_EAST,tonnage_prob_no_error_data_removing_outliers$TOT_TRN_WEST)
# here we have seen the correlation equal to -0.16 but this is not correct as the relation
## between x nad y was not linear but is something like quardatic see the below graph
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_TRN_EAST,tonnage_prob_no_error_data_removing_outliers$TOT_TRN_WEST,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage$TOT_DFLT_MGT,tonnage$TOT_CAR_EAST)
scatter.smooth(tonnage_prob_no_error_data$TOT_DFLT_MGT,tonnage_prob_no_error_data$TOT_CAR_EAST)
scatter.smooth(tonnage_prob_no_error_data$TOT_DFLT_MGT,tonnage_prob_no_error_data$TOT_CAR_EAST)
scatter.smooth(tonnage_prob_no_error_data$TOT_DFLT_MGT,tonnage_prob_no_error_data$TOT_CAR_EAST, lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_no_error_data$TOT_DFLT_MGT,tonnage_prob_no_error_data$TOT_CAR_WEST, lpars =list(col = "red", lwd = 3, lty = 3))
library(MASS)
boxcox(lm(tonnage_prob_no_error_data_removing_outliers$TOT_DFLT_MGT~tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,data=tonnage_prob_no_error_data_removing_outliers),lambda=seq(0,1,by=.1))
scatter.smooth(z,tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(z,tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,z,lpars =list(col = "red", lwd = 3, lty = 3))
z=tonnage_prob_no_error_data_removing_outliers$TOT_DFLT_MGT^1.2
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,z,lpars =list(col = "red", lwd = 3, lty = 3))
z=tonnage_prob_no_error_data_removing_outliers$TOT_DFLT_MGT^1.3
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,z,lpars =list(col = "red", lwd = 3, lty = 3))
z=tonnage_prob_no_error_data_removing_outliers$TOT_DFLT_MGT^1.5
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,z,lpars =list(col = "red", lwd = 3, lty = 3))
z=tonnage_prob_no_error_data_removing_outliers$TOT_DFLT_MGT^2
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,z,lpars =list(col = "red", lwd = 3, lty = 3))
z=tonnage_prob_no_error_data_removing_outliers$TOT_DFLT_MGT^2.5
z=tonnage_prob_no_error_data_removing_outliers$TOT_DFLT_MGT^2.5
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,z,lpars =list(col = "red", lwd = 3, lty = 3))
z=(tonnage_prob_no_error_data_removing_outliers$TOT_DFLT_MGT^1.15)-1
z/1.15
z=z/1.15
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,z,lpars =list(col = "red", lwd = 3, lty = 3))
head(m)
m=subset(tonnage_prob_no_error_data_removing_outliers,tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST<10000)
scatter.smooth(m$TOT_CAR_EAST,m$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth
m=subset(tonnage_prob_no_error_data_removing_outliers,tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST<5000)
scatter.smooth(m$TOT_CAR_EAST,m$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(m0$TOT_CAR_EAST,m0$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
m0=subset(tonnage_prob_no_error_data_removing_outliers,tonnage_prob_no_error_data_removing_outliers$TRACK_SDTK_NBR==0)
m0_less_than_10000=subset(tonnage_prob_no_error_data_removing_outliers,tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST<10000)
scatter.smooth(m0$TOT_DFLT_MGT,m0$TOT_CAR_EAST)
scatter.smooth(m0$TOT_DFLT_MGT,m0$TOT_CAR_EAST,lpars =list(col = "red", lwd = 3, lty = 3))
a1=log(m0$TOT_DFLT_MGT)
m0_greater_10000=subset(tonnage_prob_no_error_data_removing_outliers,tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST>10000)
scatter.smooth(m0_greater_10000$TOT_DFLT_MGT,m0_greater_10000$TOT_CAR_EAST,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(m0_greater_10000$TOT_DFLT_MGT,m0_greater_10000$TOT_CAR_EAST,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(m0$TOT_CAR_EAST,m0$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,a1,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,a1,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(m0$TOT_CAR_EAST,a1,lpars =list(col = "red", lwd = 3, lty = 3))
plot(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,tonnage_prob_no_error_data_removing_outliers$TOT_TRN_EAST)
plot(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,tonnage_prob_no_error_data_removing_outliers$TOT_TRN_EAST)
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,tonnage_prob_no_error_data_removing_outliers$TOT_TRN_EAST,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_no_error_data_removing_outliers$TOT_CAR_EAST,tonnage_prob_no_error_data_removing_outliers$TOT_TRN_EAST,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(m0$TOT_CAR_EAST,m0$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(m0_less_than_10000$TOT_CAR_EAST,m0_less_than_10000$TOT_TRN_EAST,lpars =list(col = "red", lwd = 3, lty = 3))
### Just trying to find out the linear relation in betrween tonnage and cars east
### Date 23/05/2016 16:15 
dim(tonnage_prob_no_error_data_removing_outliers)
23293*0.20
d=sample(1:nrow(tonnage_prob_no_error_data_removing_outliers),4600,replace = F)
tonnage_prob_train_data=tonnage_prob_no_error_data_removing_outliers[-d,]
tonnage_prob_test_data=tonnage_prob_no_error_data_removing_outliers[d,]
dim(tonnage_prob_test_data)
dim(tonnage_prob_train_data)
tonnage_prob_train_data_line_1=subset(tonnage_prob_train_data,tonnage_prob_train_data$LINE_SEG_NBR==1)
tonnage_prob_train_data_line_2=subset(tonnage_prob_train_data,tonnage_prob_train_data$LINE_SEG_NBR==2)
tonnage_prob_train_data_line_3=subset(tonnage_prob_train_data,tonnage_prob_train_data$LINE_SEG_NBR==3)
tonnage_prob_train_data_line_4=subset(tonnage_prob_train_data,tonnage_prob_train_data$LINE_SEG_NBR==4)
tonnage_prob_train_data_line_4=subset(tonnage_prob_train_data,tonnage_prob_train_data$LINE_SEG_NBR==4)
scatter.smooth(tonnage_prob_train_data_line_1$TOT_CAR_EAST,tonnage_prob_train_data_line_1$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_train_data_line_2$TOT_CAR_EAST,tonnage_prob_train_data_line_2$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_train_data_line_3$TOT_CAR_EAST,tonnage_prob_train_data_line_3$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_train_data_line_4$TOT_CAR_EAST,tonnage_prob_train_data_line_4$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
tonnage_prob_train_data_line_4_CarEastGreater12000=subset(tonnage_prob_train_data_line_4,tonnage_prob_train_data_line_4$TOT_CAR_EAST>12000)
scatter.smooth(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_EAST,tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_DFLT_MGT,lpars =list(col = "red", lwd = 3, lty = 3))
## Linear trend abtained for TOT_DFLT_GMT and TOT_CAR_EAST when TOT_CAR_EAST >12000
fit_line4_CarEastGreater_12000=lm(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_DFLT_MGT~tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_EAST+tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_TRN_EAST+tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_TRN_WEST)
anova(fit_line4_CarEastGreater_12000)
plot(fit_line4_CarEastGreater_12000$residuals)
mean(fit_line4_CarEastGreater_12000$residuals)
plot(fit_line4_CarEastGreater_12000$fitted.values,fit_line4_CarEastGreater_12000$residuals)
scatter.smooth(fit_line4_CarEastGreater_12000$fitted.values,fit_line4_CarEastGreater_12000$residuals,lpars =list(col = "red", lwd = 3, lty = 3))
summary(fit_line4_CarEastGreater_12000)
summary(fit_line4_CarEastGreater_12000$fitted.values)
summary(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_DFLT_MGT)
summary(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_DFLT_MGT)



##Date 24/05/2016 
summary(fit_line4_CarEastGreater_12000)
cor(tonnage_prob_train_data_line_4$TOT_DFLT_MGT,tonnage_prob_train_data_line_4$TOT_TRN_EAST)
cor(tonnage_prob_train_data_line_4$TOT_DFLT_MGT,tonnage_prob_train_data_line_4$TOT_TRN_WEST)
corrplot(cor(tonnage_prob_train_data_line_4_CarEastGreater12000[,c(7:11)]))
cor(tonnage_prob_train_data_line_4_CarEastGreater12000[,c(7:11)])
scatter.smooth(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_EAST,tonnage_prob_train_data_line_4$TOT_CAR_WEST)
scatter.smooth(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_EAST,tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_WEST)
scatter.smooth(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_EAST,tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_WEST,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_EAST,tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_TRN_WEST,lpars =list(col = "red", lwd = 3, lty = 3))
scatter.smooth(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_EAST,tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_TRN_WEST,lpars =list(col = "red", lwd = 3, lty = 3))
fit_line4_CarEastGreater_12000=lm(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_DFLT_MGT~tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_EAST+tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_CAR_WEST+tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_TRN_EAST+tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_TRN_WEST)
summary(fit_line4_CarEastGreater_12000) ### can see that there is positive correlation in between tonnage and tot no of trains east but even then also we have -ve coefficient so we can say that this is happenning due to multicollinearity
dim(tonnage_prob_train_data_line_4_CarEastGreater12000)
xy=stdd1(tonnage_prob_train_data_line_4_CarEastGreater12000[,c(7:10)])
head(xy)
vdm1=colldiag(xy, scale = F, center = F, add.intercept = F)
vdm1
xy1=xy[,-4]
dim(xy1)
vdm2=colldiag(xy1, scale = F, center = FALSE, add.intercept = F)
vdm2
our_vif(xy1)
xy2=xy1[,-2]
vdm3=colldiag(xy2, scale = F, center = FALSE, add.intercept = F)
vdm3
our_vif(xy2)
fit_line4_CarEastGreater_12000_TRN_EAST=lm(tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_DFLT_MGT~tonnage_prob_train_data_line_4_CarEastGreater12000$TOT_TRN_EAST)
summary(fit_line4_CarEastGreater_12000_TRN_EAST)
tonnage_prob_test_data_line4_CarsEastGreater_12000=subset(tonnage_prob_test_data,tonnage_prob_test_data$LINE_SEG_NBR==4 & tonnage_prob_test_data$TOT_CAR_EAST>12000)
View(tonnage_prob_test_data_line4_CarsEastGreater_12000)
z=tonnage_prediction(tonnage_prob_test_data_line4_CarsEastGreater_12000[,9])
head(z)
plot(z- tonnage_prob_test_data_line4_CarsEastGreater_12000[,11])
a=TRN_EAST_prediction(tonnage_prob_test_data_line4_CarsEastGreater_12000$TOT_DFLT_MGT)
summary(a)
summary(tonnage_prob_test_data_line4_CarsEastGreater_12000$TOT_TRN_EAST)
plot(a- tonnage_prob_test_data_line4_CarsEastGreater_12000$TOT_TRN_EAST)
summary(abs(a-tonnage_prob_test_data_line4_CarsEastGreater_12000$TOT_TRN_EAST))
summary(tonnage_prob_test_data_line4_CarsEastGreater_12000$TOT_TRN_EAST)
tonnage_prob_train_data_line_4_CarEastLess_12000=subset(tonnage_prob_train_data_line_4,tonnage_prob_train_data_line_4$TOT_CAR_EAST<12000)
dim(tonnage_prob_train_data_line_4_CarEastLess_12000) #should be 4551


##### Starting of missing Values Finding Systematically
## Date 25-05-2016
# my aim is to find the missing values excluding those which have all the zero entries in TOT_CARS_EAST ..
length(which(tonnage$TOT_CAR_EAST<tonnage$TOT_TRN_EAST & tonnage$TOT_CAR_EAST!=0 & tonnage$TOT_TRN_EAST!=0))
# 71 values are such that they have total no of car east is less than total no of trains east
# we will make both of them zero
a=which(tonnage$TOT_CAR_EAST<tonnage$TOT_TRN_EAST & tonnage$TOT_CAR_EAST!=0 & tonnage$TOT_TRN_EAST!=0)
# vector having row no of those rows in which above condition is being satisfied
# Make them equal to 0
tonnage$TOT_CAR_EAST[a]=0
tonnage$TOT_TRN_EAST[a]=0
a1=which(tonnage$TOT_CAR_WEST<tonnage$TOT_TRN_WEST & tonnage$TOT_CAR_WEST!=0 & tonnage$TOT_TRN_WEST!=0)
head(tonnage[a1,])
tail(tonnage[a1,]) # just to check the what we are doing is correct or not 
length(a1)
# 2054 data point are satisfying the above condition make them zero
tonnage$TOT_CAR_WEST[a1]=0
tonnage$TOT_TRN_WEST[a1]=0
head(tonnage[a1,])
tail(tonnage[a1,])
# so upto now we have found the data points which were violating the condition that the no of cars can't be less than the no of trains in any side either east or west
# and also put them equal to zero
length(which(tonnage$TOT_CAR_EAST==0 | tonnage$TOT_CAR_WEST==0 | tonnage$TOT_TRN_EAST==0 | tonnage$TOT_TRN_WEST==0))
# we got 13518
length(which(tonnage$TOT_CAR_EAST==0 & tonnage$TOT_CAR_WEST==0 & tonnage$TOT_TRN_EAST==0 & tonnage$TOT_TRN_WEST==0))
# we got 9611
dim(subset(tonnage,tonnage$YEAR==2007 | tonnage$YEAR==2008))
# we got 9515  11
length(which(tonnage$TOT_CAR_EAST==0 & tonnage$TOT_CAR_WEST==0 & tonnage$TOT_TRN_EAST==0 & tonnage$TOT_TRN_WEST==0 & tonnage$YEAR!=2007 & tonnage$YEAR!=2008))
# we got 96  9515+96=9611  which confirms that in other than year 2007 and 2008 we have 96 such data points at which we have no value on CARs and TRNS
# 13518-9611=3907 data points at which we have not all zero but some of them are zero
# so my aim is to first predict these data points by using some techniques
length(which(tonnage$TOT_CAR_EAST==0 & tonnage$TOT_CAR_WEST==0 & tonnage$TOT_TRN_EAST!=0 & tonnage$TOT_TRN_WEST!=0))
# got 1616 showing that we have 1616 data points which have TOT_Cars =0 but TOT_TRNS has input 
length(which(tonnage$TOT_CAR_EAST==0 & tonnage$TOT_CAR_WEST==0 & tonnage$TOT_TRN_EAST==0 & tonnage$TOT_TRN_WEST!=0))
# got 141 such data points at we have only data on TOT_TRN_WEST
length(which(tonnage$TOT_CAR_EAST==0 & tonnage$TOT_CAR_WEST!=0 & tonnage$TOT_TRN_EAST!=0 & tonnage$TOT_TRN_WEST!=0))
# 0 shows there is no data point at which only TOT_CARS_EAST is missing
length(which(tonnage$TOT_CAR_EAST!=0 & tonnage$TOT_CAR_WEST==0 & tonnage$TOT_TRN_EAST!=0 & tonnage$TOT_TRN_WEST!=0))
# 10 there are 10 data point at which only TOT_CARS_EAST is missing
length(which(tonnage$TOT_CAR_EAST!=0 & tonnage$TOT_CAR_WEST!=0 & tonnage$TOT_TRN_EAST==0 & tonnage$TOT_TRN_WEST!=0))
# 1 data points at which there is only TOT_TRNS_EAST is missing
length(which(tonnage$TOT_CAR_EAST!=0 & tonnage$TOT_CAR_WEST!=0 & tonnage$TOT_TRN_EAST!=0 & tonnage$TOT_TRN_WEST==0))
# 143 data points at which only TOT_TRNS_WEST is missing
# the problem is that sometimes a train carry only 3 cars and some times 1 train carry 111 cars see row no 749 and 1925
# Now we will divide the tonnage data into more subsets having some characteristics like as no zero, TOT car east is zero etc
tonnage_prob_no_zero=subset(tonnage,tonnage$TOT_CAR_EAST!=0 & tonnage$TOT_CAR_WEST!=0 & tonnage$TOT_TRN_EAST!=0 & tonnage$TOT_TRN_WEST!=0)
dim(tonnage_prob_no_zero)
# 23300 11 shows that 23300 data points have no zeros
dim(tonnage)
# 36818 11 
# 36818-23300=13518 data points have atleast one zero
tonnage_atleast_one_zero=subset(tonnage,tonnage$TOT_CAR_EAST==0 | tonnage$TOT_CAR_WEST==0 | tonnage$TOT_TRN_EAST==0 | tonnage$TOT_TRN_WEST==0)
dim(tonnage_atleast_one_zero)
# 13518 11 confirms the above 
tonnage_prob_all_zero=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_prob_all_zero) ## 9611  11  9611 data points are having all zeros
tonnage_only_Cars_east_0=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_Cars_east_0) # 0 11 no data having only cars east 0
tonnage_only_Cars_west_0=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_Cars_west_0) # 10 11    10 rows has only cars west missing
tonnage_only_train_east_0=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_train_east_0) # 1 11 only one have trains east 0
tonnage_only_train_west_0=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_only_train_west_0)  # 143 11  143 rows have only trains west missing
tonnage_only_cars_missing_both_sides=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_cars_missing_both_sides)## 1616 11   1616 rows have cars missing both sides 
tonnage_only_trains_missing_both_sides=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_only_trains_missing_both_sides) ## 1  11 only one data which has trains both sides are missing
tonnage_only_CarsWest_and_TrainWest_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_CarsWest_and_TrainWest_missing) # 1984 11   1984 have not satif=sfied the above conditions
tonnage_only_CarsEast_and_TrainEast_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_CarsEast_and_TrainEast_missing)  # 1  11    only one data 
tonnage_prob_CarsEastAndWest_TrainsEast_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_prob_CarsEastAndWest_TrainsEast_missing) # 141  11 
tonnage_prob_CarsWest_TrainsEastAndWest_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_prob_CarsWest_TrainsEastAndWest_missing)  #2  11   
tonnage_CarsEastAndWest_TrainsWest_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_CarsEastAndWest_TrainsWest_missing)  # 7  11 
tonnage_CarsEast_TrainsEastAndWest_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_CarsEast_TrainsEastAndWest_missing) # 1 11

# We will find the missing values which has only one missing value
# we will first see the correlation matrix
cor(tonnage_prob_no_zero[,c(7:11)]) # Can see there is high correlation in between TOT_CAR_EAST and TOT_CAR_WEST
# and by scatter plot we can see that there is almost linear realtion in between TOT_CAR_EAST and TOT_CAR_WEST
scatter.smooth(tonnage_prob_no_zero$TOT_CAR_EAST,tonnage_prob_no_zero$TOT_CAR_WEST,lpars =list(col = "red", lwd = 3, lty = 3))
# we will predict the TOT_CAR_WEST by using the following linear regression model
fit_for_CarsWest_when_only_CarsWest_0=lm(tonnage_prob_no_zero$TOT_CAR_WEST~tonnage_prob_no_zero$TOT_CAR_EAST)
summary(fit_for_CarsWest_when_only_CarsWest_0)
CarsWest_prediction=function(x){
y=c()
for(i in 1:nrow(x)){
y[i]=746.3+0.8440*x[i,7]
}
return(y)
}
predicted_CarsWest=CarsWest_prediction(tonnage_only_Cars_west_0)
predicted_CarsWest ### 767.40  767.40  767.40  767.40  767.40  763.18  763.18  763.18  763.18  763.18 
length(predicted_CarsWest) # 10
## one more prob as we can see by the following graph that there are only 4 rows for which we have tonnage values is very high so I ommitted that 
tonnage_prob_no_zero=subset(tonnage_prob_no_zero,tonnage_prob_no_zero$TOT_DFLT_MGT<16)
dim(tonnage_prob_no_zero) # 23300-4=23296
# as we have seen that no one is following a linear or quardatic patterns so we have find that if we add 
# total no of trains east and west then total is becoming almost linear with tonnage so we have used this fact in finding the total no of cars
# and also now our aim is to predicted the total no of cars and trains where any values are zero so we fitted these models and predicted the following
# here we used the strategy that first fit the model in between tonnage and trains totals and hence find the predicted trains totals 
#After finding the predicted trains totals we make a model tonnage as response variable and train totals and cars totals as regressors and from here to get the value of predicted value of cars totals
# We will fist subset the data in two parts Train data and Test data in 80-20 % 
d=sample(1:length(tonnage_prob_no_zero),4600)
tonnage_prob_no_zero_train_data=tonnage_prob_no_zero[-d,]
tonnage_prob_no_zero_test_data=tonnage_prob_no_zero[d,]
dim(tonnage_prob_no_zero_train_data)
dim(tonnage_prob_no_zero_test_data)
Trains_total_east_west_train_data=tonnage_prob_no_zero_train_data$TOT_TRN_EAST+tonnage_prob_no_zero_train_data$TOT_TRN_WEST
Cars_total_east_west_train_data=tonnage_prob_no_zero_train_data$TOT_CAR_EAST+tonnage_prob_no_zero_train_data$TOT_CAR_WEST
length(Cars_total_east_west_train_data)
Trains_total_east_west_test_data=tonnage_prob_no_zero_test_data$TOT_TRN_EAST+tonnage_prob_no_zero_test_data$TOT_TRN_WEST
Cars_total_east_west_test_data=tonnage_prob_no_zero_test_data$TOT_CAR_EAST+tonnage_prob_no_zero_test_data$TOT_CAR_WEST
length(Cars_total_east_west_test_data)
fit_Trains_total=lm(Trains_total_east_west_train_data~tonnage_prob_no_zero_train_data$TOT_DFLT_MGT)
summary(fit_Trains_total)
train_total_prediction=function(x){
y=c()
for(i in 1:length(x)){
y[i]=91.4845+130.3542*x[i]
}
return(y)
}
predicted_train_totals_test_data=train_total_prediction(tonnage_prob_no_zero_test_data$TOT_DFLT_MGT)
summary(predicted_train_totals_test_data)
head(predicted_train_totals_test_data)
head(Trains_total_east_west_test_data)
predicted_train_totals=train_total_prediction(tonnage_atleast_one_zero$TOT_DFLT_MGT)
summary(predicted_train_totals)

## Date 26-05-2016
# Now we will substitute the predicted value in the tonnage data and make a new column in tonnage data of total trains 
a2=which(tonnage$TOT_CAR_EAST==0 | tonnage$TOT_CAR_WEST==0 | tonnage$TOT_TRN_EAST==0 | tonnage$TOT_TRN_WEST==0)
head(a2)
Trains_total_tonnage[a2]=predicted_train_totals
length(Trains_total_tonnage) # EQUAL TO ROW NO OF TONNAGE DATA
tonnage_new=data.frame(tonnage,Trains_total_tonnage)
View(tonnage_new)
dim(tonnage_new)# 36818 12 CORRECT



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$







#################
# Now we wil do the above job by time series analysis

## Date 26-05-2016
# Now I have to read the tonnage file again as I have made changes in original file named as tonnage
tonnage1=read.csv("tonnage.csv")
# to see a better look of data I sorted the data first by line segment no after that by track after that by mile post start
tonnage_line_track_mileStart=tonnage1[order(tonnage1$LINE_SEG_NBR,tonnage1$TRACK_SDTK_NBR,tonnage1$MILEPOST_START),]
View(tonnage_line_track_mileStart)
# here further I also want to sort the by year and then month also
tonnage_line_track_mileStart_year_month=tonnage1[order(tonnage1$LINE_SEG_NBR,tonnage1$TRACK_SDTK_NBR,tonnage1$MILEPOST_START,tonnage1$YEAR,tonnage1$MONTH),]
View(tonnage_line_track_mileStart_year_month) ##36818 ***
# There is some difference in mile post start at the 3rd place after decimal
# So I rounded off the milepost start upto 1 decimal place
tonnage_line_track_mileStart_year_month$MILEPOST_START=round(tonnage_line_track_mileStart_year_month$MILEPOST_START, digits = 1)
tonnage_line_track_mileStart_year_month$MILEPOST_END=round(tonnage_line_track_mileStart_year_month$MILEPOST_END, digits = 1)
View(tonnage_line_track_mileStart_year_month)
tonnage_line_track_mileStart_year_month=tonnage_line_track_mileStart_year_month[order(tonnage_line_track_mileStart_year_month$LINE_SEG_NBR,tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR,tonnage_line_track_mileStart_year_month$MILEPOST_START,tonnage_line_track_mileStart_year_month$YEAR,tonnage_line_track_mileStart_year_month$MONTH),]
# I want to remove the data from year 2007 and 2008
tonnage_line_track_mileStart_year_month=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$YEAR!=2007 & tonnage_line_track_mileStart_year_month$YEAR!=2008)
# As here we have made an assumption that no of cars east can't be less than no of trains east and same for west
# So now I will find the Row no of those rows in which above condition is being satisfied
dim(tonnage_line_track_mileStart_year_month)  # 27303  11 as I have removed the data for year 2007 and 2008
# Below is the vector having row no of those rows which have train east greater than cars east
d2=which(tonnage_line_track_mileStart_year_month$TOT_CAR_EAST<tonnage_line_track_mileStart_year_month$TOT_TRN_EAST & tonnage_line_track_mileStart_year_month$TOT_CAR_EAST!=0)
length(d2) ## 71 rows are satisfying the above condition
# Below is the vector having row no of those rows which have train west greater than cars west
d3=which(tonnage_line_track_mileStart_year_month$TOT_CAR_WEST<tonnage_line_track_mileStart_year_month$TOT_TRN_WEST & tonnage_line_track_mileStart_year_month$TOT_CAR_WEST!=0)
length(d3)  ## 2054 rows are satisfying the above condition
# Now I make those entries equal to 0 who are satisfying the above condition
# we will do the above for no of trains east and west only not for cars 
tonnage_line_track_mileStart_year_month$TOT_TRN_EAST[d2]=NA
head(tonnage_line_track_mileStart_year_month[d2,],10)
tonnage_line_track_mileStart_year_month$TOT_TRN_WEST[d3]=NA
head(tonnage_line_track_mileStart_year_month[d3,],10)
## Now if there is any 0 in trains east and trains west then I will make them as NA
d4=which(tonnage_line_track_mileStart_year_month$TOT_TRN_EAST==0) # row no of those rows in which total train east is zero
length(d4) ## 172
d5=which(tonnage_line_track_mileStart_year_month$TOT_TRN_WEST==0)
length(d5) ## 180
# Making them NA
tonnage_line_track_mileStart_year_month$TOT_TRN_EAST[d4]=NA
tonnage_line_track_mileStart_year_month$TOT_TRN_WEST[d5]=NA
# Now we will do Train eaat and trains west total
total_trains_east_west=tonnage_line_track_mileStart_year_month$TOT_TRN_EAST+tonnage_line_track_mileStart_year_month$TOT_TRN_WEST
sum(is.na(total_trains_east_west))  # 2376 missing values
2376/27303=0.0870234 # Shows that there are about 9% data is missing
# now we will store the rows in which we have total trains east and west is NA
d6=which(is.na(total_trains_east_west))
length(d6) ## 2376 missing values
# Now we will add the column total tarins east west in the data frame tonnage_line_...month
tonnage_line_track_mileStart_year_month=data.frame(tonnage_line_track_mileStart_year_month,total_trains_east_west)
# Now we will find the record on that position at which we have missing values
tonnage_line_track_mileStart_year_month[d6[1],] ## can see that there is missing value at starting MilePost= 28.9
# collecting record on that position
data_1_0_28.9_32.0=tonnage_line_track_mileStart_year_month[which(tonnage_line_track_mileStart_year_month$MILEPOST_START==28.9 & tonnage_line_track_mileStart_year_month$MILEPOST_END==32.0 & tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==1 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0),]
data_1_0_28.9_32.0
# making the total_trains_east_west as a time series data
ts_1_0_28.9_32.0=ts(data_1_0_28.9_32.0$total_trains_east_west,frequency = 12,start = c(2009))
ts_1_0_28.9_32.0
# Now we will plot this time series data
plot.ts(ts_1_0_28.9_32.0)
# now we will predict the values of NA's using na.spline()
na.spline(ts_1_0_28.9_32.0, maxgap = 4)
ceiling(na.spline(ts_1_0_28.9_32.0))
ts_1_0_28.9_32.0_pred=ceiling(na.spline(ts_1_0_28.9_32.0))
# contains the values of Total number of train without any missing values
# Now we will do the same process for the missing value at the other position
tonnage_line_track_mileStart_year_month[d6[2],] ## can see that there is missing value at starting MilePost= 28.9
data_1_0_32_40.7=tonnage_line_track_mileStart_year_month[which(tonnage_line_track_mileStart_year_month$MILEPOST_START==32 & tonnage_line_track_mileStart_year_month$MILEPOST_END==40.7 & tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==1 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0),]
data_1_0_32_40.7
ts_1_0_32_40.7=ts(data_1_0_32_40.7$total_trains_east_west,frequency = 12,start = c(2009))
ts_1_0_32_40.7
plot.ts(ts_1_0_32_40.7)
na.spline(ts_1_0_32_40.7)
ts(ceiling(na.spline(ts_1_0_32_40.7)),frequency = 12,start = 2009)
plot.ts(ts(ceiling(na.spline(ts_1_0_32_40.7)),frequency = 12,start = 2009))

# we have made a function but is not working as at some position mile posts are given in intersected form so I will make a seperate data frame for those values


# Line segment 3 and track segment 0 has good data on tonnage as there is no missing value

# prediction for (3,1,4.6,5.8)
# will not find out as there is no data in year 2009

## prediction for (3,1,7.3,12.9)
data_for_3_1_7.3_12.9=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==3&tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==1 & tonnage_line_track_mileStart_year_month$MILEPOST_START==7.3)
data_for_3_1_7.3_12.9
ts_3_1_7.3_12.9=ts(data_for_3_1_7.3_12.9$total_trains_east_west,frequency = 12,start=2009)
ts_3_1_7.3_12.9
ts_3_1_7.3_12.9_pred=na.spline(ts_3_1_7.3_12.9)
ts(ts_3_1_7.3_12.9_pred,frequency = 12,start=2009)  # it may be the case that we will not use the data for 2014 as there is large missing values

#prediction for (3,1,29.3,37.1)
data_for_3_1_29.3_37.1=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==3&tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==1 & tonnage_line_track_mileStart_year_month$MILEPOST_START==29.3)
data_for_3_1_29.3_37.1
ts_3_1_29.3_37.1=ts(data_for_3_1_29.3_37.1$total_trains_east_west,frequency = 12,start=2009)
ts_3_1_29.3_37.1
ts_3_1_29.3_37.1_pred=na.spline(ts_3_1_29.3_37.1,maxgap = 5)
ts(ceiling(ts_3_1_29.3_37.1_pred),frequency = 12,start=2009)


#prediction of (3,1,37.1,46.0)
#### here have seen an error (may not be but we have to confirm) that we have data for 37.1_46.0 and 37.1_41.5 but in the same month we have data conflict
data_for_3_1_37.1_46.0=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==3 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==1 & tonnage_line_track_mileStart_year_month$MILEPOST_END==46.0 & (tonnage_line_track_mileStart_year_month$MILEPOST_START==29.3 | tonnage_line_track_mileStart_year_month$MILEPOST_START==37.1 ))
data_for_3_1_37.1_46.0=data_for_3_1_37.1_46.0[order(data_for_3_1_37.1_46.0$YEAR,data_for_3_1_37.1_46.0$MONTH),]
data_for_3_1_37.1_46.0
ts_3_1_37.1_46.0=ts(data_for_3_1_37.1_46.0$total_trains_east_west,frequency = 12,start = 2009)
ts_3_1_37.1_46.0
ts_3_1_37.1_46.0_pred=ts(ceiling(na.spline(ts_3_1_37.1_46.0,maxgap = 4)),frequency=12,start=2009)
ts_3_1_37.1_46.0_pred

# prediction for (3,1,236.3,237.2)
data_for_3_1_236.3_237.2=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==3&tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==1 & tonnage_line_track_mileStart_year_month$MILEPOST_START==236.3)
ts_3_1_236.3_237.2=ts(data_for_3_1_236.3_237.2$total_trains_east_west,frequency = 12,start=2009)
ts_3_1_236.3_237.2
ts_3_1_236.3_237.2_pred=ts(ceiling(na.spline(ts_3_1_236.3_237.2,maxgap = 5)),frequency=12,start=2009)
ts_3_1_236.3_237.2_pred

# prediction for (3,1,237.2,246.2)
# we can't interpolate as we have data for 2012 to 2014 and among those we have only one data point is there

# and there are also so much position at which we have data for only 1 year or 2 year

#prediction of (3,2,7.3,13.2)
data_for_3_2_7.3_13.2=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==3&tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==2 & tonnage_line_track_mileStart_year_month$MILEPOST_START==7.3)
data_for_3_2_7.3_13.2
ts_3_2_7.3_13.2=ts(data_for_3_2_7.3_13.2$total_trains_east_west,frequency = 12,start=2009)
ts_3_2_7.3_13.2
ts_3_2_7.3_13.2_pred=na.spline(ts_3_2_7.3_13.2,maxgap = 4)
ts(ceiling(ts_3_2_7.3_13.2_pred),frequency = 12,start=2009)

# prediction for (3,2,236.3,237.2)
# we can remove these NA's as we have almost same data for 237.2-246.2 and instead of taking 236.3-237.2 take the segment as 236.3-246.2


# predicted value for (4,0,314.6,317.5)
data_for_314.6_317.5=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==4 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0 & tonnage_line_track_mileStart_year_month$MILEPOST_END==317.5 & tonnage_line_track_mileStart_year_month$MILEPOST_START==311.9 | tonnage_line_track_mileStart_year_month$MILEPOST_START==314.6 )
data_for_314.6_317.5
data_for_314.6_317.5=data_for_311.9_317.5[order(data_for_311.9_317.5$YEAR,data_for_311.9_317.5$MONTH),]
data_for_314.6_317.5
ts_4_0_314.6_317.5=data_for_314.6_317.5$total_trains_east_west
ts_4_0_314.6_317.5=ts(ts_4_0_314.6_317.5,frequency = 12,start = 2009)
ts(na.spline(ts_4_0_314.6_317.5),frequency = 12,start = 2009)
ts_4_0_314.6_317.5_pred=na.spline(ts_4_0_314.6_317.5) # prediction looking good
 # prediction for(4,0,311.9,314.6)
data_for_4_0_311.9_314.6=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==4&tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0 & tonnage_line_track_mileStart_year_month$MILEPOST_START==311.9)
data_for_4_0_311.9_314.6
ts_4_0_311.9_314.6=ts(data_for_4_0_311.9_314.6$total_trains_east_west,frequency = 12,start=2009)
ts_4_0_311.9_314.6_pred=na.spline(ts_4_0_311.9_314.6)
ts(ts_4_0_311.9_314.6_pred,frequency = 12,start=2009)

# prediction for (4,0,410.6)
data_for_4_0_410.6_412.7=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==4&tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0 & tonnage_line_track_mileStart_year_month$MILEPOST_START==410.6)
data_for_4_0_410.6_412.7
ts_4_0_410.6_412.7=ts(data_for_4_0_410.6_412.7$total_trains_east_west,frequency = 12,start=2009)
ts(ts_4_0_410.6_412.7_pred,frequency = 12,start=2009)
ts_4_0_410.6_412.7

# prediction for (4,0,411.4,412.7)
data_for_411.4_412.7=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==4 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0 & tonnage_line_track_mileStart_year_month$MILEPOST_END==412.7 & tonnage_line_track_mileStart_year_month$MILEPOST_START==410.6 | tonnage_line_track_mileStart_year_month$MILEPOST_START==411.4 )
data_for_411.4_412.7
data_for_411.4_412.7=data_for_411.4_412.7[order(data_for_411.4_412.7$YEAR,data_for_411.4_412.7$MONTH),]
data_for_411.4_412.7
ts_4_0_411.4_412.7=data_for_411.4_412.7$total_trains_east_west
ts_4_0_411.4_412.7=ts(ts_4_0_411.4_412.7,frequency = 12,start = 2009)
ts_4_0_411.4_412.7=ts(ts_4_0_411.4_412.7,frequency = 12,start = 2009)
ts_4_0_411.4_412.7_pred=na.spline(ts_4_0_411.4_412.7) # prediction looking good
ts_4_0_411.4_412.7_pred

#prediction for (4,0,461.2,462.6)
# we will not find missing values for this as it has missing values in the last and having 6 in numbers

# prediction for (4,0,463.1,468.6)
#will not find out as this position has only observation in 2014


















## I have made a function which will take a data frame as its argument and returns a dataframe which have interpolated values of total trains
## As In this we have taken care of all the things but we will have to do manually at some positions as if only 1 yr data is available then this will put all the nas= mean of the data of that year 
## Which is not correct as we cant interpolate 4/5 year data based on one year data and taking its mean only so we will manually make them as NA and may use regression Analysis to find out these NAs
## As We have so many spots where we have all the data missing and these were creating problem in running our function na.spline so we have removed those position so that our function can run without any error
tonnage_line_track_mileStart_year_month=tonnage_line_track_mileStart_year_month[-which(tonnage_line_track_mileStart_year_month$MILEPOST_START==86.1 & tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==2 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==1),]
dim(tonnage_line_track_mileStart_year_month)##27300 12
tonnage_line_track_mileStart_year_month=tonnage_line_track_mileStart_year_month[-which(tonnage_line_track_mileStart_year_month$MILEPOST_START==26.2 & tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==2 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==2),]
tonnage_line_track_mileStart_year_month=tonnage_line_track_mileStart_year_month[-which(tonnage_line_track_mileStart_year_month$MILEPOST_START==83.1 & tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==2 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==2),]
tonnage_line_track_mileStart_year_month=tonnage_line_track_mileStart_year_month[-which(tonnage_line_track_mileStart_year_month$MILEPOST_START==86.1 & tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==2 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==2),]
tonnage_line_track_mileStart_year_month=tonnage_line_track_mileStart_year_month[-which(tonnage_line_track_mileStart_year_month$MILEPOST_START==86.5 & tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==2 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==2),]
dim(tonnage_line_track_mileStart_year_month) ## 27290  12

total_train_pred=function(x){
  d6=c()
  Row_names=list()
  result=list()
  maxmin=list()
  data1=data.frame()
  d6=which(is.na(tonnage_line_track_mileStart_year_month$total_trains_east_west))
  i=1
  library(zoo)
  a=tonnage_line_track_mileStart_year_month[d6,]
  m=900 # any no which is greater than 600
  while(i<(length(d6)+1)){
    if(m!=a[i,5]){ ## to remove those lines which have same milepost as one milepost should be investigated only once . To handle this prob I have used the first argument 
      d7=c()
      ts1=c()
      ts1_pred=c()
      name=c()
      rown=c()
      mxmn=c()
      mx=0
      mn=0
      mxm=c()
      data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
      d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      ts1=ts(data1$total_trains_east_west,frequency = 12,start = c(2009))
      
      #plot.ts(ts1,main=paste("line segment",a[i,3],"track seg",a[i,4],"milepost start",a[i,5]))
      ts1_pred=ceiling(na.spline(ts1))
      cat('\n',"line segment",a[i,3],"track seg",a[i,4],"milepost start",a[i,5],'\n')
      print(ts1)
      print(ts(ts1_pred,frequency = 12,start=2009))
      mx=max(data1$total_trains_east_west,na.rm = T)
      mn=min(data1$total_trains_east_west,na.rm = T)
      mxm=c(mx,mn)
      m=a[i,5]
      name <- paste0('pred_',a[i,3],'_',a[i,4],'_',a[i,5])
      rown<- paste0('rown_',a[i,3],'_',a[i,4],'_',a[i,5])
      mxmn<- paste0('maxmi_',a[i,3],'_',a[i,4],'_',a[i,5])
      i=i+1
      result[[name]]=ts1_pred
      Row_names[[rown]]=d7
      maxmin[[mxmn]]=mxm
    }
    else{
      i=i+1
    }
  }
  for(i in 1:length(result)){
    m6=c()
    m6=which(result[[i]]<maxmin[[i]][2] | result[[i]]>maxmin[[i]][1])
    if(length(m6!=0)){
      result[[i]][m6]=NA
    }
  }
  for(i in 1:length(result)){
    m2=c()
    m3=c()
    m2=which(result[[i]]<0)
    if(length(m2!=0)){
      result[[i]][m2]=NA
    }
    m3=which(is.na(result[[i]]))
    if(length(m3)!=0){
      result[[i]][m3]=round(mean(result[[i]],na.rm=T),digits = 0)
    }
  }
  for(i in 1:length(Row_names)){
    d8=c()
    d8=Row_names[[i]]
    x$total_trains_east_west[d8]=result[[i]]
  }
  return(x)
} # This is the code for finding out missing values using spline function in time series 
tonnage_line_track_mileStart_year_month_Ttrains_pred=total_train_pred(tonnage_line_track_mileStart_year_month)
sum(is.na(tonnage_line_track_mileStart_year_month_Ttrains_pred$total_trains_east_west)) # 0 which shows that there is no NAs in this data set's total train variable
# Now we will put NA's which have only 2 or 3 or 1 years entries are there  manually 
total_train_pred=function(x){
  d6=c()
  Row_names=list()
  result=list()
  maxmin=list()
  data1=data.frame()
  d6=which(is.na(tonnage_line_track_mileStart_year_month$total_trains_east_west))
  i=1
  library(zoo)
  a=tonnage_line_track_mileStart_year_month[d6,]
  m=900 # any no which is greater than 600
  while(i<(length(d6)+1)){
    if(m!=a[i,5]& a[i,3]==1 & a[i,4]==0){ ## to remove those lines which have same milepost as one milepost should be investigated only once . To handle this prob I have used the first argument 
      d7=c()
      ts1=c()
      ts1_pred=c()
      name=c()
      rown=c()
      mxmn=c()
      mx=0
      mn=0
      mxm=c()
      data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
      d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      ts1=ts(data1$total_trains_east_west,frequency = 12,start = c(2009))
      
      #plot.ts(ts1,main=paste("line segment",a[i,3],"track seg",a[i,4],"milepost start",a[i,5]))
      ts1_pred=ceiling(na.spline(ts1))
      cat('\n',"line segment",a[i,3],"track seg",a[i,4],"milepost start",a[i,5],'\n')
      print(ts1)
      print(ts(ts1_pred,frequency = 12,start=2009))
      mx=max(data1$total_trains_east_west,na.rm = T)
      mn=min(data1$total_trains_east_west,na.rm = T)
      mxm=c(mx,mn)
      m=a[i,5]
      name <- paste0('pred_',a[i,3],'_',a[i,4],'_',a[i,5])
      rown<- paste0('rown_',a[i,3],'_',a[i,4],'_',a[i,5])
      mxmn<- paste0('maxmi_',a[i,3],'_',a[i,4],'_',a[i,5])
      i=i+1
      result[[name]]=ts1_pred
      Row_names[[rown]]=d7
      maxmin[[mxmn]]=mxm
    }
    else{
      i=i+1
    }
  }
  for(i in 1:length(result)){
    m6=c()
    m6=which(result[[i]]<maxmin[[i]][2] | result[[i]]>maxmin[[i]][1])
    if(length(m6!=0)){
      result[[i]][m6]=NA
    }
  }
  for(i in 1:length(result)){
    m2=c()
    m3=c()
    m2=which(result[[i]]<0)
    if(length(m2!=0)){
      result[[i]][m2]=NA
    }
    m3=which(is.na(result[[i]]))
    if(length(m3)!=0){
      result[[i]][m3]=round(mean(result[[i]],na.rm=T),digits = 0)
    }
  }
  for(i in 1:length(Row_names)){
    d8=c()
    d8=Row_names[[i]]
    x$total_trains_east_west[d8]=result[[i]]
  }
  #return(x)
} # this is the same code as the above code but will run only for line seg 1 and tracl seg 0 and will return nothing
total_train_pred(tonnage_line_track_mileStart_year_month) # printed all the instances where there were NAs and we can see that there is no any data where we have only 2 or 3 or 1 year data sso we have nothing to do
# Now we will run the same code for line seg 2 and trck seg 0
total_train_pred=function(x){
  d6=c()
  Row_names=list()
  result=list()
  maxmin=list()
  data1=data.frame()
  d6=which(is.na(tonnage_line_track_mileStart_year_month$total_trains_east_west))
  i=1
  library(zoo)
  a=tonnage_line_track_mileStart_year_month[d6,]
  m=900 # any no which is greater than 600
  while(i<(length(d6)+1)){
    if(m!=a[i,5]& a[i,3]==2 & a[i,4]==0){ ## to remove those lines which have same milepost as one milepost should be investigated only once . To handle this prob I have used the first argument 
      d7=c()
      ts1=c()
      ts1_pred=c()
      name=c()
      rown=c()
      mxmn=c()
      mx=0
      mn=0
      mxm=c()
      data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
      d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      ts1=ts(data1$total_trains_east_west,frequency = 12,start = c(2009))
      ts1_pred=ceiling(na.spline(ts1))
      cat('\n',"line segment",a[i,3],"track seg",a[i,4],"milepost start",a[i,5],'\n')
      print(ts1)
      print(ts(ts1_pred,frequency = 12,start=2009))
      mx=max(data1$total_trains_east_west,na.rm = T)
      mn=min(data1$total_trains_east_west,na.rm = T)
      mxm=c(mx,mn)
      m=a[i,5]
      name <- paste0('pred_',a[i,3],'_',a[i,4],'_',a[i,5])
      rown<- paste0('rown_',a[i,3],'_',a[i,4],'_',a[i,5])
      mxmn<- paste0('maxmi_',a[i,3],'_',a[i,4],'_',a[i,5])
      i=i+1
      result[[name]]=ts1_pred
      Row_names[[rown]]=d7
      maxmin[[mxmn]]=mxm
    }
    else{
      i=i+1
    }
  }
  for(i in 1:length(result)){
    m6=c()
    m6=which(result[[i]]<maxmin[[i]][2] | result[[i]]>maxmin[[i]][1])
    if(length(m6!=0)){
      result[[i]][m6]=NA
    }
  }
  for(i in 1:length(result)){
    m2=c()
    m3=c()
    m2=which(result[[i]]<0)
    if(length(m2!=0)){
      result[[i]][m2]=NA
    }
    m3=which(is.na(result[[i]]))
    if(length(m3)!=0){
      result[[i]][m3]=round(mean(result[[i]],na.rm=T),digits = 0)
    }
  }
  for(i in 1:length(Row_names)){
    d8=c()
    d8=Row_names[[i]]
    x$total_trains_east_west[d8]=result[[i]]
  }
  #return(x)
}
total_train_pred(tonnage_line_track_mileStart_year_month) ## Shows that there is some problem at milepost start =24.5
# we have calculated the value of Nas manually as follows
data_for_24.5_30.0=subset(tonnage_line_track_mileStart_year_month,tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==2 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0 & tonnage_line_track_mileStart_year_month$MILEPOST_START==24.5 | tonnage_line_track_mileStart_year_month$MILEPOST_START==29.7)
rows_no=c()
rows_no=which(tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==2 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0 & tonnage_line_track_mileStart_year_month$MILEPOST_START==24.5 | tonnage_line_track_mileStart_year_month$MILEPOST_START==29.7)
data_for_24.5_30.0
rows_no=rows_no[order(data_for_24.5_30.0$YEAR,data_for_24.5_30.0$MONTH)] # We have to do this as we have chaged the order of the data in above line so accordingly we have to change the data also for rows_no
data_for_24.5_30.0=data_for_24.5_30.0[order(data_for_24.5_30.0$YEAR,data_for_24.5_30.0$MONTH),]
data_for_24.5_30.0
ts_2_0_24.5_30.0=data_for_24.5_30.0$total_trains_east_west
ts_2_0_24.5_30.0
ts_2_0_24.5_30.0=ts(ts_2_0_24.5_30.0,frequency = 12,start = 2009)
plot.ts(ts_2_0_24.5_30.0)
ts_2_0_24.5_30.0_pred=ceiling(na.spline(ts_2_0_24.5_30.0))
ts_2_0_24.5_30.0_pred # Is giving the negative value at the last position so we will make it NA and substitute by the mean of all the data in that position
m2=c()
m2=which(ts_2_0_24.5_30.0_pred<0)
if(length(m2!=0)){
  ts_2_0_24.5_30.0_pred[m2]=NA
}
m3=c()
m3=which(is.na(ts_2_0_24.5_30.0_pred))
if(length(m3)!=0){
  ts_2_0_24.5_30.0_pred[m3]=round(mean(ts_2_0_24.5_30.0_pred,na.rm=T),digits = 0)
}
ts_2_0_24.5_30.0_pred # giving a good value 877 
# Now we will put these values in the dataframe tonnage......pred
tonnage_line_track_mileStart_year_month_Ttrains_pred[rows_no,12]=ts_2_0_24.5_30.0_pred   ## Now It is ok What We had wanted has done
total_train_pred(tonnage_line_track_mileStart_year_month) ## Shows that there is some problem at milepost start =67.5 but by interpolation its loooks good so we are not doing anything wwith that
 
# We will check for line seg 3 and track 1 as there is no missing value in line 3 and track 0
total_train_pred=function(x){
  d6=c()
  Row_names=list()
  result=list()
  maxmin=list()
  data1=data.frame()
  d6=which(is.na(tonnage_line_track_mileStart_year_month$total_trains_east_west))
  i=1
  library(zoo)
  a=tonnage_line_track_mileStart_year_month[d6,]
  m=900 # any no which is greater than 600
  while(i<(length(d6)+1)){
    if(m!=a[i,5]& a[i,3]==3 & a[i,4]==1){ ## to remove those lines which have same milepost as one milepost should be investigated only once . To handle this prob I have used the first argument 
      d7=c()
      ts1=c()
      ts1_pred=c()
      name=c()
      rown=c()
      mxmn=c()
      mx=0
      mn=0
      mxm=c()
      data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
      d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      ts1=ts(data1$total_trains_east_west,frequency = 12,start = c(2009))
      ts1_pred=ceiling(na.spline(ts1))
      cat('\n',"line segment",a[i,3],"track seg",a[i,4],"milepost start",a[i,5],'\n')
      print(ts1)
      print(ts(ts1_pred,frequency = 12,start=2009))
      mx=max(data1$total_trains_east_west,na.rm = T)
      mn=min(data1$total_trains_east_west,na.rm = T)
      mxm=c(mx,mn)
      m=a[i,5]
      name <- paste0('pred_',a[i,3],'_',a[i,4],'_',a[i,5])
      rown<- paste0('rown_',a[i,3],'_',a[i,4],'_',a[i,5])
      mxmn<- paste0('maxmi_',a[i,3],'_',a[i,4],'_',a[i,5])
      i=i+1
      result[[name]]=ts1_pred
      Row_names[[rown]]=d7
      maxmin[[mxmn]]=mxm
    }
    else{
      i=i+1
    }
  }
  for(i in 1:length(result)){
    m6=c()
    m6=which(result[[i]]<maxmin[[i]][2] | result[[i]]>maxmin[[i]][1])
    if(length(m6!=0)){
      result[[i]][m6]=NA
    }
  }
  for(i in 1:length(result)){
    m2=c()
    m3=c()
    m2=which(result[[i]]<0)
    if(length(m2!=0)){
      result[[i]][m2]=NA
    }
    m3=which(is.na(result[[i]]))
    if(length(m3)!=0){
      result[[i]][m3]=round(mean(result[[i]],na.rm=T),digits = 0)
    }
  }
  for(i in 1:length(Row_names)){
    d8=c()
    d8=Row_names[[i]]
    x$total_trains_east_west[d8]=result[[i]]
  }
  #return(x)
}
total_train_pred(tonnage_line_track_mileStart_year_month) ## Shows that there is some problem at milepost start =37.1 
#Here we had observed that at some position interpolation is not so good as I had substituted the mean. Now I'll again make them NA
## These are the rows 9368:9381 so we will make these NAs
tonnage_line_track_mileStart_year_month_Ttrains_pred[9368:9381,12]=NA
## At 9581 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[9581,12]=NA
## At 9719 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[9719,12]=NA
## At 9788 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[9788,12]=NA
## At 9927 and +1 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[9927:9928,12]=NA
## At 9995 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[9995,12]=NA
## At 10133 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[10133,12]=NA
## At 10198:10200 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[10198:10200,12]=NA
## At 10202 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[10202,12]=NA
## At 10477 +1 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[10477:10478,12]=NA
## At 10546 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[10546,12]=NA
## At 10786 +2 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[10786:10788,12]=NA
## At 10822 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[10822,12]=NA
## At 10961 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[10961,12]=NA
## At 11030 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[11030,12]=NA
## At 11407 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[11407,12]=NA
## At 11476 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[11476,12]=NA
## At 11683 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[11683,12]=NA
## At 11821 there is a absurd value So we will make it NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[11821,12]=NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[11959,12]=NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[12028,12]=NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[12097,12]=NA
# Now all the entries in Line segment 3 and track seg 1 looking good
#Another method to above manual assignment is assign NA to all the elements from year 2014 and month 2 and which are interpolated by the formula i.e. they were missing values
# We will check for line seg 3 and track 2
total_train_pred=function(x){
  d6=c()
  Row_names=list()
  result=list()
  maxmin=list()
  data1=data.frame()
  d6=which(is.na(tonnage_line_track_mileStart_year_month$total_trains_east_west))
  i=1
  library(zoo)
  a=tonnage_line_track_mileStart_year_month[d6,]
  m=900 # any no which is greater than 600
  while(i<(length(d6)+1)){
    if(m!=a[i,5]& a[i,3]==3 & a[i,4]==2){ ## to remove those lines which have same milepost as one milepost should be investigated only once . To handle this prob I have used the first argument 
      d7=c()
      ts1=c()
      ts1_pred=c()
      name=c()
      rown=c()
      mxmn=c()
      mx=0
      mn=0
      mxm=c()
      data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
      d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      ts1=ts(data1$total_trains_east_west,frequency = 12,start = c(2009))
      ts1_pred=ceiling(na.spline(ts1))
      cat('\n',"line segment",a[i,3],"track seg",a[i,4],"milepost start",a[i,5],'\n')
      print(ts1)
      print(ts(ts1_pred,frequency = 12,start=2009))
      mx=max(data1$total_trains_east_west,na.rm = T)
      mn=min(data1$total_trains_east_west,na.rm = T)
      mxm=c(mx,mn)
      m=a[i,5]
      name <- paste0('pred_',a[i,3],'_',a[i,4],'_',a[i,5])
      rown<- paste0('rown_',a[i,3],'_',a[i,4],'_',a[i,5])
      mxmn<- paste0('maxmi_',a[i,3],'_',a[i,4],'_',a[i,5])
      i=i+1
      result[[name]]=ts1_pred
      Row_names[[rown]]=d7
      maxmin[[mxmn]]=mxm
    }
    else{
      i=i+1
    }
  }
  for(i in 1:length(result)){
    m6=c()
    m6=which(result[[i]]<maxmin[[i]][2] | result[[i]]>maxmin[[i]][1])
    if(length(m6!=0)){
      result[[i]][m6]=NA
    }
  }
  for(i in 1:length(result)){
    m2=c()
    m3=c()
    m2=which(result[[i]]<0)
    if(length(m2!=0)){
      result[[i]][m2]=NA
    }
    m3=which(is.na(result[[i]]))
    if(length(m3)!=0){
      result[[i]][m3]=round(mean(result[[i]],na.rm=T),digits = 0)
    }
  }
  for(i in 1:length(Row_names)){
    d8=c()
    d8=Row_names[[i]]
    x$total_trains_east_west[d8]=result[[i]]
  }
  #return(x)
}
total_train_pred(tonnage_line_track_mileStart_year_month)
## At 15336 +2  there is a absurd value So we will make it NA and others looking good
tonnage_line_track_mileStart_year_month_Ttrains_pred[15336:15338,12]=NA
# We will check for line seg 4 and track 0
total_train_pred=function(x){
  d6=c()
  Row_names=list()
  result=list()
  maxmin=list()
  data1=data.frame()
  d6=which(is.na(tonnage_line_track_mileStart_year_month$total_trains_east_west))
  i=1
  library(zoo)
  a=tonnage_line_track_mileStart_year_month[d6,]
  m=900 # any no which is greater than 600
  while(i<(length(d6)+1)){
    if(m!=a[i,5]& a[i,3]==4 & a[i,4]==0){ ## to remove those lines which have same milepost as one milepost should be investigated only once . To handle this prob I have used the first argument 
      d7=c()
      ts1=c()
      ts1_pred=c()
      name=c()
      rown=c()
      mxmn=c()
      mx=0
      mn=0
      mxm=c()
      data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
      d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      ts1=ts(data1$total_trains_east_west,frequency = 12,start = c(2009))
      ts1_pred=ceiling(na.spline(ts1))
      cat('\n',"line segment",a[i,3],"track seg",a[i,4],"milepost start",a[i,5],'\n')
      print(ts1)
      print(ts(ts1_pred,frequency = 12,start=2009))
      mx=max(data1$total_trains_east_west,na.rm = T)
      mn=min(data1$total_trains_east_west,na.rm = T)
      mxm=c(mx,mn)
      m=a[i,5]
      name <- paste0('pred_',a[i,3],'_',a[i,4],'_',a[i,5])
      rown<- paste0('rown_',a[i,3],'_',a[i,4],'_',a[i,5])
      mxmn<- paste0('maxmi_',a[i,3],'_',a[i,4],'_',a[i,5])
      i=i+1
      result[[name]]=ts1_pred
      Row_names[[rown]]=d7
      maxmin[[mxmn]]=mxm
    }
    else{
      i=i+1
    }
  }
  for(i in 1:length(result)){
    m6=c()
    m6=which(result[[i]]<maxmin[[i]][2] | result[[i]]>maxmin[[i]][1])
    if(length(m6!=0)){
      result[[i]][m6]=NA
    }
  }
  for(i in 1:length(result)){
    m2=c()
    m3=c()
    m2=which(result[[i]]<0)
    if(length(m2!=0)){
      result[[i]][m2]=NA
    }
    m3=which(is.na(result[[i]]))
    if(length(m3)!=0){
      result[[i]][m3]=round(mean(result[[i]],na.rm=T),digits = 0)
    }
  }
  for(i in 1:length(Row_names)){
    d8=c()
    d8=Row_names[[i]]
    x$total_trains_east_west[d8]=result[[i]]
  }
  #return(x)
}
total_train_pred(tonnage_line_track_mileStart_year_month)
tonnage_line_track_mileStart_year_month_Ttrains_pred[23604:23617,12]=NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[23673:23686,12]=NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[24456:24461,12]=NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[25302:25321,12]=NA
# We will check for line seg 4 and track 1
total_train_pred=function(x){
  d6=c()
  Row_names=list()
  result=list()
  maxmin=list()
  data1=data.frame()
  d6=which(is.na(tonnage_line_track_mileStart_year_month$total_trains_east_west))
  i=1
  library(zoo)
  a=tonnage_line_track_mileStart_year_month[d6,]
  m=900 # any no which is greater than 600
  while(i<(length(d6)+1)){
    if(m!=a[i,5]& a[i,3]==4 & a[i,4]==1){ ## to remove those lines which have same milepost as one milepost should be investigated only once . To handle this prob I have used the first argument 
      d7=c()
      ts1=c()
      ts1_pred=c()
      name=c()
      rown=c()
      mxmn=c()
      mx=0
      mn=0
      mxm=c()
      data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
      d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      if(nrow(data1)<50){
        data1=x[which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4]),]
        d7=which(x$MILEPOST_START==a[i,5] & x$MILEPOST_END==a[i,6]& x$LINE_SEG_NBR==a[i,3] & x$TRACK_SDTK_NBR==a[i,4])
      }
      ts1=ts(data1$total_trains_east_west,frequency = 12,start = c(2009))
      ts1_pred=ceiling(na.spline(ts1))
      cat('\n',"line segment",a[i,3],"track seg",a[i,4],"milepost start",a[i,5],'\n')
      print(ts1)
      print(ts(ts1_pred,frequency = 12,start=2009))
      mx=max(data1$total_trains_east_west,na.rm = T)
      mn=min(data1$total_trains_east_west,na.rm = T)
      mxm=c(mx,mn)
      m=a[i,5]
      name <- paste0('pred_',a[i,3],'_',a[i,4],'_',a[i,5])
      rown<- paste0('rown_',a[i,3],'_',a[i,4],'_',a[i,5])
      mxmn<- paste0('maxmi_',a[i,3],'_',a[i,4],'_',a[i,5])
      i=i+1
      result[[name]]=ts1_pred
      Row_names[[rown]]=d7
      maxmin[[mxmn]]=mxm
    }
    else{
      i=i+1
    }
  }
  for(i in 1:length(result)){
    m6=c()
    m6=which(result[[i]]<maxmin[[i]][2] | result[[i]]>maxmin[[i]][1])
    if(length(m6!=0)){
      result[[i]][m6]=NA
    }
  }
  for(i in 1:length(result)){
    m2=c()
    m3=c()
    m2=which(result[[i]]<0)
    if(length(m2!=0)){
      result[[i]][m2]=NA
    }
    m3=which(is.na(result[[i]]))
    if(length(m3)!=0){
      result[[i]][m3]=round(mean(result[[i]],na.rm=T),digits = 0)
    }
  }
  for(i in 1:length(Row_names)){
    d8=c()
    d8=Row_names[[i]]
    x$total_trains_east_west[d8]=result[[i]]
  }
  #return(x)
}
total_train_pred(tonnage_line_track_mileStart_year_month)
tonnage_line_track_mileStart_year_month_Ttrains_pred[25900:25910,12]=NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[26044,12]=NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[26047,12]=NA
tonnage_line_track_mileStart_year_month_Ttrains_pred[26048,12]=NA
# There are no missing values on line seg 4 and track seg 2,3,4,5,6
# Now we have imputed the Missing values using time series analysis
sum(is.na(tonnage_line_track_mileStart_year_month_Ttrains_pred$total_trains_east_west))  ## 112 Missing VAlues
# Here is the data frame having those values
tonnage_line_track_mileStart_year_month_Ttrains_pred
# Now we will find out the missing values which occurred in 2007 and 2008 and which we make NAs after the time series analysis
tonnage_2007_2008=subset(tonnage1,tonnage1$YEAR==2007 | tonnage1$YEAR==2008)
tonnage_2007_2008=cbind(tonnage_2007_2008,total_trains_east_west=tonnage_2007_2008$TOT_TRN_EAST+tonnage_2007_2008$TOT_TRN_WEST)
View(tonnage_2007_2008)
# Now combine the data for 2007 and 2008 and tonnage....pred
tonnage_combine_all_years=rbind(tonnage_2007_2008,tonnage_line_track_mileStart_year_month_Ttrains_pred)
View(tonnage_combine_all_years)
dim(tonnage_combine_all_years)  #36805  12
# Now make NAs to 0
blank_rows=c()
blank_rows=which(tonnage_combine_all_years$total_trains_east_west==0)
length(blank_rows) # 9515 equal to the number of rows in 2007 and 2008
tonnage_combine_all_years$total_trains_east_west[blank_rows]=NA
NA_rows=c()
NA_rows=which(is.na(tonnage_combine_all_years$total_trains_east_west))
length(NA_rows) # 9627
tonnage_for_prediction=tonnage_combine_all_years[-NA_rows,]

scatter.smooth(tonnage_for_prediction$TOT_DFLT_MGT,tonnage_for_prediction$total_trains_east_west)
# By scatter Plot we can see that there are 4n outliers which have tonnage greater than 20 so put them as mean of the other data
temporary_data=tonnage_for_prediction[-which(tonnage_for_prediction$TOT_DFLT_MGT>20),]
tonnage_for_prediction[which(tonnage_for_prediction$TOT_DFLT_MGT>20),11]=mean(temporary_data$TOT_DFLT_MGT)
scatter.smooth(tonnage_for_prediction$TOT_DFLT_MGT,tonnage_for_prediction$total_trains_east_west)
# Removed 
fit=lm(tonnage_for_prediction$total_trains_east_west~tonnage_for_prediction$TOT_DFLT_MGT)
summary(fit) # intercept=122.0132 means it will give atleast 122 total trains and 122.6903 for variable 80% adjusted R2
tonnage_combine_all_years[NA_rows,12]=ceiling((tonnage_combine_all_years[NA_rows,11]*122.69)+122.0132)
sum(is.na(tonnage_combine_all_years$total_trains_east_west))# 0 shows that all the values are filled
#so we have the below data frame having the data of tonnage and predicted total trains
View(tonnage_combine_all_years) #36805 ***
temporary_data=tonnage_combine_all_years[-which(tonnage_combine_all_years$TOT_DFLT_MGT>20),]
tonnage_combine_all_years[which(tonnage_combine_all_years$TOT_DFLT_MGT>20),11]=mean(temporary_data$TOT_DFLT_MGT)
tonnage_combine_all_years=tonnage_combine_all_years[order(tonnage_combine_all_years$LINE_SEG_NBR,tonnage_combine_all_years$TRACK_SDTK_NBR,tonnage_combine_all_years$MILEPOST_START,tonnage_combine_all_years$YEAR,tonnage_combine_all_years$MONTH),]



############################################################################################################################################




#Date 01/06/2016
# first of all we will divide the whole data set in 3 sub data frames according to Defect Type
train_surface=subset(train_data,train_data$DFCT_TYPE=="SURFACE")
dim(train_surface) ##7577 14
train_xlevel=subset(train_data,train_data$DFCT_TYPE=="XLEVEL")
dim(train_xlevel) ## 11909 14
train_dip=subset(train_data,train_data$DFCT_TYPE=="DIP")
dim(train_dip) ##4361 14

# Now I want to Sort my surface data by line segment number after that track after that mile post after that test date in order to find out the repeated deffect
# We can see that Test format provided is factor so in order to sort by that we have to convert the test variable we have to make it in Date format
train_surface$TEST_DT=as.Date(train_surface$TEST_DT,format="%d%b%Y")
train_surface=train_surface[order(train_surface$LINE_SEG_NBR,train_surface$TRACK_SDTK_NBR,train_surface$TEST_DT,train_surface$MILEPOST),]
View(train_surface)
# Now we want to see the summary of the data in order to see the types and minimum and maximum of the variables and also the possible errors
str(train_surface)
summary(train_surface)
###### Making given information of thresholds as a data frame
class1=c(3,3,3)
class2=c(2.75,2.75,2)
class3=c(2.25,2.25,1.75)
class4=c(2,1.75,1.25)
class5=c(1.25,1.5,1)
tag_limits=cbind(class1,class2,class3,class4,class5)
row.names(tag_limits)=c("SURFACE","DIP","XLEVEL")

## now we will work with defect type 'surface'
uniq_defect_position1=function(x){
  d=c(0)
  x=x[order(x$MILEPOST),]
  for(i in 1:nrow(x)){
    l=0
    for(j in 1:length(d)){
      if(x[i,2]>=d[j]-100*feet_mile & x[i,2]<=d[j]+100*feet_mile){
        l=l+1
      }
      if(l!=0)
        break
    }
    if(l==0){
      d=c(d,x[i,2]+100*feet_mile)
    }
  }
  return(d)
}    # FUNCTION TO FIND OUT UNIQUE DEFECT POSITIONS
mile_post_uniq=function(x,y){
  m=0
  for(i in 1:nrow(x)){
    m=which(y<=x[i,2]+100*feet_mile & y>=x[i,2]-100*feet_mile)
    x[i,2]=y[m]
  }
  return(x)
}  # FUNCTION FOR ASSIGNING UNIQUE DIFFERENT POSITIONS TO THE MILEPOSTS
# we will partition the train_surface data by line_seg=1 and track=0
train_surface_line_track_milepost=train_surface[order(train_surface$LINE_SEG_NBR,train_surface$TRACK_SDTK_NBR,train_surface$MILEPOST),]
train_surface_1_0=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==1 & train_surface_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_1_0=uniq_defect_position1(train_surface_1_0)
length(uniq_defect_pos_1_0) ##242
train_surface_1_0=mile_post_uniq(train_surface_1_0,uniq_defect_pos_1_0)
#train_surface_1_0_no_7day=correction_obs_7days(train_surface_1_0)
train_surface_1_0_no_day=remove_same_day_repeat(train_surface_1_0)
train_surface_1_0_no_day_no_single=remove_single_obs(train_surface_1_0_no_day)
length(levels(as.factor(train_surface_1_0_no_day_no_single$MILEPOST)))  ##135
## here we have 135 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
135*0.20 ## 27 unique defect positions will be in our test data and remaining 135-27 willbe in our train data
d=sample(1:135,27) # SRSWOR sample of size 27 from 135 
d1=levels(as.factor(train_surface_1_0_no_day_no_single$MILEPOST))
uniq_pos_train=d1[-d]
uniq_pos_test=d1[d]
train_surface_1_0_no_day_no_single_trn=subset(train_surface_1_0_no_day_no_single,train_surface_1_0_no_day_no_single$MILEPOST %in% uniq_pos_train)
train_surface_1_0_no_day_no_single_tst=subset(train_surface_1_0_no_day_no_single,train_surface_1_0_no_day_no_single$MILEPOST %in% uniq_pos_test)
#####################################################################################################################################################################################################################
# Now we will work with Train data
red_first_surface_1_0=start_red(train_surface_1_0_no_day_no_single_trn)
red_first_surface_1_0_mile_change=milepost_change(red_first_surface_1_0)
red_first_surface_1_0_NoLess_6Mnth=remove_less_6_month(red_first_surface_1_0_mile_change)
red_first_surface_1_0_trn_final=cum_amptd_time(red_first_surface_1_0_NoLess_6Mnth,inspect_data)
our_likelihood_1=function(theta,x){
  u=theta[1]
  c=theta[2]
  b=theta[3]
  logl=0
  a0=levels(as.factor(x$MILEPOST))
  for(i in 1:length(a0)){
    a1=which(x$MILEPOST==a0[i])
    j=1
    while(j<length(a1) & x[a1[j],17]>0 & x[a1[j],19]==0){
        print(a0[i])
        logl=logl+(c)^2*((x[a1[j+1],16])^(b)^2-(x[a1[j],16])^(b)^2)*log(u,exp(1))+(((c^2)*((x[a1[j+1],16])^(b)^2-(x[a1[j],16])^(b)^2))-1)*log(x[a1[j],17],exp(1))-u*x[a1[j],17]-lgamma((c)^2*((x[a1[j+1],16])^(b)^2-(x[a1[j],16])^(b)^2))
        j=j+1
    }
    #print("good")
  }
  return(-logl)
}
optim(c(5,0.98,0.86),our_likelihood_1,x=red_first_surface_1_0_trn_final)
prob_gamma=function(x,t){
  c=1.8908158
  b=0.5366758
  u=18.1393086
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 3))
}

surface_1_0_test_data=ampltd_difference(train_surface_1_0_no_day_no_single_tst,tonnage_combine_all_years,inspect_data)
surface_1_0_test_data=subset(surface_1_0_test_data,surface_1_0_test_data$inspect_count==0)
surface_1_0_test_data=cbind(surface_1_0_test_data,AMPLTD_NEED=1.26-abs(surface_1_0_test_data$DEF_AMP_1))
surface_1_0_test_data=subset(surface_1_0_test_data,surface_1_0_test_data$defect_amp_diff>0)
surface_1_0_test_data=subset(surface_1_0_test_data,surface_1_0_test_data$AMPLTD_NEED>0)
prob1=prob_gamma(surface_1_0_test_data$AMPLTD_NEED,surface_1_0_test_data$day_count) ### giving very high probability for small ampltd change in smalltime period 

#####################################################################################################################################################################################################################
## Prediction By assuming that Lifetime of a defect starts from the time when first time it is found Yellow
Surface_1_0_trn=mile_post_change_for_2nd_method(train_surface_1_0_no_day_no_single_trn)
Surface_1_0_trn_final=cum_amptd_time_1(Surface_1_0_trn,inspect_data)
our_likelihood_1=function(theta,x){
  u=theta[1]
  c=theta[2]
  b=theta[3]
  logl=0
  a0=levels(as.factor(x$MILEPOST))
  for(i in 1:length(a0)){
    a1=which(x$MILEPOST==a0[i])
    j=1
    while(j<length(a1) & x[a1[j],17]>0 & x[a1[j],19]==0){
      print(a0[i])
      logl=logl+(c)^2*((x[a1[j+1],16])^(b)^2-(x[a1[j],16])^(b)^2)*log(u,exp(1))+(((c^2)*((x[a1[j+1],16])^(b)^2-(x[a1[j],16])^(b)^2))-1)*log(x[a1[j],17],exp(1))-u*x[a1[j],17]-lgamma((c)^2*((x[a1[j+1],16])^(b)^2-(x[a1[j],16])^(b)^2))
      j=j+1
    }
    
    print(a0[i])
  }
  return(-logl)
}
optim(c(9,0.69,1.03),our_likelihood_1,y=Surface_1_0_trn_final) ### u=8.8449126   c=0.7714864 b=0.4182621
prob_gamma=function(x,t){
  c=0.7714864
  b=0.4182621
  u=8.8449126
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 3))
}
####### data preparation for prediction 
surface_1_0_test_data=ampltd_difference(train_surface_1_0_no_day_no_single_tst,tonnage_combine_all_years,inspect_data)
surface_1_0_test_data=subset(surface_1_0_test_data,surface_1_0_test_data$inspect_count==0)
add_threshold=function(x,y){
  THRSLD=c()
  for(i in 1:nrow(x)){
    THRSLD=c(THRSLD,y[as.character(x$DFCT_TYPE[i]),x$CLASS[i]])
  }
  x=cbind(x,THRSLD=THRSLD)
  AMPLTD_NEED=(x$THRSLD+0.01)-abs(x$DEF_AMP_1)
  x=cbind(x,AMPLTD_NEED=AMPLTD_NEED)
  return(x)
}
surface_1_0_test_data_thrsld=add_threshold(surface_1_0_test_data,tag_limits)
surface_1_0_test_data_valid=subset(surface_1_0_test_data_thrsld,surface_1_0_test_data_thrsld$defect_amp_diff>0 & surface_1_0_test_data_thrsld$AMPLTD_NEED>0)
############## Prediction 
prob=prob_gamma(surface_1_0_test_data$AMPLTD_NEED,surface_1_0_test_data$day_count)
surface_1_pred=prediction(prob,surface_1_0_test_data) ### In the last line 0 means match otherwise dismatch
length(which(surface_1_pred$pred_match==0)) ##22
nrow(surface_1_pred) ##26
length(which(surface_1_pred$pred_match==0))/nrow(surface_1_pred) ## 0.8461538  means 84.61 % accuracy of prediction


# for line seg 1 and defect type SURFACE
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    u=seq(1,20,length.out = 500)
    c=seq(0.1,2,length.out = 500)
    b=seq(0.01,1.5,length.out = 500)
  }
  length_uni_mile_surf_1_0=length(levels(as.factor(train_surface_1_0_no_day_no_single$MILEPOST)))  ##22
  surface_sample_pos_1_0=sample(1:length_uni_mile_surf_1_0,round(length_uni_mile_surf_1_0*0.20,digits = 0))
  surface_uniq_pos_1_0=levels(as.factor(train_surface_1_0_no_day_no_single$MILEPOST))
  surface_uniq_pos_train_1_0=surface_uniq_pos_1_0[-surface_sample_pos_1_0]
  surface_uniq_pos_test_1_0=surface_uniq_pos_1_0[surface_sample_pos_1_0]
  train_surface_1_0_no_day_no_single_trn=subset(train_surface_1_0_no_day_no_single,train_surface_1_0_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_1_0)
  train_surface_1_0_no_day_no_single_tst=subset(train_surface_1_0_no_day_no_single,train_surface_1_0_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_1_0)
  train_data_surface_1=rbind(train_surface_1_0_no_day_no_single_trn)
  test_data_surface_1=rbind(train_surface_1_0_no_day_no_single_tst)
  train_data_surface_1_mile_changed=mile_post_change_for_2nd_method(train_data_surface_1)
  train_data_surface_1_final=cum_amptd_time_1(train_data_surface_1_mile_changed,inspect_data)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_surface_1_final)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_surface_1_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_surface_1_ampltd_dif=ampltd_difference(test_data_surface_1,tonnage_combine_all_years,inspect_data)
  test_data_surface_1_inspect_0=subset(test_data_surface_1_ampltd_dif,test_data_surface_1_ampltd_dif$inspect_count==0)
  test_data_surface_1_threshold_add=add_threshold(test_data_surface_1_inspect_0,tag_limits)
  test_data_surface_1_valid=subset(test_data_surface_1_threshold_add,test_data_surface_1_threshold_add$defect_amp_diff>0 & test_data_surface_1_threshold_add$AMPLTD_NEED>0)
  prob_1_surf=prob_gamma(test_data_surface_1_valid$AMPLTD_NEED,test_data_surface_1_valid$day_count)
  surface_1_pred=prediction(prob_1_surf,test_data_surface_1_valid)
  match[ii]=length(which(surface_1_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(surface_1_pred))*100
  name_1=paste0("surface_1_pred","_",ii)
  false_positive[ii]=length(which(surface_1_pred$pred_match==1))
  false_negative[ii]=length(which(surface_1_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(surface_1_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(surface_1_pred))*100
  write.xlsx(surface_1_pred,"surface_1_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"surface_1_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

#########################################################################################################################################################

train_surface_1_0_final=ampltd_difference(train_surface_1_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
length(which(train_surface_1_0_final$inspect_count==0))##275
length(which(train_surface_1_0_final$defect_amp>=0 & train_surface_1_0_final$inspect_count==0))  ##187


#########################################################################################################################################################


# unique defetc position for 2_0
train_surface_2_0=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==2 & train_surface_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_2_0=uniq_defect_position1(train_surface_2_0)
length(uniq_defect_pos_2_0) ##462
train_surface_2_0=mile_post_uniq(train_surface_2_0,uniq_defect_pos_2_0)
train_surface_2_0_no_day=remove_same_day_repeat(train_surface_2_0)
train_surface_2_0_no_day_no_single=remove_single_obs(train_surface_2_0_no_day)
########################
length(levels(as.factor(train_surface_2_0_no_day_no_single$MILEPOST)))  ##249
## here we have 135 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
249*0.20 ##49.8 =~50 unique defect positions will be in our test data and remaining 135-27 willbe in our train data
sample_pos_2_0=sample(1:249,50) # SRSWOR sample of size 27 from 135 
uniq_pos_2_0=levels(as.factor(train_surface_2_0_no_day_no_single$MILEPOST))
uniq_pos_train_2_0=uniq_pos_2_0[-sample_pos_2_0]
uniq_pos_test_2_0=uniq_pos_2_0[sample_pos_2_0]
train_surface_2_0_no_day_no_single_trn=subset(train_surface_2_0_no_day_no_single,train_surface_2_0_no_day_no_single$MILEPOST %in% uniq_pos_train_2_0)
##### Make  test data set
train_surface_2_0_no_day_no_single_tst=subset(train_surface_2_0_no_day_no_single,train_surface_2_0_no_day_no_single$MILEPOST %in% uniq_pos_test_2_0)
surface_2_0_test_data=ampltd_difference(train_surface_2_0_no_day_no_single_tst,tonnage_combine_all_years,inspect_data)
surface_2_0_test_data=subset(surface_2_0_test_data,surface_2_0_test_data$inspect_count==0)
surface_2_0_test_data=add_threshold(surface_2_0_test_data,tag_limits)
surface_2_0_test_data=subset(surface_2_0_test_data,surface_2_0_test_data$defect_amp_diff>0 & surface_2_0_test_data$AMPLTD_NEED>0)
### Now we will work on train data set
Surface_2_0_trn=mile_post_change_for_2nd_method(train_surface_2_0_no_day_no_single_trn)
Surface_2_0_trn_final=cum_amptd_time_1(Surface_2_0_trn,inspect_data)
optim(c(5.06,0.69,1.03),our_likelihood_1,x=Surface_2_0_trn_final) ### u=8.8273430   c=0.7733920 b=-0.4166168
prob_gamma=function(x,t){
  c=0.7733920
  b=-0.4166168
  u=8.8273430
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
prob_2_0=prob_gamma(surface_2_0_test_data$AMPLTD_NEED,surface_2_0_test_data$day_count)
prediction=function(x,y){
  z=character()
  for(i in 1:length(x)){
    if(x[i]<0.5)
      z=c(z,"YEL")
    else
      z=c(z,"RED")
  }
  y=cbind(y,Predicted_tag=z)
  test=as.numeric(y$PRIORITY_2)-as.numeric(y$Predicted_tag)
  y=cbind(y,pred_match=test)
  return(y)
}
surface_2_pred=prediction(prob_2_0,surface_2_0_test_data) ### In the last Column 0 means match otherwise dismatch
length(which(surface_2_pred$pred_match==0)) ##27
nrow(surface_2_pred) ##37
27/37=0.7297297 #### 72.97 % Prediction accuracy
##########################################################################################################################################
train_surface_2_0_final=ampltd_difference(train_surface_2_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
View(train_surface_2_0_final)  ##1685
length(which(train_surface_2_0_final$inspect_count==0))  ## 582
length(which(train_surface_2_0_final$defect_amp>=0 & train_surface_2_0_final$inspect_count==0)) ##402




# unique defect position for 2_1
any(train_surface$LINE_SEG_NBR==2 & train_surface$TRACK_SDTK_NBR==1)
train_surface_2_1=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==2 & train_surface_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_2_1=uniq_defect_position1(train_surface_2_1)
length(uniq_defect_pos_2_1) ## 36 
train_surface_2_1=mile_post_uniq(train_surface_2_1,uniq_defect_pos_2_1)
train_surface_2_1_no_day=remove_same_day_repeat(train_surface_2_1)
train_surface_2_1_no_day_no_single=remove_single_obs(train_surface_2_1_no_day)
#####################################
length(levels(as.factor(train_surface_2_1_no_day_no_single$MILEPOST)))  ##22
22*0.20 ### 4
sample_pos_2_1=sample(1:22,4) # SRSWOR sample of size 4 from 22 
uniq_pos_2_1=levels(as.factor(train_surface_2_1_no_day_no_single$MILEPOST))
uniq_pos_train_2_1=uniq_pos_2_1[-sample_pos_2_1]
uniq_pos_test_2_1=uniq_pos_2_1[sample_pos_2_1]
train_surface_2_1_no_day_no_single_trn=subset(train_surface_2_1_no_day_no_single,train_surface_2_1_no_day_no_single$MILEPOST %in% uniq_pos_train_2_1)
##### Make  test data set
train_surface_2_1_no_day_no_single_tst=subset(train_surface_2_1_no_day_no_single,train_surface_2_1_no_day_no_single$MILEPOST %in% uniq_pos_test_2_1)
surface_2_1_test_data=ampltd_difference(train_surface_2_1_no_day_no_single_tst,tonnage_combine_all_years,inspect_data)
surface_2_1_test_data=subset(surface_2_1_test_data,surface_2_1_test_data$inspect_count==0)
surface_2_1_test_data=add_threshold(surface_2_1_test_data,tag_limits)
surface_2_1_test_data=subset(surface_2_1_test_data,surface_2_1_test_data$defect_amp_diff>0 & surface_2_1_test_data$AMPLTD_NEED>0)
### Now we will work on train data set
Surface_2_1_trn=mile_post_change_for_2nd_method(train_surface_2_1_no_day_no_single_trn)
Surface_2_1_trn_final=cum_amptd_time_1(Surface_2_1_trn,inspect_data)
optim(c(5.06,0.69,1.03),our_likelihood_1,x=Surface_2_1_trn_final) ### u=8.8273430   c=0.7733920 b=-0.4166168
prob_gamma=function(x,t){
  c=0.4471207
  b=-0.6536665
  u=8.1518178
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
prob_2_1=prob_gamma(surface_2_1_test_data$AMPLTD_NEED,surface_2_1_test_data$day_count)
prediction=function(x,y){
  z=character()
  for(i in 1:length(x)){
    if(x[i]<0.5)
      z=c(z,"YEL")
    else
      z=c(z,"RED")
  }
  y=cbind(y,Predicted_tag=z)
  test=as.numeric(y$PRIORITY_2)-as.numeric(y$Predicted_tag)
  y=cbind(y,pred_match=test)
  return(y)
}
surface_2_1_pred=prediction(prob_2_1,surface_2_1_test_data) ### In the last Column 0 means match otherwise dismatch


train_surface_2_1_final=ampltd_difference(train_surface_2_1_no_day_no_single,tonnage_combine_all_years,inspect_data)
length(which(train_surface_2_1_final$inspect_count==0)) ##30
length(which(train_surface_2_1_final$defect_amp>=0 & train_surface_2_1_final$inspect_count==0))  ## 14
red_first_surface_2_1=start_red(train_surface_2_1_no_day_no_single)

# unique defect position for 2_2
any(train_surface$LINE_SEG_NBR==2 & train_surface$TRACK_SDTK_NBR==2)
train_surface_2_2=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==2 & train_surface_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_2_2=uniq_defect_position1(train_surface_2_2)
length(uniq_defect_pos_2_2) ## 38
train_surface_2_2=mile_post_uniq(train_surface_2_2,uniq_defect_pos_2_2)
train_surface_2_2_no_day=remove_same_day_repeat(train_surface_2_2)
train_surface_2_2_no_day_no_single=remove_single_obs(train_surface_2_2_no_day)
length(levels(as.factor(train_surface_2_2_no_day_no_single$MILEPOST)))
10*0.20  =2
sample_pos_2_2=sample(1:10,2) # SRSWOR sample of size 4 from 22 
uniq_pos_2_2=levels(as.factor(train_surface_2_2_no_day_no_single$MILEPOST))
uniq_pos_train_2_2=uniq_pos_2_2[-sample_pos_2_2]
uniq_pos_test_2_2=uniq_pos_2_2[sample_pos_2_2]
train_surface_2_2_no_day_no_single_trn=subset(train_surface_2_2_no_day_no_single,train_surface_2_2_no_day_no_single$MILEPOST %in% uniq_pos_train_2_2)
##### Make  test data set
train_surface_2_2_no_day_no_single_tst=subset(train_surface_2_2_no_day_no_single,train_surface_2_2_no_day_no_single$MILEPOST %in% uniq_pos_test_2_2)
surface_2_2_test_data=ampltd_difference(train_surface_2_2_no_day_no_single_tst,tonnage_combine_all_years,inspect_data)
surface_2_2_test_data=subset(surface_2_2_test_data,surface_2_2_test_data$inspect_count==0)
surface_2_2_test_data=add_threshold(surface_2_2_test_data,tag_limits)
surface_2_2_test_data=subset(surface_2_2_test_data,surface_2_2_test_data$defect_amp_diff>0 & surface_2_2_test_data$AMPLTD_NEED>0)
### Now we will work on train data set
Surface_2_2_trn=mile_post_change_for_2nd_method(train_surface_2_2_no_day_no_single_trn)
Surface_2_2_trn_final=cum_amptd_time_1(Surface_2_2_trn,inspect_data)
optim(c(1000,10,1.03),our_likelihood_1,x=Surface_2_2_trn_final) ### u=8.8273430   c=0.7733920 b=-0.4166168
prob_gamma=function(x,t){
  c=0.4471207
  b=-0.6536665
  u=8.1518178
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
prob_2_2=prob_gamma(surface_2_2_test_data$AMPLTD_NEED,surface_2_2_test_data$day_count)
prediction=function(x,y){
  z=character()
  for(i in 1:length(x)){
    if(x[i]<0.5)
      z=c(z,"YEL")
    else
      z=c(z,"RED")
  }
  y=cbind(y,Predicted_tag=z)
  test=as.numeric(y$PRIORITY_2)-as.numeric(y$Predicted_tag)
  y=cbind(y,pred_match=test)
  return(y)
}
surface_2_2_pred=prediction(prob_2_2,surface_2_2_test_data) ### In the last Column 0 means match otherwise dismatch
#################################################################################################################
train_surface_2_2_final=ampltd_difference(train_surface_2_2_no_day_no_single,tonnage_combine_all_years,inspect_data)
length(which(train_surface_2_2_final$inspect_count==0))  ## 5
length(which(train_surface_2_2_final$defect_amp>=0 & train_surface_2_2_final$inspect_count==0)) ##2
red_first_surface_2_2=start_red(train_surface_2_2_no_day_no_single)



##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4
train_data_surface_2=rbind(train_surface_2_0_no_day_no_single_trn,train_surface_2_1_no_day_no_single_trn,train_surface_2_2_no_day_no_single_trn)
test_data_surface_2=rbind(train_surface_2_0_no_day_no_single_tst,train_surface_2_1_no_day_no_single_tst,train_surface_2_2_no_day_no_single_tst)
train_data_surface_2_mile_changed=mile_post_change_for_2nd_method(train_data_surface_2)
train_data_surface_2_final=cum_amptd_time_1(train_data_surface_2_mile_changed,inspect_data)
optim(c(10,0.69,1.03),our_likelihood_1,y=train_data_surface_2_final) ### u=7.2278443 c=0.3819970  b=0.7193171
prob_gamma=function(x,t){
  c=0.3819970
  b=0.7193171
  u=7.2278443
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}

################# prepration of test data
test_data_surface_2_ampltd_dif=ampltd_difference(test_data_surface_2,tonnage_combine_all_years,inspect_data)
test_data_surface_2_inspect_0=subset(test_data_surface_2_ampltd_dif,test_data_surface_2_ampltd_dif$inspect_count==0)
test_data_surface_2_threshold_add=add_threshold(test_data_surface_2_inspect_0,tag_limits)
test_data_surface_2_valid=subset(test_data_surface_2_threshold_add,test_data_surface_2_threshold_add$defect_amp_diff>0 & test_data_surface_2_threshold_add$AMPLTD_NEED>0)
prob_2=prob_gamma(test_data_surface_2_valid$AMPLTD_NEED,test_data_surface_2_valid$day_count)
surface_2_pred=prediction(prob_2,test_data_surface_2_valid)
length(which(surface_2_pred$pred_match==0)) ##30
30/40 =0.75 # 75 % accuracy of prediction 


# for line seg 2 and defect type SURFACE
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    u=seq(1,20,length.out = 500)
    c=seq(0.1,2,length.out = 500)
    b=seq(0.01,1.5,length.out = 500)
  }
  length_uni_mile_surf_2_1=length(levels(as.factor(train_surface_2_1_no_day_no_single$MILEPOST)))  ##22
  surface_sample_pos_2_1=sample(1:length_uni_mile_surf_2_1,round(length_uni_mile_surf_2_1*0.20,digits = 0))
  surface_uniq_pos_2_1=levels(as.factor(train_surface_2_1_no_day_no_single$MILEPOST))
  surface_uniq_pos_train_2_1=surface_uniq_pos_2_1[-surface_sample_pos_2_1]
  surface_uniq_pos_test_2_1=surface_uniq_pos_2_1[surface_sample_pos_2_1]
  train_surface_2_1_no_day_no_single_trn=subset(train_surface_2_1_no_day_no_single,train_surface_2_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_2_1)
  train_surface_2_1_no_day_no_single_tst=subset(train_surface_2_1_no_day_no_single,train_surface_2_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_2_1)
  length_uni_mile_surf_2_2=length(levels(as.factor(train_surface_2_2_no_day_no_single$MILEPOST)))  ##314
  surface_sample_pos_2_2=sample(1:length_uni_mile_surf_2_2,round(length_uni_mile_surf_2_2*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  surface_uniq_pos_2_2=levels(as.factor(train_surface_2_2_no_day_no_single$MILEPOST))
  surface_uniq_pos_train_2_2=surface_uniq_pos_2_2[-surface_sample_pos_2_2]
  surface_uniq_pos_test_2_2=surface_uniq_pos_2_2[surface_sample_pos_2_2]
  train_surface_2_2_no_day_no_single_trn=subset(train_surface_2_2_no_day_no_single,train_surface_2_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_2_2)
  train_surface_2_2_no_day_no_single_tst=subset(train_surface_2_2_no_day_no_single,train_surface_2_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_2_2)
  train_data_surface_2=rbind(train_surface_2_1_no_day_no_single_trn,train_surface_2_2_no_day_no_single_trn)
  test_data_surface_2=rbind(train_surface_2_1_no_day_no_single_tst,train_surface_2_2_no_day_no_single_tst)
  train_data_surface_2_mile_changed=mile_post_change_for_2nd_method(train_data_surface_2)
  train_data_surface_2_final=cum_amptd_time_1(train_data_surface_2_mile_changed,inspect_data)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_surface_2_final)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_surface_2_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_surface_2_ampltd_dif=ampltd_difference(test_data_surface_2,tonnage_combine_all_years,inspect_data)
  test_data_surface_2_inspect_0=subset(test_data_surface_2_ampltd_dif,test_data_surface_2_ampltd_dif$inspect_count==0)
  test_data_surface_2_threshold_add=add_threshold(test_data_surface_2_inspect_0,tag_limits)
  test_data_surface_2_valid=subset(test_data_surface_2_threshold_add,test_data_surface_2_threshold_add$defect_amp_diff>0 & test_data_surface_2_threshold_add$AMPLTD_NEED>0)
  prob_2_surf=prob_gamma(test_data_surface_2_valid$AMPLTD_NEED,test_data_surface_2_valid$day_count)
  surface_2_pred=prediction(prob_2_surf,test_data_surface_2_valid)
  match[ii]=length(which(surface_2_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(surface_2_pred))*100
  name_1=paste0("surface_2_pred","_",ii)
  false_positive[ii]=length(which(surface_2_pred$pred_match==1))
  false_negative[ii]=length(which(surface_2_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(surface_2_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(surface_2_pred))*100
  write.xlsx(surface_2_pred,"surface_2_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"surface_2_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$






# unique defect position for 3_0
any(train_surface$LINE_SEG_NBR==3 & train_surface$TRACK_SDTK_NBR==0)
train_surface_3_0=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==3 & train_surface_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_3_0=uniq_defect_position1(train_surface_3_0)
length(uniq_defect_pos_3_0) ## 2
train_surface_3_0=mile_post_uniq(train_surface_3_0,uniq_defect_pos_3_0)
train_surface_3_0_no_day=remove_same_day_repeat(train_surface_3_0)
train_surface_3_0_no_day_no_single=remove_single_obs(train_surface_3_0_no_day)
#train_surface_3_0_final=ampltd_difference(train_surface_3_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
#length(which(train_surface$LINE_SEG_NBR==3 & train_surface$TRACK_SDTK_NBR==0)) ##1
### As here we have only one observation so we can not take it in our analysis


# As the data is not available at some ponits so we had filled that data using the preceeding and successding rows
tonnage_combine_all_years=rbind(tonnage_combine_all_years,tonnage_combine_all_years[16853,])
tonnage_combine_all_years=rbind(tonnage_combine_all_years,tonnage_combine_all_years[16854,])
tonnage_combine_all_years[36806,2]=6
tonnage_combine_all_years[36807,2]=8
tonnage_combine_all_years=rbind(tonnage_combine_all_years,tonnage_combine_all_years[22346,])
tonnage_combine_all_years=rbind(tonnage_combine_all_years,tonnage_combine_all_years[22347,])
tonnage_combine_all_years[36808,2]=6
tonnage_combine_all_years[36809,2]=7
tonnage_combine_all_years[36807,2]=7
tonnage_combine_all_years=tonnage_combine_all_years[order(tonnage_combine_all_years$LINE_SEG_NBR,tonnage_combine_all_years$TRACK_SDTK_NBR,tonnage_combine_all_years$MILEPOST_START,tonnage_combine_all_years$YEAR,tonnage_combine_all_years$MONTH),]
tonnage_combine_all_years=rbind(tonnage_combine_all_years,tonnage_combine_all_years[22441,])
tonnage_combine_all_years=rbind(tonnage_combine_all_years,tonnage_combine_all_years[22442,])
tonnage_combine_all_years[36810,2]=6
tonnage_combine_all_years[36811,2]=7
tonnage_combine_all_years=tonnage_combine_all_years[order(tonnage_combine_all_years$LINE_SEG_NBR,tonnage_combine_all_years$TRACK_SDTK_NBR,tonnage_combine_all_years$MILEPOST_START,tonnage_combine_all_years$YEAR,tonnage_combine_all_years$MONTH),]
tonnage_combine_all_years=rbind(tonnage_combine_all_years,tonnage_combine_all_years[16946,])
tonnage_combine_all_years=rbind(tonnage_combine_all_years,tonnage_combine_all_years[16947,])
tonnage_combine_all_years[36812,2]=6
tonnage_combine_all_years[36813,2]=7
tonnage_combine_all_years=tonnage_combine_all_years[order(tonnage_combine_all_years$LINE_SEG_NBR,tonnage_combine_all_years$TRACK_SDTK_NBR,tonnage_combine_all_years$MILEPOST_START,tonnage_combine_all_years$YEAR,tonnage_combine_all_years$MONTH),]

# unique defect position for 3_1
any(train_surface$LINE_SEG_NBR==3 & train_surface$TRACK_SDTK_NBR==1)
train_surface_3_1=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==3 & train_surface_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_3_1=uniq_defect_position1(train_surface_3_1)
length(uniq_defect_pos_3_1) ## 762
train_surface_3_1=mile_post_uniq(train_surface_3_1,uniq_defect_pos_3_1)
train_surface_3_1_no_day=remove_same_day_repeat(train_surface_3_1)
train_surface_3_1_no_day_no_single=remove_single_obs(train_surface_3_1_no_day)
train_surface_3_1_final=ampltd_difference(train_surface_3_1_no_day_no_single,tonnage_combine_all_years,inspect_data)
length(which(train_surface_3_1_final$inspect_count==0)) ## 850
length(which(train_surface_3_1_final$defect_amp>=0 & train_surface_3_1_final$inspect_count==0)) ##602
red_first_surface_3_1=start_red(train_surface_3_1_no_day_no_single)
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_surface_3_1_no_day_no_single$MILEPOST)))  ##429
## here we have 429 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
429*0.20 ##85.8 =~86 unique defect positions will be in our test data and remaining 429-86 willbe in our train data
surface_sample_pos_3_1=sample(1:429,86) # SRSWOR sample of size 86 from 429 
surface_uniq_pos_3_1=levels(as.factor(train_surface_3_1_no_day_no_single$MILEPOST))
surface_uniq_pos_train_3_1=surface_uniq_pos_3_1[-surface_sample_pos_3_1]
surface_uniq_pos_test_3_1=surface_uniq_pos_3_1[surface_sample_pos_3_1]
train_surface_3_1_no_day_no_single_trn=subset(train_surface_3_1_no_day_no_single,train_surface_3_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_3_1)
train_surface_3_1_no_day_no_single_tst=subset(train_surface_3_1_no_day_no_single,train_surface_3_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_3_1)








# unique defect position for 3_2
any(train_surface$LINE_SEG_NBR==3 & train_surface$TRACK_SDTK_NBR==2)
train_surface_3_2=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==3 & train_surface_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_3_2=uniq_defect_position1(train_surface_3_2)
length(uniq_defect_pos_3_2) ## 553
train_surface_3_2=mile_post_uniq(train_surface_3_2,uniq_defect_pos_3_2)
train_surface_3_2_no_day=remove_same_day_repeat(train_surface_3_2)
train_surface_3_2_no_day_no_single=remove_single_obs(train_surface_3_2_no_day)
train_surface_3_2_final=ampltd_difference(train_surface_3_2_no_day_no_single,tonnage_combine_all_years,inspect_data)
length(which(train_surface_3_2_final$inspect_count==0)) ## 579
length(which(train_surface_3_2_final$defect_amp>=0 & train_surface_3_2_final$inspect_count==0))  ## 417 valid transitions
red_first_surface_3_2=start_red(train_surface_3_2_no_day_no_single)
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_surface_3_2_no_day_no_single$MILEPOST)))  ##314
## here we have 314 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
314*0.20 ##62.8 =~63 unique defect positions will be in our test data and remaining 314-63 willbe in our train data
surface_sample_pos_3_2=sample(1:314,63) # SRSWOR sample of size 63 from 314 
surface_uniq_pos_3_2=levels(as.factor(train_surface_3_2_no_day_no_single$MILEPOST))
surface_uniq_pos_train_3_2=surface_uniq_pos_3_2[-surface_sample_pos_3_2]
surface_uniq_pos_test_3_2=surface_uniq_pos_3_2[surface_sample_pos_3_2]
train_surface_3_2_no_day_no_single_trn=subset(train_surface_3_2_no_day_no_single,train_surface_3_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_3_2)
train_surface_3_2_no_day_no_single_tst=subset(train_surface_3_2_no_day_no_single,train_surface_3_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_3_2)

#######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ main work for method 2nd 
library(xlsx)

for(deep in 1:10){
  if(deep==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
  }
length_uni_mile_surf_3_1=length(levels(as.factor(train_surface_3_1_no_day_no_single$MILEPOST)))  ##429
surface_sample_pos_3_1=sample(1:length_uni_mile_surf_3_1,round(length_uni_mile_surf_3_1*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
surface_uniq_pos_3_1=levels(as.factor(train_surface_3_1_no_day_no_single$MILEPOST))
surface_uniq_pos_train_3_1=surface_uniq_pos_3_1[-surface_sample_pos_3_1]
surface_uniq_pos_test_3_1=surface_uniq_pos_3_1[surface_sample_pos_3_1]
train_surface_3_1_no_day_no_single_trn=subset(train_surface_3_1_no_day_no_single,train_surface_3_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_3_1)
train_surface_3_1_no_day_no_single_tst=subset(train_surface_3_1_no_day_no_single,train_surface_3_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_3_1)
length_uni_mile_surf_3_2=length(levels(as.factor(train_surface_3_2_no_day_no_single$MILEPOST)))  ##314
surface_sample_pos_3_2=sample(1:length_uni_mile_surf_3_2,round(length_uni_mile_surf_3_2*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
surface_uniq_pos_3_2=levels(as.factor(train_surface_3_2_no_day_no_single$MILEPOST))
surface_uniq_pos_train_3_2=surface_uniq_pos_3_2[-surface_sample_pos_3_2]
surface_uniq_pos_test_3_2=surface_uniq_pos_3_2[surface_sample_pos_3_2]
train_surface_3_2_no_day_no_single_trn=subset(train_surface_3_2_no_day_no_single,train_surface_3_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_3_2)
train_surface_3_2_no_day_no_single_tst=subset(train_surface_3_2_no_day_no_single,train_surface_3_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_3_2)
train_data_surface_3=rbind(train_surface_3_1_no_day_no_single_trn,train_surface_3_2_no_day_no_single_trn)
test_data_surface_3=rbind(train_surface_3_1_no_day_no_single_tst,train_surface_3_2_no_day_no_single_tst)
train_data_surface_3_mile_changed=mile_post_change_for_2nd_method(train_data_surface_3)
train_data_surface_3_final=cum_amptd_time_1(train_data_surface_3_mile_changed,inspect_data)
u=seq(1,20,length.out = 500)
c=seq(0.1,2,length.out = 500)
b=seq(0.01,1.5,length.out = 500)
u_sample=sample(u,100,replace = T)
c_sample=sample(c,100,replace = T)
b_sample=sample(b,100,replace = T)
u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
test=lik_value(u_c_b_sample,train_data_surface_3_final)
rw_no=which(test$lik_val==min(test$lik_val))
opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_surface_3_final) ### u=7.2278443 c=0.3819970  b=0.7193171
prob_gamma=function(x,t){
  c=opt$par[2]
  b=opt$par[3]
  u=opt$par[1]
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
################# prepration of test data
test_data_surface_3_ampltd_dif=ampltd_difference(test_data_surface_3,tonnage_combine_all_years,inspect_data)
test_data_surface_3_inspect_0=subset(test_data_surface_3_ampltd_dif,test_data_surface_3_ampltd_dif$inspect_count==0)
test_data_surface_3_threshold_add=add_threshold(test_data_surface_3_inspect_0,tag_limits)
test_data_surface_3_valid=subset(test_data_surface_3_threshold_add,test_data_surface_3_threshold_add$defect_amp_diff>0 & test_data_surface_3_threshold_add$AMPLTD_NEED>0)
prob_3_surf=prob_gamma(test_data_surface_3_valid$AMPLTD_NEED,test_data_surface_3_valid$day_count)
surface_3_pred=prediction(prob_3_surf,test_data_surface_3_valid)
match[deep]=length(which(surface_3_pred$pred_match==0)) ##155
predt_accrcy[deep]=(match[deep]/nrow(surface_3_pred))*100
name_1=paste0("surface_3_pred","_",deep)
false_positive[deep]=length(which(surface_3_pred$pred_match==1))
false_negative[deep]=length(which(surface_3_pred$pred_match==(-1)))
false_positive_per[deep]=(false_positive[deep]/nrow(surface_3_pred))*100
false_negative_per[deep]=(false_negative[deep]/nrow(surface_3_pred))*100
write.xlsx(surface_3_pred,"surface_3_pred.xlsx",sheetName = name_1,append = T,row.names = F)
if(deep==10){
  sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
  write.xlsx(sumary,"surface_3_pred.xlsx",sheetName = "summary",append = T,row.names = F)
}
}





##### Data on line 3 and track 3 are useless as in test data this has never occured
# unique defect position for 3_3
any(train_surface$LINE_SEG_NBR==3 & train_surface$TRACK_SDTK_NBR==3)
train_surface_3_3=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==3 & train_surface_line_track_milepost$TRACK_SDTK_NBR==3)
uniq_defect_pos_3_3=uniq_defect_position1(train_surface_3_3)
length(uniq_defect_pos_3_3) ## 9
train_surface_3_3=mile_post_uniq(train_surface_3_3,uniq_defect_pos_3_3)
train_surface_3_3_no_day=remove_same_day_repeat(train_surface_3_3)
train_surface_3_3_no_day_no_single=remove_single_obs(train_surface_3_3_no_day)
train_surface_3_3_final=ampltd_difference(train_surface_3_3_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_surface_3_3_final)
length(which(train_surface_3_3_final$inspect_count==0))  ##2
length(which(train_surface_3_3_final$defect_amp>=0 & train_surface_3_3_final$inspect_count==0)) ##2
# red_first_surface_3_3=start_red(train_surface_3_3_no_day_no_single)  As there are no ponts where we get RED->Yel


### We will not do any work on this as there is no data in test data for line segment 4
# unique defect position for 4_0
any(train_surface$LINE_SEG_NBR==4 & train_surface$TRACK_SDTK_NBR==0)
train_surface_4_0=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==4 & train_surface_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_4_0=uniq_defect_position1(train_surface_4_0)
length(uniq_defect_pos_4_0) ## 50
train_surface_4_0=mile_post_uniq(train_surface_4_0,uniq_defect_pos_4_0)
train_surface_4_0_no_day=remove_same_day_repeat(train_surface_4_0)
train_surface_4_0_no_day_no_single=remove_single_obs(train_surface_4_0_no_day)
## Creating problem at milepost 23.7979194 as at this milepost we have data only for year 2007 but to calculate this we need the data upto 2010 so we will manually remove this point from data set
train_surface_4_0_no_day_no_single=train_surface_4_0_no_day_no_single[-c(4,5),]
train_surface_4_0_final=ampltd_difference(train_surface_4_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
length(which(train_surface_4_0_final$inspect_count==0)) ##7
length(which(train_surface_4_0_final$defect_amp>=0 & train_surface_4_0_final$inspect_count==0)) ##4
red_first_surface_4_0=start_red(train_surface_4_0_no_day_no_single)


# unique defect position for 4_1
any(train_surface$LINE_SEG_NBR==4 & train_surface$TRACK_SDTK_NBR==1)
train_surface_4_1=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==4 & train_surface_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_4_1=uniq_defect_position1(train_surface_4_1)
length(uniq_defect_pos_4_1) ## 2
train_surface_4_1=mile_post_uniq(train_surface_4_1,uniq_defect_pos_4_1)
train_surface_4_1_no_day=remove_same_day_repeat(train_surface_4_1)
train_surface_4_1_no_day_no_single=remove_single_obs(train_surface_4_1_no_day)
train_surface_4_1_final=ampltd_difference(train_surface_4_1_no_day_no_single,tonnage_combine_all_years,inspect_data)
length(which(train_surface$LINE_SEG_NBR==4 & train_surface$TRACK_SDTK_NBR==1))  ##1
### As here we have only one observation so we can not take it in our analysis

### Now we will combine those data which have non negative amplitude and 0 inspection record
train_surface_1_0_valid_trans=train_surface_1_0_final[which(train_surface_1_0_final$defect_amp>=0 & train_surface_1_0_final$inspect_count==0),]
train_surface_2_0_valid_trans=train_surface_2_0_final[which(train_surface_2_0_final$defect_amp>=0 & train_surface_2_0_final$inspect_count==0),]
train_surface_2_1_valid_trans=train_surface_2_1_final[which(train_surface_2_1_final$defect_amp>=0 & train_surface_2_1_final$inspect_count==0),]
train_surface_2_2_valid_trans=train_surface_2_2_final[which(train_surface_2_2_final$defect_amp>=0 & train_surface_2_2_final$inspect_count==0),]
train_surface_3_1_valid_trans=train_surface_3_1_final[which(train_surface_3_1_final$defect_amp>=0 & train_surface_3_1_final$inspect_count==0),]
train_surface_3_2_valid_trans=train_surface_3_2_final[which(train_surface_3_2_final$defect_amp>=0 & train_surface_3_2_final$inspect_count==0),]
train_surface_3_3_valid_trans=train_surface_3_3_final[which(train_surface_3_3_final$defect_amp>=0 & train_surface_3_3_final$inspect_count==0),]
train_surface_4_0_valid_trans=train_surface_4_0_final[which(train_surface_4_0_final$defect_amp>=0 & train_surface_4_0_final$inspect_count==0),]

train_surface_valid=rbind(train_surface_1_0_valid_trans,train_surface_2_0_valid_trans,train_surface_2_1_valid_trans,train_surface_2_2_valid_trans,train_surface_3_1_valid_trans,train_surface_3_2_valid_trans,train_surface_3_3_valid_trans,train_surface_4_0_valid_trans)
View(train_surface_valid)  ## 1630 26




##############################################################




# now we will do the same work for DIP Type of defect

train_dip$TEST_DT=as.Date(train_dip$TEST_DT,format="%d%b%Y")
train_dip=train_dip[order(train_dip$LINE_SEG_NBR,train_dip$TRACK_SDTK_NBR,train_dip$TEST_DT,train_dip$MILEPOST),]
summary(train_dip)
train_dip_line_track_milepost=train_dip[order(train_dip$LINE_SEG_NBR,train_dip$TRACK_SDTK_NBR,train_dip$MILEPOST),]
View(train_dip_line_track_milepost)

#### line seg 1 and track 0
train_dip_1_0=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==1 & train_dip_line_track_milepost$TRACK_SDTK_NBR==0)
View(train_dip_1_0)
uniq_defect_pos_1_0_dip=uniq_defect_position1(train_dip_1_0)
uniq_defect_pos_1_0_dip
length(uniq_defect_pos_1_0_dip) ## 340
train_dip_1_0=mile_post_uniq(train_dip_1_0,uniq_defect_pos_1_0_dip)
View(train_dip_1_0)
train_dip_1_0_no_day=remove_same_day_repeat(train_dip_1_0)
train_dip_1_0_no_day_no_single=remove_single_obs(train_dip_1_0_no_day)
View(train_dip_1_0_no_day_no_single)
train_dip_1_0_final=ampltd_difference(train_dip_1_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_1_0_final)
length(which(train_dip_1_0_final$inspect_count==0))##271
length(which(train_dip_1_0_final$defect_amp>=0 & train_dip_1_0_final$inspect_count==0))##168
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_dip_1_0_no_day_no_single$MILEPOST)))  ##182
## here we have 182 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
182*0.20 ##36.4 =~36 unique defect positions will be in our test data and remaining 182-36 willbe in our train data
dip_sample_pos_1_0=sample(1:182,36) # SRSWOR sample of size 36 from 182 
dip_uniq_pos_1_0=levels(as.factor(train_dip_1_0_no_day_no_single$MILEPOST))
dip_uniq_pos_train_1_0=dip_uniq_pos_1_0[-dip_sample_pos_1_0]
dip_uniq_pos_test_1_0=dip_uniq_pos_1_0[dip_sample_pos_1_0]
train_dip_1_0_no_day_no_single_trn=subset(train_dip_1_0_no_day_no_single,train_dip_1_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_1_0)
train_dip_1_0_no_day_no_single_tst=subset(train_dip_1_0_no_day_no_single,train_dip_1_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_1_0)
#######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ main work for method 2nd 
train_data_dip_1=train_dip_1_0_no_day_no_single_trn
test_data_dip_1=train_dip_1_0_no_day_no_single_tst
train_data_dip_1_mile_changed=mile_post_change_for_2nd_method(train_data_dip_1)
train_data_dip_1_final=cum_amptd_time_1(train_data_dip_1_mile_changed,inspect_data)
optim(c(10,0.69,1.03),our_likelihood_1,y=train_data_dip_1_final) ### u= 6.8886032 c=0.6749676  b=0.4876595
prob_gamma=function(x,t){
  c=0.6749676
  b=0.4876595
  u=6.8886032
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
################# prepration of test data
test_data_dip_1_ampltd_dif=ampltd_difference(test_data_dip_1,tonnage_combine_all_years,inspect_data)
test_data_dip_1_inspect_0=subset(test_data_dip_1_ampltd_dif,test_data_dip_1_ampltd_dif$inspect_count==0)
test_data_dip_1_threshold_add=add_threshold(test_data_dip_1_inspect_0,tag_limits)
test_data_dip_1_valid=subset(test_data_dip_1_threshold_add,test_data_dip_1_threshold_add$defect_amp_diff>0 & test_data_dip_1_threshold_add$AMPLTD_NEED>0)
prob_1_dip=prob_gamma(test_data_dip_1_valid$AMPLTD_NEED,test_data_dip_1_valid$day_count)
dip_1_pred=prediction(prob_1_dip,test_data_dip_1_valid)
length(which(dip_1_pred$pred_match==0)) ##25
nrow(dip_1_pred) ###29
25/29==0.862069 # 86 % accuracy of prediction 

# for line seg 1 and defect type dip
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    u=seq(1,20,length.out = 500)
    c=seq(0.1,2,length.out = 500)
    b=seq(0.01,1.5,length.out = 500)
  }
  length_uni_mile_dip_1_0=length(levels(as.factor(train_dip_1_0_no_day_no_single$MILEPOST)))  ##22
  dip_sample_pos_1_0=sample(1:length_uni_mile_dip_1_0,round(length_uni_mile_dip_1_0*0.20,digits = 0))
  dip_uniq_pos_1_0=levels(as.factor(train_dip_1_0_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_1_0=dip_uniq_pos_1_0[-dip_sample_pos_1_0]
  dip_uniq_pos_test_1_0=dip_uniq_pos_1_0[dip_sample_pos_1_0]
  train_dip_1_0_no_day_no_single_trn=subset(train_dip_1_0_no_day_no_single,train_dip_1_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_1_0)
  train_dip_1_0_no_day_no_single_tst=subset(train_dip_1_0_no_day_no_single,train_dip_1_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_1_0)
  train_data_dip_1=rbind(train_dip_1_0_no_day_no_single_trn)
  test_data_dip_1=rbind(train_dip_1_0_no_day_no_single_tst)
  train_data_dip_1_mile_changed=mile_post_change_for_2nd_method(train_data_dip_1)
  train_data_dip_1_final=cum_amptd_time_1(train_data_dip_1_mile_changed,inspect_data)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_dip_1_final)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_dip_1_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_dip_1_ampltd_dif=ampltd_difference(test_data_dip_1,tonnage_combine_all_years,inspect_data)
  test_data_dip_1_inspect_0=subset(test_data_dip_1_ampltd_dif,test_data_dip_1_ampltd_dif$inspect_count==0)
  test_data_dip_1_threshold_add=add_threshold(test_data_dip_1_inspect_0,tag_limits)
  test_data_dip_1_valid=subset(test_data_dip_1_threshold_add,test_data_dip_1_threshold_add$defect_amp_diff>0 & test_data_dip_1_threshold_add$AMPLTD_NEED>0)
  prob_1_surf=prob_gamma(test_data_dip_1_valid$AMPLTD_NEED,test_data_dip_1_valid$day_count)
  dip_1_pred=prediction(prob_1_surf,test_data_dip_1_valid)
  match[ii]=length(which(dip_1_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(dip_1_pred))*100
  name_1=paste0("dip_1_pred","_",ii)
  false_positive[ii]=length(which(dip_1_pred$pred_match==1))
  false_negative[ii]=length(which(dip_1_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(dip_1_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(dip_1_pred))*100
  write.xlsx(dip_1_pred,"dip_1_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"dip_1_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#### line seg 2 and track 0
## problem has occured as at mile post 275.8618694 has no record on tonnage data so we removed them
train_dip_2_0_no_day_no_single=train_dip_2_0_no_day_no_single[-which(train_dip_2_0_no_day_no_single$MILEPOST==train_dip_2_0_no_day_no_single[523,2]),] ## at this stage we saw that command was not working if we give the value and will work if the location is provided
train_dip_2_0=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==2 & train_dip_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_2_0_dip=uniq_defect_position1(train_dip_2_0)
uniq_defect_pos_2_0_dip
length(uniq_defect_pos_2_0_dip) ## 291
train_dip_2_0=mile_post_uniq(train_dip_2_0,uniq_defect_pos_2_0_dip)
train_dip_2_0_no_day=remove_same_day_repeat(train_dip_2_0)
train_dip_2_0_no_day_no_single=remove_single_obs(train_dip_2_0_no_day)
train_dip_2_0_final=ampltd_difference(train_dip_2_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_2_0_final)
length(which(train_dip_2_0_final$inspect_count==0))##250
length(which(train_dip_2_0_final$defect_amp>=0 & train_dip_2_0_final$inspect_count==0))##167
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_dip_2_0_no_day_no_single$MILEPOST)))  ##137
## here we have 137 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
137*0.20 ##27.4 =~27 unique defect positions will be in our test data and remaining 137-27 willbe in our train data
dip_sample_pos_2_0=sample(1:137,27) # SRSWOR sample of size 27 from 137 
dip_uniq_pos_2_0=levels(as.factor(train_dip_2_0_no_day_no_single$MILEPOST))
dip_uniq_pos_train_2_0=dip_uniq_pos_2_0[-dip_sample_pos_2_0]
dip_uniq_pos_test_2_0=dip_uniq_pos_2_0[dip_sample_pos_2_0]
train_dip_2_0_no_day_no_single_trn=subset(train_dip_2_0_no_day_no_single,train_dip_2_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_2_0)
train_dip_2_0_no_day_no_single_tst=subset(train_dip_2_0_no_day_no_single,train_dip_2_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_2_0)



#### line seg 2 and track 1
train_dip_2_1=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==2 & train_dip_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_2_1_dip=uniq_defect_position1(train_dip_2_1)
uniq_defect_pos_2_1_dip
length(uniq_defect_pos_2_1_dip) ## 
train_dip_2_1=mile_post_uniq(train_dip_2_1,uniq_defect_pos_2_1_dip)
train_dip_2_1_no_day=remove_same_day_repeat(train_dip_2_1)
train_dip_2_1_no_day_no_single=remove_single_obs(train_dip_2_1_no_day)
train_dip_2_1_final=ampltd_difference(train_dip_2_1_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_2_1_final)
length(which(train_dip_2_1_final$inspect_count==0))##36
length(which(train_dip_2_1_final$defect_amp>=0 & train_dip_2_1_final$inspect_count==0))##25
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_dip_2_1_no_day_no_single$MILEPOST)))  ##22
## here we have 22 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
22*0.20 ##4.4 =~4 unique defect positions will be in our test data and remaining 22-4 willbe in our train data
dip_sample_pos_2_1=sample(1:22,4) # SRSWOR sample of size 4 from 22 
dip_uniq_pos_2_1=levels(as.factor(train_dip_2_1_no_day_no_single$MILEPOST))
dip_uniq_pos_train_2_1=dip_uniq_pos_2_1[-dip_sample_pos_2_1]
dip_uniq_pos_test_2_1=dip_uniq_pos_2_1[dip_sample_pos_2_1]
train_dip_2_1_no_day_no_single_trn=subset(train_dip_2_1_no_day_no_single,train_dip_2_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_2_1)
train_dip_2_1_no_day_no_single_tst=subset(train_dip_2_1_no_day_no_single,train_dip_2_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_2_1)
#######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ main work for method 2nd 
train_data_dip_2=rbind(train_dip_2_0_no_day_no_single_trn,train_dip_2_1_no_day_no_single_trn)
test_data_dip_2=rbind(train_dip_2_0_no_day_no_single_tst, train_dip_2_1_no_day_no_single_tst)
train_data_dip_2_mile_changed=mile_post_change_for_2nd_method(train_data_dip_2)
train_data_dip_2_final=cum_amptd_time_1(train_data_dip_2_mile_changed,inspect_data)
optim(c(10,0.69,1.03),our_likelihood_1,y=train_data_dip_2_final) ### u= 7.9470123 c=0.4618675  b=0.6726382
prob_gamma=function(x,t){
  c=0.4618675
  b=0.6726382
  u=7.9470123
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
################# prepration of test data
test_data_dip_2_ampltd_dif=ampltd_difference(test_data_dip_2,tonnage_combine_all_years,inspect_data)
test_data_dip_2_inspect_0=subset(test_data_dip_2_ampltd_dif,test_data_dip_2_ampltd_dif$inspect_count==0)
test_data_dip_2_threshold_add=add_threshold(test_data_dip_2_inspect_0,tag_limits)
test_data_dip_2_valid=subset(test_data_dip_2_threshold_add,test_data_dip_2_threshold_add$defect_amp_diff>0 & test_data_dip_2_threshold_add$AMPLTD_NEED>0)
prob_2_dip=prob_gamma(test_data_dip_2_valid$AMPLTD_NEED,test_data_dip_2_valid$day_count)
dip_2_pred=prediction(prob_2_dip,test_data_dip_2_valid)
length(which(dip_2_pred$pred_match==0)) ##15
nrow(dip_2_pred) ###19
15/19==0.7894737 # ~=79 % accuracy of prediction 

# for line seg 2 and defect type dip
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
  }
  length_uni_mile_dip_2_0=length(levels(as.factor(train_dip_2_0_no_day_no_single$MILEPOST)))  ##229
  dip_sample_pos_2_0=sample(1:length_uni_mile_dip_2_0,round(length_uni_mile_dip_2_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  dip_uniq_pos_2_0=levels(as.factor(train_dip_2_0_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_2_0=dip_uniq_pos_2_0[-dip_sample_pos_2_0]
  dip_uniq_pos_test_2_0=dip_uniq_pos_2_0[dip_sample_pos_2_0]
  train_dip_2_0_no_day_no_single_trn=subset(train_dip_2_0_no_day_no_single,train_dip_2_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_2_0)
  train_dip_2_0_no_day_no_single_tst=subset(train_dip_2_0_no_day_no_single,train_dip_2_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_2_0)
  length_uni_mile_dip_2_1=length(levels(as.factor(train_dip_2_1_no_day_no_single$MILEPOST)))  ##212
  dip_sample_pos_2_1=sample(1:length_uni_mile_dip_2_1,round(length_uni_mile_dip_2_1*0.20,digits = 0)) # SRSWOR sample of size 86 from 229 
  dip_uniq_pos_2_1=levels(as.factor(train_dip_2_1_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_2_1=dip_uniq_pos_2_1[-dip_sample_pos_2_1]
  dip_uniq_pos_test_2_1=dip_uniq_pos_2_1[dip_sample_pos_2_1]
  train_dip_2_1_no_day_no_single_trn=subset(train_dip_2_1_no_day_no_single,train_dip_2_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_2_1)
  train_dip_2_1_no_day_no_single_tst=subset(train_dip_2_1_no_day_no_single,train_dip_2_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_2_1)
  train_data_dip_2=rbind(train_dip_2_0_no_day_no_single_trn,train_dip_2_1_no_day_no_single_trn)
  test_data_dip_2=rbind(train_dip_2_0_no_day_no_single_tst,train_dip_2_1_no_day_no_single_tst)
  train_data_dip_2_mile_changed=mile_post_change_for_2nd_method(train_data_dip_2)
  train_data_dip_2_final=cum_amptd_time_1(train_data_dip_2_mile_changed,inspect_data)
  u=seq(1,20,length.out = 500)
  c=seq(0.1,2,length.out = 500)
  b=seq(0.01,1.5,length.out = 500)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_dip_2_final)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_dip_2_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_dip_2_ampltd_dif=ampltd_difference(test_data_dip_2,tonnage_combine_all_years,inspect_data)
  test_data_dip_2_inspect_0=subset(test_data_dip_2_ampltd_dif,test_data_dip_2_ampltd_dif$inspect_count==0)
  test_data_dip_2_threshold_add=add_threshold(test_data_dip_2_inspect_0,tag_limits)
  test_data_dip_2_valid=subset(test_data_dip_2_threshold_add,test_data_dip_2_threshold_add$defect_amp_diff>0 & test_data_dip_2_threshold_add$AMPLTD_NEED>0)
  prob_2_dip=prob_gamma(test_data_dip_2_valid$AMPLTD_NEED,test_data_dip_2_valid$day_count)
  dip_2_pred=prediction(prob_2_dip,test_data_dip_2_valid)
  match[ii]=length(which(dip_2_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(dip_2_pred))*100
  name_1=paste0("dip_2_pred","_",ii)
  false_positive[ii]=length(which(dip_2_pred$pred_match==1))
  false_negative[ii]=length(which(dip_2_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(dip_2_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(dip_2_pred))*100
  write.xlsx(dip_2_pred,"dip_2_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"dip_2_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



#### line seg 2 and track 2 #### at this line and track combination there is data in test data so we will not do of it
length(which(train_dip$LINE_SEG_NBR==2 & train_dip$TRACK_SDTK_NBR==2))
train_dip_2_2=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==2 & train_dip_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_2_2_dip=uniq_defect_position1(train_dip_2_2)
uniq_defect_pos_2_2_dip
length(uniq_defect_pos_2_2_dip) ## 
train_dip_2_2=mile_post_uniq(train_dip_2_2,uniq_defect_pos_2_2_dip)
train_dip_2_2_no_day=remove_same_day_repeat(train_dip_2_2)
train_dip_2_2_no_day_no_single=remove_single_obs(train_dip_2_2_no_day)
train_dip_2_2_final=ampltd_difference(train_dip_2_2_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_2_2_final)
length(which(train_dip_2_2_final$inspect_count==0))##3
length(which(train_dip_2_2_final$defect_amp>=0 & train_dip_2_2_final$inspect_count==0))##2


#### line seg 3 and track 0  $$$$$ Never asked in the test data set for this line and segment combination
length(which(train_dip$LINE_SEG_NBR==3 & train_dip$TRACK_SDTK_NBR==0))
train_dip_3_0=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==3 & train_dip_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_3_0_dip=uniq_defect_position1(train_dip_3_0)
uniq_defect_pos_3_0_dip
length(uniq_defect_pos_3_0_dip) ## 
train_dip_3_0=mile_post_uniq(train_dip_3_0,uniq_defect_pos_3_0_dip)
train_dip_3_0_no_day=remove_same_day_repeat(train_dip_3_0)
train_dip_3_0_no_day_no_single=remove_single_obs(train_dip_3_0_no_day)
train_dip_3_0_final=ampltd_difference(train_dip_3_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_3_0_final)
length(which(train_dip_3_0_final$inspect_count==0))##10
length(which(train_dip_3_0_final$defect_amp>=0 & train_dip_3_0_final$inspect_count==0))##9



#### line seg 3 and track 1
length(which(train_dip$LINE_SEG_NBR==3 & train_dip$TRACK_SDTK_NBR==1))
train_dip_3_1=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==3 & train_dip_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_3_1_dip=uniq_defect_position1(train_dip_3_1)
uniq_defect_pos_3_1_dip
length(uniq_defect_pos_3_1_dip) ## 
train_dip_3_1=mile_post_uniq(train_dip_3_1,uniq_defect_pos_3_1_dip)
train_dip_3_1_no_day=remove_same_day_repeat(train_dip_3_1)
train_dip_3_1_no_day_no_single=remove_single_obs(train_dip_3_1_no_day)
train_dip_3_1_final=ampltd_difference(train_dip_3_1_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_3_1_final)
length(which(train_dip_3_1_final$inspect_count==0))##302
length(which(train_dip_3_1_final$defect_amp>=0 & train_dip_3_1_final$inspect_count==0))##207
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_dip_3_1_no_day_no_single$MILEPOST)))  ##236
## here we have 236 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
236*0.20 ##47.2=~47 unique defect positions will be in our test data and remaining 236-47 willbe in our train data
dip_sample_pos_3_1=sample(1:236,47) # SRSWOR sample of size 47 from 236 
dip_uniq_pos_3_1=levels(as.factor(train_dip_3_1_no_day_no_single$MILEPOST))
dip_uniq_pos_train_3_1=dip_uniq_pos_3_1[-dip_sample_pos_3_1]
dip_uniq_pos_test_3_1=dip_uniq_pos_3_1[dip_sample_pos_3_1]
train_dip_3_1_no_day_no_single_trn=subset(train_dip_3_1_no_day_no_single,train_dip_3_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_3_1)
train_dip_3_1_no_day_no_single_tst=subset(train_dip_3_1_no_day_no_single,train_dip_3_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_3_1)



#### line seg 3 and track 2
length(which(train_dip$LINE_SEG_NBR==3 & train_dip$TRACK_SDTK_NBR==2))
train_dip_3_2=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==3 & train_dip_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_3_2_dip=uniq_defect_position1(train_dip_3_2)
uniq_defect_pos_3_2_dip
length(uniq_defect_pos_3_2_dip) ## 
train_dip_3_2=mile_post_uniq(train_dip_3_2,uniq_defect_pos_3_2_dip)
train_dip_3_2_no_day=remove_same_day_repeat(train_dip_3_2)
train_dip_3_2_no_day_no_single=remove_single_obs(train_dip_3_2_no_day)
train_dip_3_2_final=ampltd_difference(train_dip_3_2_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_3_2_final)
length(which(train_dip_3_2_final$inspect_count==0))##204
length(which(train_dip_3_2_final$defect_amp>=0 & train_dip_3_2_final$inspect_count==0))##126
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_dip_3_2_no_day_no_single$MILEPOST)))  ##173
## here we have 173 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
173*0.20 ##34.6=~35 unique defect positions will be in our test data and remaining 173-35 willbe in our train data
dip_sample_pos_3_2=sample(1:173,35) # SRSWOR sample of size 35 from 173 
dip_uniq_pos_3_2=levels(as.factor(train_dip_3_2_no_day_no_single$MILEPOST))
dip_uniq_pos_train_3_2=dip_uniq_pos_3_2[-dip_sample_pos_3_2]
dip_uniq_pos_test_3_2=dip_uniq_pos_3_2[dip_sample_pos_3_2]
train_dip_3_2_no_day_no_single_trn=subset(train_dip_3_2_no_day_no_single,train_dip_3_2_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_3_2)
train_dip_3_2_no_day_no_single_tst=subset(train_dip_3_2_no_day_no_single,train_dip_3_2_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_3_2)
#######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ main work for method 2nd 
train_data_dip_3=rbind(train_dip_3_1_no_day_no_single_trn,train_dip_3_2_no_day_no_single_trn)
test_data_dip_3=rbind(train_dip_3_1_no_day_no_single_tst, train_dip_3_2_no_day_no_single_tst)
train_data_dip_3_mile_changed=mile_post_change_for_2nd_method(train_data_dip_3)
train_data_dip_3_final=cum_amptd_time_1(train_data_dip_3_mile_changed,inspect_data)
optim(c(10,0.69,1.03),our_likelihood_1,y=train_data_dip_3_final) ### u= 9.2737518 c=0.4771250  b=0.6756932
prob_gamma=function(x,t){
  c=0.4771250
  b=0.6756932
  u=9.2737518
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
################# prepration of test data
test_data_dip_3_ampltd_dif=ampltd_difference(test_data_dip_3,tonnage_combine_all_years,inspect_data)
test_data_dip_3_inspect_0=subset(test_data_dip_3_ampltd_dif,test_data_dip_3_ampltd_dif$inspect_count==0)
test_data_dip_3_threshold_add=add_threshold(test_data_dip_3_inspect_0,tag_limits)
test_data_dip_3_valid=subset(test_data_dip_3_threshold_add,test_data_dip_3_threshold_add$defect_amp_diff>0 & test_data_dip_3_threshold_add$AMPLTD_NEED>0)
prob_3_dip=prob_gamma(test_data_dip_3_valid$AMPLTD_NEED,test_data_dip_3_valid$day_count)
dip_3_pred=prediction(prob_3_dip,test_data_dip_3_valid)
length(which(dip_3_pred$pred_match==0)) ##40
nrow(dip_3_pred) ###63
40/63==0.6349206 # ~=63 % accuracy of prediction  There is one error in defining tag in this data set In row no 21 there should be Yellow according to rule but they have written it as RED


# for line seg 3 and defect type dip
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
  }
  length_uni_mile_dip_3_1=length(levels(as.factor(train_dip_3_1_no_day_no_single$MILEPOST)))  ##429
  dip_sample_pos_3_1=sample(1:length_uni_mile_dip_3_1,round(length_uni_mile_dip_3_1*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  dip_uniq_pos_3_1=levels(as.factor(train_dip_3_1_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_3_1=dip_uniq_pos_3_1[-dip_sample_pos_3_1]
  dip_uniq_pos_test_3_1=dip_uniq_pos_3_1[dip_sample_pos_3_1]
  train_dip_3_1_no_day_no_single_trn=subset(train_dip_3_1_no_day_no_single,train_dip_3_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_3_1)
  train_dip_3_1_no_day_no_single_tst=subset(train_dip_3_1_no_day_no_single,train_dip_3_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_3_1)
  length_uni_mile_dip_3_2=length(levels(as.factor(train_dip_3_2_no_day_no_single$MILEPOST)))  ##314
  dip_sample_pos_3_2=sample(1:length_uni_mile_dip_3_2,round(length_uni_mile_dip_3_2*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  dip_uniq_pos_3_2=levels(as.factor(train_dip_3_2_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_3_2=dip_uniq_pos_3_2[-dip_sample_pos_3_2]
  dip_uniq_pos_test_3_2=dip_uniq_pos_3_2[dip_sample_pos_3_2]
  train_dip_3_2_no_day_no_single_trn=subset(train_dip_3_2_no_day_no_single,train_dip_3_2_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_3_2)
  train_dip_3_2_no_day_no_single_tst=subset(train_dip_3_2_no_day_no_single,train_dip_3_2_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_3_2)
  train_data_dip_3=rbind(train_dip_3_1_no_day_no_single_trn,train_dip_3_2_no_day_no_single_trn)
  test_data_dip_3=rbind(train_dip_3_1_no_day_no_single_tst,train_dip_3_2_no_day_no_single_tst)
  train_data_dip_3_mile_changed=mile_post_change_for_2nd_method(train_data_dip_3)
  train_data_dip_3_final=cum_amptd_time_1(train_data_dip_3_mile_changed,inspect_data)
  u=seq(1,20,length.out = 500)
  c=seq(0.1,2,length.out = 500)
  b=seq(0.01,1.5,length.out = 500)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_dip_3_final)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_dip_3_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_dip_3_ampltd_dif=ampltd_difference(test_data_dip_3,tonnage_combine_all_years,inspect_data)
  test_data_dip_3_inspect_0=subset(test_data_dip_3_ampltd_dif,test_data_dip_3_ampltd_dif$inspect_count==0)
  test_data_dip_3_threshold_add=add_threshold(test_data_dip_3_inspect_0,tag_limits)
  test_data_dip_3_valid=subset(test_data_dip_3_threshold_add,test_data_dip_3_threshold_add$defect_amp_diff>0 & test_data_dip_3_threshold_add$AMPLTD_NEED>0)
  prob_3_dip=prob_gamma(test_data_dip_3_valid$AMPLTD_NEED,test_data_dip_3_valid$day_count)
  dip_3_pred=prediction(prob_3_dip,test_data_dip_3_valid)
  match[ii]=length(which(dip_3_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(dip_3_pred))*100
  name_1=paste0("dip_3_pred","_",ii)
  false_positive[ii]=length(which(dip_3_pred$pred_match==1))
  false_negative[ii]=length(which(dip_3_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(dip_3_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(dip_3_pred))*100
  write.xlsx(dip_3_pred,"dip_3_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"dip_3_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$




#### line seg 3 and track 3 ### Never asked in the test data set
length(which(train_dip$LINE_SEG_NBR==3 & train_dip$TRACK_SDTK_NBR==3))
train_dip_3_3=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==3 & train_dip_line_track_milepost$TRACK_SDTK_NBR==3)
uniq_defect_pos_3_3_dip=uniq_defect_position1(train_dip_3_3)
uniq_defect_pos_3_3_dip
length(uniq_defect_pos_3_3_dip) ## 
train_dip_3_3=mile_post_uniq(train_dip_3_3,uniq_defect_pos_3_3_dip)
train_dip_3_3_no_day=remove_same_day_repeat(train_dip_3_3)
train_dip_3_3_no_day_no_single=remove_single_obs(train_dip_3_3_no_day)
train_dip_3_3_final=ampltd_difference(train_dip_3_3_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_3_3_final)
length(which(train_dip_3_3_final$inspect_count==0))##0
length(which(train_dip_3_3_final$defect_amp>=0 & train_dip_3_3_final$inspect_count==0))##0


#### line seg 4 and track 0
length(which(train_dip$LINE_SEG_NBR==4 & train_dip$TRACK_SDTK_NBR==0))
train_dip_4_0=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==4 & train_dip_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_4_0_dip=uniq_defect_position1(train_dip_4_0)
uniq_defect_pos_4_0_dip
length(uniq_defect_pos_4_0_dip) ## 
train_dip_4_0=mile_post_uniq(train_dip_4_0,uniq_defect_pos_4_0_dip)
train_dip_4_0_no_day=remove_same_day_repeat(train_dip_4_0)
train_dip_4_0_no_day_no_single=remove_single_obs(train_dip_4_0_no_day)
# problem is occurring at milepost 12.0795494 so remove that point
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[8,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[9,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[9,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[13,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[18,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[16,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[21,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[21,2]),]
train_dip_4_0_final=ampltd_difference(train_dip_4_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_4_0_final)
length(which(train_dip_4_0_final$inspect_count==0))##233
length(which(train_dip_4_0_final$defect_amp>=0 & train_dip_4_0_final$inspect_count==0))##168

#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_dip_4_0_no_day_no_single$MILEPOST)))  ##129
## here we have 129 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
129*0.20 ##25.8 =~26 unique defect positions will be in our test data and remaining 129-26 willbe in our train data
dip_sample_pos_4_0=sample(1:129,26) # SRSWOR sample of size 26 from 129 
dip_uniq_pos_4_0=levels(as.factor(train_dip_4_0_no_day_no_single$MILEPOST))
dip_uniq_pos_train_4_0=dip_uniq_pos_4_0[-dip_sample_pos_4_0]
dip_uniq_pos_test_4_0=dip_uniq_pos_4_0[dip_sample_pos_4_0]
train_dip_4_0_no_day_no_single_trn=subset(train_dip_4_0_no_day_no_single,train_dip_4_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_4_0)
train_dip_4_0_no_day_no_single_tst=subset(train_dip_4_0_no_day_no_single,train_dip_4_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_4_0)
#######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ main work for method 2nd 
train_data_dip_4=train_dip_4_0_no_day_no_single_trn
test_data_dip_4=train_dip_4_0_no_day_no_single_tst
train_data_dip_4_mile_changed=mile_post_change_for_2nd_method(train_data_dip_4)
train_data_dip_4_final=cum_amptd_time_1(train_data_dip_4_mile_changed,inspect_data)
optim(c(10,0.69,1.03),our_likelihood_1,y=train_data_dip_4_final) ### u= 11.0490693 c=0.2686848  b= 0.8140151
prob_gamma=function(x,t){
  c=0.2686848
  b=0.8140151
  u=11.0490693
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
################# prepration of test data
test_data_dip_4_ampltd_dif=ampltd_difference(test_data_dip_4,tonnage_combine_all_years,inspect_data)
test_data_dip_4_inspect_0=subset(test_data_dip_4_ampltd_dif,test_data_dip_4_ampltd_dif$inspect_count==0)
test_data_dip_4_threshold_add=add_threshold(test_data_dip_4_inspect_0,tag_limits)
test_data_dip_4_valid=subset(test_data_dip_4_threshold_add,test_data_dip_4_threshold_add$defect_amp_diff>0 & test_data_dip_4_threshold_add$AMPLTD_NEED>0)
prob_4_dip=prob_gamma(test_data_dip_4_valid$AMPLTD_NEED,test_data_dip_4_valid$day_count)
dip_4_pred=prediction(prob_4_dip,test_data_dip_4_valid)
length(which(dip_4_pred$pred_match==0)) ##20
nrow(dip_4_pred) ###29
20/29==0.6896552 # 69 % accuracy of prediction 


# for line seg 4 and defect type dip
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
  }
  length_uni_mile_dip_4_0=length(levels(as.factor(train_dip_4_0_no_day_no_single$MILEPOST)))  ##449
  dip_sample_pos_4_0=sample(1:length_uni_mile_dip_4_0,round(length_uni_mile_dip_4_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  dip_uniq_pos_4_0=levels(as.factor(train_dip_4_0_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_4_0=dip_uniq_pos_4_0[-dip_sample_pos_4_0]
  dip_uniq_pos_test_4_0=dip_uniq_pos_4_0[dip_sample_pos_4_0]
  train_dip_4_0_no_day_no_single_trn=subset(train_dip_4_0_no_day_no_single,train_dip_4_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_4_0)
  train_dip_4_0_no_day_no_single_tst=subset(train_dip_4_0_no_day_no_single,train_dip_4_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_4_0) 
  train_data_dip_4=rbind(train_dip_4_0_no_day_no_single_trn)
  test_data_dip_4=rbind(train_dip_4_0_no_day_no_single_tst)
  train_data_dip_4_mile_changed=mile_post_change_for_2nd_method(train_data_dip_4)
  train_data_dip_4_final=cum_amptd_time_1(train_data_dip_4_mile_changed,inspect_data)
  u=seq(1,20,length.out = 500)
  c=seq(0.1,2,length.out = 500)
  b=seq(0.01,1.5,length.out = 500)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_dip_4_final)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_dip_4_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_dip_4_ampltd_dif=ampltd_difference(test_data_dip_4,tonnage_combine_all_years,inspect_data)
  test_data_dip_4_inspect_0=subset(test_data_dip_4_ampltd_dif,test_data_dip_4_ampltd_dif$inspect_count==0)
  test_data_dip_4_threshold_add=add_threshold(test_data_dip_4_inspect_0,tag_limits)
  test_data_dip_4_valid=subset(test_data_dip_4_threshold_add,test_data_dip_4_threshold_add$defect_amp_diff>0 & test_data_dip_4_threshold_add$AMPLTD_NEED>0)
  prob_4_dip=prob_gamma(test_data_dip_4_valid$AMPLTD_NEED,test_data_dip_4_valid$day_count)
  dip_4_pred=prediction(prob_4_dip,test_data_dip_4_valid)
  match[ii]=length(which(dip_4_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(dip_4_pred))*100
  name_1=paste0("dip_4_pred","_",ii)
  false_positive[ii]=length(which(dip_4_pred$pred_match==1))
  false_negative[ii]=length(which(dip_4_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(dip_4_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(dip_4_pred))*100
  write.xlsx(dip_4_pred,"dip_4_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"dip_4_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}
###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$





#### line seg 4 and track 1 #### Never asked in the test data set
length(which(train_dip$LINE_SEG_NBR==4 & train_dip$TRACK_SDTK_NBR==1))
train_dip_4_1=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==4 & train_dip_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_4_1_dip=uniq_defect_position1(train_dip_4_1)
uniq_defect_pos_4_1_dip
length(uniq_defect_pos_4_1_dip) ## 
train_dip_4_1=mile_post_uniq(train_dip_4_1,uniq_defect_pos_4_1_dip)
train_dip_4_1_no_day=remove_same_day_repeat(train_dip_4_1)
train_dip_4_1_no_day_no_single=remove_single_obs(train_dip_4_1_no_day)
train_dip_4_1_final=ampltd_difference(train_dip_4_1_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_4_1_final)
length(which(train_dip_4_1_final$inspect_count==0))##3
length(which(train_dip_4_1_final$defect_amp>=0 & train_dip_4_1_final$inspect_count==0))##2

#### line seg 4 and track 2  #### Never asked in the test data set
length(which(train_dip$LINE_SEG_NBR==4 & train_dip$TRACK_SDTK_NBR==2))
train_dip_4_2=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==4 & train_dip_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_4_2_dip=uniq_defect_position1(train_dip_4_2)
uniq_defect_pos_4_2_dip
length(uniq_defect_pos_4_2_dip) ## 
train_dip_4_2=mile_post_uniq(train_dip_4_2,uniq_defect_pos_4_2_dip)
train_dip_4_2_no_day=remove_same_day_repeat(train_dip_4_2)
train_dip_4_2_no_day_no_single=remove_single_obs(train_dip_4_2_no_day)
train_dip_4_2_final=ampltd_difference(train_dip_4_2_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_4_2_final)
length(which(train_dip_4_2_final$inspect_count==0))##3
length(which(train_dip_4_2_final$defect_amp>=0 & train_dip_4_2_final$inspect_count==0))##2

#### line seg 4 and track 3  #### Never asked in the test data set
length(which(train_dip$LINE_SEG_NBR==4 & train_dip$TRACK_SDTK_NBR==3))
train_dip_4_3=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==4 & train_dip_line_track_milepost$TRACK_SDTK_NBR==3)
uniq_defect_pos_4_3_dip=uniq_defect_position1(train_dip_4_3)
uniq_defect_pos_4_3_dip
length(uniq_defect_pos_4_3_dip) ## 
train_dip_4_3=mile_post_uniq(train_dip_4_3,uniq_defect_pos_4_3_dip)
train_dip_4_3_no_day=remove_same_day_repeat(train_dip_4_3)
train_dip_4_3_no_day_no_single=remove_single_obs(train_dip_4_3_no_day)
train_dip_4_3_final=ampltd_difference(train_dip_4_3_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_4_3_final)
length(which(train_dip_4_3_final$inspect_count==0))##4
length(which(train_dip_4_3_final$defect_amp>=0 & train_dip_4_3_final$inspect_count==0))##4

#### line seg 4 and track 4  #### Never asked in the test data set
length(which(train_dip$LINE_SEG_NBR==4 & train_dip$TRACK_SDTK_NBR==4))
train_dip_4_4=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==4 & train_dip_line_track_milepost$TRACK_SDTK_NBR==4)
uniq_defect_pos_4_4_dip=uniq_defect_position1(train_dip_4_4)
uniq_defect_pos_4_4_dip
length(uniq_defect_pos_4_4_dip) ## 
train_dip_4_4=mile_post_uniq(train_dip_4_4,uniq_defect_pos_4_4_dip)
train_dip_4_4_no_day=remove_same_day_repeat(train_dip_4_4)
train_dip_4_4_no_day_no_single=remove_single_obs(train_dip_4_4_no_day)## cnat be found  0  14
##train_dip_4_4_final=ampltd_difference(train_dip_4_4_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_4_4_final)
#length(which(train_dip_4_4_final$inspect_count==0))###Error
#length(which(train_dip_4_4_final$defect_amp>=0 & train_dip_4_4_final$inspect_count==0))##  ERRRRORRR


#### line seg 4 and track 5  #### Never asked in the test data set
length(which(train_dip$LINE_SEG_NBR==4 & train_dip$TRACK_SDTK_NBR==5))
train_dip_4_5=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==4 & train_dip_line_track_milepost$TRACK_SDTK_NBR==5)
uniq_defect_pos_4_5_dip=uniq_defect_position1(train_dip_4_5)
uniq_defect_pos_4_5_dip
length(uniq_defect_pos_4_5_dip) ## 
train_dip_4_5=mile_post_uniq(train_dip_4_5,uniq_defect_pos_4_5_dip)
train_dip_4_5_no_day=remove_same_day_repeat(train_dip_4_5)
train_dip_4_5_no_day_no_single=remove_single_obs(train_dip_4_5_no_day)
#train_dip_4_5_final=ampltd_difference(train_dip_4_5_no_day_no_single,tonnage_combine_all_years,inspect_data)
##View(train_dip_4_5_final)
#length(which(train_dip_4_5_final$inspect_count==0))##EEEEERRRRRRRRRROOOOOORRRR
#length(which(train_dip_4_5_final$defect_amp>=0 & train_dip_4_5_final$inspect_count==0))## EEEEERRRRRRRRRROOOOOORRRR

#### line seg 4 and track 6   #### Never asked in the test data set
length(which(train_dip$LINE_SEG_NBR==4 & train_dip$TRACK_SDTK_NBR==6))
train_dip_4_6=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==4 & train_dip_line_track_milepost$TRACK_SDTK_NBR==6)
uniq_defect_pos_4_6_dip=uniq_defect_position1(train_dip_4_6)
uniq_defect_pos_4_6_dip
length(uniq_defect_pos_4_6_dip) ## 
train_dip_4_6=mile_post_uniq(train_dip_4_6,uniq_defect_pos_4_6_dip)
train_dip_4_6_no_day=remove_same_day_repeat(train_dip_4_6)
train_dip_4_6_no_day_no_single=remove_single_obs(train_dip_4_6_no_day)
#train_dip_4_6_final=ampltd_difference(train_dip_4_6_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_dip_4_6_final)
#length(which(train_dip_4_6_final$inspect_count==0))##EEEERRRRRRRRORRRRRRR
#length(which(train_dip_4_6_final$defect_amp>=0 & train_dip_4_6_final$inspect_count==0))##EEEERRRRRRRRORRRRRRR


## Now we will combine those data which have non negative amplitude and 0 inspection record
train_dip_1_0_valid_trans=train_dip_1_0_final[which(train_dip_1_0_final$defect_amp>=0 & train_dip_1_0_final$inspect_count==0),]
train_dip_2_0_valid_trans=train_dip_2_0_final[which(train_dip_2_0_final$defect_amp>=0 & train_dip_2_0_final$inspect_count==0),]
train_dip_2_1_valid_trans=train_dip_2_1_final[which(train_dip_2_1_final$defect_amp>=0 & train_dip_2_1_final$inspect_count==0),]
train_dip_2_2_valid_trans=train_dip_2_2_final[which(train_dip_2_2_final$defect_amp>=0 & train_dip_2_2_final$inspect_count==0),]
train_dip_3_0_valid_trans=train_dip_3_0_final[which(train_dip_3_0_final$defect_amp>=0 & train_dip_3_0_final$inspect_count==0),]
train_dip_3_1_valid_trans=train_dip_3_1_final[which(train_dip_3_1_final$defect_amp>=0 & train_dip_3_1_final$inspect_count==0),]
train_dip_3_2_valid_trans=train_dip_3_2_final[which(train_dip_3_2_final$defect_amp>=0 & train_dip_3_2_final$inspect_count==0),]
train_dip_4_0_valid_trans=train_dip_4_0_final[which(train_dip_4_0_final$defect_amp>=0 & train_dip_4_0_final$inspect_count==0),]
train_dip_4_1_valid_trans=train_dip_4_1_final[which(train_dip_4_1_final$defect_amp>=0 & train_dip_4_1_final$inspect_count==0),]
train_dip_4_2_valid_trans=train_dip_4_2_final[which(train_dip_4_2_final$defect_amp>=0 & train_dip_4_2_final$inspect_count==0),]
train_dip_4_3_valid_trans=train_dip_4_3_final[which(train_dip_4_3_final$defect_amp>=0 & train_dip_4_3_final$inspect_count==0),]

train_dip_valid=rbind(train_dip_1_0_valid_trans,train_dip_2_0_valid_trans,train_dip_2_1_valid_trans,train_dip_2_2_valid_trans,train_dip_3_0_valid_trans,train_dip_3_1_valid_trans,train_dip_3_2_valid_trans,train_dip_4_0_valid_trans,train_dip_4_1_valid_trans,train_dip_4_2_valid_trans,train_dip_4_3_valid_trans)
View(train_dip_valid)  ##889  26




###########################################################################




# now we will do the same work for XLEVEL Type of defect

train_xlevel$TEST_DT=as.Date(train_xlevel$TEST_DT,format="%d%b%Y")
train_xlevel=train_xlevel[order(train_xlevel$LINE_SEG_NBR,train_xlevel$TRACK_SDTK_NBR,train_xlevel$TEST_DT,train_xlevel$MILEPOST),]
summary(train_xlevel)
train_xlevel_line_track_milepost=train_xlevel[order(train_xlevel$LINE_SEG_NBR,train_xlevel$TRACK_SDTK_NBR,train_xlevel$MILEPOST),]
View(train_xlevel_line_track_milepost)


#### line seg 1 and track 0
train_xlevel_1_0=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==1 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==0)
View(train_xlevel_1_0)
uniq_defect_pos_1_0_xlevel=uniq_defect_position1(train_xlevel_1_0)
uniq_defect_pos_1_0_xlevel
length(uniq_defect_pos_1_0_xlevel) ## 340
train_xlevel_1_0=mile_post_uniq(train_xlevel_1_0,uniq_defect_pos_1_0_xlevel)
View(train_xlevel_1_0)
train_xlevel_1_0_no_day=remove_same_day_repeat(train_xlevel_1_0)
train_xlevel_1_0_no_day_no_single=remove_single_obs(train_xlevel_1_0_no_day)
View(train_xlevel_1_0_no_day_no_single)
train_xlevel_1_0_no_day_no_single=train_xlevel_1_0_no_day_no_single[-which(train_xlevel_1_0_no_day_no_single$MILEPOST==train_xlevel_1_0_no_day_no_single[45,2]),]
train_xlevel_1_0_no_day_no_single=train_xlevel_1_0_no_day_no_single[-which(train_xlevel_1_0_no_day_no_single$MILEPOST==train_xlevel_1_0_no_day_no_single[46,2]),]
train_xlevel_1_0_no_day_no_single=train_xlevel_1_0_no_day_no_single[-which(train_xlevel_1_0_no_day_no_single$MILEPOST==train_xlevel_1_0_no_day_no_single[46,2]),]
train_xlevel_1_0_final=ampltd_difference(train_xlevel_1_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_1_0_final)
length(which(train_xlevel_1_0_final$inspect_count==0))##772
length(which(train_xlevel_1_0_final$defect_amp>=0 & train_xlevel_1_0_final$inspect_count==0))##477
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_xlevel_1_0_no_day_no_single$MILEPOST)))  ##399
## here we have 399 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
399*0.20 ##79.8 =~80 unique defect positions will be in our test data and remaining 399-80 willbe in our train data
xlevel_sample_pos_1_0=sample(1:399,80) # SRSWOR sample of size 80 from 399 
xlevel_uniq_pos_1_0=levels(as.factor(train_xlevel_1_0_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_1_0=xlevel_uniq_pos_1_0[-xlevel_sample_pos_1_0]
xlevel_uniq_pos_test_1_0=xlevel_uniq_pos_1_0[xlevel_sample_pos_1_0]
train_xlevel_1_0_no_day_no_single_trn=subset(train_xlevel_1_0_no_day_no_single,train_xlevel_1_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_1_0)
train_xlevel_1_0_no_day_no_single_tst=subset(train_xlevel_1_0_no_day_no_single,train_xlevel_1_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_1_0)
#######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ main work for method 2nd 
train_data_xlevel_1=train_xlevel_1_0_no_day_no_single_trn
test_data_xlevel_1=train_xlevel_1_0_no_day_no_single_tst
train_data_xlevel_1_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_1)
train_data_xlevel_1_final=cum_amptd_time_1(train_data_xlevel_1_mile_changed,inspect_data)
optim(c(10,0.69,1.03),our_likelihood_1,y=train_data_xlevel_1_final) ### u= 4.7614306 c=0.6619179  b=0.4638858
prob_gamma=function(x,t){
  c=0.6619179
  b=0.4638858
  u=4.7614306
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
################# prepration of test data
test_data_xlevel_1_ampltd_dif=ampltd_difference(test_data_xlevel_1,tonnage_combine_all_years,inspect_data)
test_data_xlevel_1_inspect_0=subset(test_data_xlevel_1_ampltd_dif,test_data_xlevel_1_ampltd_dif$inspect_count==0)
test_data_xlevel_1_threshold_add=add_threshold(test_data_xlevel_1_inspect_0,tag_limits)
test_data_xlevel_1_valid=subset(test_data_xlevel_1_threshold_add,test_data_xlevel_1_threshold_add$defect_amp_diff>0 & test_data_xlevel_1_threshold_add$AMPLTD_NEED>0)
prob_1_xlevel=prob_gamma(test_data_xlevel_1_valid$AMPLTD_NEED,test_data_xlevel_1_valid$day_count)
xlevel_1_pred=prediction(prob_1_xlevel,test_data_xlevel_1_valid)
length(which(xlevel_1_pred$pred_match==0)) ##41
nrow(xlevel_1_pred) ###69
41/69==0.5942029 # 59 % accuracy of prediction 


# for line seg 1 and defect type xlevel  ####### Not done yet with a reasonable accuracy
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
  }
  length_uni_mile_xlevel_1_0=length(levels(as.factor(train_xlevel_1_0_no_day_no_single$MILEPOST)))  ##119
  xlevel_sample_pos_1_0=sample(1:length_uni_mile_xlevel_1_0,round(length_uni_mile_xlevel_1_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_1_0=levels(as.factor(train_xlevel_1_0_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_1_0=xlevel_uniq_pos_1_0[-xlevel_sample_pos_1_0]
  xlevel_uniq_pos_test_1_0=xlevel_uniq_pos_1_0[xlevel_sample_pos_1_0]
  train_xlevel_1_0_no_day_no_single_trn=subset(train_xlevel_1_0_no_day_no_single,train_xlevel_1_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_1_0)
  train_xlevel_1_0_no_day_no_single_tst=subset(train_xlevel_1_0_no_day_no_single,train_xlevel_1_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_1_0) 
  train_data_xlevel_1=rbind(train_xlevel_1_0_no_day_no_single_trn)
  test_data_xlevel_1=rbind(train_xlevel_1_0_no_day_no_single_tst)
  train_data_xlevel_1_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_1)
  train_data_xlevel_1_final=cum_amptd_time_1(train_data_xlevel_1_mile_changed,inspect_data)
  u=seq(1,20,length.out = 500)
  c=seq(0.1,2,length.out = 500)
  b=seq(0.01,1.5,length.out = 500)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_xlevel_1_final)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_xlevel_1_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_xlevel_1_ampltd_dif=ampltd_difference(test_data_xlevel_1,tonnage_combine_all_years,inspect_data)
  test_data_xlevel_1_inspect_0=subset(test_data_xlevel_1_ampltd_dif,test_data_xlevel_1_ampltd_dif$inspect_count==0)
  test_data_xlevel_1_threshold_add=add_threshold(test_data_xlevel_1_inspect_0,tag_limits)
  test_data_xlevel_1_valid=subset(test_data_xlevel_1_threshold_add,test_data_xlevel_1_threshold_add$defect_amp_diff>0 & test_data_xlevel_1_threshold_add$AMPLTD_NEED>0)
  prob_1_xlevel=prob_gamma(test_data_xlevel_1_valid$AMPLTD_NEED,test_data_xlevel_1_valid$day_count)
  xlevel_1_pred=prediction(prob_1_xlevel,test_data_xlevel_1_valid)
  match[ii]=length(which(xlevel_1_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(xlevel_1_pred))*100
  name_1=paste0("xlevel_1_pred","_",ii)
  false_positive[ii]=length(which(xlevel_1_pred$pred_match==1))
  false_negative[ii]=length(which(xlevel_1_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(xlevel_1_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(xlevel_1_pred))*100
  write.xlsx(xlevel_1_pred,"xlevel_1_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"xlevel_1_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


#### line seg 2 and track 0
length(which(train_xlevel$LINE_SEG_NBR==2 & train_xlevel$TRACK_SDTK_NBR==0))
train_xlevel_2_0=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==2 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_2_0_xlevel=uniq_defect_position1(train_xlevel_2_0)
uniq_defect_pos_2_0_xlevel
length(uniq_defect_pos_2_0_xlevel)
train_xlevel_2_0=mile_post_uniq(train_xlevel_2_0,uniq_defect_pos_2_0_xlevel)
train_xlevel_2_0_no_day=remove_same_day_repeat(train_xlevel_2_0)
train_xlevel_2_0_no_day_no_single=remove_single_obs(train_xlevel_2_0_no_day)
train_xlevel_2_0_no_day_no_single=train_xlevel_2_0_no_day_no_single[-which(train_xlevel_2_0_no_day_no_single$MILEPOST==train_xlevel_2_0_no_day_no_single[604,2]),]
train_xlevel_2_0_final=ampltd_difference(train_xlevel_2_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_2_0_final)
length(which(train_xlevel_2_0_final$inspect_count==0))##881
length(which(train_xlevel_2_0_final$defect_amp>=0 & train_xlevel_2_0_final$inspect_count==0))##536
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_xlevel_2_0_no_day_no_single$MILEPOST)))  ##484
## here we have 484 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
484*0.20 ##96.8 =~97 unique defect positions will be in our test data and remaining 484-97 willbe in our train data
xlevel_sample_pos_2_0=sample(1:484,97) # SRSWOR sample of size 97 from 484 
xlevel_uniq_pos_2_0=levels(as.factor(train_xlevel_2_0_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_2_0=xlevel_uniq_pos_2_0[-xlevel_sample_pos_2_0]
xlevel_uniq_pos_test_2_0=xlevel_uniq_pos_2_0[xlevel_sample_pos_2_0]
train_xlevel_2_0_no_day_no_single_trn=subset(train_xlevel_2_0_no_day_no_single,train_xlevel_2_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_0)
train_xlevel_2_0_no_day_no_single_tst=subset(train_xlevel_2_0_no_day_no_single,train_xlevel_2_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_0)


#### line seg 2 and track 1
length(which(train_xlevel$LINE_SEG_NBR==2 & train_xlevel$TRACK_SDTK_NBR==1))
train_xlevel_2_1=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==2 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_2_1_xlevel=uniq_defect_position1(train_xlevel_2_1)
uniq_defect_pos_2_1_xlevel
length(uniq_defect_pos_2_1_xlevel)
train_xlevel_2_1=mile_post_uniq(train_xlevel_2_1,uniq_defect_pos_2_1_xlevel)
train_xlevel_2_1_no_day=remove_same_day_repeat(train_xlevel_2_1)
train_xlevel_2_1_no_day_no_single=remove_single_obs(train_xlevel_2_1_no_day)
train_xlevel_2_1_no_day_no_single=train_xlevel_2_1_no_day_no_single[-which(train_xlevel_2_1_no_day_no_single$MILEPOST==train_xlevel_2_1_no_day_no_single[176,2]),]
train_xlevel_2_1_no_day_no_single=train_xlevel_2_1_no_day_no_single[-which(train_xlevel_2_1_no_day_no_single$MILEPOST==train_xlevel_2_1_no_day_no_single[176,2]),]
train_xlevel_2_1_final=ampltd_difference(train_xlevel_2_1_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_2_1_final)
length(which(train_xlevel_2_1_final$inspect_count==0))##94
length(which(train_xlevel_2_1_final$defect_amp>=0 & train_xlevel_2_1_final$inspect_count==0))##58
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_xlevel_2_1_no_day_no_single$MILEPOST)))  ##53
## here we have 53 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
53*0.20 ##10.6 =~11 unique defect positions will be in our test data and remaining 53-11 willbe in our train data
xlevel_sample_pos_2_1=sample(1:53,11) # SRSWOR sample of size 11 from 53 
xlevel_uniq_pos_2_1=levels(as.factor(train_xlevel_2_1_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_2_1=xlevel_uniq_pos_2_1[-xlevel_sample_pos_2_1]
xlevel_uniq_pos_test_2_1=xlevel_uniq_pos_2_1[xlevel_sample_pos_2_1]
train_xlevel_2_1_no_day_no_single_trn=subset(train_xlevel_2_1_no_day_no_single,train_xlevel_2_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_1)
train_xlevel_2_1_no_day_no_single_tst=subset(train_xlevel_2_1_no_day_no_single,train_xlevel_2_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_1)


#### line seg 2 and track 2
length(which(train_xlevel$LINE_SEG_NBR==2 & train_xlevel$TRACK_SDTK_NBR==2))
train_xlevel_2_2=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==2 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_2_2_xlevel=uniq_defect_position1(train_xlevel_2_2)
uniq_defect_pos_2_2_xlevel
length(uniq_defect_pos_2_2_xlevel)
train_xlevel_2_2=mile_post_uniq(train_xlevel_2_2,uniq_defect_pos_2_2_xlevel)
train_xlevel_2_2_no_day=remove_same_day_repeat(train_xlevel_2_2)
train_xlevel_2_2_no_day_no_single=remove_single_obs(train_xlevel_2_2_no_day)
train_xlevel_2_2_final=ampltd_difference(train_xlevel_2_2_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_2_2_final)
length(which(train_xlevel_2_2_final$inspect_count==0))##73
length(which(train_xlevel_2_2_final$defect_amp>=0 & train_xlevel_2_2_final$inspect_count==0))##45
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_xlevel_2_2_no_day_no_single$MILEPOST)))  ##60
## here we have 60 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
60*0.20 ##12 unique defect positions will be in our test data and remaining 60-11 willbe in our train data
xlevel_sample_pos_2_2=sample(1:60,12) # SRSWOR sample of size 12 from 60 
xlevel_uniq_pos_2_2=levels(as.factor(train_xlevel_2_2_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_2_2=xlevel_uniq_pos_2_2[-xlevel_sample_pos_2_2]
xlevel_uniq_pos_test_2_2=xlevel_uniq_pos_2_2[xlevel_sample_pos_2_2]
train_xlevel_2_2_no_day_no_single_trn=subset(train_xlevel_2_2_no_day_no_single,train_xlevel_2_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_2)
train_xlevel_2_2_no_day_no_single_tst=subset(train_xlevel_2_2_no_day_no_single,train_xlevel_2_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_2)
#######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ main work for method 2nd 
train_data_xlevel_2=rbind(train_xlevel_2_0_no_day_no_single_trn,train_xlevel_2_1_no_day_no_single_trn,train_xlevel_2_2_no_day_no_single_trn)
test_data_xlevel_2=rbind(train_xlevel_2_0_no_day_no_single_tst,train_xlevel_2_1_no_day_no_single_tst,train_xlevel_2_2_no_day_no_single_tst)
train_data_xlevel_2_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_2)
train_data_xlevel_2_final=cum_amptd_time_1(train_data_xlevel_2_mile_changed,inspect_data)
optim(c(7.701403,0.5721443,0.634268537),our_likelihood_1,y=train_data_xlevel_2_final) ### u= 7.506833 c=0.6897941  b=0.5021613   
prob_gamma=function(x,t){
  c=0.6897941
  b=0.5021613
  u=7.506833
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
################# prepration of test data
test_data_xlevel_2_ampltd_dif=ampltd_difference(test_data_xlevel_2,tonnage_combine_all_years,inspect_data)
test_data_xlevel_2_inspect_0=subset(test_data_xlevel_2_ampltd_dif,test_data_xlevel_2_ampltd_dif$inspect_count==0)
test_data_xlevel_2_threshold_add=add_threshold(test_data_xlevel_2_inspect_0,tag_limits)
test_data_xlevel_2_valid=subset(test_data_xlevel_2_threshold_add,test_data_xlevel_2_threshold_add$defect_amp_diff>0 & test_data_xlevel_2_threshold_add$AMPLTD_NEED>0)
prob_2_xlevel=prob_gamma(test_data_xlevel_2_valid$AMPLTD_NEED,test_data_xlevel_2_valid$day_count)
xlevel_2_pred=prediction(prob_2_xlevel,test_data_xlevel_2_valid)
length(which(xlevel_2_pred$pred_match==0)) ##72
nrow(xlevel_2_pred) ###117
72/117== 0.6153846 # 62 % accuracy of prediction 


# for line seg 2 and defect type xlevel 
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    u=seq(1,20,length.out = 500)
    c=seq(0.1,2,length.out = 500)
    b=seq(0.01,1.5,length.out = 500)
  }
  length_uni_mile_xlevel_2_0=length(levels(as.factor(train_xlevel_2_0_no_day_no_single$MILEPOST)))  ##429
  xlevel_sample_pos_2_0=sample(1:length_uni_mile_xlevel_2_0,round(length_uni_mile_xlevel_2_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_2_0=levels(as.factor(train_xlevel_2_0_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_2_0=xlevel_uniq_pos_2_0[-xlevel_sample_pos_2_0]
  xlevel_uniq_pos_test_2_0=xlevel_uniq_pos_2_0[xlevel_sample_pos_2_0]
  train_xlevel_2_0_no_day_no_single_trn=subset(train_xlevel_2_0_no_day_no_single,train_xlevel_2_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_0)
  train_xlevel_2_0_no_day_no_single_tst=subset(train_xlevel_2_0_no_day_no_single,train_xlevel_2_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_0)
  
  length_uni_mile_xlevel_2_1=length(levels(as.factor(train_xlevel_2_1_no_day_no_single$MILEPOST)))  ##429
  xlevel_sample_pos_2_1=sample(1:length_uni_mile_xlevel_2_1,round(length_uni_mile_xlevel_2_1*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_2_1=levels(as.factor(train_xlevel_2_1_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_2_1=xlevel_uniq_pos_2_1[-xlevel_sample_pos_2_1]
  xlevel_uniq_pos_test_2_1=xlevel_uniq_pos_2_1[xlevel_sample_pos_2_1]
  train_xlevel_2_1_no_day_no_single_trn=subset(train_xlevel_2_1_no_day_no_single,train_xlevel_2_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_1)
  train_xlevel_2_1_no_day_no_single_tst=subset(train_xlevel_2_1_no_day_no_single,train_xlevel_2_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_1)
  length_uni_mile_xlevel_2_2=length(levels(as.factor(train_xlevel_2_2_no_day_no_single$MILEPOST)))  ##214
  xlevel_sample_pos_2_2=sample(1:length_uni_mile_xlevel_2_2,round(length_uni_mile_xlevel_2_2*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_2_2=levels(as.factor(train_xlevel_2_2_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_2_2=xlevel_uniq_pos_2_2[-xlevel_sample_pos_2_2]
  xlevel_uniq_pos_test_2_2=xlevel_uniq_pos_2_2[xlevel_sample_pos_2_2]
  train_xlevel_2_2_no_day_no_single_trn=subset(train_xlevel_2_2_no_day_no_single,train_xlevel_2_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_2)
  train_xlevel_2_2_no_day_no_single_tst=subset(train_xlevel_2_2_no_day_no_single,train_xlevel_2_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_2)
  
  train_data_xlevel_2=rbind(train_xlevel_2_0_no_day_no_single_trn,train_xlevel_2_1_no_day_no_single_trn,train_xlevel_2_2_no_day_no_single_trn)
  test_data_xlevel_2=rbind(train_xlevel_2_0_no_day_no_single_tst,train_xlevel_2_1_no_day_no_single_tst,train_xlevel_2_2_no_day_no_single_tst)
  train_data_xlevel_2_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_2)
  train_data_xlevel_2_final=cum_amptd_time_1(train_data_xlevel_2_mile_changed,inspect_data)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_xlevel_2_final)
  #min(test$lik_val)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_xlevel_2_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_xlevel_2_ampltd_dif=ampltd_difference(test_data_xlevel_2,tonnage_combine_all_years,inspect_data)
  test_data_xlevel_2_inspect_0=subset(test_data_xlevel_2_ampltd_dif,test_data_xlevel_2_ampltd_dif$inspect_count==0)
  test_data_xlevel_2_threshold_add=add_threshold(test_data_xlevel_2_inspect_0,tag_limits)
  test_data_xlevel_2_valid=subset(test_data_xlevel_2_threshold_add,test_data_xlevel_2_threshold_add$defect_amp_diff>0 & test_data_xlevel_2_threshold_add$AMPLTD_NEED>0)
  prob_2_xlevel=prob_gamma(test_data_xlevel_2_valid$AMPLTD_NEED,test_data_xlevel_2_valid$day_count)
  xlevel_2_pred=prediction(prob_2_xlevel,test_data_xlevel_2_valid)
  match[ii]=length(which(xlevel_2_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(xlevel_2_pred))*100
  name_1=paste0("xlevel_2_pred","_",ii)
  false_positive[ii]=length(which(xlevel_2_pred$pred_match==1))
  false_negative[ii]=length(which(xlevel_2_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(xlevel_2_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(xlevel_2_pred))*100
  write.xlsx(xlevel_2_pred,"xlevel_2_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"xlevel_2_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$




#### line seg 3 and track 1
length(which(train_xlevel$LINE_SEG_NBR==3 & train_xlevel$TRACK_SDTK_NBR==1))
train_xlevel_3_1=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==3 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_3_1_xlevel=uniq_defect_position1(train_xlevel_3_1)
uniq_defect_pos_3_1_xlevel
length(uniq_defect_pos_3_1_xlevel)
train_xlevel_3_1=mile_post_uniq(train_xlevel_3_1,uniq_defect_pos_3_1_xlevel)
train_xlevel_3_1_no_day=remove_same_day_repeat(train_xlevel_3_1)
train_xlevel_3_1_no_day_no_single=remove_single_obs(train_xlevel_3_1_no_day)
train_xlevel_3_1_final=ampltd_difference(train_xlevel_3_1_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_3_1_final)
length(which(train_xlevel_3_1_final$inspect_count==0))##664
length(which(train_xlevel_3_1_final$defect_amp>=0 & train_xlevel_3_1_final$inspect_count==0))##418
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST)))  ##293
## here we have 293 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
293*0.20 ##58.6 =~57 unique defect positions will be in our test data and remaining 293-57 willbe in our train data
xlevel_sample_pos_3_1=sample(1:293,57) # SRSWOR sample of size 57 from 293 
xlevel_uniq_pos_3_1=levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_3_1=xlevel_uniq_pos_3_1[-xlevel_sample_pos_3_1]
xlevel_uniq_pos_test_3_1=xlevel_uniq_pos_3_1[xlevel_sample_pos_3_1]
train_xlevel_3_1_no_day_no_single_trn=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_1)
train_xlevel_3_1_no_day_no_single_tst=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_1)


#### line seg 3 and track 2
length(which(train_xlevel$LINE_SEG_NBR==3 & train_xlevel$TRACK_SDTK_NBR==2))
train_xlevel_3_2=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==3 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_3_2_xlevel=uniq_defect_position1(train_xlevel_3_2)
uniq_defect_pos_3_2_xlevel
length(uniq_defect_pos_3_2_xlevel)
train_xlevel_3_2=mile_post_uniq(train_xlevel_3_2,uniq_defect_pos_3_2_xlevel)
train_xlevel_3_2_no_day=remove_same_day_repeat(train_xlevel_3_2)
train_xlevel_3_2_no_day_no_single=remove_single_obs(train_xlevel_3_2_no_day)
train_xlevel_3_2_final=ampltd_difference(train_xlevel_3_2_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_3_2_final)
length(which(train_xlevel_3_2_final$inspect_count==0))## 721
length(which(train_xlevel_3_2_final$defect_amp>=0 & train_xlevel_3_2_final$inspect_count==0))## 440
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST)))  ##254
## here we have 254 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
254*0.20 ##50.8 =~51 unique defect positions will be in our test data and remaining 254-51 willbe in our train data
xlevel_sample_pos_3_2=sample(1:254,51) # SRSWOR sample of size 51 from 254
xlevel_uniq_pos_3_2=levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_3_2=xlevel_uniq_pos_3_2[-xlevel_sample_pos_3_2]
xlevel_uniq_pos_test_3_2=xlevel_uniq_pos_3_2[xlevel_sample_pos_3_2]
train_xlevel_3_2_no_day_no_single_trn=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_2)
train_xlevel_3_2_no_day_no_single_tst=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_2)
#######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ main work for method 2nd 
train_data_xlevel_3=rbind(train_xlevel_3_1_no_day_no_single_trn,train_xlevel_3_2_no_day_no_single_trn)
test_data_xlevel_3=rbind(train_xlevel_3_1_no_day_no_single_tst,train_xlevel_3_2_no_day_no_single_tst)
train_data_xlevel_3_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_3)
train_data_xlevel_3_final=cum_amptd_time_1(train_data_xlevel_3_mile_changed,inspect_data)
optim(c(10,0.69,1.03),our_likelihood_1,y=train_data_xlevel_3_final) ### u= 11.0338319 c=0.4733896  b=0.6586510
prob_gamma=function(x,t){
  c=0.4733896
  b=0.6586510
  u=11.0338319
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
################# prepration of test data
test_data_xlevel_3_ampltd_dif=ampltd_difference(test_data_xlevel_3,tonnage_combine_all_years,inspect_data)
test_data_xlevel_3_inspect_0=subset(test_data_xlevel_3_ampltd_dif,test_data_xlevel_3_ampltd_dif$inspect_count==0)
test_data_xlevel_3_threshold_add=add_threshold(test_data_xlevel_3_inspect_0,tag_limits)
test_data_xlevel_3_valid=subset(test_data_xlevel_3_threshold_add,test_data_xlevel_3_threshold_add$defect_amp_diff>0 & test_data_xlevel_3_threshold_add$AMPLTD_NEED>0)
prob_3_xlevel=prob_gamma(test_data_xlevel_3_valid$AMPLTD_NEED,test_data_xlevel_3_valid$day_count)
xlevel_3_pred=prediction(prob_3_xlevel,test_data_xlevel_3_valid)
length(which(xlevel_3_pred$pred_match==0)) ##101
nrow(xlevel_3_pred) ###150
101/150==0.6733333 # 67 % accuracy of prediction 


# for line seg 3 and defect type xlevel 
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    u=seq(1,20,length.out = 500)
    c=seq(0.1,2,length.out = 500)
    b=seq(0.01,1.5,length.out = 500)
  }
  length_uni_mile_xlevel_3_1=length(levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST)))  ##429
  xlevel_sample_pos_3_1=sample(1:length_uni_mile_xlevel_3_1,round(length_uni_mile_xlevel_3_1*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_3_1=levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_3_1=xlevel_uniq_pos_3_1[-xlevel_sample_pos_3_1]
  xlevel_uniq_pos_test_3_1=xlevel_uniq_pos_3_1[xlevel_sample_pos_3_1]
  train_xlevel_3_1_no_day_no_single_trn=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_1)
  train_xlevel_3_1_no_day_no_single_tst=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_1)
  length_uni_mile_xlevel_3_2=length(levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST)))  ##314
  xlevel_sample_pos_3_2=sample(1:length_uni_mile_xlevel_3_2,round(length_uni_mile_xlevel_3_2*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_3_2=levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_3_2=xlevel_uniq_pos_3_2[-xlevel_sample_pos_3_2]
  xlevel_uniq_pos_test_3_2=xlevel_uniq_pos_3_2[xlevel_sample_pos_3_2]
  train_xlevel_3_2_no_day_no_single_trn=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_2)
  train_xlevel_3_2_no_day_no_single_tst=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_2)
  train_data_xlevel_3=rbind(train_xlevel_3_1_no_day_no_single_trn,train_xlevel_3_2_no_day_no_single_trn)
  test_data_xlevel_3=rbind(train_xlevel_3_1_no_day_no_single_tst,train_xlevel_3_2_no_day_no_single_tst)
  train_data_xlevel_3_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_3)
  train_data_xlevel_3_final=cum_amptd_time_1(train_data_xlevel_3_mile_changed,inspect_data)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_xlevel_3_final)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_xlevel_3_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_xlevel_3_ampltd_dif=ampltd_difference(test_data_xlevel_3,tonnage_combine_all_years,inspect_data)
  test_data_xlevel_3_inspect_0=subset(test_data_xlevel_3_ampltd_dif,test_data_xlevel_3_ampltd_dif$inspect_count==0)
  test_data_xlevel_3_threshold_add=add_threshold(test_data_xlevel_3_inspect_0,tag_limits)
  test_data_xlevel_3_valid=subset(test_data_xlevel_3_threshold_add,test_data_xlevel_3_threshold_add$defect_amp_diff>0 & test_data_xlevel_3_threshold_add$AMPLTD_NEED>0)
  prob_3_xlevel=prob_gamma(test_data_xlevel_3_valid$AMPLTD_NEED,test_data_xlevel_3_valid$day_count)
  xlevel_3_pred=prediction(prob_3_xlevel,test_data_xlevel_3_valid)
  match[ii]=length(which(xlevel_3_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(xlevel_3_pred))*100
  name_1=paste0("xlevel_3_pred","_",ii)
  false_positive[ii]=length(which(xlevel_3_pred$pred_match==1))
  false_negative[ii]=length(which(xlevel_3_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(xlevel_3_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(xlevel_3_pred))*100
  write.xlsx(xlevel_3_pred,"xlevel_3_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"xlevel_3_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



#### line seg 3 and track 3  #### never asked in tets data set
length(which(train_xlevel$LINE_SEG_NBR==3 & train_xlevel$TRACK_SDTK_NBR==3))
train_xlevel_3_3=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==3 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==3)
uniq_defect_pos_3_3_xlevel=uniq_defect_position1(train_xlevel_3_3)
uniq_defect_pos_3_3_xlevel
length(uniq_defect_pos_3_3_xlevel)
train_xlevel_3_3=mile_post_uniq(train_xlevel_3_3,uniq_defect_pos_3_3_xlevel)
train_xlevel_3_3_no_day=remove_same_day_repeat(train_xlevel_3_3)
train_xlevel_3_3_no_day_no_single=remove_single_obs(train_xlevel_3_3_no_day)
#train_xlevel_2_2_no_day_no_single=train_xlevel_2_2_no_day_no_single[-which(train_xlevel_2_2_no_day_no_single$MILEPOST==train_xlevel_2_2_no_day_no_single[176,2]),]
#train_xlevel_2_2_no_day_no_single=train_xlevel_2_2_no_day_no_single[-which(train_xlevel_2_2_no_day_no_single$MILEPOST==train_xlevel_2_2_no_day_no_single[176,2]),]
train_xlevel_3_3_final=ampltd_difference(train_xlevel_3_3_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_3_3_final)
length(which(train_xlevel_3_3_final$inspect_count==0))## 2
length(which(train_xlevel_3_3_final$defect_amp>=0 & train_xlevel_3_3_final$inspect_count==0))## 1



#### line seg 4 and track 0
length(which(train_xlevel$LINE_SEG_NBR==4 & train_xlevel$TRACK_SDTK_NBR==0))
train_xlevel_4_0=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==4 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_4_0_xlevel=uniq_defect_position1(train_xlevel_4_0)
uniq_defect_pos_4_0_xlevel
length(uniq_defect_pos_4_0_xlevel)
train_xlevel_4_0=mile_post_uniq(train_xlevel_4_0,uniq_defect_pos_4_0_xlevel)
train_xlevel_4_0_no_day=remove_same_day_repeat(train_xlevel_4_0)
train_xlevel_4_0_no_day_no_single=remove_single_obs(train_xlevel_4_0_no_day)
train_xlevel_4_0_no_day_no_single=train_xlevel_4_0_no_day_no_single[-which(train_xlevel_4_0_no_day_no_single$MILEPOST==train_xlevel_4_0_no_day_no_single[9,2]),]
train_xlevel_4_0_no_day_no_single=train_xlevel_4_0_no_day_no_single[-which(train_xlevel_4_0_no_day_no_single$MILEPOST==train_xlevel_4_0_no_day_no_single[14,2]),]
train_xlevel_4_0_no_day_no_single=train_xlevel_4_0_no_day_no_single[-which(train_xlevel_4_0_no_day_no_single$MILEPOST==train_xlevel_4_0_no_day_no_single[19,2]),]
train_xlevel_4_0_no_day_no_single=train_xlevel_4_0_no_day_no_single[-which(train_xlevel_4_0_no_day_no_single$MILEPOST==train_xlevel_4_0_no_day_no_single[490,2]),]
train_xlevel_4_0_no_day_no_single=train_xlevel_4_0_no_day_no_single[-which(train_xlevel_4_0_no_day_no_single$MILEPOST==train_xlevel_4_0_no_day_no_single[490,2]),]
train_xlevel_4_0_final=ampltd_difference(train_xlevel_4_0_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_4_0_final)
length(which(train_xlevel_4_0_final$inspect_count==0))## 177
length(which(train_xlevel_4_0_final$defect_amp>=0 & train_xlevel_4_0_final$inspect_count==0))## 117
#### Work for the 2nd method i.e. in which we have assumed that there lifetime of the defects starts when they are tagged as Yellow tag for very first time
length(levels(as.factor(train_xlevel_4_0_no_day_no_single$MILEPOST)))  ##155
## here we have 155 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
155*0.20 ##31 =~31 unique defect positions will be in our test data and remaining 155-31 willbe in our train data
xlevel_sample_pos_4_0=sample(1:155,31) # SRSWOR sample of size 31 from 155 
xlevel_uniq_pos_4_0=levels(as.factor(train_xlevel_4_0_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_4_0=xlevel_uniq_pos_4_0[-xlevel_sample_pos_4_0]
xlevel_uniq_pos_test_4_0=xlevel_uniq_pos_4_0[xlevel_sample_pos_4_0]
train_xlevel_4_0_no_day_no_single_trn=subset(train_xlevel_4_0_no_day_no_single,train_xlevel_4_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_4_0)
train_xlevel_4_0_no_day_no_single_tst=subset(train_xlevel_4_0_no_day_no_single,train_xlevel_4_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_4_0)
#######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ main work for method 2nd 
train_data_xlevel_4=train_xlevel_4_0_no_day_no_single_trn
test_data_xlevel_4=train_xlevel_4_0_no_day_no_single_tst
train_data_xlevel_4_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_4)
train_data_xlevel_4_final=cum_amptd_time_1(train_data_xlevel_4_mile_changed,inspect_data)
optim(c(10,0.69,1.03),our_likelihood_1,y=train_data_xlevel_4_final) ### u= 12.1428970 c=0.5352920  b=0.6243311
prob_gamma=function(x,t){
  c=0.5352920
  b=0.4638858
  u=12.1428970
  shape=(c^2)*'^'(t,'^'(b,2))
  scale=u
  print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
}
################# prepration of test data
test_data_xlevel_4_ampltd_dif=ampltd_difference(test_data_xlevel_4,tonnage_combine_all_years,inspect_data)
test_data_xlevel_4_inspect_0=subset(test_data_xlevel_4_ampltd_dif,test_data_xlevel_4_ampltd_dif$inspect_count==0)
test_data_xlevel_4_threshold_add=add_threshold(test_data_xlevel_4_inspect_0,tag_limits)
test_data_xlevel_4_valid=subset(test_data_xlevel_4_threshold_add,test_data_xlevel_4_threshold_add$defect_amp_diff>0 & test_data_xlevel_4_threshold_add$AMPLTD_NEED>0)
prob_4_xlevel=prob_gamma(test_data_xlevel_4_valid$AMPLTD_NEED,test_data_xlevel_4_valid$day_count)
xlevel_4_pred=prediction(prob_4_xlevel,test_data_xlevel_4_valid)
length(which(xlevel_4_pred$pred_match==0)) ##14
nrow(xlevel_4_pred) ###18
14/18==0.7777778 # 78 % accuracy of prediction 

# for line seg 4 and defect type xlevel 
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
  }
  length_uni_mile_xlevel_4_0=length(levels(as.factor(train_xlevel_4_0_no_day_no_single$MILEPOST)))  ##119
  xlevel_sample_pos_4_0=sample(1:length_uni_mile_xlevel_4_0,round(length_uni_mile_xlevel_4_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_4_0=levels(as.factor(train_xlevel_4_0_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_4_0=xlevel_uniq_pos_4_0[-xlevel_sample_pos_4_0]
  xlevel_uniq_pos_test_4_0=xlevel_uniq_pos_4_0[xlevel_sample_pos_4_0]
  train_xlevel_4_0_no_day_no_single_trn=subset(train_xlevel_4_0_no_day_no_single,train_xlevel_4_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_4_0)
  train_xlevel_4_0_no_day_no_single_tst=subset(train_xlevel_4_0_no_day_no_single,train_xlevel_4_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_4_0) 
  train_data_xlevel_4=rbind(train_xlevel_4_0_no_day_no_single_trn)
  test_data_xlevel_4=rbind(train_xlevel_4_0_no_day_no_single_tst)
  train_data_xlevel_4_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_4)
  train_data_xlevel_4_final=cum_amptd_time_1(train_data_xlevel_4_mile_changed,inspect_data)
  u=seq(1,20,length.out = 500)
  c=seq(0.1,2,length.out = 500)
  b=seq(0.01,1.5,length.out = 500)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_xlevel_4_final)
  #min(test$lik_val)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_xlevel_4_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    c=opt$par[2]
    b=opt$par[3]
    u=opt$par[1]
    shape=(c^2)*'^'(t,'^'(b,2))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_xlevel_4_ampltd_dif=ampltd_difference(test_data_xlevel_4,tonnage_combine_all_years,inspect_data)
  test_data_xlevel_4_inspect_0=subset(test_data_xlevel_4_ampltd_dif,test_data_xlevel_4_ampltd_dif$inspect_count==0)
  test_data_xlevel_4_threshold_add=add_threshold(test_data_xlevel_4_inspect_0,tag_limits)
  test_data_xlevel_4_valid=subset(test_data_xlevel_4_threshold_add,test_data_xlevel_4_threshold_add$defect_amp_diff>0 & test_data_xlevel_4_threshold_add$AMPLTD_NEED>0)
  prob_4_xlevel=prob_gamma(test_data_xlevel_4_valid$AMPLTD_NEED,test_data_xlevel_4_valid$day_count)
  xlevel_4_pred=prediction(prob_4_xlevel,test_data_xlevel_4_valid)
  match[ii]=length(which(xlevel_4_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(xlevel_4_pred))*100
  name_1=paste0("xlevel_4_pred","_",ii)
  false_positive[ii]=length(which(xlevel_4_pred$pred_match==1))
  false_negative[ii]=length(which(xlevel_4_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(xlevel_4_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(xlevel_4_pred))*100
  write.xlsx(xlevel_4_pred,"xlevel_4_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"xlevel_4_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


#### line seg 4 and track 1  ### Never asked
length(which(train_xlevel$LINE_SEG_NBR==4 & train_xlevel$TRACK_SDTK_NBR==1))
train_xlevel_4_1=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==4 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_4_1_xlevel=uniq_defect_position1(train_xlevel_4_1)
uniq_defect_pos_4_1_xlevel
length(uniq_defect_pos_4_1_xlevel)
train_xlevel_4_1=mile_post_uniq(train_xlevel_4_1,uniq_defect_pos_4_1_xlevel)
train_xlevel_4_1_no_day=remove_same_day_repeat(train_xlevel_4_1)
train_xlevel_4_1_no_day_no_single=remove_single_obs(train_xlevel_4_1_no_day)
train_xlevel_4_1_final=ampltd_difference(train_xlevel_4_1_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_4_1_final)
length(which(train_xlevel_4_1_final$inspect_count==0))## 2
length(which(train_xlevel_4_1_final$defect_amp>=0 & train_xlevel_4_1_final$inspect_count==0))## 1

#### line seg 4 and track 2  ### Never asked
length(which(train_xlevel$LINE_SEG_NBR==4 & train_xlevel$TRACK_SDTK_NBR==2))
train_xlevel_4_2=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==4 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_4_2_xlevel=uniq_defect_position1(train_xlevel_4_2)
uniq_defect_pos_4_2_xlevel
length(uniq_defect_pos_4_2_xlevel)
train_xlevel_4_2=mile_post_uniq(train_xlevel_4_2,uniq_defect_pos_4_2_xlevel)
train_xlevel_4_2_no_day=remove_same_day_repeat(train_xlevel_4_2)
train_xlevel_4_2_no_day_no_single=remove_single_obs(train_xlevel_4_2_no_day)
train_xlevel_4_2_final=ampltd_difference(train_xlevel_4_2_no_day_no_single,tonnage_combine_all_years,inspect_data)
#View(train_xlevel_4_2_final)
length(which(train_xlevel_4_2_final$inspect_count==0))## 4
length(which(train_xlevel_4_2_final$defect_amp>=0 & train_xlevel_4_2_final$inspect_count==0))## 3


## Now we will combine those data which have non negative amplitude and 0 inspection record
train_xlevel_1_0_valid_trans=train_xlevel_1_0_final[which(train_xlevel_1_0_final$defect_amp>=0 & train_xlevel_1_0_final$inspect_count==0),]
train_xlevel_2_0_valid_trans=train_xlevel_2_0_final[which(train_xlevel_2_0_final$defect_amp>=0 & train_xlevel_2_0_final$inspect_count==0),]
train_xlevel_2_1_valid_trans=train_xlevel_2_1_final[which(train_xlevel_2_1_final$defect_amp>=0 & train_xlevel_2_1_final$inspect_count==0),]
train_xlevel_2_2_valid_trans=train_xlevel_2_2_final[which(train_xlevel_2_2_final$defect_amp>=0 & train_xlevel_2_2_final$inspect_count==0),]
train_xlevel_3_1_valid_trans=train_xlevel_3_1_final[which(train_xlevel_3_1_final$defect_amp>=0 & train_xlevel_3_1_final$inspect_count==0),]
train_xlevel_3_2_valid_trans=train_xlevel_3_2_final[which(train_xlevel_3_2_final$defect_amp>=0 & train_xlevel_3_2_final$inspect_count==0),]
train_xlevel_3_3_valid_trans=train_xlevel_3_3_final[which(train_xlevel_3_3_final$defect_amp>=0 & train_xlevel_3_3_final$inspect_count==0),]
train_xlevel_4_0_valid_trans=train_xlevel_4_0_final[which(train_xlevel_4_0_final$defect_amp>=0 & train_xlevel_4_0_final$inspect_count==0),]
train_xlevel_4_1_valid_trans=train_xlevel_4_1_final[which(train_xlevel_4_1_final$defect_amp>=0 & train_xlevel_4_1_final$inspect_count==0),]

train_xlevel_valid=rbind(train_xlevel_1_0_valid_trans,train_xlevel_2_0_valid_trans,train_xlevel_2_1_valid_trans,train_xlevel_2_2_valid_trans,train_xlevel_3_1_valid_trans,train_xlevel_3_2_valid_trans,train_xlevel_3_3_valid_trans,train_xlevel_4_0_valid_trans,train_xlevel_4_1_valid_trans)
View(train_xlevel_valid) #2093  26
write.xlsx(train_surface_valid,"train_surface_valid_new.xlsx")
write.xlsx(train_dip_valid,"train_dip_valid_new.xlsx")
write.xlsx(train_xlevel_valid,"train_xlevel_valid_new.xlsx")



###########################################################################################################################################################################













###########################################################################################################################################################################
train_surface_valid=read.csv("Surface1.csv")
View(train_surface_valid)
which(train_surface_valid$tonnage_sum==0)
train_surface_valid=train_surface_valid[which(train_surface_valid$defect_amp_diff>0 & train_surface_valid$day_count>7 ),]
summary(train_surface_valid$day_count)
plot(train_surface_valid$day_count)  ## Looks like there are some points where there is no inspection for even more than 700 days
which(test_data$LINE_SEG_NBR==4 & test_data$DFCT_TYPE=="SURFACE") ##integer(0)  Looks that there is nothing asked for line 4 and Defect type SURFACE
train_surface_valid=train_surface_valid[-which(train_surface_valid$LINE_SEG_NBR==4),] ## REmoved them
which(test_data$LINE_SEG_NBR==3 & test_data$DFCT_TYPE=="SURFACE" & test_data$TRACK_SDTK_NBR==3) ##integer(0)  Looks that there is nothing asked for line 3 and Defect type SURFACE and track 3
train_surface_valid=train_surface_valid[-which(train_surface_valid$LINE_SEG_NBR==3 &train_surface_valid$TRACK_SDTK_NBR==3 ),] ## Removed them
which(test_data$LINE_SEG_NBR==2 & test_data$DFCT_TYPE=="SURFACE" & test_data$TRACK_SDTK_NBR==2)##integer(0)  Looks that there is nothing asked for line 2 and Defect type SURFACE and track 2 and also there were only 2 observations for this
train_surface_valid=train_surface_valid[-which(train_surface_valid$LINE_SEG_NBR==2 &train_surface_valid$TRACK_SDTK_NBR==2 ),]  #Rmoved them
which(test_data$LINE_SEG_NBR==2 & test_data$DFCT_TYPE=="SURFACE" & test_data$TRACK_SDTK_NBR==1) ##integer(0)  Looks that there is nothing asked for line 2 and Defect type SURFACE and track 1 and also there were only 6 or 7 observations for this
train_surface_valid=train_surface_valid[-which(train_surface_valid$LINE_SEG_NBR==2 &train_surface_valid$TRACK_SDTK_NBR==1 ),]  #Removed them
train_surface_valid_1=train_surface_valid[which(train_surface_valid$LINE_SEG_NBR==1),]
train_surface_valid_2=train_surface_valid[which(train_surface_valid$LINE_SEG_NBR==2),]
train_surface_valid_3=train_surface_valid[which(train_surface_valid$LINE_SEG_NBR==3),]




### Prepration of a dataframe for the values of u c b which are the outputs of the optim function
DFCT_TYPE=c(rep("SURFACE",9),rep("DIP",12),rep("XLEVEL",12))
LINE_SEG_NBR=c(rep(1,3),rep(2,3),rep(3,3),rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(1,3),rep(2,3),rep(3,3),rep(4,3))
PRMTR=c(rep(c("u","c","b"),11))
VALUES=c(8.8449126,0.7714864,0.4182621,7.2278443,0.3819970,0.7193171,9.6514790,0.4281433,0.7212561,6.8886032,0.6749676,0.4876595,7.9470123,0.4618675,0.6726382,9.2737518,0.4771250,0.6756932,11.0490693,0.2686848,0.8140151,4.7614306,0.6619179,0.4638858,7.506833,0.6897941,0.5021613,11.0338319,0.4733896,0.6586510,12.1428970,0.5352920,0.4638858)
u_c_b_values=data.frame(DFCT_TYPE,LINE_SEG_NBR,PRMTR,VALUES)

#### Addition of column for threshold and amplitude needed in test_data
add_threshold_for_test_data=function(x,y){
  THRSLD=c()
  for(i in 1:nrow(x)){
    THRSLD=c(THRSLD,y[as.character(x$DFCT_TYPE[i]),x$CLASS[i]])
  }
  x=cbind(x,THRSLD=THRSLD)
  AMPLTD_NEED=(x$THRSLD+0.01)-abs(x$DEF_AMPLTD)
  x=cbind(x,AMPLTD_NEED=AMPLTD_NEED)
  return(x)
}
test_data_thrsld_add=add_threshold_for_test_data(test_data,tag_limits)
prediction_for_test_data=function(x,m){
  result=data.frame()
  for(i in 1:length(levels(as.factor(x$DFCT_TYPE)))){
    y=subset(x,x$DFCT_TYPE==levels(as.factor(x$DFCT_TYPE))[i])
    for(j in 1:length(levels(as.factor(y$LINE_SEG_NBR)))){
      PRED=character()
      z=subset(y,y$LINE_SEG_NBR==levels(as.factor(y$LINE_SEG_NBR))[j])
      for(k in 1:nrow(z)){
        if(z$AMPLTD_NEED[k]<=0){
          PRED=c(PRED,"RED")
        }
        else{
          u_c_b=m[which(m$DFCT_TYPE==levels(as.factor(x$DFCT_TYPE))[i] & m$LINE_SEG_NBR==levels(as.factor(y$LINE_SEG_NBR))[j]),]
          u=u_c_b[1,4]
          c=u_c_b[2,4]
          b=u_c_b[3,4]
          shape=(c^2)*'^'(z$INTERVAL[k],'^'(b,2))
          scale=u
          prob=round((1-pgamma(z$AMPLTD_NEED[k],scale =1/scale,shape = shape)),digits = 1)
          if(prob<0.5){
            PRED=c(PRED,"YEL")
          }
          else{
            PRED=c(PRED,"RED")
          }
        }
      }
      z=cbind(z,PRED=PRED)
      result=rbind(result,z)
    }
  }
  return(result)
}
our_pred=prediction_for_test_data(test_data_thrsld_add,u_c_b_values)
#### To match Our result with TCS explORers result
tcs_result=read.csv("TCS_RESULT.csv")
View(tcs_result)
### Sort their result first by Defect Type after that by line segment and after that milepost
tcs_result_sort=tcs_result[order(tcs_result$DFCT_TYPE,tcs_result$LINE_SEG_NBR,tcs_result$MILEPOST),]
### Sort our result first by Defect Type after that by line segment and after that milepost
our_pred_sort=our_pred[order(our_pred$DFCT_TYPE,our_pred$LINE_SEG_NBR,our_pred$MILEPOST),]
#### Now comparision i.e. how many prediction are matching with their prediction result
match_with_tcs_result=as.numeric(our_pred_sort$PRED)-as.numeric(tcs_result_sort$PRE_DEF_PRTY)
length(which(match_with_tcs_result==0)) ####173
## match % 
173/180 == 0.9611111 ### 96 % predictions are matching with them 


u=seq(1,20,length.out = 500)
c=seq(0.1,2,length.out = 500)
b=seq(-1.5,1.5,length.out = 500)
u_sample=sample(u,100,replace = T)
c_sample=sample(c,100,replace = T)
b_sample=sample(b,100,replace = T)
u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
test=lik_value(u_c_b_sample,train_data_xlevel_2_final)
min(test$lik_val)
which(test$lik_val==min(test$lik_val))
optim(c(7.701403,0.5721443,0.634268537),our_likelihood_1,y=train_data_xlevel_2_final) ### u= 7.506833 c=0.6897941  b=0.5021613




















#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#New method taking another form of shape parameter, shape parameter=A*t^(-x*beta)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@





for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
  }
  length_uni_mile_xlevel_4_0=length(levels(as.factor(train_xlevel_4_0_no_day_no_single$MILEPOST)))  ##119
  xlevel_sample_pos_4_0=sample(1:length_uni_mile_xlevel_4_0,round(length_uni_mile_xlevel_4_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_4_0=levels(as.factor(train_xlevel_4_0_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_4_0=xlevel_uniq_pos_4_0[-xlevel_sample_pos_4_0]
  xlevel_uniq_pos_test_4_0=xlevel_uniq_pos_4_0[xlevel_sample_pos_4_0]
  train_xlevel_4_0_no_day_no_single_trn=subset(train_xlevel_4_0_no_day_no_single,train_xlevel_4_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_4_0)
  train_xlevel_4_0_no_day_no_single_tst=subset(train_xlevel_4_0_no_day_no_single,train_xlevel_4_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_4_0) 
  train_data_xlevel_4=rbind(train_xlevel_4_0_no_day_no_single_trn)
  test_data_xlevel_4=rbind(train_xlevel_4_0_no_day_no_single_tst)
  train_data_xlevel_4_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_4)
  train_data_xlevel_4_cum_amptd=cum_amptd_time_1(train_data_xlevel_4_mile_changed,inspect_data)
  train_data_xlevel_4_final=add_train_tonnage(train_data_xlevel_4_cum_amptd,tonnage_combine_all_years)
  
  u=seq(1,20,length.out = 500)
  c=seq(0.1,2,length.out = 500)
  b=seq(0.01,1.5,length.out = 500)
  u_sample=sample(u,100,replace = T)
  c_sample=sample(c,100,replace = T)
  b_sample=sample(b,100,replace = T)
  u_c_b_sample=data.frame(u_sample,c_sample,b_sample)
  test=lik_value(u_c_b_sample,train_data_xlevel_4_final)
  #min(test$lik_val)
  rw_no=which(test$lik_val==min(test$lik_val))
  opt=optim(c(u_c_b_sample[rw_no,1],u_c_b_sample[rw_no,2],u_c_b_sample[rw_no,3]),our_likelihood_1,y=train_data_xlevel_4_final) ### u=7.2278443 c=0.3819970  b=0.7193171
  prob_gamma=function(x,t){
    B=opt_2$par[2]
    D=opt_2$par[3]
    A=opt_2$par[1]
    E=opt_2$par[3]
    S=opt_2$par[3]
    G=opt_2$par[3]
    H=opt_2$par[3]
    I=opt_2$par[3]
    J=opt_2$par[3]
    K=opt_2$par[3]
    L=opt_2$par[3]
    shape=(B^2)*'^'(t,'^'((D^2)*abs(x$DEF_AMPLTD[a1[j]])+(E^2)*x$DEF_LGTH[a1[j]]+(S^2)*x$TRACK_SDTK_NBR[a1[j]]+(G^2)*as.numeric(x$TSC_CD[a1[j]])+(H^2)*x$CLASS[a1[j]]+(I^2)*x$TEST_FSPD[a1[j]]+(J^2)*x$TEST_PSPD[a1[j]]+(K^2)*x$tonnage_passed[a1[j]]+(L^2)*(x$trains_passed[a1[j]]/1000)))
    scale=u
    print(round((1-pgamma(x,scale =1/scale,shape = shape)),digits = 1))
  }
  ################# prepration of test data
  test_data_xlevel_4_ampltd_dif=ampltd_difference(test_data_xlevel_4,tonnage_combine_all_years,inspect_data)
  test_data_xlevel_4_inspect_0=subset(test_data_xlevel_4_ampltd_dif,test_data_xlevel_4_ampltd_dif$inspect_count==0)
  test_data_xlevel_4_threshold_add=add_threshold(test_data_xlevel_4_inspect_0,tag_limits)
  test_data_xlevel_4_valid=subset(test_data_xlevel_4_threshold_add,test_data_xlevel_4_threshold_add$defect_amp_diff>0 & test_data_xlevel_4_threshold_add$AMPLTD_NEED>0)
  prob_4_xlevel=prob_gamma(test_data_xlevel_4_valid$AMPLTD_NEED,test_data_xlevel_4_valid$day_count)
  xlevel_4_pred=prediction(prob_4_xlevel,test_data_xlevel_4_valid)
  match[ii]=length(which(xlevel_4_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(xlevel_4_pred))*100
  name_1=paste0("xlevel_4_pred","_",ii)
  false_positive[ii]=length(which(xlevel_4_pred$pred_match==1))
  false_negative[ii]=length(which(xlevel_4_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(xlevel_4_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(xlevel_4_pred))*100
  write.xlsx(xlevel_4_pred,"xlevel_4_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"xlevel_4_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}




opt_2=optim(c(1.2992567696,-0.5084136011,0.0660356870,-0.1047320607, 20.3443024731,0.4373998483, 0.1114081376, 0.0116916612,-0.0806235689,-0.0007682038,-0.0007301939),our_likelihood_2,y=train_data_xlevel_4_final,control = c(maxit=5000))
opt_3=optim(c(4.60300171,1.86649499,0.99969066,0.27198179, 1.78204352,1.04066750, 0.50728343, 0.06306867,0.09895474,0.10240925,0.19335324),our_likelihood_3,y=train_data_xlevel_2_final)
opt_3=optim(c(6.66862642,1.42687645,0.69067467,0.06712203, 0.29847392,0.15491522, -0.20376813, -0.09730187,-0.01544765,0.03287172,0.09917237),our_likelihood_3,y=train_data_xlevel_2_final)
opt_2_1=optim(c(1.2854329689,-0.6195713799,0.0356637286,-0.1115978378,0.4445033169, 0.0500745725, -0.0132389769,0.0031152903,-0.0009658773,0.0185464327),our_likelihood_4,y=train_data_xlevel_4_final,control = c(maxit=5000))
opt_2_2=optim(c(1.2854329689,-0.6195713799,0.0356637286,-0.1115978378,0.4445033169, 0.0500745725,-0.0009658773,0.0185464327),our_likelihood_4,y=train_data_xlevel_4_final,control = c(maxit=5000))
opt_3_1=optim(c(1.2854329689,-0.6195713799,0.0356637286,-0.1115978378,0.4445033169, 0.0500745725, -0.0132389769,0.0031152903,-0.0009658773,0.0185464327),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))
opt_3_2=optim(c(1.210163035,-0.730046630,0.184429434,-0.090467823,0.045159948, 0.150905169,1.227435731,1.212767053),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))
opt_3_3=optim(c(1.34549828,-0.50166481,0.10769180,0.02463990,0.05533743, 0.32991862,0.70694419,1.18501451),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))
opt_3_4=optim(c(1.45200275,-0.23994844,0.16569255,0.26771634,0.01806302, 0.38181801,0.06825125,1.14011339),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))
opt_3_5=optim(c(1.26127013,0.73135226,0.11675001,0.09812871,0.18256590, 0.04957844,-0.04512887,-0.11721730),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))
opt_3_6=optim(c(1.212575164,0.731330530,0.024919566,0.089627254,0.113368753, 0.162094283,0.004549023,0.004037794),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))



A=seq(1,3,length.out = 5000)
B=seq(0.001,2,length.out = 5000)
D=seq(0.00001,1,length.out = 5000)
E=seq(0.000001,1.5,length.out = 5000)
G=seq(0.000001,1.5,length.out = 5000)
H=seq(0.000001,1.5,length.out = 5000)
K=seq(0.000000001,0.01,length.out = 5000)
L=seq(0.000000001,0.01,length.out = 5000)


A_sample=sample(A,500,replace = T)
B_sample=sample(B,500,replace = T)
D_sample=sample(D,500,replace = T)
E_sample=sample(E,500,replace = T)
G_sample=sample(G,500,replace = T)
H_sample=sample(H,500,replace = T)
K_sample=sample(K,500,replace = T)
L_sample=sample(L,500,replace = T)
par_sample=data.frame(A_sample,B_sample,D_sample,E_sample,G_sample,H_sample,K_sample,L_sample)

test_2=lik_value_1(par_sample,train_data_xlevel_2_final)

View(test_1)
which(test_1$lik_val==min(test_1$lik_val))

opt_3_8=optim(c(par_sample[122,1],par_sample[122,2],par_sample[122,3],par_sample[122,4],par_sample[122,5], par_sample[122,6],par_sample[122,7],par_sample[122,8]),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))
## Looks Likes Good 


# Foe line seg 4 and defect type XLEVEL

A=seq(1,3,length.out = 5000)
B=seq(0.001,2,length.out = 5000)
D=seq(0.00001,1,length.out = 5000)
E=seq(0.000001,1.5,length.out = 5000)
G=seq(0.000001,1.5,length.out = 5000)
H=seq(0.000001,1.5,length.out = 5000)
K=seq(0.000000001,0.01,length.out = 5000)
L=seq(0.000000001,0.01,length.out = 5000)
A_sample=sample(A,500,replace = T)
B_sample=sample(B,500,replace = T)
D_sample=sample(D,500,replace = T)
E_sample=sample(E,500,replace = T)
G_sample=sample(G,500,replace = T)
H_sample=sample(H,500,replace = T)
K_sample=sample(K,500,replace = T)
L_sample=sample(L,500,replace = T)
par_sample=data.frame(A_sample,B_sample,D_sample,E_sample,G_sample,H_sample,K_sample,L_sample)
test_4=lik_value_1(par_sample,train_data_xlevel_4_final)

which(test_4$lik_val==min(test_4$lik_val))

opt_4_1=optim(c(par_sample[238,1],par_sample[238,2],par_sample[238,3],par_sample[238,4],par_sample[238,5], par_sample[238,6],par_sample[238,7],par_sample[238,8]),our_likelihood_4,y=train_data_xlevel_4_final,control = c(maxit=5000))


### Line seg 3 and defect type XLEVEL
for(ii in 1:10){
  if(ii==1)
  {
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    u=seq(1,20,length.out = 500)
    c=seq(0.1,2,length.out = 500)
    b=seq(0.01,1.5,length.out = 500)
  }
  length_uni_mile_xlevel_3_1=length(levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST)))  ##429
  xlevel_sample_pos_3_1=sample(1:length_uni_mile_xlevel_3_1,round(length_uni_mile_xlevel_3_1*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_3_1=levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_3_1=xlevel_uniq_pos_3_1[-xlevel_sample_pos_3_1]
  xlevel_uniq_pos_test_3_1=xlevel_uniq_pos_3_1[xlevel_sample_pos_3_1]
  train_xlevel_3_1_no_day_no_single_trn=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_1)
  train_xlevel_3_1_no_day_no_single_tst=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_1)
  length_uni_mile_xlevel_3_2=length(levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST)))  ##314
  xlevel_sample_pos_3_2=sample(1:length_uni_mile_xlevel_3_2,round(length_uni_mile_xlevel_3_2*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_3_2=levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_3_2=xlevel_uniq_pos_3_2[-xlevel_sample_pos_3_2]
  xlevel_uniq_pos_test_3_2=xlevel_uniq_pos_3_2[xlevel_sample_pos_3_2]
  train_xlevel_3_2_no_day_no_single_trn=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_2)
  train_xlevel_3_2_no_day_no_single_tst=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_2)
  train_data_xlevel_3=rbind(train_xlevel_3_1_no_day_no_single_trn,train_xlevel_3_2_no_day_no_single_trn)
  test_data_xlevel_3=rbind(train_xlevel_3_1_no_day_no_single_tst,train_xlevel_3_2_no_day_no_single_tst)
  train_data_xlevel_3_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_3)
  train_data_xlevel_3_cum_amptd=cum_amptd_time_1(train_data_xlevel_3_mile_changed,inspect_data)
  train_data_xlevel_3_final=add_train_tonnage(train_data_xlevel_3_cum_amptd,tonnage_combine_all_years)
  
  A_sample_xlevel_3=sample(A,500,replace = T)
  B_sample_xlevel_3=sample(B,500,replace = T)
  D_sample_xlevel_3=sample(D,500,replace = T)
  E_sample_xlevel_3=sample(E,500,replace = T)
  G_sample_xlevel_3=sample(G,500,replace = T)
  H_sample_xlevel_3=sample(H,500,replace = T)
  K_sample_xlevel_3=sample(K,500,replace = T)
  L_sample_xlevel_3=sample(L,500,replace = T)
  par_sample_xlevel_3=data.frame(A_sample_xlevel_3,B_sample_xlevel_3,D_sample_xlevel_3,E_sample_xlevel_3,G_sample_xlevel_3,H_sample_xlevel_3,K_sample_xlevel_3,L_sample_xlevel_3)
  lik_xlevel_3=lik_value_1(par_sample_xlevel_3,train_data_xlevel_3_final)
  min_rw_xlevel_3=which(lik_xlevel_3$lik_val==min(lik_xlevel_3$lik_val))
  opt_xlevel_3=optim(c(par_sample_xlevel_3[min_rw_xlevel_3,1],par_sample_xlevel_3[min_rw_xlevel_3,2],par_sample_xlevel_3[min_rw_xlevel_3,3],par_sample_xlevel_3[min_rw_xlevel_3,4],par_sample_xlevel_3[min_rw_xlevel_3,5], par_sample_xlevel_3[min_rw_xlevel_3,6],par_sample_xlevel_3[min_rw_xlevel_3,7],par_sample_xlevel_3[min_rw_xlevel_3,8]),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))
  
  prob_gamma_surface_3=function(x){
    result=c()
    A=opt_surface_3$par[1]
    B=opt_surface_3$par[2]
    D=opt_surface_3$par[3]
    E=opt_surface_3$par[4]
    G=opt_surface_3$par[5]
    H=opt_surface_3$par[6]
    K=opt_surface_3$par[7]
    L=opt_surface_3$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_xlevel_3_ampltd_dif=ampltd_difference(test_data_xlevel_3,tonnage_combine_all_years,inspect_data)
  test_data_xlevel_3_inspect_0=subset(test_data_xlevel_3_ampltd_dif,test_data_xlevel_3_ampltd_dif$inspect_count==0)
  test_data_xlevel_3_threshold_add=add_threshold(test_data_xlevel_3_inspect_0,tag_limits)
  test_data_xlevel_3_valid=subset(test_data_xlevel_3_threshold_add,test_data_xlevel_3_threshold_add$defect_amp_diff>0 & test_data_xlevel_3_threshold_add$AMPLTD_NEED>0)
  prob_3_xlevel=prob_gamma(test_data_xlevel_3_valid$AMPLTD_NEED,test_data_xlevel_3_valid$day_count)
  xlevel_3_pred=prediction(prob_3_xlevel,test_data_xlevel_3_valid)
  match[ii]=length(which(xlevel_3_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(xlevel_3_pred))*100
  name_1=paste0("xlevel_3_pred","_",ii)
  false_positive[ii]=length(which(xlevel_3_pred$pred_match==1))
  false_negative[ii]=length(which(xlevel_3_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(xlevel_3_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(xlevel_3_pred))*100
  write.xlsx(xlevel_3_pred,"xlevel_3_pred.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"xlevel_3_pred.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}
A_sample_xlevel_3=sample(A,500,replace = T)
B_sample_xlevel_3=sample(B,500,replace = T)
D_sample_xlevel_3=sample(D,500,replace = T)
E_sample_xlevel_3=sample(E,500,replace = T)
G_sample_xlevel_3=sample(G,500,replace = T)
H_sample_xlevel_3=sample(H,500,replace = T)
K_sample_xlevel_3=sample(K,500,replace = T)
L_sample_xlevel_3=sample(L,500,replace = T)
par_sample_xlevel_3=data.frame(A_sample_xlevel_3,B_sample_xlevel_3,D_sample_xlevel_3,E_sample_xlevel_3,G_sample_xlevel_3,H_sample_xlevel_3,K_sample_xlevel_3,L_sample_xlevel_3)
lik_xlevel_3=lik_value_1(par_sample_xlevel_3,train_data_xlevel_3_final)
min_rw_xlevel_3=which(lik_xlevel_3$lik_val==min(lik_xlevel_3$lik_val))

opt_xlevel_3=optim(c(par_sample_xlevel_3[min_rw_xlevel_3,1],par_sample_xlevel_3[min_rw_xlevel_3,2],par_sample_xlevel_3[min_rw_xlevel_3,3],par_sample_xlevel_3[min_rw_xlevel_3,4],par_sample_xlevel_3[min_rw_xlevel_3,5], par_sample_xlevel_3[min_rw_xlevel_3,6],par_sample_xlevel_3[min_rw_xlevel_3,7],par_sample_xlevel_3[min_rw_xlevel_3,8]),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))

## Line seg 1 and defect type xlevel

for(ii in 1:10){
  library(xlsx)
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    A=seq(0.1,2.5,length.out = 5000)
    B=seq(0.1,1.5,length.out = 5000)
    D=seq(0.1,1.5,length.out = 5000)
    E=seq(0.1,1.5,length.out = 5000)
    G=seq(0.1,1.5,length.out = 5000)
    H=seq(0.1,1.5,length.out = 5000)
    K=seq(0.001,0.1,length.out = 5000)
    L=seq(0.00001,0.001,length.out = 5000)
  }
  length_uni_mile_xlevel_1_0=length(levels(as.factor(train_xlevel_1_0_no_day_no_single$MILEPOST)))  ##119
  xlevel_sample_pos_1_0=sample(1:length_uni_mile_xlevel_1_0,round(length_uni_mile_xlevel_1_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  xlevel_uniq_pos_1_0=levels(as.factor(train_xlevel_1_0_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_1_0=xlevel_uniq_pos_1_0[-xlevel_sample_pos_1_0]
  xlevel_uniq_pos_test_1_0=xlevel_uniq_pos_1_0[xlevel_sample_pos_1_0]
  train_xlevel_1_0_no_day_no_single_trn=subset(train_xlevel_1_0_no_day_no_single,train_xlevel_1_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_1_0)
  train_xlevel_1_0_no_day_no_single_tst=subset(train_xlevel_1_0_no_day_no_single,train_xlevel_1_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_1_0) 
  train_data_xlevel_1=rbind(train_xlevel_1_0_no_day_no_single_trn)
  test_data_xlevel_1=rbind(train_xlevel_1_0_no_day_no_single_tst)
  train_data_xlevel_1_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_1)
  train_data_xlevel_1_cum_amptd=cum_amptd_time_1(train_data_xlevel_1_mile_changed,inspect_data)
  train_data_xlevel_1_final=add_train_tonnage(train_data_xlevel_1_cum_amptd,tonnage_combine_all_years)
  A_sample_xlevel_1=sample(A,500,replace = T)
  B_sample_xlevel_1=sample(B,500,replace = T)
  D_sample_xlevel_1=sample(D,500,replace = T)
  E_sample_xlevel_1=sample(E,500,replace = T)
  G_sample_xlevel_1=sample(G,500,replace = T)
  H_sample_xlevel_1=sample(H,500,replace = T)
  K_sample_xlevel_1=sample(K,500,replace = T)
  L_sample_xlevel_1=sample(L,500,replace = T)
  par_sample_xlevel_1=data.frame(A_sample_xlevel_1,B_sample_xlevel_1,D_sample_xlevel_1,E_sample_xlevel_1,G_sample_xlevel_1,H_sample_xlevel_1,K_sample_xlevel_1,L_sample_xlevel_1)
  lik_xlevel_1=lik_value_1(par_sample_xlevel_1,train_data_xlevel_1_final)
  min_rw_xlevel_1=which(lik_xlevel_1$lik_val==min(lik_xlevel_1$lik_val))
  
  opt_xlevel_1=optim(c(par_sample_xlevel_1[min_rw_xlevel_1,1],par_sample_xlevel_1[min_rw_xlevel_1,2],par_sample_xlevel_1[min_rw_xlevel_1,1],par_sample_xlevel_1[min_rw_xlevel_1,4],par_sample_xlevel_1[min_rw_xlevel_1,5], par_sample_xlevel_1[min_rw_xlevel_1,6],par_sample_xlevel_1[min_rw_xlevel_1,7],par_sample_xlevel_1[min_rw_xlevel_1,8]),our_likelihood_4,y=train_data_xlevel_1_final,control = c(maxit=5000))
  
  prob_gamma_xlevel_1=function(x){
    result=c()
    A=opt_xlevel_1$par[1]
    B=opt_xlevel_1$par[2]
    D=opt_xlevel_1$par[3]
    E=opt_xlevel_1$par[4]
    G=opt_xlevel_1$par[5]
    H=opt_xlevel_1$par[6]
    K=opt_xlevel_1$par[7]
    L=opt_xlevel_1$par[8]
    scale=A
    for(j in 1:nrow(x)){
    shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
    result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    return(result)
  }
  ################# prepration of test data
  test_data_xlevel_1_ampltd_dif=ampltd_difference(test_data_xlevel_1,tonnage_combine_all_years,inspect_data)
  test_data_xlevel_1_inspect_0=subset(test_data_xlevel_1_ampltd_dif,test_data_xlevel_1_ampltd_dif$inspect_count==0)
  test_data_xlevel_1_threshold_add=add_threshold(test_data_xlevel_1_inspect_0,tag_limits)
  test_data_xlevel_1_valid=subset(test_data_xlevel_1_threshold_add,test_data_xlevel_1_threshold_add$defect_amp_diff>0 & test_data_xlevel_1_threshold_add$AMPLTD_NEED>0 & test_data_xlevel_1_threshold_add$day_count>7)
  
  prob_1_xlevel=prob_gamma_xlevel_1(test_data_xlevel_1_valid)
  xlevel_1_pred=prediction(prob_1_xlevel,test_data_xlevel_1_valid)
  match[ii]=length(which(xlevel_1_pred$pred_match==0)) ##155
  predt_accrcy[ii]=(match[ii]/nrow(xlevel_1_pred))*100
  name_1=paste0("xlevel_1_pred","_",ii)
  false_positive[ii]=length(which(xlevel_1_pred$pred_match==1))
  false_negative[ii]=length(which(xlevel_1_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(xlevel_1_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(xlevel_1_pred))*100
  write.xlsx(xlevel_1_pred,"xlevel_1_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"xlevel_1_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
  }
}

## Line seg 2 and defect type xlevel
for(ii in 1:10){
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    A=seq(1,3,length.out = 5000)
    B=seq(0.001,2,length.out = 5000)
    D=seq(0.00001,1,length.out = 5000)
    E=seq(0.000001,1.5,length.out = 5000)
    G=seq(0.000001,1.5,length.out = 5000)
    H=seq(0.000001,1.5,length.out = 5000)
    K=seq(0.000000001,0.01,length.out = 5000)
    L=seq(0.000000001,0.01,length.out = 5000)
    Optim_val_of_likeld=c()
    par_A=c()
    par_B=c()
    par_D=c()
    par_E=c()
    par_G=c()
    par_H=c()
    par_K=c()
    par_L=c()
  }
  length_uni_mile_xlevel_2_0=length(levels(as.factor(train_xlevel_2_0_no_day_no_single$MILEPOST)))  ##22
  xlevel_sample_pos_2_0=sample(1:length_uni_mile_xlevel2_0,round(length_uni_mile_xlevel2_0*0.20,digits = 0))
  xlevel_uniq_pos_2_0=levels(as.factor(train_xlevel_2_0_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_2_0=xlevel_uniq_pos_2_0[-xlevel_sample_pos_2_0]
  xlevel_uniq_pos_test_2_0=xlevel_uniq_pos_2_0[xlevel_sample_pos_2_0]
  train_xlevel_2_0_no_day_no_single_trn=subset(train_xlevel_2_0_no_day_no_single,train_xlevel_2_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_0)
  train_xlevel_2_0_no_day_no_single_tst=subset(train_xlevel_2_0_no_day_no_single,train_xlevel_2_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_0)
  
  
  length_uni_mile_xlevel2_1=length(levels(as.factor(train_xlevel_2_1_no_day_no_single$MILEPOST)))
  xlevel_sample_pos_2_1=sample(1:length_uni_mile_xlevel2_1,round(length_uni_mile_xlevel2_1*0.20,digits = 0))
  xlevel_uniq_pos_2_1=levels(as.factor(train_xlevel_2_1_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_2_1=xlevel_uniq_pos_2_1[-xlevel_sample_pos_2_1]
  xlevel_uniq_pos_test_2_1=xlevel_uniq_pos_2_1[xlevel_sample_pos_2_1]
  train_xlevel_2_1_no_day_no_single_trn=subset(train_xlevel_2_1_no_day_no_single,train_xlevel_2_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_1)
  train_xlevel_2_1_no_day_no_single_tst=subset(train_xlevel_2_1_no_day_no_single,train_xlevel_2_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_1)
  length_uni_mile_xlevel2_2=length(levels(as.factor(train_xlevel_2_2_no_day_no_single$MILEPOST)))  
  xlevel_sample_pos_2_2=sample(1:length_uni_mile_xlevel2_2,round(length_uni_mile_xlevel2_2*0.20,digits = 0))  
  xlevel_uniq_pos_2_2=levels(as.factor(train_xlevel_2_2_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_2_2=xlevel_uniq_pos_2_2[-xlevel_sample_pos_2_2]
  xlevel_uniq_pos_test_2_2=xlevel_uniq_pos_2_2[xlevel_sample_pos_2_2]
  train_xlevel_2_2_no_day_no_single_trn=subset(train_xlevel_2_2_no_day_no_single,train_xlevel_2_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_2)
  train_xlevel_2_2_no_day_no_single_tst=subset(train_xlevel_2_2_no_day_no_single,train_xlevel_2_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_2)
  train_data_xlevel_2=rbind(train_xlevel_2_0_no_day_no_single_trn,train_xlevel_2_1_no_day_no_single_trn,train_xlevel_2_2_no_day_no_single_trn)
  test_data_xlevel_2=rbind(train_xlevel_2_0_no_day_no_single_tst,train_xlevel_2_1_no_day_no_single_tst,train_xlevel_2_2_no_day_no_single_tst)
  train_data_xlevel_2_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_2)
  train_data_xlevel_2_cum_amptd=cum_amptd_time_1(train_data_xlevel_2_mile_changed,inspect_data)
  train_data_xlevel_2_final=add_train_tonnage(train_data_xlevel_2_cum_amptd,tonnage_combine_all_years)
  
  
  A_sample_xlevel_2=sample(A,500,replace = T)
  B_sample_xlevel_2=sample(B,500,replace = T)
  D_sample_xlevel_2=sample(D,500,replace = T)
  E_sample_xlevel_2=sample(E,500,replace = T)
  G_sample_xlevel_2=sample(G,500,replace = T)
  H_sample_xlevel_2=sample(H,500,replace = T)
  K_sample_xlevel_2=sample(K,500,replace = T)
  L_sample_xlevel_2=sample(L,500,replace = T)
  par_sample_xlevel_2=data.frame(A_sample_xlevel_2,B_sample_xlevel_2,D_sample_xlevel_2,E_sample_xlevel_2,G_sample_xlevel_2,H_sample_xlevel_2,K_sample_xlevel_2,L_sample_xlevel_2)
  lik_xlevel_2=lik_value_1(par_sample_xlevel_2, train_data_xlevel_2_final)
  min_rw_xlevel_2=which(lik_xlevel_2$lik_val==min(lik_xlevel_2$lik_val))
  
  opt_xlevel_2=optim(c(par_sample_xlevel_2[min_rw_xlevel_2,1],par_sample_xlevel_2[min_rw_xlevel_2,2],par_sample_xlevel_2[min_rw_xlevel_2,1],par_sample_xlevel_2[min_rw_xlevel_2,4],par_sample_xlevel_2[min_rw_xlevel_2,5], par_sample_xlevel_2[min_rw_xlevel_2,6],par_sample_xlevel_2[min_rw_xlevel_2,7],par_sample_xlevel_2[min_rw_xlevel_2,8]),our_likelihood_4,y=train_data_xlevel_2_final,control = c(maxit=5000))
  par_A=c(par_A,opt_xlevel_2$par[1])
  par_B=c(par_B,opt_xlevel_2$par[2])
  par_D=c(par_D,opt_xlevel_2$par[3])
  par_E=c(par_E,opt_xlevel_2$par[4])
  par_G=c(par_G,opt_xlevel_2$par[5])
  par_H=c(par_H,opt_xlevel_2$par[6])
  par_K=c(par_K,opt_xlevel_2$par[7])
  par_L=c(par_L,opt_xlevel_2$par[8])
  Optim_val_of_likeld=c(Optim_val_of_likeld,opt_xlevel_2$value)
  prob_gamma_xlevel_2=function(x){
    result=c()
    A=opt_xlevel_2$par[1]
    B=opt_xlevel_2$par[2]
    D=opt_xlevel_2$par[3]
    E=opt_xlevel_2$par[4]
    G=opt_xlevel_2$par[5]
    H=opt_xlevel_2$par[6]
    K=opt_xlevel_2$par[7]
    L=opt_xlevel_2$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_xlevel_2_ampltd_dif=ampltd_difference(test_data_xlevel_2,tonnage_combine_all_years,inspect_data)
  test_data_xlevel_2_inspect_0=subset(test_data_xlevel_2_ampltd_dif,test_data_xlevel_2_ampltd_dif$inspect_count==0)
  test_data_xlevel_2_threshold_add=add_threshold(test_data_xlevel_2_inspect_0,tag_limits)
  test_data_xlevel_2_valid=subset(test_data_xlevel_2_threshold_add,test_data_xlevel_2_threshold_add$defect_amp_diff>0 & test_data_xlevel_2_threshold_add$AMPLTD_NEED>0 & test_data_xlevel_2_threshold_add$day_count>7)
  
  prob_1_xlevel=prob_gamma_xlevel_2(test_data_xlevel_2_valid)
  xlevel_2_pred=prediction(prob_1_xlevel,test_data_xlevel_2_valid)
  match[ii]=length(which(xlevel_2_pred$pred_match==0)) 
  predt_accrcy[ii]=(match[ii]/nrow(xlevel_2_pred))*100
  name_1=paste0("xlevel_2_pred","_",ii)
  false_positive[ii]=length(which(xlevel_2_pred$pred_match==1))
  false_negative[ii]=length(which(xlevel_2_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(xlevel_2_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(xlevel_2_pred))*100
  write.xlsx(xlevel_2_pred,"xlevel_2_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    pars=cbind(par_A,par_B,par_D,par_E,par_G,par_H,par_K,par_L,Optim_val_of_likeld)
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"xlevel_2_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
    write.xlsx(pars,"xlevel_2_pred_new.xlsx",sheetName = "Parameters",append = T,row.names = F)
  }
}


## Line seg 3 and defect type xlevel
for(ii in 1:10){
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    A=seq(1,3,length.out = 5000)
    B=seq(0.001,2,length.out = 5000)
    D=seq(0.00001,1,length.out = 5000)
    E=seq(0.000001,1.5,length.out = 5000)
    G=seq(0.000001,1.5,length.out = 5000)
    H=seq(0.000001,1.5,length.out = 5000)
    K=seq(0.000000001,0.01,length.out = 5000)
    L=seq(0.000000001,0.01,length.out = 5000)
    Optim_val_of_likeld=c()
    par_A=c()
    par_B=c()
    par_D=c()
    par_E=c()
    par_G=c()
    par_H=c()
    par_K=c()
    par_L=c()
  }
  length_uni_mile_xlevel_3_1=length(levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST)))
  xlevel_sample_pos_3_1=sample(1:length_uni_mile_xlevel_3_1,round(length_uni_mile_xlevel_3_1*0.20,digits = 0))
  xlevel_uniq_pos_3_1=levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_3_1=xlevel_uniq_pos_3_1[-xlevel_sample_pos_3_1]
  xlevel_uniq_pos_test_3_1=xlevel_uniq_pos_3_1[xlevel_sample_pos_3_1]
  train_xlevel_3_1_no_day_no_single_trn=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_1)
  train_xlevel_3_1_no_day_no_single_tst=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_1)
  length_uni_mile_xlevel_3_2=length(levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST)))  
  xlevel_sample_pos_3_2=sample(1:length_uni_mile_xlevel_3_2,round(length_uni_mile_xlevel_3_2*0.20,digits = 0))  
  xlevel_uniq_pos_3_2=levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST))
  xlevel_uniq_pos_train_3_2=xlevel_uniq_pos_3_2[-xlevel_sample_pos_3_2]
  xlevel_uniq_pos_test_3_2=xlevel_uniq_pos_3_2[xlevel_sample_pos_3_2]
  train_xlevel_3_2_no_day_no_single_trn=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_2)
  train_xlevel_3_2_no_day_no_single_tst=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_2)
  train_data_xlevel_3=rbind(train_xlevel_3_1_no_day_no_single_trn,train_xlevel_3_2_no_day_no_single_trn)
  test_data_xlevel_3=rbind(train_xlevel_3_1_no_day_no_single_tst,train_xlevel_3_2_no_day_no_single_tst)
  train_data_xlevel_3_mile_changed=mile_post_change_for_2nd_method(train_data_xlevel_3)
  train_data_xlevel_3_cum_amptd=cum_amptd_time_1(train_data_xlevel_3_mile_changed,inspect_data)
  train_data_xlevel_3_final=add_train_tonnage(train_data_xlevel_3_cum_amptd,tonnage_combine_all_years)
  
  
  A_sample_xlevel_3=sample(A,500,replace = T)
  B_sample_xlevel_3=sample(B,500,replace = T)
  D_sample_xlevel_3=sample(D,500,replace = T)
  E_sample_xlevel_3=sample(E,500,replace = T)
  G_sample_xlevel_3=sample(G,500,replace = T)
  H_sample_xlevel_3=sample(H,500,replace = T)
  K_sample_xlevel_3=sample(K,500,replace = T)
  L_sample_xlevel_3=sample(L,500,replace = T)
  par_sample_xlevel_3=data.frame(A_sample_xlevel_3,B_sample_xlevel_3,D_sample_xlevel_3,E_sample_xlevel_3,G_sample_xlevel_3,H_sample_xlevel_3,K_sample_xlevel_3,L_sample_xlevel_3)
  lik_xlevel_3=lik_value_1(par_sample_xlevel_3, train_data_xlevel_3_final)
  min_rw_xlevel_3=which(lik_xlevel_3$lik_val==min(lik_xlevel_3$lik_val))
  
  opt_xlevel_3=optim(c(par_sample_xlevel_3[min_rw_xlevel_3,1],par_sample_xlevel_3[min_rw_xlevel_3,2],par_sample_xlevel_3[min_rw_xlevel_3,1],par_sample_xlevel_3[min_rw_xlevel_3,4],par_sample_xlevel_3[min_rw_xlevel_3,5], par_sample_xlevel_3[min_rw_xlevel_3,6],par_sample_xlevel_3[min_rw_xlevel_3,7],par_sample_xlevel_3[min_rw_xlevel_3,8]),our_likelihood_4,y=train_data_xlevel_3_final,control = c(maxit=5000))
  par_A=c(par_A,opt_xlevel_3$par[1])
  par_B=c(par_B,opt_xlevel_3$par[2])
  par_D=c(par_D,opt_xlevel_3$par[3])
  par_E=c(par_E,opt_xlevel_3$par[4])
  par_G=c(par_G,opt_xlevel_3$par[5])
  par_H=c(par_H,opt_xlevel_3$par[6])
  par_K=c(par_K,opt_xlevel_3$par[7])
  par_L=c(par_L,opt_xlevel_3$par[8])
  Optim_val_of_likeld=c(Optim_val_of_likeld,opt_xlevel_3$value)
  prob_gamma_xlevel_3=function(x){
    result=c()
    A=opt_xlevel_3$par[1]
    B=opt_xlevel_3$par[2]
    D=opt_xlevel_3$par[3]
    E=opt_xlevel_3$par[4]
    G=opt_xlevel_3$par[5]
    H=opt_xlevel_3$par[6]
    K=opt_xlevel_3$par[7]
    L=opt_xlevel_3$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_xlevel_3_ampltd_dif=ampltd_difference(test_data_xlevel_3,tonnage_combine_all_years,inspect_data)
  test_data_xlevel_3_inspect_0=subset(test_data_xlevel_3_ampltd_dif,test_data_xlevel_3_ampltd_dif$inspect_count==0)
  test_data_xlevel_3_threshold_add=add_threshold(test_data_xlevel_3_inspect_0,tag_limits)
  test_data_xlevel_3_valid=subset(test_data_xlevel_3_threshold_add,test_data_xlevel_3_threshold_add$defect_amp_diff>0 & test_data_xlevel_3_threshold_add$AMPLTD_NEED>0 & test_data_xlevel_3_threshold_add$day_count>7)
  
  prob_1_xlevel=prob_gamma_xlevel_3(test_data_xlevel_3_valid)
  xlevel_3_pred=prediction(prob_1_xlevel,test_data_xlevel_3_valid)
  match[ii]=length(which(xlevel_3_pred$pred_match==0)) 
  predt_accrcy[ii]=(match[ii]/nrow(xlevel_3_pred))*100
  name_1=paste0("xlevel_3_pred","_",ii)
  false_positive[ii]=length(which(xlevel_3_pred$pred_match==1))
  false_negative[ii]=length(which(xlevel_3_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(xlevel_3_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(xlevel_3_pred))*100
  write.xlsx(xlevel_3_pred,"xlevel_3_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    pars=cbind(par_A,par_B,par_D,par_E,par_G,par_H,par_K,par_L,Optim_val_of_likeld)
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"xlevel_3_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
    write.xlsx(pars,"xlevel_3_pred_new.xlsx",sheetName = "Parameters",append = T,row.names = F)
  }
}

## # Surface Line 1
for(ii in 1:10){
  library(xlsx)
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    cut_off_prob=c()
    A=seq(1,3,length.out = 5000)
    B=seq(0.001,2,length.out = 5000)
    D=seq(0.00001,1,length.out = 5000)
    E=seq(0.000001,1.5,length.out = 5000)
    G=seq(0.000001,1.5,length.out = 5000)
    H=seq(0.000001,1.5,length.out = 5000)
    K=seq(0.000000001,0.01,length.out = 5000)
    L=seq(0.000000001,0.01,length.out = 5000)
    Optim_val_of_likeld=c()
    par_A=c()
    par_B=c()
    par_D=c()
    par_E=c()
    par_G=c()
    par_H=c()
    par_K=c()
    par_L=c()
  }
  length_uni_mile_surface_1_0=length(levels(as.factor(train_surface_1_0_no_day_no_single$MILEPOST)))  ##119
  surface_sample_pos_1_0=sample(1:length_uni_mile_surface_1_0,round(length_uni_mile_surface_1_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  surface_uniq_pos_1_0=levels(as.factor(train_surface_1_0_no_day_no_single$MILEPOST))
  surface_uniq_pos_train_1_0=surface_uniq_pos_1_0[-surface_sample_pos_1_0]
  surface_uniq_pos_test_1_0=surface_uniq_pos_1_0[surface_sample_pos_1_0]
  train_surface_1_0_no_day_no_single_trn=subset(train_surface_1_0_no_day_no_single,train_surface_1_0_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_1_0)
  train_surface_1_0_no_day_no_single_tst=subset(train_surface_1_0_no_day_no_single,train_surface_1_0_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_1_0) 
  train_data_surface_1=rbind(train_surface_1_0_no_day_no_single_trn)
  test_data_surface_1=rbind(train_surface_1_0_no_day_no_single_tst)
  train_data_surface_1_mile_changed=mile_post_change_for_2nd_method(train_data_surface_1)
  train_data_surface_1_cum_amptd=cum_amptd_time_1(train_data_surface_1_mile_changed,inspect_data)
  train_data_surface_1_final=add_train_tonnage(train_data_surface_1_cum_amptd,tonnage_combine_all_years)
  A_sample_surface_1=sample(A,500,replace = T)
  B_sample_surface_1=sample(B,500,replace = T)
  D_sample_surface_1=sample(D,500,replace = T)
  E_sample_surface_1=sample(E,500,replace = T)
  G_sample_surface_1=sample(G,500,replace = T)
  H_sample_surface_1=sample(H,500,replace = T)
  K_sample_surface_1=sample(K,500,replace = T)
  L_sample_surface_1=sample(L,500,replace = T)
  par_sample_surface_1=data.frame(A_sample_surface_1,B_sample_surface_1,D_sample_surface_1,E_sample_surface_1,G_sample_surface_1,H_sample_surface_1,K_sample_surface_1,L_sample_surface_1)
  lik_surface_1=lik_value_1(par_sample_surface_1, train_data_surface_1_final)
  min_rw_surface_1=which(lik_surface_1$lik_val==min(lik_surface_1$lik_val))
  
  opt_surface_1=optim(c(par_sample_surface_1[min_rw_surface_1,1],par_sample_surface_1[min_rw_surface_1,2],par_sample_surface_1[min_rw_surface_1,1],par_sample_surface_1[min_rw_surface_1,4],par_sample_surface_1[min_rw_surface_1,5], par_sample_surface_1[min_rw_surface_1,6],par_sample_surface_1[min_rw_surface_1,7],par_sample_surface_1[min_rw_surface_1,8]),our_likelihood_4,y=train_data_surface_1_final,control = c(maxit=5000))
  par_A=c(par_A,opt_surface_1$par[1])
  par_B=c(par_B,opt_surface_1$par[2])
  par_D=c(par_D,opt_surface_1$par[3])
  par_E=c(par_E,opt_surface_1$par[4])
  par_G=c(par_G,opt_surface_1$par[5])
  par_H=c(par_H,opt_surface_1$par[6])
  par_K=c(par_K,opt_surface_1$par[7])
  par_L=c(par_L,opt_surface_1$par[8])
  Optim_val_of_likeld=c(Optim_val_of_likeld,opt_surface_1$value)
  prob_gamma_surface_1=function(x){
    result=c()
    A=opt_surface_1$par[1]
    B=opt_surface_1$par[2]
    D=opt_surface_1$par[3]
    E=opt_surface_1$par[4]
    G=opt_surface_1$par[5]
    H=opt_surface_1$par[6]
    K=opt_surface_1$par[7]
    L=opt_surface_1$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_surface_1_ampltd_dif=ampltd_difference(test_data_surface_1,tonnage_combine_all_years,inspect_data)
  test_data_surface_1_inspect_0=subset(test_data_surface_1_ampltd_dif,test_data_surface_1_ampltd_dif$inspect_count==0)
  test_data_surface_1_threshold_add=add_threshold(test_data_surface_1_inspect_0,tag_limits)
  test_data_surface_1_valid=subset(test_data_surface_1_threshold_add,test_data_surface_1_threshold_add$defect_amp_diff>0 & test_data_surface_1_threshold_add$AMPLTD_NEED>0 & test_data_surface_1_threshold_add$day_count>7)
  
  prob_1_surface=prob_gamma_surface_1(test_data_surface_1_valid)
  cut_off_prob[ii]=cut_off_prob_selection(prob_1_surface,test_data_surface_1_valid)
  surface_1_pred=prediction_1(prob_1_surface,test_data_surface_1_valid,cut_off_prob[ii])
  match[ii]=length(which(surface_1_pred$pred_match==0)) 
  predt_accrcy[ii]=(match[ii]/nrow(surface_1_pred))*100
  name_1=paste0("surface_1_pred","_",ii)
  false_positive[ii]=length(which(surface_1_pred$pred_match==1))
  false_negative[ii]=length(which(surface_1_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(surface_1_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(surface_1_pred))*100
  write.xlsx(surface_1_pred,"surface_1_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    pars=cbind(par_A,par_B,par_D,par_E,par_G,par_H,par_K,par_L,Optim_val_of_likeld)
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"surface_1_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
    write.xlsx(pars,"surface_1_pred_new.xlsx",sheetName = "Parameters",append = T,row.names = F)
    write.xlsx(cut_off_prob,"surface_1_pred_new.xlsx",sheetName = "cut_off_probalility",append = T,row.names = F)
  }
}

cut_off_prob[ii]=cut_off_prob_selection(prob_1_surface,test_data_surface_1_valid)
surface_1_pred=prediction_1(prob_1_surface,test_data_surface_1_valid,cut_off_prob[ii])
write.xlsx(cut_off_prob,"surface_1_pred_new.xlsx",sheetName = "cut_off_probalility",append = T,row.names = F)


## Line segment 2 and surface

for(ii in 1:10){
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    cut_off_prob=c()
    A=seq(1,3,length.out = 5000)
    B=seq(0.001,2,length.out = 5000)
    D=seq(0.00001,1,length.out = 5000)
    E=seq(0.000001,1.5,length.out = 5000)
    G=seq(0.000001,1.5,length.out = 5000)
    H=seq(0.000001,1.5,length.out = 5000)
    K=seq(0.000000001,0.01,length.out = 5000)
    L=seq(0.000000001,0.01,length.out = 5000)
    Optim_val_of_likeld=c()
    par_A=c()
    par_B=c()
    par_D=c()
    par_E=c()
    par_G=c()
    par_H=c()
    par_K=c()
    par_L=c()
  }
  length_uni_mile_surface_2_0=length(levels(as.factor(train_surface_2_0_no_day_no_single$MILEPOST)))  ##22
  surface_sample_pos_2_0=sample(1:length_uni_mile_surface_2_0,round(length_uni_mile_surface_2_0*0.20,digits = 0))
  surface_uniq_pos_2_0=levels(as.factor(train_surface_2_0_no_day_no_single$MILEPOST))
  surface_uniq_pos_train_2_0=surface_uniq_pos_2_0[-surface_sample_pos_2_0]
  surface_uniq_pos_test_2_0=surface_uniq_pos_2_0[surface_sample_pos_2_0]
  train_surface_2_0_no_day_no_single_trn=subset(train_surface_2_0_no_day_no_single,train_surface_2_0_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_2_0)
  train_surface_2_0_no_day_no_single_tst=subset(train_surface_2_0_no_day_no_single,train_surface_2_0_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_2_0)
  
  
  length_uni_mile_surface_2_1=length(levels(as.factor(train_surface_2_1_no_day_no_single$MILEPOST)))
  surface_sample_pos_2_1=sample(1:length_uni_mile_surface_2_1,round(length_uni_mile_surface_2_1*0.20,digits = 0))
  surface_uniq_pos_2_1=levels(as.factor(train_surface_2_1_no_day_no_single$MILEPOST))
  surface_uniq_pos_train_2_1=surface_uniq_pos_2_1[-surface_sample_pos_2_1]
  surface_uniq_pos_test_2_1=surface_uniq_pos_2_1[surface_sample_pos_2_1]
  train_surface_2_1_no_day_no_single_trn=subset(train_surface_2_1_no_day_no_single,train_surface_2_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_2_1)
  train_surface_2_1_no_day_no_single_tst=subset(train_surface_2_1_no_day_no_single,train_surface_2_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_2_1)
  length_uni_mile_surface_2_2=length(levels(as.factor(train_surface_2_2_no_day_no_single$MILEPOST)))  
  surface_sample_pos_2_2=sample(1:length_uni_mile_surface_2_2,round(length_uni_mile_surface_2_2*0.20,digits = 0))  
  surface_uniq_pos_2_2=levels(as.factor(train_surface_2_2_no_day_no_single$MILEPOST))
  surface_uniq_pos_train_2_2=surface_uniq_pos_2_2[-surface_sample_pos_2_2]
  surface_uniq_pos_test_2_2=surface_uniq_pos_2_2[surface_sample_pos_2_2]
  train_surface_2_2_no_day_no_single_trn=subset(train_surface_2_2_no_day_no_single,train_surface_2_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_2_2)
  train_surface_2_2_no_day_no_single_tst=subset(train_surface_2_2_no_day_no_single,train_surface_2_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_2_2)
  train_data_surface_2=rbind(train_surface_2_0_no_day_no_single_trn,train_surface_2_1_no_day_no_single_trn,train_surface_2_2_no_day_no_single_trn)
  test_data_surface_2=rbind(train_surface_2_0_no_day_no_single_tst,train_surface_2_1_no_day_no_single_tst,train_surface_2_2_no_day_no_single_tst)
  train_data_surface_2_mile_changed=mile_post_change_for_2nd_method(train_data_surface_2)
  train_data_surface_2_cum_amptd=cum_amptd_time_1(train_data_surface_2_mile_changed,inspect_data)
  train_data_surface_2_final=add_train_tonnage(train_data_surface_2_cum_amptd,tonnage_combine_all_years)
  
  
  A_sample_surface_2=sample(A,500,replace = T)
  B_sample_surface_2=sample(B,500,replace = T)
  D_sample_surface_2=sample(D,500,replace = T)
  E_sample_surface_2=sample(E,500,replace = T)
  G_sample_surface_2=sample(G,500,replace = T)
  H_sample_surface_2=sample(H,500,replace = T)
  K_sample_surface_2=sample(K,500,replace = T)
  L_sample_surface_2=sample(L,500,replace = T)
  par_sample_surface_2=data.frame(A_sample_surface_2,B_sample_surface_2,D_sample_surface_2,E_sample_surface_2,G_sample_surface_2,H_sample_surface_2,K_sample_surface_2,L_sample_surface_2)
  lik_surface_2=lik_value_1(par_sample_surface_2, train_data_surface_2_final)
  min_rw_surface_2=which(lik_surface_2$lik_val==min(lik_surface_2$lik_val))
  
  opt_surface_2=optim(c(par_sample_surface_2[min_rw_surface_2,1],par_sample_surface_2[min_rw_surface_2,2],par_sample_surface_2[min_rw_surface_2,1],par_sample_surface_2[min_rw_surface_2,4],par_sample_surface_2[min_rw_surface_2,5], par_sample_surface_2[min_rw_surface_2,6],par_sample_surface_2[min_rw_surface_2,7],par_sample_surface_2[min_rw_surface_2,8]),our_likelihood_4,y=train_data_surface_2_final,control = c(maxit=5000))
  par_A=c(par_A,opt_surface_2$par[1])
  par_B=c(par_B,opt_surface_2$par[2])
  par_D=c(par_D,opt_surface_2$par[3])
  par_E=c(par_E,opt_surface_2$par[4])
  par_G=c(par_G,opt_surface_2$par[5])
  par_H=c(par_H,opt_surface_2$par[6])
  par_K=c(par_K,opt_surface_2$par[7])
  par_L=c(par_L,opt_surface_2$par[8])
  Optim_val_of_likeld=c(Optim_val_of_likeld,opt_surface_2$value)
  prob_gamma_surface_2=function(x){
    result=c()
    A=opt_surface_2$par[1]
    B=opt_surface_2$par[2]
    D=opt_surface_2$par[3]
    E=opt_surface_2$par[4]
    G=opt_surface_2$par[5]
    H=opt_surface_2$par[6]
    K=opt_surface_2$par[7]
    L=opt_surface_2$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_surface_2_ampltd_dif=ampltd_difference(test_data_surface_2,tonnage_combine_all_years,inspect_data)
  test_data_surface_2_inspect_0=subset(test_data_surface_2_ampltd_dif,test_data_surface_2_ampltd_dif$inspect_count==0)
  test_data_surface_2_threshold_add=add_threshold(test_data_surface_2_inspect_0,tag_limits)
  test_data_surface_2_valid=subset(test_data_surface_2_threshold_add,test_data_surface_2_threshold_add$defect_amp_diff>0 & test_data_surface_2_threshold_add$AMPLTD_NEED>0 & test_data_surface_2_threshold_add$day_count>7)
  
  prob_2_surface=prob_gamma_surface_2(test_data_surface_2_valid)
  cut_off_prob[ii]=cut_off_prob_selection(prob_2_surface,test_data_surface_2_valid)
  surface_2_pred=prediction_1(prob_2_surface,test_data_surface_2_valid,cut_off_prob[ii])
  match[ii]=length(which(surface_2_pred$pred_match==0)) 
  predt_accrcy[ii]=(match[ii]/nrow(surface_2_pred))*100
  name_1=paste0("surface_2_pred","_",ii)
  false_positive[ii]=length(which(surface_2_pred$pred_match==1))
  false_negative[ii]=length(which(surface_2_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(surface_2_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(surface_2_pred))*100
  write.xlsx(surface_2_pred,"surface_2_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    pars=cbind(par_A,par_B,par_D,par_E,par_G,par_H,par_K,par_L,Optim_val_of_likeld)
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"surface_2_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
    write.xlsx(pars,"surface_2_pred_new.xlsx",sheetName = "Parameters",append = T,row.names = F)
    write.xlsx(cut_off_prob,"surface_2_pred_new.xlsx",sheetName = "cut_off_probalility",append = T,row.names = F)
    
  }
}


## Line segment 3 and surface

for(ii in 1:10){
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    A=seq(1,3,length.out = 5000)
    B=seq(0.001,2,length.out = 5000)
    D=seq(0.00001,1,length.out = 5000)
    E=seq(0.000001,1.5,length.out = 5000)
    G=seq(0.000001,1.5,length.out = 5000)
    H=seq(0.000001,1.5,length.out = 5000)
    K=seq(0.000000001,0.01,length.out = 5000)
    L=seq(0.000000001,0.01,length.out = 5000)
    Optim_val_of_likeld=c()
    par_A=c()
    par_B=c()
    par_D=c()
    par_E=c()
    par_G=c()
    par_H=c()
    par_K=c()
    par_L=c()
  }
  length_uni_mile_surface_3_1=length(levels(as.factor(train_surface_3_1_no_day_no_single$MILEPOST)))
  surface_sample_pos_3_1=sample(1:length_uni_mile_surface_3_1,round(length_uni_mile_surface_3_1*0.20,digits = 0))
  surface_uniq_pos_3_1=levels(as.factor(train_surface_3_1_no_day_no_single$MILEPOST))
  surface_uniq_pos_train_3_1=surface_uniq_pos_3_1[-surface_sample_pos_3_1]
  surface_uniq_pos_test_3_1=surface_uniq_pos_3_1[surface_sample_pos_3_1]
  train_surface_3_1_no_day_no_single_trn=subset(train_surface_3_1_no_day_no_single,train_surface_3_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_3_1)
  train_surface_3_1_no_day_no_single_tst=subset(train_surface_3_1_no_day_no_single,train_surface_3_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_3_1)
  length_uni_mile_surface_3_2=length(levels(as.factor(train_surface_3_2_no_day_no_single$MILEPOST)))  
  surface_sample_pos_3_2=sample(1:length_uni_mile_surface_3_2,round(length_uni_mile_surface_3_2*0.20,digits = 0))  
  surface_uniq_pos_3_2=levels(as.factor(train_surface_3_2_no_day_no_single$MILEPOST))
  surface_uniq_pos_train_3_2=surface_uniq_pos_3_2[-surface_sample_pos_3_2]
  surface_uniq_pos_test_3_2=surface_uniq_pos_3_2[surface_sample_pos_3_2]
  train_surface_3_2_no_day_no_single_trn=subset(train_surface_3_2_no_day_no_single,train_surface_3_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_3_2)
  train_surface_3_2_no_day_no_single_tst=subset(train_surface_3_2_no_day_no_single,train_surface_3_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_3_2)
  train_data_surface_3=rbind(train_surface_3_1_no_day_no_single_trn,train_surface_3_2_no_day_no_single_trn)
  test_data_surface_3=rbind(train_surface_3_1_no_day_no_single_tst,train_surface_3_2_no_day_no_single_tst)
  train_data_surface_3_mile_changed=mile_post_change_for_2nd_method(train_data_surface_3)
  train_data_surface_3_cum_amptd=cum_amptd_time_1(train_data_surface_3_mile_changed,inspect_data)
  train_data_surface_3_final=add_train_tonnage(train_data_surface_3_cum_amptd,tonnage_combine_all_years)
  
  
  A_sample_surface_3=sample(A,500,replace = T)
  B_sample_surface_3=sample(B,500,replace = T)
  D_sample_surface_3=sample(D,500,replace = T)
  E_sample_surface_3=sample(E,500,replace = T)
  G_sample_surface_3=sample(G,500,replace = T)
  H_sample_surface_3=sample(H,500,replace = T)
  K_sample_surface_3=sample(K,500,replace = T)
  L_sample_surface_3=sample(L,500,replace = T)
  par_sample_surface_3=data.frame(A_sample_surface_3,B_sample_surface_3,D_sample_surface_3,E_sample_surface_3,G_sample_surface_3,H_sample_surface_3,K_sample_surface_3,L_sample_surface_3)
  lik_surface_3=lik_value_1(par_sample_surface_3, train_data_surface_3_final)
  min_rw_surface_3=which(lik_surface_3$lik_val==min(lik_surface_3$lik_val))
  
  opt_surface_3=optim(c(par_sample_surface_3[min_rw_surface_3,1],par_sample_surface_3[min_rw_surface_3,2],par_sample_surface_3[min_rw_surface_3,3],par_sample_surface_3[min_rw_surface_3,4],par_sample_surface_3[min_rw_surface_3,5], par_sample_surface_3[min_rw_surface_3,6],par_sample_surface_3[min_rw_surface_3,7],par_sample_surface_3[min_rw_surface_3,8]),our_likelihood_4,y=train_data_surface_3_final,control = c(maxit=5000))
  par_A=c(par_A,opt_surface_3$par[1])
  par_B=c(par_B,opt_surface_3$par[2])
  par_D=c(par_D,opt_surface_3$par[3])
  par_E=c(par_E,opt_surface_3$par[4])
  par_G=c(par_G,opt_surface_3$par[5])
  par_H=c(par_H,opt_surface_3$par[6])
  par_K=c(par_K,opt_surface_3$par[7])
  par_L=c(par_L,opt_surface_3$par[8])
  Optim_val_of_likeld=c(Optim_val_of_likeld,opt_surface_3$value)
  prob_gamma_surface_3=function(x){
    result=c()
    A=opt_surface_3$par[1]
    B=opt_surface_3$par[2]
    D=opt_surface_3$par[3]
    E=opt_surface_3$par[4]
    G=opt_surface_3$par[5]
    H=opt_surface_3$par[6]
    K=opt_surface_3$par[7]
    L=opt_surface_3$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_surface_3_ampltd_dif=ampltd_difference(test_data_surface_3,tonnage_combine_all_years,inspect_data)
  test_data_surface_3_inspect_0=subset(test_data_surface_3_ampltd_dif,test_data_surface_3_ampltd_dif$inspect_count==0)
  test_data_surface_3_threshold_add=add_threshold(test_data_surface_3_inspect_0,tag_limits)
  test_data_surface_3_valid=subset(test_data_surface_3_threshold_add,test_data_surface_3_threshold_add$defect_amp_diff>0 & test_data_surface_3_threshold_add$AMPLTD_NEED>0 & test_data_surface_3_threshold_add$day_count>7)
  prob_3_surface=prob_gamma_surface_3(test_data_surface_3_valid)
  cut_off_prob[ii]=cut_off_prob_selection(prob_3_surface,test_data_surface_3_valid)
  surface_3_pred=prediction_1(prob_3_surface,test_data_surface_3_valid,cut_off_prob[ii])
  match[ii]=length(which(surface_3_pred$pred_match==0)) 
  predt_accrcy[ii]=(match[ii]/nrow(surface_3_pred))*100
  name_1=paste0("surface_3_pred","_",ii)
  false_positive[ii]=length(which(surface_3_pred$pred_match==1))
  false_negative[ii]=length(which(surface_3_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(surface_3_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(surface_3_pred))*100
  write.xlsx(surface_3_pred,"surface_3_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    pars=cbind(par_A,par_B,par_D,par_E,par_G,par_H,par_K,par_L,Optim_val_of_likeld)
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"surface_3_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
    write.xlsx(pars,"surface_3_pred_new.xlsx",sheetName = "Parameters",append = T,row.names = F)
    write.xlsx(cut_off_prob,"surface_3_pred_new.xlsx",sheetName = "cut_off_probalility",append = T,row.names = F)
    
  }
}



## line 1 dip
for(ii in 1:10){
  library(xlsx)
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    A=seq(1,3,length.out = 5000)
    B=seq(0.001,2,length.out = 5000)
    D=seq(0.00001,1,length.out = 5000)
    E=seq(0.000001,1.5,length.out = 5000)
    G=seq(0.000001,1.5,length.out = 5000)
    H=seq(0.000001,1.5,length.out = 5000)
    K=seq(0.000000001,0.01,length.out = 5000)
    L=seq(0.000000001,0.01,length.out = 5000)
    Optim_val_of_likeld=c()
    par_A=c()
    par_B=c()
    par_D=c()
    par_E=c()
    par_G=c()
    par_H=c()
    par_K=c()
    par_L=c()
  }
  length_uni_mile_dip_1_0=length(levels(as.factor(train_dip_1_0_no_day_no_single$MILEPOST)))  ##119
  dip_sample_pos_1_0=sample(1:length_uni_mile_dip_1_0,round(length_uni_mile_dip_1_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  dip_uniq_pos_1_0=levels(as.factor(train_dip_1_0_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_1_0=dip_uniq_pos_1_0[-dip_sample_pos_1_0]
  dip_uniq_pos_test_1_0=dip_uniq_pos_1_0[dip_sample_pos_1_0]
  train_dip_1_0_no_day_no_single_trn=subset(train_dip_1_0_no_day_no_single,train_dip_1_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_1_0)
  train_dip_1_0_no_day_no_single_tst=subset(train_dip_1_0_no_day_no_single,train_dip_1_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_1_0) 
  train_data_dip_1=rbind(train_dip_1_0_no_day_no_single_trn)
  test_data_dip_1=rbind(train_dip_1_0_no_day_no_single_tst)
  train_data_dip_1_mile_changed=mile_post_change_for_2nd_method(train_data_dip_1)
  train_data_dip_1_cum_amptd=cum_amptd_time_1(train_data_dip_1_mile_changed,inspect_data)
  train_data_dip_1_final=add_train_tonnage(train_data_dip_1_cum_amptd,tonnage_combine_all_years)
  A_sample_dip_1=sample(A,500,replace = T)
  B_sample_dip_1=sample(B,500,replace = T)
  D_sample_dip_1=sample(D,500,replace = T)
  E_sample_dip_1=sample(E,500,replace = T)
  G_sample_dip_1=sample(G,500,replace = T)
  H_sample_dip_1=sample(H,500,replace = T)
  K_sample_dip_1=sample(K,500,replace = T)
  L_sample_dip_1=sample(L,500,replace = T)
  par_sample_dip_1=data.frame(A_sample_dip_1,B_sample_dip_1,D_sample_dip_1,E_sample_dip_1,G_sample_dip_1,H_sample_dip_1,K_sample_dip_1,L_sample_dip_1)
  lik_dip_1=lik_value_1(par_sample_dip_1, train_data_dip_1_final)
  min_rw_dip_1=which(lik_dip_1$lik_val==min(lik_dip_1$lik_val))
  
  opt_dip_1=optim(c(par_sample_dip_1[min_rw_dip_1,1],par_sample_dip_1[min_rw_dip_1,2],par_sample_dip_1[min_rw_dip_1,3],par_sample_dip_1[min_rw_dip_1,4],par_sample_dip_1[min_rw_dip_1,5], par_sample_dip_1[min_rw_dip_1,6],par_sample_dip_1[min_rw_dip_1,7],par_sample_dip_1[min_rw_dip_1,8]),our_likelihood_4,y=train_data_dip_1_final,control = c(maxit=5000))
  par_A=c(par_A,opt_dip_1$par[1])
  par_B=c(par_B,opt_dip_1$par[2])
  par_D=c(par_D,opt_dip_1$par[3])
  par_E=c(par_E,opt_dip_1$par[4])
  par_G=c(par_G,opt_dip_1$par[5])
  par_H=c(par_H,opt_dip_1$par[6])
  par_K=c(par_K,opt_dip_1$par[7])
  par_L=c(par_L,opt_dip_1$par[8])
  Optim_val_of_likeld=c(Optim_val_of_likeld,opt_dip_1$value)
  prob_gamma_dip_1=function(x){
    result=c()
    A=opt_dip_1$par[1]
    B=opt_dip_1$par[2]
    D=opt_dip_1$par[3]
    E=opt_dip_1$par[4]
    G=opt_dip_1$par[5]
    H=opt_dip_1$par[6]
    K=opt_dip_1$par[7]
    L=opt_dip_1$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_dip_1_ampltd_dif=ampltd_difference(test_data_dip_1,tonnage_combine_all_years,inspect_data)
  test_data_dip_1_inspect_0=subset(test_data_dip_1_ampltd_dif,test_data_dip_1_ampltd_dif$inspect_count==0)
  test_data_dip_1_threshold_add=add_threshold(test_data_dip_1_inspect_0,tag_limits)
  test_data_dip_1_valid=subset(test_data_dip_1_threshold_add,test_data_dip_1_threshold_add$defect_amp_diff>0 & test_data_dip_1_threshold_add$AMPLTD_NEED>0 & test_data_dip_1_threshold_add$day_count>7)
  
  prob_1_dip=prob_gamma_dip_1(test_data_dip_1_valid)
  dip_1_pred=prediction(prob_1_dip,test_data_dip_1_valid)
  match[ii]=length(which(dip_1_pred$pred_match==0)) 
  predt_accrcy[ii]=(match[ii]/nrow(dip_1_pred))*100
  name_1=paste0("dip_1_pred","_",ii)
  false_positive[ii]=length(which(dip_1_pred$pred_match==1))
  false_negative[ii]=length(which(dip_1_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(dip_1_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(dip_1_pred))*100
  write.xlsx(dip_1_pred,"dip_1_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    pars=cbind(par_A,par_B,par_D,par_E,par_G,par_H,par_K,par_L,Optim_val_of_likeld)
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"dip_1_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
    write.xlsx(pars,"dip_1_pred_new.xlsx",sheetName = "Parameters",append = T,row.names = F)
  }
}

## line 2 dip

for(ii in 1:10){
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    A=seq(1,3,length.out = 5000)
    B=seq(0.001,2,length.out = 5000)
    D=seq(0.00001,1,length.out = 5000)
    E=seq(0.000001,1.5,length.out = 5000)
    G=seq(0.000001,1.5,length.out = 5000)
    H=seq(0.000001,1.5,length.out = 5000)
    K=seq(0.000000001,0.01,length.out = 5000)
    L=seq(0.000000001,0.01,length.out = 5000)
    Optim_val_of_likeld=c()
    par_A=c()
    par_B=c()
    par_D=c()
    par_E=c()
    par_G=c()
    par_H=c()
    par_K=c()
    par_L=c()
  }
  length_uni_mile_dip_2_0=length(levels(as.factor(train_dip_2_0_no_day_no_single$MILEPOST)))  ##22
  dip_sample_pos_2_0=sample(1:length_uni_mile_dip2_0,round(length_uni_mile_dip2_0*0.20,digits = 0))
  dip_uniq_pos_2_0=levels(as.factor(train_dip_2_0_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_2_0=dip_uniq_pos_2_0[-dip_sample_pos_2_0]
  dip_uniq_pos_test_2_0=dip_uniq_pos_2_0[dip_sample_pos_2_0]
  train_dip_2_0_no_day_no_single_trn=subset(train_dip_2_0_no_day_no_single,train_dip_2_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_2_0)
  train_dip_2_0_no_day_no_single_tst=subset(train_dip_2_0_no_day_no_single,train_dip_2_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_2_0)
  
  
  length_uni_mile_dip2_1=length(levels(as.factor(train_dip_2_1_no_day_no_single$MILEPOST)))
  dip_sample_pos_2_1=sample(1:length_uni_mile_dip2_1,round(length_uni_mile_dip2_1*0.20,digits = 0))
  dip_uniq_pos_2_1=levels(as.factor(train_dip_2_1_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_2_1=dip_uniq_pos_2_1[-dip_sample_pos_2_1]
  dip_uniq_pos_test_2_1=dip_uniq_pos_2_1[dip_sample_pos_2_1]
  train_dip_2_1_no_day_no_single_trn=subset(train_dip_2_1_no_day_no_single,train_dip_2_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_2_1)
  train_dip_2_1_no_day_no_single_tst=subset(train_dip_2_1_no_day_no_single,train_dip_2_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_2_1)
  train_data_dip_2=rbind(train_dip_2_0_no_day_no_single_trn,train_dip_2_1_no_day_no_single_trn)
  test_data_dip_2=rbind(train_dip_2_0_no_day_no_single_tst,train_dip_2_1_no_day_no_single_tst)
  train_data_dip_2_mile_changed=mile_post_change_for_2nd_method(train_data_dip_2)
  train_data_dip_2_cum_amptd=cum_amptd_time_1(train_data_dip_2_mile_changed,inspect_data)
  train_data_dip_2_final=add_train_tonnage(train_data_dip_2_cum_amptd,tonnage_combine_all_years)
  
  
  A_sample_dip_2=sample(A,500,replace = T)
  B_sample_dip_2=sample(B,500,replace = T)
  D_sample_dip_2=sample(D,500,replace = T)
  E_sample_dip_2=sample(E,500,replace = T)
  G_sample_dip_2=sample(G,500,replace = T)
  H_sample_dip_2=sample(H,500,replace = T)
  K_sample_dip_2=sample(K,500,replace = T)
  L_sample_dip_2=sample(L,500,replace = T)
  par_sample_dip_2=data.frame(A_sample_dip_2,B_sample_dip_2,D_sample_dip_2,E_sample_dip_2,G_sample_dip_2,H_sample_dip_2,K_sample_dip_2,L_sample_dip_2)
  lik_dip_2=lik_value_1(par_sample_dip_2, train_data_dip_2_final)
  min_rw_dip_2=which(lik_dip_2$lik_val==min(lik_dip_2$lik_val))
  
  opt_dip_2=optim(c(par_sample_dip_2[min_rw_dip_2,1],par_sample_dip_2[min_rw_dip_2,2],par_sample_dip_2[min_rw_dip_2,3],par_sample_dip_2[min_rw_dip_2,4],par_sample_dip_2[min_rw_dip_2,5], par_sample_dip_2[min_rw_dip_2,6],par_sample_dip_2[min_rw_dip_2,7],par_sample_dip_2[min_rw_dip_2,8]),our_likelihood_4,y=train_data_dip_2_final,control = c(maxit=5000))
  par_A=c(par_A,opt_dip_2$par[1])
  par_B=c(par_B,opt_dip_2$par[2])
  par_D=c(par_D,opt_dip_2$par[3])
  par_E=c(par_E,opt_dip_2$par[4])
  par_G=c(par_G,opt_dip_2$par[5])
  par_H=c(par_H,opt_dip_2$par[6])
  par_K=c(par_K,opt_dip_2$par[7])
  par_L=c(par_L,opt_dip_2$par[8])
  Optim_val_of_likeld=c(Optim_val_of_likeld,opt_dip_2$value)
  prob_gamma_dip_2=function(x){
    result=c()
    A=opt_dip_2$par[1]
    B=opt_dip_2$par[2]
    D=opt_dip_2$par[3]
    E=opt_dip_2$par[4]
    G=opt_dip_2$par[5]
    H=opt_dip_2$par[6]
    K=opt_dip_2$par[7]
    L=opt_dip_2$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_dip_2_ampltd_dif=ampltd_difference(test_data_dip_2,tonnage_combine_all_years,inspect_data)
  test_data_dip_2_inspect_0=subset(test_data_dip_2_ampltd_dif,test_data_dip_2_ampltd_dif$inspect_count==0)
  test_data_dip_2_threshold_add=add_threshold(test_data_dip_2_inspect_0,tag_limits)
  test_data_dip_2_valid=subset(test_data_dip_2_threshold_add,test_data_dip_2_threshold_add$defect_amp_diff>0 & test_data_dip_2_threshold_add$AMPLTD_NEED>0 & test_data_dip_2_threshold_add$day_count>7)
  
  prob_1_dip=prob_gamma_dip_2(test_data_dip_2_valid)
  dip_2_pred=prediction(prob_1_dip,test_data_dip_2_valid)
  match[ii]=length(which(dip_2_pred$pred_match==0)) 
  predt_accrcy[ii]=(match[ii]/nrow(dip_2_pred))*100
  name_1=paste0("dip_2_pred","_",ii)
  false_positive[ii]=length(which(dip_2_pred$pred_match==1))
  false_negative[ii]=length(which(dip_2_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(dip_2_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(dip_2_pred))*100
  write.xlsx(dip_2_pred,"dip_2_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    pars=cbind(par_A,par_B,par_D,par_E,par_G,par_H,par_K,par_L,Optim_val_of_likeld)
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"dip_2_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
    write.xlsx(pars,"dip_2_pred_new.xlsx",sheetName = "Parameters",append = T,row.names = F)
  }
}

## Line 3 dip

for(ii in 1:10){
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    A=seq(1,3,length.out = 5000)
    B=seq(0.001,2,length.out = 5000)
    D=seq(0.00001,1,length.out = 5000)
    E=seq(0.000001,1.5,length.out = 5000)
    G=seq(0.000001,1.5,length.out = 5000)
    H=seq(0.000001,1.5,length.out = 5000)
    K=seq(0.000000001,0.01,length.out = 5000)
    L=seq(0.000000001,0.01,length.out = 5000)
    Optim_val_of_likeld=c()
    par_A=c()
    par_B=c()
    par_D=c()
    par_E=c()
    par_G=c()
    par_H=c()
    par_K=c()
    par_L=c()
  }
  length_uni_mile_dip_3_1=length(levels(as.factor(train_dip_3_1_no_day_no_single$MILEPOST)))
  dip_sample_pos_3_1=sample(1:length_uni_mile_dip_3_1,round(length_uni_mile_dip_3_1*0.20,digits = 0))
  dip_uniq_pos_3_1=levels(as.factor(train_dip_3_1_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_3_1=dip_uniq_pos_3_1[-dip_sample_pos_3_1]
  dip_uniq_pos_test_3_1=dip_uniq_pos_3_1[dip_sample_pos_3_1]
  train_dip_3_1_no_day_no_single_trn=subset(train_dip_3_1_no_day_no_single,train_dip_3_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_3_1)
  train_dip_3_1_no_day_no_single_tst=subset(train_dip_3_1_no_day_no_single,train_dip_3_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_3_1)
  length_uni_mile_dip_3_2=length(levels(as.factor(train_dip_3_2_no_day_no_single$MILEPOST)))  
  dip_sample_pos_3_2=sample(1:length_uni_mile_dip_3_2,round(length_uni_mile_dip_3_2*0.20,digits = 0))  
  dip_uniq_pos_3_2=levels(as.factor(train_dip_3_2_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_3_2=dip_uniq_pos_3_2[-dip_sample_pos_3_2]
  dip_uniq_pos_test_3_2=dip_uniq_pos_3_2[dip_sample_pos_3_2]
  train_dip_3_2_no_day_no_single_trn=subset(train_dip_3_2_no_day_no_single,train_dip_3_2_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_3_2)
  train_dip_3_2_no_day_no_single_tst=subset(train_dip_3_2_no_day_no_single,train_dip_3_2_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_3_2)
  train_data_dip_3=rbind(train_dip_3_1_no_day_no_single_trn,train_dip_3_2_no_day_no_single_trn)
  test_data_dip_3=rbind(train_dip_3_1_no_day_no_single_tst,train_dip_3_2_no_day_no_single_tst)
  train_data_dip_3_mile_changed=mile_post_change_for_2nd_method(train_data_dip_3)
  train_data_dip_3_cum_amptd=cum_amptd_time_1(train_data_dip_3_mile_changed,inspect_data)
  train_data_dip_3_final=add_train_tonnage(train_data_dip_3_cum_amptd,tonnage_combine_all_years)
  
  
  A_sample_dip_3=sample(A,500,replace = T)
  B_sample_dip_3=sample(B,500,replace = T)
  D_sample_dip_3=sample(D,500,replace = T)
  E_sample_dip_3=sample(E,500,replace = T)
  G_sample_dip_3=sample(G,500,replace = T)
  H_sample_dip_3=sample(H,500,replace = T)
  K_sample_dip_3=sample(K,500,replace = T)
  L_sample_dip_3=sample(L,500,replace = T)
  par_sample_dip_3=data.frame(A_sample_dip_3,B_sample_dip_3,D_sample_dip_3,E_sample_dip_3,G_sample_dip_3,H_sample_dip_3,K_sample_dip_3,L_sample_dip_3)
  lik_dip_3=lik_value_1(par_sample_dip_3, train_data_dip_3_final)
  min_rw_dip_3=which(lik_dip_3$lik_val==min(lik_dip_3$lik_val))
  
  opt_dip_3=optim(c(par_sample_dip_3[min_rw_dip_3,1],par_sample_dip_3[min_rw_dip_3,2],par_sample_dip_3[min_rw_dip_3,3],par_sample_dip_3[min_rw_dip_3,4],par_sample_dip_3[min_rw_dip_3,5], par_sample_dip_3[min_rw_dip_3,6],par_sample_dip_3[min_rw_dip_3,7],par_sample_dip_3[min_rw_dip_3,8]),our_likelihood_4,y=train_data_dip_3_final,control = c(maxit=5000))
  par_A=c(par_A,opt_dip_3$par[1])
  par_B=c(par_B,opt_dip_3$par[2])
  par_D=c(par_D,opt_dip_3$par[3])
  par_E=c(par_E,opt_dip_3$par[4])
  par_G=c(par_G,opt_dip_3$par[5])
  par_H=c(par_H,opt_dip_3$par[6])
  par_K=c(par_K,opt_dip_3$par[7])
  par_L=c(par_L,opt_dip_3$par[8])
  Optim_val_of_likeld=c(Optim_val_of_likeld,opt_dip_3$value)
  prob_gamma_dip_3=function(x){
    result=c()
    A=opt_dip_3$par[1]
    B=opt_dip_3$par[2]
    D=opt_dip_3$par[3]
    E=opt_dip_3$par[4]
    G=opt_dip_3$par[5]
    H=opt_dip_3$par[6]
    K=opt_dip_3$par[7]
    L=opt_dip_3$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_dip_3_ampltd_dif=ampltd_difference(test_data_dip_3,tonnage_combine_all_years,inspect_data)
  test_data_dip_3_inspect_0=subset(test_data_dip_3_ampltd_dif,test_data_dip_3_ampltd_dif$inspect_count==0)
  test_data_dip_3_threshold_add=add_threshold(test_data_dip_3_inspect_0,tag_limits)
  test_data_dip_3_valid=subset(test_data_dip_3_threshold_add,test_data_dip_3_threshold_add$defect_amp_diff>0 & test_data_dip_3_threshold_add$AMPLTD_NEED>0 & test_data_dip_3_threshold_add$day_count>7)
  
  prob_1_dip=prob_gamma_dip_3(test_data_dip_3_valid)
  dip_3_pred=prediction(prob_1_dip,test_data_dip_3_valid)
  match[ii]=length(which(dip_3_pred$pred_match==0)) 
  predt_accrcy[ii]=(match[ii]/nrow(dip_3_pred))*100
  name_1=paste0("dip_3_pred","_",ii)
  false_positive[ii]=length(which(dip_3_pred$pred_match==1))
  false_negative[ii]=length(which(dip_3_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(dip_3_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(dip_3_pred))*100
  write.xlsx(dip_3_pred,"dip_3_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    pars=cbind(par_A,par_B,par_D,par_E,par_G,par_H,par_K,par_L,Optim_val_of_likeld)
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"dip_3_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
    write.xlsx(pars,"dip_3_pred_new.xlsx",sheetName = "Parameters",append = T,row.names = F)
  }
}


## Line 4 dip

for(ii in 1:10){
  library(xlsx)
  if(ii==1)
  {
    library(xlsx)
    match=c()
    predt_accrcy=c()
    false_positive=c()
    false_negative=c()
    false_positive_per=c()
    false_negative_per=c()
    A=seq(1,3,length.out = 5000)
    B=seq(0.001,2,length.out = 5000)
    D=seq(0.00001,1,length.out = 5000)
    E=seq(0.000001,1.5,length.out = 5000)
    G=seq(0.000001,1.5,length.out = 5000)
    H=seq(0.000001,1.5,length.out = 5000)
    K=seq(0.000000001,0.01,length.out = 5000)
    L=seq(0.000000001,0.01,length.out = 5000)
    Optim_val_of_likeld=c()
    par_A=c()
    par_B=c()
    par_D=c()
    par_E=c()
    par_G=c()
    par_H=c()
    par_K=c()
    par_L=c()
  }
  length_uni_mile_dip_4_0=length(levels(as.factor(train_dip_4_0_no_day_no_single$MILEPOST)))  ##119
  dip_sample_pos_4_0=sample(1:length_uni_mile_dip_4_0,round(length_uni_mile_dip_4_0*0.20,digits = 0)) # SRSWOR sample of size 86 from 429 
  dip_uniq_pos_4_0=levels(as.factor(train_dip_4_0_no_day_no_single$MILEPOST))
  dip_uniq_pos_train_4_0=dip_uniq_pos_4_0[-dip_sample_pos_4_0]
  dip_uniq_pos_test_4_0=dip_uniq_pos_4_0[dip_sample_pos_4_0]
  train_dip_4_0_no_day_no_single_trn=subset(train_dip_4_0_no_day_no_single,train_dip_4_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_4_0)
  train_dip_4_0_no_day_no_single_tst=subset(train_dip_4_0_no_day_no_single,train_dip_4_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_4_0) 
  train_data_dip_4=rbind(train_dip_4_0_no_day_no_single_trn)
  test_data_dip_4=rbind(train_dip_4_0_no_day_no_single_tst)
  train_data_dip_4_mile_changed=mile_post_change_for_2nd_method(train_data_dip_4)
  train_data_dip_4_cum_amptd=cum_amptd_time_1(train_data_dip_4_mile_changed,inspect_data)
  train_data_dip_4_final=add_train_tonnage(train_data_dip_4_cum_amptd,tonnage_combine_all_years)
  A_sample_dip_4=sample(A,500,replace = T)
  B_sample_dip_4=sample(B,500,replace = T)
  D_sample_dip_4=sample(D,500,replace = T)
  E_sample_dip_4=sample(E,500,replace = T)
  G_sample_dip_4=sample(G,500,replace = T)
  H_sample_dip_4=sample(H,500,replace = T)
  K_sample_dip_4=sample(K,500,replace = T)
  L_sample_dip_4=sample(L,500,replace = T)
  par_sample_dip_4=data.frame(A_sample_dip_4,B_sample_dip_4,D_sample_dip_4,E_sample_dip_4,G_sample_dip_4,H_sample_dip_4,K_sample_dip_4,L_sample_dip_4)
  lik_dip_4=lik_value_1(par_sample_dip_4, train_data_dip_4_final)
  min_rw_dip_4=which(lik_dip_4$lik_val==min(lik_dip_4$lik_val))
  
  opt_dip_4=optim(c(par_sample_dip_4[min_rw_dip_4,1],par_sample_dip_4[min_rw_dip_4,2],par_sample_dip_4[min_rw_dip_4,3],par_sample_dip_4[min_rw_dip_4,4],par_sample_dip_4[min_rw_dip_4,5], par_sample_dip_4[min_rw_dip_4,6],par_sample_dip_4[min_rw_dip_4,7],par_sample_dip_4[min_rw_dip_4,8]),our_likelihood_4,y=train_data_dip_4_final,control = c(maxit=5000))
  par_A=c(par_A,opt_dip_4$par[1])
  par_B=c(par_B,opt_dip_4$par[2])
  par_D=c(par_D,opt_dip_4$par[3])
  par_E=c(par_E,opt_dip_4$par[4])
  par_G=c(par_G,opt_dip_4$par[5])
  par_H=c(par_H,opt_dip_4$par[6])
  par_K=c(par_K,opt_dip_4$par[7])
  par_L=c(par_L,opt_dip_4$par[8])
  Optim_val_of_likeld=c(Optim_val_of_likeld,opt_dip_4$value)
  prob_gamma_dip_4=function(x){
    result=c()
    A=opt_dip_4$par[1]
    B=opt_dip_4$par[2]
    D=opt_dip_4$par[3]
    E=opt_dip_4$par[4]
    G=opt_dip_4$par[5]
    H=opt_dip_4$par[6]
    K=opt_dip_4$par[7]
    L=opt_dip_4$par[8]
    scale=A
    for(j in 1:nrow(x)){
      shape=(B)^2*((x$day_count[j]/62)^((D^2)*abs(x$DEF_AMP_1[j])+(E^2)*x$LENGTH_1[j]+(G^2)*as.numeric(x$TSC_CD[j])+(H^2)*x$CLASS[j]+(K^2)*x$tonnage_passed[j]+(L^2)*(x$trains_passed[j]/1000)))
      result[j]=round((1-pgamma(x$AMPLTD_NEED[j],scale =1/scale,shape = shape)),digits = 2)
    }
    print(result)
    return(result)
  }
  ################# prepration of test data
  test_data_dip_4_ampltd_dif=ampltd_difference(test_data_dip_4,tonnage_combine_all_years,inspect_data)
  test_data_dip_4_inspect_0=subset(test_data_dip_4_ampltd_dif,test_data_dip_4_ampltd_dif$inspect_count==0)
  test_data_dip_4_threshold_add=add_threshold(test_data_dip_4_inspect_0,tag_limits)
  test_data_dip_4_valid=subset(test_data_dip_4_threshold_add,test_data_dip_4_threshold_add$defect_amp_diff>0 & test_data_dip_4_threshold_add$AMPLTD_NEED>0 & test_data_dip_4_threshold_add$day_count>7)
  
  prob_4_dip=prob_gamma_dip_4(test_data_dip_4_valid)
  dip_4_pred=prediction(prob_4_dip,test_data_dip_4_valid)
  match[ii]=length(which(dip_4_pred$pred_match==0)) 
  predt_accrcy[ii]=(match[ii]/nrow(dip_4_pred))*100
  name_1=paste0("dip_4_pred","_",ii)
  false_positive[ii]=length(which(dip_4_pred$pred_match==1))
  false_negative[ii]=length(which(dip_4_pred$pred_match==(-1)))
  false_positive_per[ii]=(false_positive[ii]/nrow(dip_4_pred))*100
  false_negative_per[ii]=(false_negative[ii]/nrow(dip_4_pred))*100
  write.xlsx(dip_4_pred,"dip_4_pred_new.xlsx",sheetName = name_1,append = T,row.names = F)
  if(ii==10){
    pars=cbind(par_A,par_B,par_D,par_E,par_G,par_H,par_K,par_L,Optim_val_of_likeld)
    sumary=cbind(match,predt_accrcy,false_positive,false_negative,false_positive_per,false_negative_per)
    write.xlsx(sumary,"dip_4_pred_new.xlsx",sheetName = "summary",append = T,row.names = F)
    write.xlsx(pars,"dip_4_pred_new.xlsx",sheetName = "Parameters",append = T,row.names = F)
  }
}

