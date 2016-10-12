training=read.csv('training.csv')
tonnage=read.csv('tonnage.csv')
inspect_data=read.csv('Inspection.csv')

#$$$$$$$$$$$$$$ first we will look at the tonnage data
summary(tonnage)
# Year as we can see that minimum year is 2007 and maximum is 2014 whoch is the case in the data also so looks good
# Month  As we can verify that there is no problem with the month data
# line segment number is also looking good as its minimum is 1 and max is 4 which we can verify from the data
# MILe_post_start_and_end also looks good and we can verify that min mile_post_start is less that min mile_post_end  and the same can be assured in the case of max also which should be the case
# TOT_CAR_EAST_&_TOT_CAR_WEST_&_TOT_TRN_EAST_&_TOT_TRN_WEST In these 4 columns we can observe that in all the columns the minimum is zero and also the 1st quartile is 0 which says that 
# atleast 25% data has zero entries and we can see that for TOT_DFLT_MGT we have minimum 0.000206 and 1st quartile as 3.720816 which shows us that there is something wrong with these columns
# so we would like to see those columns

temp_df=tonnage[,c(1,2,7:11)]
View(temp_df)
temp_df=temp_df[order(temp_df$YEAR,temp_df$MONTH),]
View(temp_df)
# here we can see that in year 2007 and 2008 all the data on TOT_CAR_EAST_&_TOT_CAR_WEST_&_TOT_TRN_EAST_&_TOT_TRN_WEST is zero i.e. there is no movement in this period but we can observe that we are given data on 
##TOT_DFLT_MGT for year 2007 and 2008 which clearly says that there weight had passed without passing any train which is not correct so we will treat these values as missing values
## we can also see that in year 2009 data on TOT_CAR_EAST_&_TOT_CAR_WEST is missing upto month of april so we will take these values as missing values. We will try to imput these values using some techniques.

# now we will collect those data ponit on which we have no zeros
temp_df=subset(temp_df,temp_df$TOT_CAR_EAST!=0 & temp_df$TOT_CAR_WEST!=0 & temp_df$TOT_TRN_EAST!=0 & temp_df$TOT_TRN_WEST!=0)
View(temp_df)
dim(temp_df) ## 25356  7 which shows that 25356 data points have all the values 
cor(temp_df[,3:7])
## here in the output we can see hogh correlation between TOT_CAR_EAST and TOT_CAR_WEST but we can see in the data that there are too much occurances where both TOT_CAR_EAST and TOT_CAR_WEST are simultaneously zeros
## So we can not use this as pediction criteria
## Again we can see that almost 0.5 correlation coefficient in TOT_DFLT_MGT with TOT_TRN_EAST and TOT_TRN_WEST which may be useful
### But I want to make sure that these all are linear relationship 
#### As a guess I want to see the correlation of TOT_TRN_EAST+TOT_TRN_WEST with TOT_DFLT_MGT
temp_df=data.frame(temp_df,total_train=temp_df$TOT_TRN_EAST+temp_df$TOT_TRN_WEST)
View(temp_df)
cor(temp_df$TOT_DFLT_MGT,temp_df$total_train)
# 0.9148062 which shows a high linear relationship in between TOT_DFLT_MGT and total_train so we can use this fact in our missing value imputation method
## so here we can decide that instead of using TOT_TRN_EAST,TOT_TRN_WEST,TOT_CAR_EAST and TOT_CAR_WEST as independent variables we will use only one variable
### total_train as it is not so bad as we have used the information on TOT_TRN_EAST and TOT_TRN_WEST and we know that TOT_TRN and TOT_CAR are dependent 
#### As the TOT_CAR is the number of waigens however TOT_TRN is the number of trains i.e. on TARIN contains too many cars
plot(temp_df$TOT_DFLT_MGT,temp_df$total_train)
## Here in the plot we can observe that there are some outlairs thay are 4 in count
### We awnt to remove them 
temp_df=subset(temp_df,temp_df$TOT_DFLT_MGT<20)
plot(temp_df$TOT_DFLT_MGT,temp_df$total_train) ## Yes outlairs removed Party.....Abhi to ...
# Now I would like to see that is there any values for which are total trains is low but tonnage is extremly high
temp_df_2=subset(temp_df,temp_df$TOT_TRN_EAST<20 & temp_df$TOT_TRN_WEST<20)
View(temp_df_2)
plot(temp_df_2$TOT_DFLT_MGT,temp_df_2$TOT_TRN_EAST+temp_df_2$TOT_TRN_WEST)
# in the plot we can see that there are no outlier
temp_df_1=subset(tonnage,tonnage$TOT_CAR_EAST==0| tonnage$TOT_CAR_WEST==0 | tonnage$TOT_TRN_EAST==0 | tonnage$TOT_TRN_WEST==0)
View(temp_df_1) ## 11462 entries have atleast one 0 in thier last five columns
## This temp_df_1 is complement of temp_df is we mix them then we will get the whole data
# so we have to predict the values of total train for given TOT_DFLT_MGT in temp_df_1
### So we will fit a linear regression model in by taking total train as depeandent variable and TOT_DFLT_MGT as indepandent variable 
fit=lm(temp_df$total_train~temp_df$TOT_DFLT_MGT)
summary(fit)
## In summary we can see that we are getting Adjusted R-squared as 0.8515 meaning that we are able to capture 85% of variation in the data
### In the summary we can see that intercept term is 96.722 meaning that our model will give atleast 96 total train even if we have no trains are moving
### Form here we got an inspiration that we can devide the whole data into two parts and after that use regression line
temp_df_4=subset(temp_df,temp_df$total_train<100)
View(temp_df_4)
plot(temp_df_4$total_train,temp_df_4$TOT_DFLT_MGT)
##in the plot we can observe that there is no linear trend for total_train <100 
##In the previous we were seeing the data collectively which was looking like a linear trend so we will not use this as a prediction rule 
### We are interested in behaviour for total_train>=100
temp_df_5=subset(temp_df,temp_df$total_train>=100)
View(temp_df_5)
plot(temp_df_5$total_train,temp_df_5$TOT_DFLT_MGT)
# we can see the linear pattern but which is not actually linear it collectively looks like linear
## SO here one question comes into our mind is this data is time series data so we want to check that aspect also
### I would like to see the behaviour of data at atleast 3-4 points
#### All these are done in the below section

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
temp_df_1=subset(tonnage,tonnage$TOT_CAR_EAST==0| tonnage$TOT_CAR_WEST==0 | tonnage$TOT_TRN_EAST==0 | tonnage$TOT_TRN_WEST==0)
View(temp_df_1) ## 11462 entries have atleast one 0 in thier last five columns
temp_df_1=temp_df_1[order(temp_df_1$YEAR, temp_df_1$MONTH),]
View(temp_df_1) ## 11462 entries have atleast one 0 in thier last five columns
## In the data we can see that there is missing values at line segment 1 tarck 0 milepost start 61.344 and milepost end 72.788
### So we would like to gather information on line segment 1 tarck 0 milepost start 61.344 and milepost end 72.788
data_1_0_28.9_32.0=tonnage_line_track_mileStart_year_month[which(tonnage_line_track_mileStart_year_month$MILEPOST_START==28.9 & tonnage_line_track_mileStart_year_month$MILEPOST_END==32.0 & tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==1 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0),]
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



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
ts_1_0_28.9_32.0_pred=ts(ceiling(na.spline(ts_1_0_28.9_32.0)),frequency = 12,start=c(2009))
ts_1_0_28.9_32.0_pred
# hEre we can see that we are getting predicted values as 3 which looks like reasonable as it is giving small values as well

# Now we will do the same process for the missing value at the other position
tonnage_line_track_mileStart_year_month[d6[2],] ## can see that there is missing value at starting MilePost= 28.9
data_1_0_32_40.7=tonnage_line_track_mileStart_year_month[which(tonnage_line_track_mileStart_year_month$MILEPOST_START==32 & tonnage_line_track_mileStart_year_month$MILEPOST_END==40.7 & tonnage_line_track_mileStart_year_month$LINE_SEG_NBR==1 & tonnage_line_track_mileStart_year_month$TRACK_SDTK_NBR==0),]
data_1_0_32_40.7
ts_1_0_32_40.7=ts(data_1_0_32_40.7$total_trains_east_west,frequency = 12,start = c(2009))
ts_1_0_32_40.7
plot.ts(ts_1_0_32_40.7)
na.spline(ts_1_0_32_40.7)
ts_1_0_32_40.7_pred=ts(ceiling(na.spline(ts_1_0_32_40.7)),frequency = 12,start = 2009)
ts_1_0_32_40.7_pred
plot.ts(ts_1_0_32_40.7_pred)
## As here we can see that we are getting exceptionally hogh value at 2009 JAn as it is greater than the maximum of the series
### But other values are looking good so we will use this method in predicting missing values.
## So we have to take care of these situation in making the code 
## These high values will obviously come as here we are extraploating the values and one more reason is that as we are using na.spline 
## This function makes a cubic curve taking 4 avalable values and after that using this curve predict the missing values
## So taking all these care we have made a function which will take care of all these problems


## I have made a function which will take a data frame as its argument and returns a dataframe which have interpolated values of total trains
## As In this we have taken care of all the things but we will have to do manually at some positions as if only 1 yr data is available then this will put all the nas= mean of the data of that year 
## Which is not correct as we cant interpolate 4/5 year data based on one year data and taking its mean only so we will manually make them as NA and may use regression Analysis to find out these NAs
## As We have so many spots where we have all the data missing and these were creating problem in running our function na.spline so we have removed those position so that our function can run without any error
### Actually these are problem of data we can verify them as obsering them
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
write.csv(tonnage_combine_all_years,"tonnage_missing_handled.csv")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

##### Here i think Our Journey of finding missing data is completed and now we will do other works
# first of all we will divide the whole data set in 3 sub data frames according to Defect Type
train_surface=subset(training,training$DFCT_TYPE=="SURFACE")
dim(train_surface) ##7577 14
train_xlevel=subset(training,training$DFCT_TYPE=="XLEVEL")
dim(train_xlevel) ## 11909 14
train_dip=subset(training,training$DFCT_TYPE=="DIP")
dim(train_dip) ##4361 14








