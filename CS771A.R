training=read.csv('training.csv')
tonnage=read.csv('tonnage.csv')
inspect_data=read.csv('Inspection.csv')
testing_data=read.csv('testing.csv')

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
# Now I want to Sort my surface data by line segment number after that track after that mile post after that test date in order to find out the repeated deffect
# We can see that Test format provided is factor so in order to sort by that we have to convert the test variable we have to make it in Date format
train_surface$TEST_DT=as.Date(train_surface$TEST_DT,format="%d%b%Y")
train_surface=train_surface[order(train_surface$LINE_SEG_NBR,train_surface$TRACK_SDTK_NBR,train_surface$TEST_DT,train_surface$MILEPOST),]
View(train_surface)
# Now we want to see the summary of the data in order to see the types and minimum and maximum of the variables and also the possible errors
str(train_surface)
## Gives the structure of the data See this..
summary(train_surface)
###### Making given information of thresholds as a data frame
### These are the values which are given in the Question answers provided by INFORMS
class1=c(3,3,3)
class2=c(2.75,2.75,2)
class3=c(2.25,2.25,1.75)
class4=c(2,1.75,1.25)
class5=c(1.25,1.5,1)
tag_limits=cbind(class1,class2,class3,class4,class5)
row.names(tag_limits)=c("SURFACE","DIP","XLEVEL")
tag_limits
## We will use the tagLimits at later stage may be in prediction function
feet_mile=0.000189394 # the values of 1 feet in miles
## This function gives the unique defect positions by using the fact of repeated defects 200 ft wala
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
# FUNCTION FOR ASSIGNING UNIQUE DIFFERENT POSITIONS TO THE MILEPOSTS
## This function assign a unique defect position( find in the above function) to each observation of milepost of data
mile_post_uniq=function(x,y){
  m=0
  for(i in 1:nrow(x)){
    m=which(y<=x[i,2]+100*feet_mile & y>=x[i,2]-100*feet_mile)
    x[i,2]=y[m]
  }
  return(x)
} 
# Now we will sort the surface data by line segment number, track segment number and milepost
train_surface_line_track_milepost=train_surface[order(train_surface$LINE_SEG_NBR,train_surface$TRACK_SDTK_NBR,train_surface$MILEPOST),]


######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
## Some prepocessing (not Complete) For Line segment number 1 and track segment 0
# we will partition the train_surface data by line_seg=1 and track=0
train_surface_1_0=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==1 & train_surface_line_track_milepost$TRACK_SDTK_NBR==0)
# Now we will find the unique defect positions on line segment number 1 and track segment number 0
uniq_defect_pos_1_0=uniq_defect_position1(train_surface_1_0)
length(uniq_defect_pos_1_0) ##242
train_surface_1_0=mile_post_uniq(train_surface_1_0,uniq_defect_pos_1_0)
## There in the manual it is given that if the inspection is done within a 7 days period of the previous inspection then we will consider it as the same observation
## we will take care of this factor later
## Below is the code for removing the same day repeat as there are too many instances where they have reported a defect several timein a day itself
remove_same_day_repeat=function(x){
  d=c()
  m=c()
  m1=c()
  m2=c()
  m3=c()
  m4=0
  m5=c()
  m6=c()
  d=levels(as.factor(x$TEST_DT))
  for(i in 1:length(d)){
    m=0
    m1=0
    m2=0
    m3=0
    m4=0
    m5=0
    m6=0
    m=which(x[,4]==d[i])
    m1=levels(as.factor(x[m,2]))
    for(j in 1:length(m1)){
      m2=which(x[,2]==m1[j] & x[,4]==d[i])
      if(length(m2)>1){
        m3=which(abs(x[,9])==max(abs(x[m2,9])))
        m6=intersect(m3,m2)
        if(length(m6)>1){
          m4=max(x[m6[1],8])
          x[m6[1],8]=m4
          m5=m2[m2!=m6[1]]
          x=x[-m5,]
        }
        else{
          m4=max(x[m6,8])
          x[m6,8]=m4
          m5=m2[m2!=m6]
          x=x[-m5,]
        }
      }
    }
    
  }
  return(x)
}
train_surface_1_0_no_day=remove_same_day_repeat(train_surface_1_0) ## data which has no defetcs reported on the same day
## This function removes those data point which has only one observation means at that point is inspected only once
remove_single_obs=function(x){
  d=c()
  d=levels(as.factor(x$MILEPOST))
  for(i in 1:length(d)){
    m=c()
    m=which(x[,2]==d[i])
    if(length(m)==1){
      x=x[-m,]
    }
  }
  return(x)
}
train_surface_1_0_no_day_no_single=remove_single_obs(train_surface_1_0_no_day)
length(levels(as.factor(train_surface_1_0_no_day_no_single$MILEPOST)))  ##135
## here we have 135 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
## To partition the data into two part we have to partition the defect position just think about it ..only this way we can partition
## Here we want to make you clear that here We will partition the training data into two parts and name it as train data and test data and sometimes we will write test data as vcalidating data and now we have two test data one they have given and other we have made  so dont be confused
135*0.20 ## 27 unique defect positions will be in our test data and remaining 135-27 willbe in our train data
d=sample(1:135,27) # SRSWOR sample of size 27 from 135 
d1=levels(as.factor(train_surface_1_0_no_day_no_single$MILEPOST))
uniq_pos_train=d1[-d]
uniq_pos_test=d1[d]
## Now break the data into two parts train and test data
train_surface_1_0_no_day_no_single_trn=subset(train_surface_1_0_no_day_no_single,train_surface_1_0_no_day_no_single$MILEPOST %in% uniq_pos_train)
train_surface_1_0_no_day_no_single_tst=subset(train_surface_1_0_no_day_no_single,train_surface_1_0_no_day_no_single$MILEPOST %in% uniq_pos_test)
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
###### I think there are no data for defect type surface and Line 1 and track other than 0
### So we will move to line segment 2
## The same we will do for line segment number 2 and track segment 0
train_surface_2_0=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==2 & train_surface_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_2_0=uniq_defect_position1(train_surface_2_0)
length(uniq_defect_pos_2_0) ##462
train_surface_2_0=mile_post_uniq(train_surface_2_0,uniq_defect_pos_2_0)
train_surface_2_0_no_day=remove_same_day_repeat(train_surface_2_0)
train_surface_2_0_no_day_no_single=remove_single_obs(train_surface_2_0_no_day)
length(levels(as.factor(train_surface_2_0_no_day_no_single$MILEPOST)))  ##249
## here we have 135 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
249*0.20 ##49.8 =~50 unique defect positions will be in our test data and remaining 135-27 willbe in our train data
sample_pos_2_0=sample(1:249,50) # SRSWOR sample of size 27 from 135 
uniq_pos_2_0=levels(as.factor(train_surface_2_0_no_day_no_single$MILEPOST))
uniq_pos_train_2_0=uniq_pos_2_0[-sample_pos_2_0]
uniq_pos_test_2_0=uniq_pos_2_0[sample_pos_2_0]
train_surface_2_0_no_day_no_single_trn=subset(train_surface_2_0_no_day_no_single,train_surface_2_0_no_day_no_single$MILEPOST %in% uniq_pos_train_2_0)
train_surface_2_0_no_day_no_single_tst=subset(train_surface_2_0_no_day_no_single,train_surface_2_0_no_day_no_single$MILEPOST %in% uniq_pos_test_2_0)
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
# unique defect position for 2_1
any(train_surface$LINE_SEG_NBR==2 & train_surface$TRACK_SDTK_NBR==1) ## TRUE Showing that there are data satisfying the conditions
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
train_surface_2_1_no_day_no_single_tst=subset(train_surface_2_1_no_day_no_single,train_surface_2_1_no_day_no_single$MILEPOST %in% uniq_pos_test_2_1)
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
# unique defect position for 2_2
any(train_surface$LINE_SEG_NBR==2 & train_surface$TRACK_SDTK_NBR==2)
train_surface_2_2=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==2 & train_surface_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_2_2=uniq_defect_position1(train_surface_2_2)
length(uniq_defect_pos_2_2) ## 38
train_surface_2_2=mile_post_uniq(train_surface_2_2,uniq_defect_pos_2_2)
train_surface_2_2_no_day=remove_same_day_repeat(train_surface_2_2)
train_surface_2_2_no_day_no_single=remove_single_obs(train_surface_2_2_no_day)
length(levels(as.factor(train_surface_2_2_no_day_no_single$MILEPOST))) ## 10
10*0.20  # 2
sample_pos_2_2=sample(1:10,2) # SRSWOR sample of size 4 from 22 
uniq_pos_2_2=levels(as.factor(train_surface_2_2_no_day_no_single$MILEPOST))
uniq_pos_train_2_2=uniq_pos_2_2[-sample_pos_2_2]
uniq_pos_test_2_2=uniq_pos_2_2[sample_pos_2_2]
train_surface_2_2_no_day_no_single_trn=subset(train_surface_2_2_no_day_no_single,train_surface_2_2_no_day_no_single$MILEPOST %in% uniq_pos_train_2_2)
##### Make  test data set
train_surface_2_2_no_day_no_single_tst=subset(train_surface_2_2_no_day_no_single,train_surface_2_2_no_day_no_single$MILEPOST %in% uniq_pos_test_2_2)
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
# unique defect position for 3_0
any(train_surface$LINE_SEG_NBR==3 & train_surface$TRACK_SDTK_NBR==0) ## TRUE
length(which(train_surface$LINE_SEG_NBR==3 & train_surface$TRACK_SDTK_NBR==0)) ##1
### As here we have only one observation so we can not take it in our analysis
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************


## Something different part of preprocessing
# As the data is not available at some ponits so we had filled that data using the preceeding and successding rows
## Run these codes on caution as if you run the same code twice then it will creat problem and you ahve to do thing again from starting
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

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

# unique defect position for 3_1
any(train_surface$LINE_SEG_NBR==3 & train_surface$TRACK_SDTK_NBR==1)
train_surface_3_1=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==3 & train_surface_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_3_1=uniq_defect_position1(train_surface_3_1)
length(uniq_defect_pos_3_1) ## 761
train_surface_3_1=mile_post_uniq(train_surface_3_1,uniq_defect_pos_3_1)
train_surface_3_1_no_day=remove_same_day_repeat(train_surface_3_1)
train_surface_3_1_no_day_no_single=remove_single_obs(train_surface_3_1_no_day)
length(levels(as.factor(train_surface_3_1_no_day_no_single$MILEPOST)))  ##429
## here we have 429 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
429*0.20 ##85.8 =~86 unique defect positions will be in our test data and remaining 429-86 willbe in our train data
surface_sample_pos_3_1=sample(1:429,86) # SRSWOR sample of size 86 from 429 
surface_uniq_pos_3_1=levels(as.factor(train_surface_3_1_no_day_no_single$MILEPOST))
surface_uniq_pos_train_3_1=surface_uniq_pos_3_1[-surface_sample_pos_3_1]
surface_uniq_pos_test_3_1=surface_uniq_pos_3_1[surface_sample_pos_3_1]
train_surface_3_1_no_day_no_single_trn=subset(train_surface_3_1_no_day_no_single,train_surface_3_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_3_1)
train_surface_3_1_no_day_no_single_tst=subset(train_surface_3_1_no_day_no_single,train_surface_3_1_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_3_1)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

# unique defect position for 3_2
any(train_surface$LINE_SEG_NBR==3 & train_surface$TRACK_SDTK_NBR==2)
train_surface_3_2=subset(train_surface_line_track_milepost,train_surface_line_track_milepost$LINE_SEG_NBR==3 & train_surface_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_3_2=uniq_defect_position1(train_surface_3_2)
length(uniq_defect_pos_3_2) ## 553
train_surface_3_2=mile_post_uniq(train_surface_3_2,uniq_defect_pos_3_2)
train_surface_3_2_no_day=remove_same_day_repeat(train_surface_3_2)
train_surface_3_2_no_day_no_single=remove_single_obs(train_surface_3_2_no_day)
length(levels(as.factor(train_surface_3_2_no_day_no_single$MILEPOST)))  ##314
## here we have 314 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
314*0.20 ##62.8 =~63 unique defect positions will be in our test data and remaining 314-63 willbe in our train data
surface_sample_pos_3_2=sample(1:314,63) # SRSWOR sample of size 63 from 314 
surface_uniq_pos_3_2=levels(as.factor(train_surface_3_2_no_day_no_single$MILEPOST))
surface_uniq_pos_train_3_2=surface_uniq_pos_3_2[-surface_sample_pos_3_2]
surface_uniq_pos_test_3_2=surface_uniq_pos_3_2[surface_sample_pos_3_2]
train_surface_3_2_no_day_no_single_trn=subset(train_surface_3_2_no_day_no_single,train_surface_3_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_train_3_2)
train_surface_3_2_no_day_no_single_tst=subset(train_surface_3_2_no_day_no_single,train_surface_3_2_no_day_no_single$MILEPOST %in% surface_uniq_pos_test_3_2)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

length(which(testing_data$DFCT_TYPE=='SURFACE' & testing_data$LINE_SEG_NBR==4))
# 0 Shows that there is no data satisfying the condition in the test data

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

#@@@@@@@@@@@@@@@@@@@@@@@@@@12345678901234567890-12345678902345678902134567890-234567890-34567890-234567890-234567890-34567890234567890324567890234567890234567890-
#@@@@@@@@@@@@@@@@@@@@@@@@@@12345678901234567890-12345678902345678902134567890-234567890-34567890-234567890-234567890-34567890234567890324567890234567890234567890-
#@@@@@@@@@@@@@@@@@@@@@@@@@@12345678901234567890-12345678902345678902134567890-234567890-34567890-234567890-234567890-34567890234567890324567890234567890234567890-
#@@@@@@@@@@@@@@@@@@@@@@@@@@12345678901234567890-12345678902345678902134567890-234567890-34567890-234567890-234567890-34567890234567890324567890234567890234567890-


# now we will do the same work for DIP Type of defect
train_dip$TEST_DT=as.Date(train_dip$TEST_DT,format="%d%b%Y")
train_dip=train_dip[order(train_dip$LINE_SEG_NBR,train_dip$TRACK_SDTK_NBR,train_dip$TEST_DT,train_dip$MILEPOST),]
summary(train_dip)
train_dip_line_track_milepost=train_dip[order(train_dip$LINE_SEG_NBR,train_dip$TRACK_SDTK_NBR,train_dip$MILEPOST),]
View(train_dip_line_track_milepost)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

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
length(levels(as.factor(train_dip_1_0_no_day_no_single$MILEPOST)))  ##182
## here we have 182 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
182*0.20 ##36.4 =~36 unique defect positions will be in our test data and remaining 182-36 willbe in our train data
dip_sample_pos_1_0=sample(1:182,36) # SRSWOR sample of size 36 from 182 
dip_uniq_pos_1_0=levels(as.factor(train_dip_1_0_no_day_no_single$MILEPOST))
dip_uniq_pos_train_1_0=dip_uniq_pos_1_0[-dip_sample_pos_1_0]
dip_uniq_pos_test_1_0=dip_uniq_pos_1_0[dip_sample_pos_1_0]
train_dip_1_0_no_day_no_single_trn=subset(train_dip_1_0_no_day_no_single,train_dip_1_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_1_0)
train_dip_1_0_no_day_no_single_tst=subset(train_dip_1_0_no_day_no_single,train_dip_1_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_1_0)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

#### line seg 2 and track 0
## problem has occured as at mile post 275.8618694 has no record on tonnage data so we removed them
train_dip_2_0=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==2 & train_dip_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_2_0_dip=uniq_defect_position1(train_dip_2_0)
uniq_defect_pos_2_0_dip
length(uniq_defect_pos_2_0_dip) ## 291
train_dip_2_0=mile_post_uniq(train_dip_2_0,uniq_defect_pos_2_0_dip)
train_dip_2_0_no_day=remove_same_day_repeat(train_dip_2_0)
train_dip_2_0_no_day_no_single=remove_single_obs(train_dip_2_0_no_day)
train_dip_2_0_no_day_no_single=train_dip_2_0_no_day_no_single[-which(train_dip_2_0_no_day_no_single$MILEPOST==train_dip_2_0_no_day_no_single[523,2]),] 
length(levels(as.factor(train_dip_2_0_no_day_no_single$MILEPOST)))  ##137
## here we have 137 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
137*0.20 ##27.4 =~27 unique defect positions will be in our test data and remaining 137-27 willbe in our train data
dip_sample_pos_2_0=sample(1:137,27) # SRSWOR sample of size 27 from 137 
dip_uniq_pos_2_0=levels(as.factor(train_dip_2_0_no_day_no_single$MILEPOST))
dip_uniq_pos_train_2_0=dip_uniq_pos_2_0[-dip_sample_pos_2_0]
dip_uniq_pos_test_2_0=dip_uniq_pos_2_0[dip_sample_pos_2_0]
train_dip_2_0_no_day_no_single_trn=subset(train_dip_2_0_no_day_no_single,train_dip_2_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_2_0)
train_dip_2_0_no_day_no_single_tst=subset(train_dip_2_0_no_day_no_single,train_dip_2_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_2_0)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

train_dip_2_1=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==2 & train_dip_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_2_1_dip=uniq_defect_position1(train_dip_2_1)
uniq_defect_pos_2_1_dip
length(uniq_defect_pos_2_1_dip) ## 
train_dip_2_1=mile_post_uniq(train_dip_2_1,uniq_defect_pos_2_1_dip)
train_dip_2_1_no_day=remove_same_day_repeat(train_dip_2_1)
train_dip_2_1_no_day_no_single=remove_single_obs(train_dip_2_1_no_day)
length(levels(as.factor(train_dip_2_1_no_day_no_single$MILEPOST)))  ##22
## here we have 22 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
22*0.20 ##4.4 =~4 unique defect positions will be in our test data and remaining 22-4 willbe in our train data
dip_sample_pos_2_1=sample(1:22,4) # SRSWOR sample of size 4 from 22 
dip_uniq_pos_2_1=levels(as.factor(train_dip_2_1_no_day_no_single$MILEPOST))
dip_uniq_pos_train_2_1=dip_uniq_pos_2_1[-dip_sample_pos_2_1]
dip_uniq_pos_test_2_1=dip_uniq_pos_2_1[dip_sample_pos_2_1]
train_dip_2_1_no_day_no_single_trn=subset(train_dip_2_1_no_day_no_single,train_dip_2_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_2_1)
train_dip_2_1_no_day_no_single_tst=subset(train_dip_2_1_no_day_no_single,train_dip_2_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_2_1)

#### line seg 2 and track 2 #### at this line and track combination there is no data in test data so we will not do of it
#### line seg 3 and track 0  $$$$$ Never asked in the test data set for this line and segment combination

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

#### line seg 3 and track 1
length(which(train_dip$LINE_SEG_NBR==3 & train_dip$TRACK_SDTK_NBR==1))
train_dip_3_1=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==3 & train_dip_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_3_1_dip=uniq_defect_position1(train_dip_3_1)
uniq_defect_pos_3_1_dip
length(uniq_defect_pos_3_1_dip) ## 
train_dip_3_1=mile_post_uniq(train_dip_3_1,uniq_defect_pos_3_1_dip)
train_dip_3_1_no_day=remove_same_day_repeat(train_dip_3_1)
train_dip_3_1_no_day_no_single=remove_single_obs(train_dip_3_1_no_day)
length(levels(as.factor(train_dip_3_1_no_day_no_single$MILEPOST)))  ##236
## here we have 236 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
236*0.20 ##47.2=~47 unique defect positions will be in our test data and remaining 236-47 willbe in our train data
dip_sample_pos_3_1=sample(1:236,47) # SRSWOR sample of size 47 from 236 
dip_uniq_pos_3_1=levels(as.factor(train_dip_3_1_no_day_no_single$MILEPOST))
dip_uniq_pos_train_3_1=dip_uniq_pos_3_1[-dip_sample_pos_3_1]
dip_uniq_pos_test_3_1=dip_uniq_pos_3_1[dip_sample_pos_3_1]
train_dip_3_1_no_day_no_single_trn=subset(train_dip_3_1_no_day_no_single,train_dip_3_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_3_1)
train_dip_3_1_no_day_no_single_tst=subset(train_dip_3_1_no_day_no_single,train_dip_3_1_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_3_1)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

#### line seg 3 and track 2
length(which(train_dip$LINE_SEG_NBR==3 & train_dip$TRACK_SDTK_NBR==2))
train_dip_3_2=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==3 & train_dip_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_3_2_dip=uniq_defect_position1(train_dip_3_2)
uniq_defect_pos_3_2_dip
length(uniq_defect_pos_3_2_dip) ## 
train_dip_3_2=mile_post_uniq(train_dip_3_2,uniq_defect_pos_3_2_dip)
train_dip_3_2_no_day=remove_same_day_repeat(train_dip_3_2)
train_dip_3_2_no_day_no_single=remove_single_obs(train_dip_3_2_no_day)
length(levels(as.factor(train_dip_3_2_no_day_no_single$MILEPOST)))  ##173
## here we have 173 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
173*0.20 ##34.6=~35 unique defect positions will be in our test data and remaining 173-35 willbe in our train data
dip_sample_pos_3_2=sample(1:173,35) # SRSWOR sample of size 35 from 173 
dip_uniq_pos_3_2=levels(as.factor(train_dip_3_2_no_day_no_single$MILEPOST))
dip_uniq_pos_train_3_2=dip_uniq_pos_3_2[-dip_sample_pos_3_2]
dip_uniq_pos_test_3_2=dip_uniq_pos_3_2[dip_sample_pos_3_2]
train_dip_3_2_no_day_no_single_trn=subset(train_dip_3_2_no_day_no_single,train_dip_3_2_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_3_2)
train_dip_3_2_no_day_no_single_tst=subset(train_dip_3_2_no_day_no_single,train_dip_3_2_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_3_2)


#### line seg 3 and track 3 ### Never asked in the test data set

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

#### line seg 4 and track 0
length(which(train_dip$LINE_SEG_NBR==4 & train_dip$TRACK_SDTK_NBR==0))
train_dip_4_0=subset(train_dip_line_track_milepost,train_dip_line_track_milepost$LINE_SEG_NBR==4 & train_dip_line_track_milepost$TRACK_SDTK_NBR==0)
uniq_defect_pos_4_0_dip=uniq_defect_position1(train_dip_4_0)
uniq_defect_pos_4_0_dip
length(uniq_defect_pos_4_0_dip) ## 300
train_dip_4_0=mile_post_uniq(train_dip_4_0,uniq_defect_pos_4_0_dip)
train_dip_4_0_no_day=remove_same_day_repeat(train_dip_4_0)
train_dip_4_0_no_day_no_single=remove_single_obs(train_dip_4_0_no_day)
# Some problem was occurring at the below given position So we have removed that # Again Run the below code only once
# problem is occurring at milepost 12.0795494 so remove that point 
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[8,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[9,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[9,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[13,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[18,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[16,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[21,2]),]
train_dip_4_0_no_day_no_single=train_dip_4_0_no_day_no_single[-which(train_dip_4_0_no_day_no_single$MILEPOST==train_dip_4_0_no_day_no_single[21,2]),]

length(levels(as.factor(train_dip_4_0_no_day_no_single$MILEPOST)))  ##129
## here we have 129 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
129*0.20 ##25.8 =~26 unique defect positions will be in our test data and remaining 129-26 willbe in our train data
dip_sample_pos_4_0=sample(1:129,26) # SRSWOR sample of size 26 from 129 
dip_uniq_pos_4_0=levels(as.factor(train_dip_4_0_no_day_no_single$MILEPOST))
dip_uniq_pos_train_4_0=dip_uniq_pos_4_0[-dip_sample_pos_4_0]
dip_uniq_pos_test_4_0=dip_uniq_pos_4_0[dip_sample_pos_4_0]
train_dip_4_0_no_day_no_single_trn=subset(train_dip_4_0_no_day_no_single,train_dip_4_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_train_4_0)
train_dip_4_0_no_day_no_single_tst=subset(train_dip_4_0_no_day_no_single,train_dip_4_0_no_day_no_single$MILEPOST %in% dip_uniq_pos_test_4_0)


#### line seg 4 and track 1 #### Never asked in the test data set
#### line seg 4 and track 2  #### Never asked in the test data set
#### line seg 4 and track 3  #### Never asked in the test data set
#### line seg 4 and track 4  #### Never asked in the test data set
#### line seg 4 and track 5  #### Never asked in the test data set
#### line seg 4 and track 6   #### Never asked in the test data set

#@@@@@@@@@@@@@@@@@@@@@@@@@@12345678901234567890-12345678902345678902134567890-234567890-34567890-234567890-234567890-34567890234567890324567890234567890234567890-
#@@@@@@@@@@@@@@@@@@@@@@@@@@12345678901234567890-12345678902345678902134567890-234567890-34567890-234567890-234567890-34567890234567890324567890234567890234567890-
#@@@@@@@@@@@@@@@@@@@@@@@@@@12345678901234567890-12345678902345678902134567890-234567890-34567890-234567890-234567890-34567890234567890324567890234567890234567890-
#@@@@@@@@@@@@@@@@@@@@@@@@@@12345678901234567890-12345678902345678902134567890-234567890-34567890-234567890-234567890-34567890234567890324567890234567890234567890-

# now we will do the same work for XLEVEL Type of defect
train_xlevel$TEST_DT=as.Date(train_xlevel$TEST_DT,format="%d%b%Y")
train_xlevel=train_xlevel[order(train_xlevel$LINE_SEG_NBR,train_xlevel$TRACK_SDTK_NBR,train_xlevel$TEST_DT,train_xlevel$MILEPOST),]
summary(train_xlevel)
train_xlevel_line_track_milepost=train_xlevel[order(train_xlevel$LINE_SEG_NBR,train_xlevel$TRACK_SDTK_NBR,train_xlevel$MILEPOST),]
View(train_xlevel_line_track_milepost)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

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
length(levels(as.factor(train_xlevel_1_0_no_day_no_single$MILEPOST)))  ##399
## here we have 399 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
399*0.20 ##79.8 =~80 unique defect positions will be in our test data and remaining 399-80 willbe in our train data
xlevel_sample_pos_1_0=sample(1:399,80) # SRSWOR sample of size 80 from 399 
xlevel_uniq_pos_1_0=levels(as.factor(train_xlevel_1_0_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_1_0=xlevel_uniq_pos_1_0[-xlevel_sample_pos_1_0]
xlevel_uniq_pos_test_1_0=xlevel_uniq_pos_1_0[xlevel_sample_pos_1_0]
train_xlevel_1_0_no_day_no_single_trn=subset(train_xlevel_1_0_no_day_no_single,train_xlevel_1_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_1_0)
train_xlevel_1_0_no_day_no_single_tst=subset(train_xlevel_1_0_no_day_no_single,train_xlevel_1_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_1_0)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

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
length(levels(as.factor(train_xlevel_2_0_no_day_no_single$MILEPOST)))  ##484
## here we have 484 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
484*0.20 ##96.8 =~97 unique defect positions will be in our test data and remaining 484-97 willbe in our train data
xlevel_sample_pos_2_0=sample(1:484,97) # SRSWOR sample of size 97 from 484 
xlevel_uniq_pos_2_0=levels(as.factor(train_xlevel_2_0_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_2_0=xlevel_uniq_pos_2_0[-xlevel_sample_pos_2_0]
xlevel_uniq_pos_test_2_0=xlevel_uniq_pos_2_0[xlevel_sample_pos_2_0]
train_xlevel_2_0_no_day_no_single_trn=subset(train_xlevel_2_0_no_day_no_single,train_xlevel_2_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_0)
train_xlevel_2_0_no_day_no_single_tst=subset(train_xlevel_2_0_no_day_no_single,train_xlevel_2_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_0)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

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
length(levels(as.factor(train_xlevel_2_1_no_day_no_single$MILEPOST)))  ##53
## here we have 53 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
53*0.20 ##10.6 =~11 unique defect positions will be in our test data and remaining 53-11 willbe in our train data
xlevel_sample_pos_2_1=sample(1:53,11) # SRSWOR sample of size 11 from 53 
xlevel_uniq_pos_2_1=levels(as.factor(train_xlevel_2_1_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_2_1=xlevel_uniq_pos_2_1[-xlevel_sample_pos_2_1]
xlevel_uniq_pos_test_2_1=xlevel_uniq_pos_2_1[xlevel_sample_pos_2_1]
train_xlevel_2_1_no_day_no_single_trn=subset(train_xlevel_2_1_no_day_no_single,train_xlevel_2_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_1)
train_xlevel_2_1_no_day_no_single_tst=subset(train_xlevel_2_1_no_day_no_single,train_xlevel_2_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_1)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

#### line seg 2 and track 2
length(which(train_xlevel$LINE_SEG_NBR==2 & train_xlevel$TRACK_SDTK_NBR==2))
train_xlevel_2_2=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==2 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_2_2_xlevel=uniq_defect_position1(train_xlevel_2_2)
uniq_defect_pos_2_2_xlevel
length(uniq_defect_pos_2_2_xlevel)
train_xlevel_2_2=mile_post_uniq(train_xlevel_2_2,uniq_defect_pos_2_2_xlevel)
train_xlevel_2_2_no_day=remove_same_day_repeat(train_xlevel_2_2)
train_xlevel_2_2_no_day_no_single=remove_single_obs(train_xlevel_2_2_no_day)
length(levels(as.factor(train_xlevel_2_2_no_day_no_single$MILEPOST)))  ##60
## here we have 60 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
60*0.20 ##12 unique defect positions will be in our test data and remaining 60-11 willbe in our train data
xlevel_sample_pos_2_2=sample(1:60,12) # SRSWOR sample of size 12 from 60 
xlevel_uniq_pos_2_2=levels(as.factor(train_xlevel_2_2_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_2_2=xlevel_uniq_pos_2_2[-xlevel_sample_pos_2_2]
xlevel_uniq_pos_test_2_2=xlevel_uniq_pos_2_2[xlevel_sample_pos_2_2]
train_xlevel_2_2_no_day_no_single_trn=subset(train_xlevel_2_2_no_day_no_single,train_xlevel_2_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_2_2)
train_xlevel_2_2_no_day_no_single_tst=subset(train_xlevel_2_2_no_day_no_single,train_xlevel_2_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_2_2)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

#### line seg 3 and track 1
length(which(train_xlevel$LINE_SEG_NBR==3 & train_xlevel$TRACK_SDTK_NBR==1))
train_xlevel_3_1=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==3 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==1)
uniq_defect_pos_3_1_xlevel=uniq_defect_position1(train_xlevel_3_1)
uniq_defect_pos_3_1_xlevel
length(uniq_defect_pos_3_1_xlevel)
train_xlevel_3_1=mile_post_uniq(train_xlevel_3_1,uniq_defect_pos_3_1_xlevel)
train_xlevel_3_1_no_day=remove_same_day_repeat(train_xlevel_3_1)
train_xlevel_3_1_no_day_no_single=remove_single_obs(train_xlevel_3_1_no_day)
length(levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST)))  ##293
## here we have 293 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
293*0.20 ##58.6 =~57 unique defect positions will be in our test data and remaining 293-57 willbe in our train data
xlevel_sample_pos_3_1=sample(1:293,57) # SRSWOR sample of size 57 from 293 
xlevel_uniq_pos_3_1=levels(as.factor(train_xlevel_3_1_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_3_1=xlevel_uniq_pos_3_1[-xlevel_sample_pos_3_1]
xlevel_uniq_pos_test_3_1=xlevel_uniq_pos_3_1[xlevel_sample_pos_3_1]
train_xlevel_3_1_no_day_no_single_trn=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_1)
train_xlevel_3_1_no_day_no_single_tst=subset(train_xlevel_3_1_no_day_no_single,train_xlevel_3_1_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_1)

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

#### line seg 3 and track 2
length(which(train_xlevel$LINE_SEG_NBR==3 & train_xlevel$TRACK_SDTK_NBR==2))
train_xlevel_3_2=subset(train_xlevel_line_track_milepost,train_xlevel_line_track_milepost$LINE_SEG_NBR==3 & train_xlevel_line_track_milepost$TRACK_SDTK_NBR==2)
uniq_defect_pos_3_2_xlevel=uniq_defect_position1(train_xlevel_3_2)
uniq_defect_pos_3_2_xlevel
length(uniq_defect_pos_3_2_xlevel)
train_xlevel_3_2=mile_post_uniq(train_xlevel_3_2,uniq_defect_pos_3_2_xlevel)
train_xlevel_3_2_no_day=remove_same_day_repeat(train_xlevel_3_2)
train_xlevel_3_2_no_day_no_single=remove_single_obs(train_xlevel_3_2_no_day)
length(levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST)))  ##254
## here we have 254 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
254*0.20 ##50.8 =~51 unique defect positions will be in our test data and remaining 254-51 willbe in our train data
xlevel_sample_pos_3_2=sample(1:254,51) # SRSWOR sample of size 51 from 254
xlevel_uniq_pos_3_2=levels(as.factor(train_xlevel_3_2_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_3_2=xlevel_uniq_pos_3_2[-xlevel_sample_pos_3_2]
xlevel_uniq_pos_test_3_2=xlevel_uniq_pos_3_2[xlevel_sample_pos_3_2]
train_xlevel_3_2_no_day_no_single_trn=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_3_2)
train_xlevel_3_2_no_day_no_single_tst=subset(train_xlevel_3_2_no_day_no_single,train_xlevel_3_2_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_3_2)

#### line seg 3 and track 3  #### never asked in tets data set

######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************
######**************************************************************************************************************************************************************************

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
length(levels(as.factor(train_xlevel_4_0_no_day_no_single$MILEPOST)))  ##156
## here we have 155 unique defect positions now we will make two datasets one as train and test dataset 80 and 20%
156*0.20 ##31.2 =~31 unique defect positions will be in our test data and remaining 155-31 willbe in our train data
xlevel_sample_pos_4_0=sample(1:155,31) # SRSWOR sample of size 31 from 155 
xlevel_uniq_pos_4_0=levels(as.factor(train_xlevel_4_0_no_day_no_single$MILEPOST))
xlevel_uniq_pos_train_4_0=xlevel_uniq_pos_4_0[-xlevel_sample_pos_4_0]
xlevel_uniq_pos_test_4_0=xlevel_uniq_pos_4_0[xlevel_sample_pos_4_0]
train_xlevel_4_0_no_day_no_single_trn=subset(train_xlevel_4_0_no_day_no_single,train_xlevel_4_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_train_4_0)
train_xlevel_4_0_no_day_no_single_tst=subset(train_xlevel_4_0_no_day_no_single,train_xlevel_4_0_no_day_no_single$MILEPOST %in% xlevel_uniq_pos_test_4_0)

#### line seg 4 and track 1  ### Never asked
#### line seg 4 and track 2  ### Never asked






