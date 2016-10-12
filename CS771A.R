training=read.csv('training.csv')
tonnage=read.csv('tonnage.csv')
inspect_data=read.csv('Inspection.csv')
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
# we got 13517
ength(which(tonnage$TOT_CAR_EAST==0 & tonnage$TOT_CAR_WEST==0 & tonnage$TOT_TRN_EAST==0 & tonnage$TOT_TRN_WEST==0))
# we got 9612
dim(subset(tonnage,tonnage$YEAR==2007 | tonnage$YEAR==2008))
# we got 9515  11
length(which(tonnage$TOT_CAR_EAST==0 & tonnage$TOT_CAR_WEST==0 & tonnage$TOT_TRN_EAST==0 & tonnage$TOT_TRN_WEST==0 & tonnage$YEAR!=2007 & tonnage$YEAR!=2008))
# we got 97  9515+97=9612  which confirms that in other than year 2007 and 2008 we have 97 such data points at which we have no value on CARs and TRNS
# 13518-9612=3906 data points at which we have not all zero but some of them are zero
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
# 0 data points at which there is only TOT_TRNS_EAST is missing
length(which(tonnage$TOT_CAR_EAST!=0 & tonnage$TOT_CAR_WEST!=0 & tonnage$TOT_TRN_EAST!=0 & tonnage$TOT_TRN_WEST==0))
# the problem is that sometimes a train carry only 3 cars and some times 1 train carry 111 cars see row no 749 and 1925
# Now we will divide the tonnage data into more subsets having some characteristics like as no zero, TOT car east is zero etc
tonnage_prob_no_zero=subset(tonnage,tonnage$TOT_CAR_EAST!=0 & tonnage$TOT_CAR_WEST!=0 & tonnage$TOT_TRN_EAST!=0 & tonnage$TOT_TRN_WEST!=0)
dim(tonnage_prob_no_zero)
# 23301 11 shows that 23300 data points have no zeros
dim(tonnage)
# 36818 11 
# 36818-23301=13517 data points have atleast one zero
tonnage_atleast_one_zero=subset(tonnage,tonnage$TOT_CAR_EAST==0 | tonnage$TOT_CAR_WEST==0 | tonnage$TOT_TRN_EAST==0 | tonnage$TOT_TRN_WEST==0)
dim(tonnage_atleast_one_zero)
# 13517 11 confirms the above
tonnage_prob_all_zero=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_prob_all_zero) ## 9611  11  9611 data points are having all zeros
tonnage_only_Cars_east_0=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_Cars_east_0) # 0 11 no data having only cars east 0
tonnage_only_Cars_west_0=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_Cars_west_0) # 10 11    10 rows has only cars west missing
tonnage_only_train_east_0=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_train_east_0) # 0 11 only one have trains east 0
tonnage_only_train_west_0=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_only_train_west_0)  # 143 11  143 rows have only trains west missing
tonnage_only_cars_missing_both_sides=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_cars_missing_both_sides)## 1616 11   1616 rows have cars missing both sides 
tonnage_only_trains_missing_both_sides=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_only_trains_missing_both_sides) ## 1  11 only one data which has trains both sides are missing
tonnage_only_CarsWest_and_TrainWest_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_CarsWest_and_TrainWest_missing) ### Something Wrong Here

### Something Wrong Here not matching
### Something Wrong Here
### Something Wrong Here
### Something Wrong Here
### Something Wrong Here
### Something Wrong Here
### Something Wrong Here
### Something Wrong Here
### Something Wrong Here


tonnage_only_CarsEast_and_TrainEast_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_only_CarsEast_and_TrainEast_missing)  # 1  11    only one data 
tonnage_prob_CarsEastAndWest_TrainsEast_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST!=0)
dim(tonnage_prob_CarsEastAndWest_TrainsEast_missing) # 141  11 
tonnage_prob_CarsWest_TrainsEastAndWest_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST!=0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_prob_CarsWest_TrainsEastAndWest_missing)  #2  11   
tonnage_CarsEastAndWest_TrainsWest_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST==0 & tonnage_atleast_one_zero$TOT_TRN_EAST!=0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_CarsEastAndWest_TrainsWest_missing)  # 6  11 
tonnage_CarsEast_TrainsEastAndWest_missing=subset(tonnage_atleast_one_zero,tonnage_atleast_one_zero$TOT_CAR_EAST==0 & tonnage_atleast_one_zero$TOT_CAR_WEST!=0 & tonnage_atleast_one_zero$TOT_TRN_EAST==0 & tonnage_atleast_one_zero$TOT_TRN_WEST==0)
dim(tonnage_CarsEast_TrainsEastAndWest_missing) # 1 11






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










