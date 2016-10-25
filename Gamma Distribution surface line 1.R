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
