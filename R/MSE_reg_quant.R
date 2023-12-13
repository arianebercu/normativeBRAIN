###############################################################################
### CALCULATE MSE FOR A GIVEN METHODE #########################################
##############################################################################

# N = number of observations necessary in each sub
MSE_reg<-function(traindata,newdata,tau,level = 0.05,N_obs,N_min,Y,spline,knots,time=NULL,
                  X1,X2,subject,death,wcompute,intermittent,weight,B=300,seed=floor(runif(300)*1000000),form_time){

  if(class(traindata)!="data.frame" | class(newdata)!="data.frame"){stop("Traindata and newdata need to be of time data.frame ")}
  if(is.null(time)){stop("You need to specify the time variable")}

  if(!is.character(time)){stop("Time need to be the name of the variable of time so a character")}

  if(!time%in%colnames(newdata)){stop("Time variable need to be in newdata")}
  if(!time%in%colnames(traindata)){stop("Time variable need to be in traindata")}


  traindata<-traindata[!is.na(traindata[,time]),]
  newdata<-newdata[!is.na(newdata[,time]),]

  min_age<-min(na.omit(traindata[,time]))
  max_age<-max(na.omit(traindata[,time]))
  seg_age<-c()
  seg_age[1]<-min_age

  k<-traindata$NUM_ID[traindata[,time]<=seg_age[1]]
  n<-length(traindata$NUM_ID[traindata[,time]<=seg_age[1]])
  l<-1

  while(n <= dim(traindata)[1]){
    if(l==1){ # define k before while
      k<-traindata$NUM_ID[traindata[,time]<=seg_age[l]]
    }
    if(l>1){
      k<-traindata$NUM_ID[traindata[,time]<=seg_age[l] & traindata[,time]>seg_age[l-1]]
    }
    while (length(k)<N_obs){

      if(l==1){  # define k in while
        k<-traindata$NUM_ID[traindata[,time]<=seg_age[l]]
      }
      if(l>1){
        k<-traindata$NUM_ID[traindata[,time]<=seg_age[l] & traindata[,time]>seg_age[l-1]]
      }

      if(seg_age[l]>max_age){break}

      seg_age[l]<-seg_age[l]+0.001
    }
    if(length(k)<N_min){
      seg_age<-seg_age[-length(seg_age)]
      break
    }
    #if(seg_age[l]>max_age){break}
    seg_age[l+1]<-seg_age[l]+0.001
    l<-l+1
    n<-n+length(k)
  }

  # verif all id are taken
  id<-c()
  traindata$id_train<-NA
  for(i in 1:(length(seg_age))){
    if(i==1){
      traindata[traindata[,time]<=seg_age[i],"id_train"]<-i
      newdata[newdata[,time]<=seg_age[i],"id_train"]<-i
    }
    if(i>1 & i!=length(seg_age)){
      traindata[traindata[,time]<=seg_age[i] & traindata[,time]>seg_age[i-1],"id_train"]<-i
      newdata[newdata[,time]<=seg_age[i] & newdata[,time]>seg_age[i-1],"id_train"]<-i
    }

    if(i==(length(seg_age))){
      traindata[traindata[,time]>seg_age[i-1],"id_train"]<-i
      newdata[newdata[,time]>seg_age[i-1],"id_train"]<-i}
  }

  quantile_obs<-c()
  tau_obs<-rep(tau,length(seg_age))


  index_age<-c()
  u<-1

  ########## calcul empirique quantiles ###########
  for( i in 1:(length(seg_age))){
    sub_table<-newdata[newdata$id_train==i,]
    sub_table<-sub_table[!is.na(sub_table[,colnames(sub_table)%in%Y]),]

    index_age[u:(u+length(tau)-1)]<-i
    u<-(u+length(tau))


    quantile_obs<-c(quantile_obs,
                    wtd.quantile(sub_table[,colnames(sub_table)%in%Y],probs=tau,weights = sub_table$w_imd))

  }

  data_quantile<-data.frame(tau=tau_obs,quantile_emp=quantile_obs,index_age=index_age)

  ######### calculate prediction of quantiles ###########################
  # create indicator for time


  form<-paste0(Y,"~",form_time)
  form<-as.formula(form)

  boot_quad_real <-bootwrq(B=B, form=form, tau=tau,
                                        data=traindata, Y=Y,X1=X1,X2=NULL,subject=subject,
                                        death=death,time=time,wcompute=wcompute,intermittent=intermittent,weight=weight,
                                        seed=seed)


  boot_quad_cov<-summary.bootwrq(boot_quad_real,seed=seed)$matrix_covariance_res0

  rq_quad<-rq(form,data=traindata,weights=traindata$w_imd,tau=tau)


  pred_quad<-predict_WQ(object_boot=boot_quad_real,object_rq=rq_quad,object_cov=boot_quad_cov,newdata=newdata,tau=tau,level = 0.05)
  pred_quad<-as.data.frame(pred_quad)
  pred_plot<-data_plot(pred_quad)
  pred_plot$index_age<-NA

  for (i in 1: dim(pred_plot)[1]){
    for (j in length(seg_age):1){
      if(j==length(seg_age)){
        if(pred_plot[i,time]>seg_age[j-1]) {pred_plot$index_age[i]<-j}
      }
      if(j < length(seg_age) & (j!=1)){
        if((pred_plot[i,time]<=seg_age[j]) & (pred_plot[i,time]>seg_age[j-1])){
          pred_plot$index_age[i]<-j
        }
      }
      if(j==1){
        if(pred_plot[i,time]<=seg_age[j]) {pred_plot$index_age[i]<-j}

      }
    }
  }

  pred_plot<-pred_plot[,colnames(pred_plot)%in% c(time,"pred","tau","index_age")]
  pred_plot$tau<-as.numeric(pred_plot$tau)
  pred_plot$index_age<-as.numeric(pred_plot$index_age)
  data_quantile$tau<-as.numeric(data_quantile$tau)
  data_quantile$index_age<-as.numeric(data_quantile$index_age)

  # Does not work
  pred_MSE<-merge(x=pred_plot,y=data_quantile,by=c("index_age","tau"),all.x=T)

  MSE<-data.frame(tau=tau,value=NA)
  for(i in 1:length(tau)){
    pred_MSE_tau<-pred_MSE[pred_MSE$tau==tau[i],]
    MSE$value[i]<-sum((pred_MSE_tau$pred-pred_MSE_tau$quantile_emp)^2)/dim(pred_MSE_tau)[1]
  }
  return(list(MSE=MSE,pred_MSE=pred_MSE))
}


###############################################################################
### CALCULATE MSE FOR A WINDOW METHOD #########################################
##############################################################################

# N = number of observations necessary in each sub
MSE_reg_long<-function(traindata,newdata,tau,level = 0.05,N_obs,N_min,Y,pace,form_time,spline,knots,time=NULL,
                       X1,X2,subject,death,wcompute,intermittent,weight,B=300,seed=floor(runif(300)*1000000)){

  if(class(traindata)!="data.frame" | class(newdata)!="data.frame"){stop("Traindata and newdata need to be of time data.frame ")}
  if(is.null(time)){stop("You need to specify the time variable")}

  if(!is.character(time)){stop("Time need to be the name of the variable of time so a character")}

  if(!time%in%colnames(newdata)){stop("Time variable need to be in newdata")}
  if(!time%in%colnames(traindata)){stop("Time variable need to be in traindata")}



  traindata<-traindata[!is.na(traindata[,time]),]
  newdata<-newdata[!is.na(newdata[,time]),]

  min_age<-min(na.omit(traindata[,time]))
  max_age<-max(na.omit(traindata[,time]))


  k<-traindata$NUM_ID[traindata[,time]<=seg_age[1]]
  n<-length(traindata$NUM_ID[traindata[,time]<=seg_age[1]])
  l<-1


  born_sup<-c()
  born_sup[1]<-min_age
  born_inf<-c()
  born_inf[1]<-min_age

  ## need to verify
  sup_age<-born_sup[1]
  while(sup_age<=max_age){
    if(l==1){ # define k before while
      k<-traindata$NUM_ID[traindata$AGE_VIS_cr[,time]<= born_sup[l]]
    }
    if(l>1){
      k<-traindata$NUM_ID[traindata$AGE_VIS_cr[,time]<= born_sup[l] & traindata$AGE_VIS_cr[,time]>born_inf[l]]
    }
    while (length(k)<N_obs){
      if(l==1){  # define k in while
        k<-traindata$NUM_ID[traindata$AGE_VIS_cr[,time]<= born_sup[l]]
      }
      if(l>1){
        k<-traindata$NUM_ID[traindata$AGE_VIS_cr[,time]<= born_sup[l] & traindata$AGE_VIS_cr[,time]>born_inf[l]]
      }
      born_sup[l]<- born_sup[l]+0.0001
      sup_age<-born_sup[l]
      if( born_sup[l]>max_age){break}
    }

    if (length(k)< N_min){ # keep a minimum number of subject in each subset
      born_inf<-born_inf[-length(born_inf)]
      born_sup<-born_sup[-length(born_sup)]
      supp_break<-1
      break
    }
    born_inf[l+1]<- born_inf[l]+pace
    born_sup[l+1]<- born_inf[l]+pace+0.0001
    sup_age<-born_sup[l+1]
    l<-l+1

  }

  # verif all id are taken
  traindata$id_train<-NA
  for(i in 1:(length(born_inf))){
    if(i==1){
      traindata$id_train<-ifelse(traindata[,time]<=born_sup[i],1,0)
      colnames(traindata)[colnames(traindata)%in%"id_train"]<-paste0(time,"_",round(born_inf[i]),"_",round(born_sup[i]))

      newdata$id_train<-ifelse(newdata[,time]<=born_sup[i],1,0)
      colnames(newdata)[colnames(newdata)%in%"id_train"]<-paste0(time,"_",round(born_inf[i]),"_",round(born_sup[i]))
    }
    if(i>1 & i!=length(born_inf)){
      traindata$id_train<-ifelse(traindata[,time]<=born_sup[i] & traindata[,time]>born_inf[i],1,0)
      colnames(traindata)[colnames(traindata)%in%"id_train"]<-paste0(time,"_",round(born_inf[i]),"_",round(born_sup[i]))

      newdata$id_train<-ifelse(newdata[,time]<=born_sup[i] & newdata[,time]>born_inf[i],1,0)
      colnames(newdata)[colnames(newdata)%in%"id_train"]<-paste0(time,"_",round(born_inf[i]),"_",round(born_sup[i]))
    }

    if(i==(length(born_inf))){
      traindata$id_train<-ifelse(traindata[,time]>born_inf[i],1,0)
      colnames(traindata)[colnames(traindata)%in%"id_train"]<-paste0(time,"_",round(born_inf[i]))

      newdata$id_train<-ifelse(newdata[,time]>born_inf[i],1,0)
      colnames(newdata)[colnames(newdata)%in%"id_train"]<-paste0(time,"_",round(born_inf[i]))    }

  }


  quantile_obs<-c()
  tau_obs<-rep(tau,length(born_inf))


  ########## calcul empirique quantiles ###########

  u<-1
  index_age<-NA

  for( i in 1:(length(born_inf))){
    if(i<length(born_inf)){

      sub_table<-newdata[,colnames(newdata)%in%c(Y,paste0(time,"_",round(born_inf[i]),"_",round(born_sup[i])))]
      sub_table<-sub_table[sub_table[,str_detect(colnames(sub_table),time)]==1 ,]
      sub_table<-sub_table[!is.na(sub_table[,colnames(sub_table)%in%Y]),]


      index_age[u:(u+length(tau)-1)]<-i
      u<-(u+length(tau))

      quantile_obs<-c(quantile_obs,
                      wtd.quantile(sub_table[,colnames(sub_table)%in%Y],probs=tau,weights = sub_table$w_imd))}

    if(i==length(born_inf)){

      sub_table<-newdata[,colnames(newdata)%in%c(Y,paste0(time,"_",round(born_inf[i])))]
      sub_table<-sub_table[sub_table[,str_detect(colnames(sub_table),time)]==1,]
      sub_table<-sub_table[!is.na(sub_table[,colnames(sub_table)%in%Y]),]

      index_age[u:(u+length(tau)-1)]<-i
      u<-(u+length(tau))

      quantile_obs<-c(quantile_obs,
                      wtd.quantile(sub_table[,colnames(sub_table)%in%Y],probs=tau,weights = sub_table$w_imd))}

  }
  data_quantile<-data.frame(tau=tau_obs,quantile_emp=quantile_obs,index_age=index_age)

  ######### calculate prediction of quantiles ###########################
  # create indicator for time
  for (i in 2: length(born_inf)){
    if(i<length(born_sup)){
      traindata$indic_age<-ifelse(traindata[,time]<=born_sup[i] & traindata[,time]>born_inf[i],1,0)
      colnames(traindata)[colnames(traindata)%in%"indic_age"]<-paste0(time,"_",round(born_inf[i]),"_",round(born_sup[i]))}
    if(i==length(born_inf)){
      traindata$indic_age<-ifelse(traindata[,time]>born_inf[i],1,0)
      colnames(traindata)[colnames(traindata)%in%"indic_age"]<-paste0(time,"_",round(born_inf[i]))}
  }

  form<-paste0(Y,"~",form_time)
  form<-as.formula(form)

  boot_quad_real <-bootwrq(B=B, form=form, tau=tau,
                                        data=traindata, Y=Y,X1=X1,X2=NULL,subject=subject,
                                        death=death,time=time,wcompute=wcompute,intermittent=intermittent,weight=weight,
                                        seed=seed)

  boot_quad_cov<-summary.bootwrq(boot_quad_real,seed=seed)$matrix_covariance_res0

  rq_quad<-rq(form,data=traindata,weights=traindata$w_imd,tau=tau)


  pred_quad<-predict_WQ(object_boot=boot_quad_real,object_rq=rq_quad,object_cov=boot_quad_cov,newdata=newdata,tau=tau,level = 0.05)
  pred_quad<-as.data.frame(pred_quad)
  pred_plot<-data_plot(pred_quad)


  ### create prediction for each subdata ###
  pred_plot_new<-data.frame()

  for (i in 1:length(born_inf)){
    if(i==1){
      pred_plot_new<-pred_plot[pred_plot[,time]<=born_sup[i],]
      pred_plot_new$index_age<-i
    }
    if(i>1 & i!=length(born_inf)){
      newdata_pred<-pred_plot[pred_plot[,time]<=born_sup[i] & pred_plot[,time]>born_inf[i],]
      newdata_pred$index_age<-i
      pred_plot_new<-rbind(pred_plot_new,newdata_pred)
    }

    if(i==(length(born_inf))){

      newdata_pred<-pred_plot[pred_plot[,time]>born_inf[i],]
      newdata_pred$index_age<-i
      pred_plot_new<-rbind(pred_plot_new,newdata_pred)
    }
  }

  pred_plot_new<-pred_plot_new[,colnames(pred_plot_new)%in% c(time,"pred","tau","index_age")]
  pred_MSE<-merge(pred_plot_new,data_quantile,by=c("tau","index_age"))

  MSE<-data.frame(tau=tau,value=NA)

  for(i in 1:length(tau)){
    pred_MSE_tau<-pred_MSE[pred_MSE$tau==tau[i],]
    MSE$value[i]<-sum((pred_MSE_tau$pred-pred_MSE_tau$quantile_emp)^2)/dim(pred_MSE_tau)[1]
  }
  return(list(MSE=MSE,pred_MSE=pred_MSE))
}

###############################################################################
### CALCULATE MSE FOR WITH PRIOR SELECTION #########################################
##############################################################################

# N = number of observations necessary in each sub
MSE_reg_spec<-function(traindata,newdata,tau,level = 0.05,N_obs,Y,spline,knots,time=NULL,
                  X1,X2,subject,death,wcompute,intermittent,weight,B=300,seed=floor(runif(300)*1000000),
                  form_time,val.form_time){

  if(class(traindata)!="data.frame" | class(newdata)!="data.frame"){stop("Traindata and newdata need to be of time data.frame ")}
  if(is.null(time)){stop("You need to specify the time variable")}

  if(!is.character(time)){stop("Time need to be the name of the variable of time so a character")}
  if(!is.character(val.form_time)){stop("val.form_time need to be the name of the variable of time in the formula so a character")}

  if(!time%in%colnames(newdata)){stop("Time variable need to be in newdata")}
  if(!time%in%colnames(traindata)){stop("Time variable need to be in traindata")}


  #traindata<-traindata[!is.na(traindata[,time]),]
  traindata<-traindata[!is.na(traindata[,val.form_time]),]
  traindata<-traindata[!is.na(traindata[,X1]),]
  # KEEP ONLY SUBJECTS IN AGE IN 5 - 95% (erase out)
  #min<-quantile(traindata[,val.form_time],probs=c(0.05))
  #max<-quantile(traindata[,val.form_time],probs=c(0.95))
  #traindata<-traindata[traindata[,val.form_time]>=min & traindata[,val.form_time]<=max,]

  #newdata<-newdata[!is.na(newdata[,time]),]
  newdata<-newdata[!is.na(newdata[,val.form_time]),]
  #newdata<-newdata[newdata[,val.form_time]>=min & newdata[,val.form_time]<=max,]
  #newdata<-newdata[!is.na(newdata[,X1]),]


  quantile_obs<-rep(NA,length(tau)*dim(newdata)[1])
  tau_obs<-rep(tau,dim(newdata)[1])


  NUM<-rep(NA,length(tau)*dim(newdata)[1])
  time.scale<-rep(NA,length(tau)*dim(newdata)[1])
  u<-1

  ########## calcul empirique quantiles ###########
  # for each subject


  for( i in 1:dim(newdata)[1]){
    sub_table<-newdata
    sub_table$distance<-newdata[,val.form_time]-rep(newdata[i,val.form_time],dim(newdata)[1])
    sub_table<-sub_table[!is.na(sub_table[,colnames(sub_table)%in%Y]),]
    sub_table<-sub_table[order(abs(sub_table$distance)),]
    sub_table<-sub_table[1:N_obs,]

    NUM[u:(u+length(tau)-1)]<-rep(newdata[i,subject],length(tau))
    time.scale[u:(u+length(tau)-1)]<-rep(newdata[i,time],length(tau))
    quantile_obs[u:(u+length(tau)-1)]<-wtd.quantile(sub_table[,colnames(sub_table)%in%Y],probs=tau,weights = sub_table$w_imd)
    u<-(u+length(tau))

  }



  data_quantile<-data.frame(tau=tau_obs,
                            quantile_emp=quantile_obs,
                            NUM=NUM,
                            time.scale=time.scale)
  colnames(data_quantile)[3]<-subject
  colnames(data_quantile)[4]<-time

  ######### calculate prediction of quantiles ###########################
  # create indicator for time


  form<-paste0(Y,"~",form_time)
  form<-as.formula(form)


  boot_quad_real <-bootwrq(B=B, form=form, tau=tau,
                                        data=traindata, Y=Y,X1=X1,X2=NULL,subject=subject,
                                        death=death,time=time,wcompute=wcompute,intermittent=intermittent,weight=weight,
                                        seed=seed)

  boot_quad_cov<-summary.bootwrq(boot_quad_real,seed=seed)$matrix_covariance_res0

  rq_quad<-rq(form,data=traindata,weights=traindata$w_imd,tau=tau)


  # calculate prediction for each subject on newdata
  # depends on tau

  pred_quad<-predict_WQ(object_boot=boot_quad_real,object_rq=rq_quad,object_cov=boot_quad_cov,newdata=newdata,tau=tau,level = 0.05)

  pred_quad<-as.data.frame(pred_quad)
  # one columns with prediction and one tau
  pred_plot<-data_plot(pred_quad)

  pred_plot<-pred_plot[,colnames(pred_plot)%in% c(subject,time,"pred","tau")]
  pred_plot$tau<-as.numeric(pred_plot$tau)

  data_quantile$tau<-as.numeric(data_quantile$tau)

  # merge predictions and observed quantiles
  pred_MSE<-merge(x=pred_plot,y=data_quantile,by=c(subject,time,"tau"),all.x=T)

  # calculate MSE per tau
  MSE<-data.frame(tau=tau,value=NA)


  for(i in 1:length(tau)){
    pred_MSE_tau<-pred_MSE[pred_MSE$tau==tau[i],]
    MSE$value[i]<-sum((pred_MSE_tau$pred-pred_MSE_tau$quantile_emp)^2)/dim(pred_MSE_tau)[1]
  }
  return(list(MSE=MSE,pred_MSE=pred_MSE,
              traindata=traindata,
              testdata=newdata))
}


Emp_spec<-function(data,tau,level = 0.05,N_obs,Y,time=NULL,
                       subject,val.form_time){

  if(class(data)!="data.frame" | class(data)!="data.frame"){stop("Traindata and newdata need to be of time data.frame ")}
  if(is.null(time)){stop("You need to specify the time variable")}

  if(!is.character(time)){stop("Time need to be the name of the variable of time so a character")}
  if(!is.character(val.form_time)){stop("val.form_time need to be the name of the variable of time in the formula so a character")}

  if(!time%in%colnames(data)){stop("Time variable need to be in traindata")}


  #traindata<-traindata[!is.na(traindata[,time]),]
  data<-data[!is.na(data[,val.form_time]),]


  quantile_obs<-rep(NA,length(tau)*dim(data)[1])
  tau_obs<-rep(tau,dim(data)[1])


  NUM<-rep(NA,length(tau)*dim(data)[1])
  time.scale<-rep(NA,length(tau)*dim(data)[1])
  u<-1

  ########## calcul empirique quantiles ###########
  # for each subject

  for( i in 1:dim(data)[1]){
    sub_table<-data
    sub_table$distance<-data[,val.form_time]-rep(data[i,val.form_time],dim(data)[1])
    sub_table<-sub_table[!is.na(sub_table[,colnames(sub_table)%in%Y]),]
    sub_table<-sub_table[order(abs(sub_table$distance)),]
    sub_table<-sub_table[1:N_obs,]

    NUM[u:(u+length(tau)-1)]<-rep(data[i,subject],length(tau))
    time.scale[u:(u+length(tau)-1)]<-rep(data[i,time],length(tau))
    quantile_obs[u:(u+length(tau)-1)]<-wtd.quantile(sub_table[,colnames(sub_table)%in%Y],probs=tau,weights = sub_table$w_imd)
    u<-(u+length(tau))

  }



  data_quantile<-data.frame(tau=tau_obs,
                            quantile_emp=quantile_obs,
                            NUM=NUM,
                            time.scale=time.scale)
  colnames(data_quantile)[3]<-subject
  colnames(data_quantile)[4]<-time


  return(list(data_quantile=data_quantile))
}
