predict_WQ_solo<-function(newdata,new_subject,B=100,form,tau,data,Y,X1,subject,death="death",
                          time="time",wcompute=0,intermittent=T,weight_WQ,seed){


  # faire tourner le modèle WQ
  boot <-bootwrq(B=B, form=form, tau=tau,data=data, Y=Y,X1=X1,X2=NULL,subject=subject,
                              death=death,time=time,wcompute=wcompute,intermittent=intermittent,weight=weight_WQ,seed=seed)

  if(wcompute==0){
    boot_cov<-summary.bootwrq(boot,seed=seed)$matrix_covariance_res0}
  if(wcompute==1){
    boot_cov<-summary.bootwrq(boot,seed=seed)$matrix_covariance_res1}
  # use it only for predict_WQ so for the terms, no need of weights
  rq_model<-rq(form,data=data,tau=tau)


  pred<-predict_WQ(object_boot=boot,object_rq=rq_model,object_cov=boot_cov,newdata=newdata,tau=tau,level = 0.05)
  pred<-as.data.frame(pred)
  pred_plot<-data_plot(pred)
  pred_plot_solo<-pred_plot[pred_plot$AGE_VIS_cr==new_subject$AGE_VIS_cr,]
  # fin tau such that value pred distance minimized
  distance<-abs(new_subject[,Y]-pred_plot_solo$pred[1])
  tau_solo<-pred_plot_solo$tau[1]
  tau_appartenante<-rep(NA,dim(pred_plot_solo)[1])

  if(new_subject[,Y]>=pred_plot_solo$pred_inf[1] & new_subject[,Y]<=pred_plot_solo$pred_sup[1]){
    tau_appartenante[1]<-TRUE
  }else{tau_appartenante[1]<-FALSE}

  for (i in 2:dim(pred_plot_solo)[1]){
    if(abs(new_subject[,Y]-pred_plot_solo$pred[i])<distance){
      distance<-abs(new_subject[,Y]-pred_plot_solo$pred[i])
      tau_solo<-pred_plot_solo$tau[i]
    }
    if(new_subject[,Y]>=pred_plot_solo$pred_inf[i] & new_subject[,Y]<=pred_plot_solo$pred_sup[i]){
      tau_appartenante[i]<-TRUE
    }else{tau_appartenante[i]<-FALSE}

  }

  if(sum((tau_appartenante==F))==length(tau_appartenante)){
    if(new_subject[,Y]<pred_plot_solo$pred_inf[pred_plot_solo$tau==min(pred_plot_solo$tau)]){
      tau_solo<-paste0("<",min(pred_plot_solo$tau))
      tau_appartenante_solo_print<-paste0("<",min(pred_plot_solo$tau))
      tau_appartenante_solo_print_conf<-paste0("<",round(min(pred_plot_solo$pred_inf),4))
    }
    if(new_subject[,Y]>pred_plot_solo$pred_sup[pred_plot_solo$tau==max(pred_plot_solo$tau)]){
      tau_solo<-paste0(">",max(pred_plot_solo$tau))
      tau_appartenante_solo_print<-paste0(">",max(pred_plot_solo$tau))
      tau_appartenante_solo_print_conf<-paste0(">",round(max(pred_plot_solo$pred_sup),4))
    }

    pred_inf_solo<-"non déterminé"
    pred_sup_solo<-"non déterminé"
  }else{
    # we have the tau of interest now we need to define the interval of precision
    pred_inf_solo<-pred_plot_solo$pred_inf[pred_plot_solo$tau==tau_solo]
    pred_sup_solo<-pred_plot_solo$pred_sup[pred_plot_solo$tau==tau_solo]
    tau_appartenante_solo<-pred_plot_solo$tau[which(tau_appartenante==T)]
    tau_appartenante_solo_print<-c(min(tau_appartenante_solo),max(tau_appartenante_solo))
    tau_appartenante_solo_print_conf<-c(pred_plot_solo$pred_inf[pred_plot_solo$tau==min(tau_appartenante_solo)],pred_plot_solo$pred_sup[pred_plot_solo$tau==max(tau_appartenante_solo)])
  }

  return(list(pred_plot=pred_plot,
              tau_pred=tau_solo,
              pred_inf= pred_inf_solo,
              pred_sup=pred_sup_solo,
              precision=tau_appartenante_solo_print,
              precision_conf=tau_appartenante_solo_print_conf))

}

