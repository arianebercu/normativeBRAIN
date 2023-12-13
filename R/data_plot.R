data_plot<-function(pred){

  pred<-as.data.frame(pred)
  # have tau
  colkeep<-colnames(pred)[!str_detect(colnames(pred),"pred_")]
  inf<-pred[,str_detect(colnames(pred),"_inf_")]
  inf<-as.data.frame(inf)
  object_pred<-tidyr::gather(inf)$key
  object_pred<-unlist(str_split(as.matrix(object_pred), "_", n = 3))
  tau_pred<-object_pred[str_detect(object_pred,"[0-1]")]
  tau_pred<-unique(tau_pred)
  colnames(pred)[str_detect(colnames(pred),"pred")]<-paste0(colnames(pred)[str_detect(colnames(pred),"pred")],"_")
  pred_long<-NULL

  tau_bind<-NULL
  for( i in tau_pred){
    pred_sub<-pred[,str_detect(colnames(pred),paste0(i,"_"))| colnames(pred)%in% c(colkeep)]
    name<-colnames(pred_sub)
    tau_bind<-c(tau_bind,rep(i,dim(pred_sub)[1]))
    colnames(pred_sub)<-NA
    pred_long<-rbind(pred_long,pred_sub)
  }
  name<-str_replace(name,paste0("_",i,"_"),"")
  colnames(pred_long)<-name
  pred_long$tau<-tau_bind
  return(pred_long)

}
