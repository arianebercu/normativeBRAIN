predict_WQ<-function(object_boot,object_rq,object_cov=NULL,tau,level,newdata){

  if (missing(newdata))
    stop(paste("The data set of prediction is missing "))
  else {
    tt <- terms(object_rq)
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata,
                     xlev = object_rq$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts.arg = object_rq$contrasts)
    newdata.omit<-newdata[,colnames(newdata)%in%c("NUM_ID","time","AGE_VIS_cr","SEX")]
    newdata.omit<-na.omit(newdata.omit)
    newdata.omit<-newdata.omit[,c("NUM_ID","time")]
  }

  if(class(object_boot)!="bootwrq"){stop("Object_boot need to be of type bootwrq")}
  if(class(object_rq)!="rqs"){stop("Object_rq need to be of type rq")}

  if(is.null(object_cov)){
    object_cov<-summary.bootwrq(object_boot,seed=floor(runif(300)*1000000))$matrix_covariance_res0
  }

  x_boot<-summary.bootwrq(object_boot)
  k<-1

  matrix_coef<-matrix(NA,ncol=dim(object_rq$coefficients)[2],nrow=dim(object_rq$coefficients)[1])
  for (i in 1:length(object_rq$tau)){
    l<-k+dim(object_rq$coefficients)[1]-1
    matrix_coef[,i]<-x_boot$results0[k:l,1]
    k<-l+1
  }


  # each columns is a tau
  # row, i is prediction for subject i according to its values in X and depending on tau
  pred<-drop(X %*% matrix_coef) # for each row prediction of Q(Y) for each tau
  pred_var<-matrix(NA,nrow=dim(pred)[1],ncol=dim(pred)[2])
  k<-1

  for (i in 1 : length(tau)){
    l<-k+dim(matrix_coef)[1]-1
    matrix_cov<-object_cov[k:l,]
    pred_var[,i]<-as.vector(apply(X,MARGIN=1,FUN=function(X){ return(X%*% matrix_cov %*% X)}))
    k<-l+1

  }

  colnames(pred)<-paste0("pred",sep="_",object_rq$tau)
  colnames(pred_var)<-paste0("pred_var",sep="_",object_rq$tau)
  pred_inf<-pred-qnorm(1-level/2,lower.tail=T)*sqrt(pred_var)
  colnames(pred_inf)<-paste0("pred_inf",sep="_",object_rq$tau)
  pred_sup<-pred+qnorm(1-level/2,lower.tail=T)*sqrt(pred_var)
  colnames(pred_sup)<-paste0("pred_sup",sep="_",object_rq$tau)
  pred <- cbind(newdata.omit,pred, pred_var,pred_inf,pred_sup)
  pred<-merge(x=pred,y=newdata,by=c("NUM_ID","time"),all.x=T,all.y=T)
  return(pred)}

