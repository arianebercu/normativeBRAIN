predict_rq <-function (object, newdata, level = 0.95,R=100)
{
    if (missing(newdata))
        return(object$fitted)
    else {
        tt <- terms(object)
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata,
            xlev = object$xlevels)
        if (!is.null(cl <- attr(Terms, "dataClasses")))
            .checkMFClasses(cl, m)
        X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    }
  # get back coefficients
  matrix_coef<-object$coefficients
  pred <- drop(X %*% object$coefficients)
  cov<-summary(object,se="boot",R=R,cov=T) # creation de la matrice de covariance

  pred_var<-matrix(NA,nrow=dim(pred)[1],ncol=dim(pred)[2])
    for (i in 1 : length(object$tau)){
      matrix_cov<-cov[[i]]$cov
      pred_var[,i]<-as.vector(apply(X,MARGIN=1,FUN=function(X){ return(X%*% matrix_cov %*% X)}))

    }
  colnames(pred)<-paste0("pred",sep="_",object$tau)
  colnames(pred_var)<-paste0("pred_var",sep="_",object$tau)
  pred_inf<-pred-qnorm(1-level/2,lower.tail=T)*sqrt(pred_var)
  colnames(pred_inf)<-paste0("pred_inf",sep="_",object$tau)
  pred_sup<-pred+qnorm(1-level/2,lower.tail=T)*sqrt(pred_var)
  colnames(pred_sup)<-paste0("pred_sup",sep="_",object$tau)
  pred <- cbind(newdata,pred, pred_var,pred_inf,pred_sup)
  return(pred)
}
