summary.bootwrq.cov <-function(object,w_impute=0,...){
  # we need to detach each element
  noms<-colnames(as.matrix(object))[-nrow(as.matrix(object))]
        #tabnoms <- matrix(unlist(strsplit(rownames(object)[-nrow(object)],split="_")),ncol=3,byrow=TRUE)
        # AB need to change to avoid NA
        tabnoms <- matrix(unlist(str_split(rownames(as.matrix(object))[-nrow(as.matrix(object))], "_", n = 2)),ncol=2,byrow=TRUE)
        tau <- unique(as.numeric(tabnoms[,2]))
        object<-as.matrix(object)

        res0 <- NULL
        if(w_impute==0) #poids non recalcules
            {
                for(j in 1:length(tau))
                    {
                        x0tau <- object[grep(paste(tau[j],sep="_"),noms),,drop=FALSE]
                        m0tau <- apply(x0tau,1,mean)
                        res0 <- rbind(res0,m0tau)
                }
        }


        res1 <- NULL
        if(w_impute==1) #poids recalcules
            {
                for(j in 1:length(tau))
                    {
                        x1tau <- object[grep(paste(tau[j],sep="_"),noms),,drop=FALSE]
                        m1tau <- apply(x1tau,1,mean)
                        res1 <- rbind(res1,m1tau)
                }
        }
        #return(invisible(list(results0=res0,results1=res1)))
        # avoid invisible
        return(list(results0=res0,results1=res1))
    }
