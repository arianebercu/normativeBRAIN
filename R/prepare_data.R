

prepare_data<-function(data,volume,rescale=F){


  if(rescale==T){
    data$cr<-(data[,volume]-mean(na.omit(data[,volume])))/sd(na.omit(data[,volume]))
    name_vol<-paste0(volume,sep="_","cr")
    names(data)[names(data)=="cr"]<-name_vol}


  data$time<-ifelse(data$VISITNUM=="M0",1,
                    ifelse(data$VISITNUM=="M24",2,3))

  # recode in binary 0 -- 0 et 0.5 -- 1
  data$CDRSCR0<-ifelse(data$CDRSCR0==0,0,1)

  # recode indic_DEM_before_M24
  data$indic_DEM_before_M48<-ifelse(data$indic_DEM_before_M48=="Non dément",0,1)

  # recode indic_DEM_before_M24
  data$ID_IRM<-ifelse(data$VISITNUM=="M0",1,
                      ifelse(data$VISITNUM=="M24",2,
                             ifelse(data$VISITNUM=="M48",3,NA)))

  # recode death

data$death<-NA
for(i in unique(data$NUM_ID)){
    sub_data<-data[data$NUM_ID==i,]
    if(unique(sub_data$EVT_DC_indep_M48)=="Non décédé"){
      data$death[data$NUM_ID==i]<-NA
    }else{
      if(sum(is.na(sub_data$AGE_VIS))==2){
        data$death[data$NUM_ID==i]<-ifelse(sub_data$AGE_DC[1]<=(sub_data$AGE_VIS[1]+2.5),2,
                                            ifelse(sub_data$AGE_DC[1]<=(sub_data$AGE_VIS[1]+4.5),3,NA))

      }
      if(sum(is.na(sub_data$AGE_VIS))==1){

        if(!is.na(sub_data$AGE_VIS[2])){
        data$death[data$NUM_ID==i]<-ifelse(sub_data$AGE_DC[1]<=(sub_data$AGE_VIS[2]+2.5),3,NA)}

        if(!is.na(sub_data$AGE_VIS[3])){
          data$death[data$NUM_ID==i]<-NA}
      }
    }
}



  # recode sex
  data$SEX<-ifelse(data$SEX=="Masculin",0,1)

  # recode subject

  data$NUM_ID<-as.numeric(gsub("SUBJ", "", data$NUM_ID))

  # center and reduce AGE0

  data$AGE0_cr<-(data$AGE0-mean(data$AGE0))/sd(data$AGE0)

  # variable of interest
  if(rescale==T){
    data<-data[,colnames(data)%in%c("NUM_ID",volume,name_vol,"SEX","NIVETUD","CDRSCR0","X3T","AGE0","AGE0_cr","death","time","delai","AGE_VIS","AGE_VIS_cr","indic_DEM_before_M48","EVT_M48")]

  }else{data<-data[,colnames(data)%in%c("NUM_ID",volume,"SEX","NIVETUD","CDRSCR0","X3T","AGE0","AGE0_cr","death","time","delai","AGE_VIS","AGE_VIS_cr","indic_DEM_before_M48","EVT_M48")]}

  return(data)
}
