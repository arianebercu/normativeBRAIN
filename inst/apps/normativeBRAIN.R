#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readtext)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(splines)
library(shinyWidgets)
library(normativeBRAIN)

ui<-fluidPage(
  navbarPage(title = " ",
             tabPanel(h4("Normative curves"),
             sidebarLayout(
              sidebarPanel(
  
  
      # volume 
    selectInput(inputId="volume", label="Volume (in % TIV):",
               c("White Matter"="WHITE_MATTER_ICV",
                  "Grey Matter"="GREY_MATTER_ICV",
                  "Hippocampus"="HIPPOCAMPUS_ICV",
                  "Amygdala"="AMYGDALA_ICV",
                  "Frontal lobe"="FRONTAL_LOBE_ICV",
                  "Occipital lobe"="OCCIPITAL_LOBE_ICV",
                  "Parietal lobe"="PARIETAL_LOBE_ICV",
                  "Temporal lobe"="TEMPORAL_LOBE_ICV")),
      

    # Age
   sliderInput(inputId="AGE_VIS",
                        label="Age in years:",
                        min = 50,
                        max = 90,
                        value = c(60,80)),
    
    # sex 
  selectInput(inputId="SEX", label="Sex :",
                c("Male"=0,
                  "Female"=1)),
    
    # tau 
    
 checkboxGroupInput(inputId = "tau", label = "Quantile", 
                       choices = c("0.05", "0.25", "0.50","0.75","0.95"),
                       selected = c("0.05", "0.25", "0.50","0.75","0.95"))
    
    # plot patient
    # selectInput(inputId="obs", label="Would you like to add a patient to the graph ? :",
    #             c("No"=0,
    #               "Yes"=1)),
    # 
    # conditionalPanel(
    #   condition = "input.obs == 1",
    #   selectInput(inputId="sex_bos", label="Sex :",
    #               c("Male"=0,
    #                 "Female"=1))),
    # conditionalPanel(
    #   condition = "input.obs == 1",
    #   numericInput(inputId="age_obs",label="Age (in years)",value=70,min=50,max=90)
    ),
    mainPanel(tabsetPanel(
                 tabPanel("Quantile distribution",
            plotOutput("plot"),
          downloadButton(
            outputId = "download_plot",
            label = "Download plot"
          )
        ),
        tabPanel(
          "Quantile distribution per sex",
           plotOutput("plotcompare"),
      downloadButton(
        outputId = "download_compareplot",
        label = "Download plot per sex")
        )
      )))),

  tabPanel(h4("What about your patient ?"),
    sidebarLayout(
      sidebarPanel(
      
      # volume 
      selectInput(inputId="volume_obs", label="Volume (in % TIV):",
                                 c("White Matter"="WHITE_MATTER_ICV",
                                   "Grey Matter"="GREY_MATTER_ICV",
                                   "Hippocampus"="HIPPOCAMPUS_ICV",
                                   "Amygdala"="AMYGDALA_ICV",
                                   "Frontal lobe"="FRONTAL_LOBE_ICV",
                                   "Occipital lobe"="OCCIPITAL_LOBE_ICV",
                                   "Parietal lobe"="PARIETAL_LOBE_ICV",
                                   "Temporal lobe"="TEMPORAL_LOBE_ICV")),
      numericInput(inputId = "volume_value",label="Value of the volume (in % TIV)",
                                value=30,min=0,max=100),
    numericInput(inputId = "AGE_VIS_obs",label="Age (in years)",
                                value=60,min=50,max=90),
      
      selectInput(inputId="SEX_obs", label="Sex :",
                                          c("Male"=0,
                                            "Female"=1))
  ),
      
      
      
    mainPanel(
      #verbatimTextOutput("value"),
      
      plotOutput("plotpatient"),
               downloadButton(
                 outputId = "download_plotpatient",
                 label = "Download plot patient"
               )
      ))),
 tabPanel(h5("About the methodology"),
          position ="right",
          
          verbatimTextOutput("info"))
  )
  
  )

# Define server logic required to draw a histogram
server<-function(input, output) {
  
   my_data<-reactive({

     data_model<-data.frame(volume=c("WHITE_MATTER_ICV",
                                     "GREY_MATTER_ICV",
                                     "HIPPOCAMPUS_ICV",
                                     "AMYGDALA_ICV",
                                     "FRONTAL_LOBE_ICV",
                                     "OCCIPITAL_LOBE_ICV",
                                     "PARIETAL_LOBE_ICV",
                                     "TEMPORAL_LOBE_ICV"),
                            model_keep=c(4,9,9,9,9,9,9,4))
     
     my_data<-data_model[data_model$volume==input$volume,]


     # data <- read.csv("data/memento_SEXE_QYNAPSE_120122.txt", sep="")
     # data$AGE_VIS_cr<-(data$AGE_VIS-70)/9
     # essai<-normativeBRAIN::prepare_data(data=data,volume=my_data$volume,rescale=F)
     # model_data<-normativeBRAIN::weightsIMD(data=essai,Y=my_data$volume,X1=c("SEX","AGE0_cr"),
     #                                     X2=NULL,subject="NUM_ID",
     #                                     death="death",time="time",name="w_imd")
     # 
     
     if(my_data$model_keep==1){
       
       load(paste0("data/linear/linear_model_",my_data$volume,".RData"))
       
     }
     
     if(my_data$model_keep==2){
       
       load(paste0("data/quadratique/quadratique_model_",my_data$volume,".RData"))
     }
     
     if(my_data$model_keep==3){
       
       load(paste0("data/ns1quant/ns1quant_model_",my_data$volume,".RData"))
     }
     
     if(my_data$model_keep==4){
       
       load(paste0("data/ns2quant/ns2quant_model_",my_data$volume,".RData"))
     }
     
     if(my_data$model_keep==5){
       
       load(paste0("data/ns3quant/ns3quant_model_",my_data$volume,".RData"))
     }
     
     
     if(my_data$model_keep==6){
       
       load(paste0("data/ns4quant/ns4quant_model_",my_data$volume,".RData"))
     }
     
     if(my_data$model_keep==7){
       load(paste0("data/ns5quant/ns5quant_model_",my_data$volume,".RData"))
     }
     
     if(my_data$model_keep==8){
       load(paste0("data/ns6quant/ns6quant_model_",my_data$volume,".RData"))
     }
     
     if(my_data$model_keep==9){
       load(paste0("data/ns1fix/ns1fix_model_",my_data$volume,".RData"))
     }
     
     if(my_data$model_keep==10){
       load(paste0("data/ns2fix/ns2fix_model_",my_data$volume,".RData"))
     }
     
     if(my_data$model_keep==11){
       load(paste0("data/ns3fix/ns3fix_model_",my_data$volume,".RData"))
     }
     
     
     if(my_data$model_keep==12){
       load(paste0("data/ns5fix/ns5fix_model_",my_data$volume,".RData"))
     }
     
     tau<-c(0.05,0.25,0.50,0.75,0.95)
    
     newdata_male<-data.frame(time=(seq(input$AGE_VIS[1],input$AGE_VIS[2],length.out=100)-70)/9,
                       AGE_VIS_cr=(seq(input$AGE_VIS[1],input$AGE_VIS[2],length.out=100)-70)/9,
                       SEX=rep(0,100),
                       NUM_ID=rep(1,100))
     pred<-normativeBRAIN::predict_WQ(object_boot=boot,
                                   object_rq=rq,
                                   object_cov=boot_cov,newdata=newdata_male,tau=tau,level = 0.05)
     pred_all_male<-normativeBRAIN::data_plot(pred)
     
     newdata_female<-data.frame(time=(seq(input$AGE_VIS[1],input$AGE_VIS[2],length.out=100)-70)/9,
                              AGE_VIS_cr=(seq(input$AGE_VIS[1],input$AGE_VIS[2],length.out=100)-70)/9,
                              SEX=rep(1,100),
                              NUM_ID=rep(1,100))
     pred<-normativeBRAIN::predict_WQ(object_boot=boot,
                                   object_rq=rq,
                                   object_cov=boot_cov,newdata=newdata_female,tau=tau,level = 0.05)
     pred_all_female<-normativeBRAIN::data_plot(pred)
     #tau_col<-c("purple","blue","black","red","brown")
     #tau_col<-tau_col[tau%in%as.numeric(input$tau)]
     #tau<-tau[tau%in%as.numeric(input$tau)]
     pred_all<-rbind(pred_all_male,pred_all_female)
     return(pred_all)
                  
       })
   plot_1<-reactive({
     data<-my_data()
     tau<-c(0.05,0.25,0.5,0.75,0.95)
     col_tau<-c("purple","blue","black","green","darkgreen")
     col_tau<-col_tau[tau%in%as.numeric(input$tau)]
     tau<-tau[tau%in%as.numeric(input$tau)]
     p1<-ggplot(data=data[data$SEX%in%as.numeric(input$SEX) & data$tau%in%as.numeric(input$tau),],aes(x=(AGE_VIS_cr*9)+70,y=pred,colour=tau))+
                 geom_ribbon(aes(ymin=pred_inf, ymax=pred_sup,group=tau), alpha=0.1,color="grey")+
                  geom_line()+theme_bw()+xlab("Age (in years)")+ylab("Volume (in % TIV)")+
       scale_color_manual(name="Quantile",
                          values=col_tau,labels=tau)
     return(p1)
        
   })
   
   output$plot<-renderPlot(plot_1())
  
   plot_2<-reactive({
     data<-my_data()
     data$SEX<-ifelse(data$SEX==0,"Male","Female")
     colnames(data)[colnames(data)=="SEX"]<-"Sex"
     tau<-c(0.05,0.25,0.5,0.75,0.95)
     col_tau<-c("purple","blue","black","green","darkgreen")
     col_tau<-col_tau[tau%in%as.numeric(input$tau)]
     tau<-tau[tau%in%as.numeric(input$tau)]
     p1<-ggplot(data=data[data$tau%in%as.numeric(input$tau),],aes(x=(AGE_VIS_cr*9)+70,y=pred,color=tau,fill=tau,linetype=Sex))+
       geom_ribbon(aes(ymin=pred_inf, ymax=pred_sup), alpha=0.1,color="grey")+
       geom_line()+theme_bw()+xlab("Age(in years)")+ylab("Volume (in % TIV)")+
       scale_color_manual(name="Quantile",
                          values=col_tau,labels=tau)+
       scale_fill_manual(name="Quantile",
                          values=col_tau,labels=tau)
     return(p1)
     
   })
   
   output$plotcompare<-renderPlot(plot_2())
   
   
   output$download_plot <- downloadHandler(
     filename = function() {
       stringr::str_glue("plot_{input$volume}.png")
     },
     
     content = function(file) {
       ggsave(file, 
              plot_1(),
              width = 8, height = 5, dpi = 300)
     }
     
   )
   
   output$download_compareplot <- downloadHandler(
     filename = function() {
       stringr::str_glue("plot_compare_sex_{input$volume}.png")
     },
     
     content = function(file) {
       ggsave(file, 
              plot_2(),
              width = 8, height = 5, dpi = 300)
     }
     
   )
   
   # val<-reactive({
   #   browser()
   #   input$AGE_VIS_obs
   #   if(input$AGE_VIS_obs<50 | input$AGE_VIS_obs>90){
   #     if(input$volume_value<0 | input$volume_value>100){
   #       return("Warnings : Age should be between 50 and 90 years of age. \n Volume in % of TIV should be between 0 and 100")
   #     }else{
   #       return("Warnings : Age should be between 50 and 90 years of age")}
   #   }else{return(" ")}
   # })
   
   # output$value<-renderText(val())
   # browser()
   # if(val()!=" "){
   my_data_obs<-reactive({
     
     # data <- read.csv("data/memento_SEXE_QYNAPSE_120122.txt", sep="")
     # data$AGE_VIS_cr<-(data$AGE_VIS-70)/9
     # essai<-normativeBRAIN::prepare_data(data=data,volume=my_data$volume,rescale=F)
     # model_data<-normativeBRAIN::weightsIMD(data=essai,Y=my_data$volume,X1=c("SEX","AGE0_cr"),
     #                                     X2=NULL,subject="NUM_ID",
     #                                     death="death",time="time",name="w_imd")
     # 
       
       load(paste0("data/predictions/model_",input$volume_obs,"_0.01.RData"))
       
       tau<-seq(0.05,0.95,0.01)
       time<-(seq(50,90,length.out=100)-70)/9
       if(is.na(input$AGE_VIS_obs) | input$AGE_VIS_obs<50 | input$AGE_VIS_obs>90){
         age_obs<-(50-70)/9
       }else{age_obs<-(input$AGE_VIS_obs-70)/9}
       time<-c(time,age_obs)
     newdata<-data.frame(time=time,
                              AGE_VIS_cr=time,
                              SEX=rep(input$SEX_obs,101),
                              NUM_ID=rep(1,101))
     newdata$SEX<-as.numeric(newdata$SEX)
     
     pred<-normativeBRAIN::predict_WQ(object_boot=model$list_boot,
                                   object_rq=model$list_rq,
                                   object_cov=model$list_cov,newdata=newdata,tau=tau,level = 0.05)
     pred_all<-normativeBRAIN::data_plot(pred)
     
     if(is.na(input$volume_value)|is.na(input$AGE_VIS_obs)|input$volume_value<0 | input$volume_value>100 | input$AGE_VIS_obs<50 | input$AGE_VIS_obs>90){
       pred_all$tau_appartenance<-NA
     }else{
       distance<-abs(input$volume_value-pred[pred$time==age_obs,"pred_0.05"])
       tau_appartenante<-0.05
       for(i in 2:length(tau)){
         distance2<-abs(input$volume_value-pred[pred$time==age_obs,paste0("pred_",tau[i])])
         tau_appartenante<-ifelse(distance2<=distance,tau[i],tau_appartenante)
         distance<-ifelse(distance2<=distance,distance2,distance)
       }
       pred_all$tau_appartenance<-tau_appartenante}
       
    
     return(pred_all)
     
   })
   plot_3<-reactive({
     data<-my_data_obs()
     
     tau<-unique(na.omit(c(0.05,0.25,0.5,0.75,0.95,data$tau_appartenance)))
     col_tau<-c("purple","blue","black","green","darkgreen","red")
     col_tau<-col_tau[order(tau)]
     tau<-sort(tau)
     data<- data[ data$tau%in%tau,]

     if(is.na(input$volume_value)|is.na(input$AGE_VIS_obs)|input$volume_value<0 | input$volume_value>100 | input$AGE_VIS_obs<50 | input$AGE_VIS_obs>90){
       p1<-ggplot(data=data,aes(x=(AGE_VIS_cr*9)+70,y=pred,colour=tau))+
         geom_ribbon(aes(ymin=pred_inf, ymax=pred_sup,group=tau), alpha=0.1,color="grey")+
         geom_line()+theme_bw()+xlab("Age (in years)")+ylab("Volume (in % TIV)")+
         scale_color_manual(name="Quantile",
                            values=col_tau,labels=tau)+ggtitle("Could not add patient to plot, \n we need to have 50 ≤ Age ≤ 90 and 0 ≤ volume value ≤ 100.")+
         theme(
           plot.title = element_text(color = "red", size = 15, face = "bold"))
       }else{
     p1<-ggplot(data=data,aes(x=(AGE_VIS_cr*9)+70,y=pred,colour=tau))+
       geom_ribbon(aes(ymin=pred_inf, ymax=pred_sup,group=tau), alpha=0.1,color="grey")+
       geom_line()+theme_bw()+xlab("Age (in years)")+ylab("Volume (in % TIV)")+
       scale_color_manual(name="Quantile",
                          values=col_tau,labels=tau)+
       geom_point(aes(y=as.numeric(input$volume_value),x=as.numeric(input$AGE_VIS_obs)),colour="red")
     }
     return(p1)
     
   })
   
   output$plotpatient<-renderPlot(plot_3())
   
   output$download_plotpatient <- downloadHandler(
     filename = function() {
       stringr::str_glue("plot_patient_{input$volume}.png")
     },
     
     content = function(file) {
       ggsave(file, 
              plot_3(),
              width = 8, height = 5, dpi = 300)
     }
     
   )
   output$info<-renderText({
     readtext::readtext(file="INFO.txt")$text
     #read.csv(file="INFO.txt")
     })
   
     
     
     
   
   
   
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
