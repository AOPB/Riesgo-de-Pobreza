library(spatstat)
library(dineq)
library(viridis)
library(questionr)
library(tidyverse)
setwd("C:/Users/User/Desktop/Friki/ECV/ECV2013")
rm(list=ls())
letras<-c("r","h","d","p")
anos<-c(8:20)

for(i in seq(length(letras))){
  for(j in seq(length(anos))){
    Df<-read_csv(paste("esudb",(anos[j]),letras[i],".csv",sep=""))
    gdata::mv("Df",paste(str_to_upper(letras[i]),(anos[j]-1),sep=""))
  }
}


for(i in 7:19){
  R<-get(paste("R",i, sep=""))%>%
    mutate(HB030=RB030%/%100)
  gdata::mv("R",paste("R",i, sep=""))
  
  
  P<-get(paste("P",i, sep=""))%>%mutate(RB030=PB030)
  gdata::mv("P",paste("P",i, sep=""))
  
  D<-get(paste("D",i, sep="")) %>%
    rename(HB030 = `DB030`)
  gdata::mv("D",paste("D",i, sep=""))
  
  Dataframe<-get(paste("R",i, sep=""))%>%
    left_join(y=get(paste("P",i, sep="")), by="RB030")%>%
    left_join(y=get(paste("H",i, sep="")), by = "HB030")%>%
    left_join(y=get(paste("D",i, sep="")), by= "HB030")%>%
    mutate(IngresEquivalent = vhRentaa/HX240)%>%select(Ponderacion=RB050,
                                                       IngresEquivalent,
                                                       vhPobreza)
  
  gdata::mv("Dataframe",paste("DataFrame",i, sep=""))
  
}

NoRemove<-c()
for (i in 7:19){
  NoRemove[i-6]<-paste("DataFrame",i,sep="")
}
rm(list = setdiff(ls(), NoRemove))



output<-as.data.frame(matrix(nrow = length(7:19)*(length(7:19))+1, ncol=4))
colnames(output)<-c("Año", "PobrezaRelativa","AñoUmbral","Umbral")
IPC<-c(89.051,92.68,92.414,94.977,97.084,99.458,100.859,100.707,100.203,100.000,101.956,103.664,104.389)
AnnoUmbral<-c(2007:2019, "INE")

8379.44*92.68/89.051

for(j in 1:length(AnnoUmbral)){
  if (AnnoUmbral[j]=="INE"){
    for(i in 7:19){
      output[(i-6),1]<-(2000+i)
      output[(i-6),2]<-get(paste("DataFrame",i, sep=""))%>%mutate(Pobres=ifelse(IngresEquivalent<=as.numeric(get(paste("DataFrame",i, sep=""))%>%summarise(median=weighted.median(IngresEquivalent, Ponderacion)*0.60)),Ponderacion,0))%>%summarise(Total=sum(Pobres)*100/sum(Ponderacion))
      output[(i-6),3]<-AnnoUmbral[j]
      output[(i-6),4]<-as.numeric(get(paste("DataFrame",i, sep=""))%>%summarise(median=weighted.median(IngresEquivalent, Ponderacion)*0.60))}}
  else{
    for(i in 7:19){
      output[(i-6)+((length(AnnoUmbral)-1)*j),1]<-(2000+i)
      output[(i-6)+((length(AnnoUmbral)-1)*j),2]<-get(paste("DataFrame",i, sep=""))%>%mutate(Pobres=ifelse(IngresEquivalent<=(as.numeric(get(paste("DataFrame",(as.numeric(AnnoUmbral[j])-2000), sep=""))%>%summarise(median=weighted.median(IngresEquivalent, Ponderacion)*0.60))*(IPC[(i-6)]/IPC[j])),Ponderacion,0))%>%summarise(Total=sum(Pobres)*100/sum(Ponderacion))
      output[(i-6)+((length(AnnoUmbral)-1)*j),3]<-AnnoUmbral[j]
      output[(i-6)+((length(AnnoUmbral)-1)*j),4]<-as.numeric(get(paste("DataFrame",(as.numeric(AnnoUmbral[j])-2000), sep=""))%>%summarise(median=weighted.median(IngresEquivalent, Ponderacion)*0.60))*(IPC[(i-6)]/IPC[j])}
  }
  }




output%>%filter(AñoUmbral=="INE")%>%
  ggplot(aes(x=Año, y=PobrezaRelativa, group=1))+
  geom_line()+geom_point(size=1)+theme_classic()+
  geom_text(aes(label=paste(round(PobrezaRelativa,2),"%", sep="")), size=2, position = position_stack(v=1.01))+
  coord_cartesian(ylim=c(min((output%>%filter(AñoUmbral=="INE"))$PobrezaRelativa)*0.975, max((output%>%filter(AñoUmbral=="INE"))$PobrezaRelativa)*1.025))+
  scale_x_continuous("Año", labels = as.character((output%>%filter(AñoUmbral=="INE"))$Año), breaks = (output%>%filter(AñoUmbral=="INE"))$Año)



require("jpeg")
require("grid")
bill <- readJPEG("pov2.jpg")
bill <- rasterGrob(bill, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

output%>%filter(AñoUmbral=="INE")%>%
  ggplot(aes(x=Año, y=PobrezaRelativa, group=1))+
  geom_line()+geom_point(size=1)+
  annotation_custom(bill, xmin = 2007, xmax = 2019, ymin = 0, ymax = Inf)+
  geom_ribbon(aes(ymin = PobrezaRelativa, ymax = Inf), fill = "white") +
  geom_line(size=1, colour="#000000")+geom_point(size=1)+theme_classic()+
  geom_text(aes(label=paste(round(PobrezaRelativa,2),"%", sep="")), size=3, position = position_stack(v=1.01))+
  coord_cartesian(ylim=c(min((output%>%filter(AñoUmbral=="INE"))$PobrezaRelativa)*0.9, max((output%>%filter(AñoUmbral=="INE"))$PobrezaRelativa)*1.025))+
  labs(title = paste("Evolución de la pobreza relativa 2007-2019"), subtitle=paste("(Umbral ",2007,")",sep=""))+
  theme(plot.title=element_text(h=0.5),
        plot.subtitle=element_text(h=0.5),axis.title.y=element_blank(),
        text = element_text(family="serif",size = 11))+
  scale_x_continuous("Año", labels = as.character((output%>%filter(AñoUmbral=="INE"))$Año), breaks = (output%>%filter(AñoUmbral=="INE"))$Año)+
  scale_y_continuous(labels = function(x) paste0(x, "%"))

setwd("C:/Users/User/Desktop/Friki")
save(output, file="output.Rda")



