library(readxl)
library(MASS)
library(ggplot2)
library(dplyr)
library(plotly)
library(caret)
library(reshape2)
library(ggExtra)
library(gridExtra)

data1 <- read_excel("~/Downloads/mvs.xlsx")



########################11.25#############################
## x1=(cash flow)/(total debt)
## x2=(Net Income)/(Total Asset)
## x3=(Current Assets)/(Current Liabilities)
## x4=(Current Asset)/(Net Sales)
##########################################################
summary(data1)

class0<-subset(data1,population==0,select = c(ROW,x1,x3))

class1<-subset(data1,population==1,select = c(ROW,x1,x3))

summary(class0)
length(class0$x1)

summary(class1)
length(class1$x1)


data1$type<-factor(data1$population,labels = c("Bankrupt Firms","Sound Firms"))

#############################################3


ggMarginal(
  ggplot(data1,aes(x1,x3,color=factor(type)))+geom_point()+geom_text(aes(label=ROW),hjust=0,vjust=0)+labs(title = "Data Distribution",color="Firm types"),
  type = 'densigram',
  margins = 'both',
  size = 5,
  colour = '#FF0000',
  groupFill = TRUE
)

#################Total count of Bankrupt Firms and Sound Firms



c1<-(ggplot(data1,aes(factor(type),fill=factor(type)))+geom_bar()+
  labs(fill="Class",y="Count",x="",title = "Total count of Bankrupt Firms and Sound Firms")

)

melt1 <- melt(data1,id.vars = c('ROW','type'),measure.vars = c("x1","x3"))


c2<-(melt1%>%ggplot(aes(x=factor(type),y=value,fill=factor(type)))+geom_boxplot()+facet_grid(~variable,scales="free_y")+
  labs(fill="Firm Types",x="",y="Value",title = "Data distribution of class X1 and X3")

)


grid.arrange(c1,c2,nrow=1)


########################(a)##################################
fit=lda(population~x1+x3,data1,prior=c(1,1)/2)

fitvalues <- predict(fit,data1)

fitclass <- predict(fit)$class

data1$check<-data1$population==fitclass



d1<-(data1 %>% ggplot(aes(y=fitvalues$x[,1],x=c(1:nrow(fitvalues$x)),col=fitclass,shape=fitclass,size=check))+geom_point()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+labs(color="Predicted Class",shape="",size="Correctness",
                                                             x="Index",y="LDA AXIS 1",title = "Data and the discriminant line")+theme(legend.position = "none")

)
k<-data1 %>% ggplot(aes(x=fitvalues$x[,1],y=0,col=fitclass,shape=fitclass,size=check))+geom_point()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+labs(color="Predicted Class",shape="",size="Correctness",
                                                             x="Index",y="LDA AXIS 1",title = "Data and the discriminant line")

d2<-(ggMarginal(
  k,
  type = 'densigram',
  margins = 'x',
  size = 5,
  colour = '#FF0000',
  groupFill = TRUE
)

)


grid.arrange(d1,d2,nrow=1)


fit
ct1<-table(data1$population,fitclass)
ct1
diag(prop.table(ct1, 1))




# APER
APER1=1-sum(diag(prop.table(ct1)))    
APER1






####################Why those two points#############################

APERvec=c()

AER=0
classcall=c()
for (i in 1:length(data1$ROW)) {
  dataj<-data1[-i,]
  fitj<-lda(population~x1+x3,dataj,prior=c(1,1)/2)
  fitvaluesj <- predict(fitj,dataj)
  classcall=c(classcall,data1$population[i])
  fitclassj <- fitvaluesj$class
  AER<-0
 
 
  AER<-(1-sum(diag(prop.table(table(dataj$population,fitclassj)))))
  #print(AER)
  APERvec<-c(APERvec,AER)
}
APERvec

APERDATA=as.data.frame(cbind(data1$ROW,APERvec,data1$population))
colnames(APERDATA)<-c("Row","Value","Population")
aer=(sum(APERvec))/(length(APERvec))

p1<-(ggplot(APERDATA,aes(y=APERvec,x=c(1: length(Row)),color=factor(classcall)),label=c(Row,Population))+
  geom_point()+
  geom_line()+
  theme_bw()+
  geom_text(aes(label=Row),hjust=0, vjust=0)+
  annotate("text",x=6,y=APER1,label=paste0("APER=",APER1),vjust=1,hjust=0)+
  annotate("text",x=6,y=aer,label=paste0("AER=",aer),vjust=1,hjust=0)+
  geom_hline(yintercept = APER1)+geom_hline(yintercept = aer)+
  labs(x="",title="APER with individual points eliminated")+theme(legend.position = "none")

)

##############
p<-ggplot(data1,aes(x1,x3,color=factor(type)))+geom_point()+geom_text(aes(label=ROW),hjust=0,vjust=0)+labs(title = "Data Distribution",color="Firm types")


p2<-(ggMarginal(
  p,
  type = 'densigram',
  margins = 'both',
  size = 5,
  colour = '#FF0000',
  groupFill = TRUE
)
)

grid.arrange(p1,p2,nrow=1)

##################(b)   part 1######################


datanew1<-data1[-which(data1$ROW==16 & data1$population==0),]

fit=lda(population~x1+x3,datanew1,prior=c(1,1)/2)

fitvalues <- predict(fit,datanew1)

fitclass <- predict(fit)$class



ct2<-table(datanew1$population,fitclass)
ct2
diag(prop.table(ct2, 1))

d3<-(datanew1 %>% ggplot(aes(y=fitvalues$x[,1],x=c(1:nrow(fitvalues$x)),col=fitclass,shape=fitclass,size=check))+geom_point()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+labs(color="Predicted Class",shape="",size="Correctness",
                                                             x="Index",y="LDA AXIS 1",title = "Data and the discriminant line")+theme(legend.position = "none")
)

d4<-(datanew1 %>% ggplot(aes(x=fitvalues$x[,1],y=0,col=fitclass,shape=fitclass,size=check))+geom_point()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+labs(color="Predicted Class",shape="",size="Correctness",
                                                             x="Index",y="LDA AXIS 1",title = "Data and the discriminant line")

)

d5<-(ggMarginal(
  d4,
  type = 'densigram',
  margins = 'x',
  size = 5,
  colour = '#FF0000',
  groupFill = TRUE
)
)


# APER
APER2=1-sum(diag(prop.table(ct2)))
APER2

grid.arrange(d3,d5,nrow=1)

####################(b)   part 2 ##########################


datanew2<-data1[-which(data1$ROW==13 & data1$population==1),]

fit=lda(population~x1+x3,datanew2,prior=c(1,1)/2)

fitvalues <- predict(fit,datanew2)

fitclass <- predict(fit)$class


ct3<-table(datanew2$population,fitclass)
ct3
diag(prop.table(ct3, 1))


l1<-(datanew2 %>% ggplot(aes(y=fitvalues$x[,1],x=c(1:nrow(fitvalues$x)),col=fitclass,shape=fitclass,size=check))+geom_point()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+labs(color="Predicted Class",shape="",size="Correctness",
                                                             x="Index",y="LDA AXIS 1",title = "Data and the discriminant line")+theme(legend.position = "none")
)

l2<-(datanew2 %>% ggplot(aes(x=fitvalues$x[,1],y=0,col=fitclass,shape=fitclass,size=check))+geom_point()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+labs(color="Predicted Class",shape="",size="Correctness",
                                                             x="Index",y="LDA AXIS 1",title = "Data and the discriminant line")

)


l3<-(ggMarginal(
  l2,
  type = 'densigram',
  margins = 'x',
  size = 5,
  colour = '#FF0000',
  groupFill = TRUE
)
)

grid.arrange(l1,l3,nrow=1)

# APER

APER3=1-sum(diag(prop.table(ct3)))
APER3




###########Change in APER #############

ggplot(mapping = aes(y=factor(c(APER2,APER1,APER3)),x=factor(c(1,2,3)),col=factor(c(1,2,3))))+
  geom_point()+
  scale_x_discrete(name ="",
                   labels=c("1"="APER2","2"="APER1","3"="APER3"))+
  scale_y_discrete(name="Values")+theme(legend.position = "none")+
  labs(title = "Different APER of three problem statements")



##  This shows that although deleting the observation in row 16 for bankrupt
##  firms has no such significant effect over APER but deleting the observation
##  in row 13 for sound firms changed the APER a lot. This signifies that the
##  row 13 of sound firms is a bit influential.

