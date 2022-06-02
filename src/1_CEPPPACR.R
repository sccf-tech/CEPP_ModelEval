## Initiate project structure
## Created by: Paul Julian (pjulian@sccf.org)
## Created on: 05/18/2022

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(dssrip)

library(rcompanion)
library(flextable)
library(magrittr)

## Paths
wd="C:/Julian_LaCie/_GitHub/CEPP_ModelEval"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

## Functions
consec.startend=function(var){
  runs=rle(var)
  myruns = which(runs$values == TRUE)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  rslt=list(starts=starts,ends=ends)
  return(rslt)
}


# -------------------------------------------------------------------------
Alts=c("ECB","FWO","C240")
n.alts=length(Alts)

cols.alts=c(grey.colors(2),wesanderson::wes_palette("Zissou1",1,"continuous"))
alts=Alts
# Discharge ---------------------------------------------------------------
RSM.sites=c("S77","S77BF","S79","S79_QPFCSOURCE_LAKE",
            "S80","S308","S308BF",
            "TMC2EST","S48","S49","NSF2EST",
            "S80_QFC","C10A","S155A","S354","S351",
            "S351_QFC","S351_FC_SHIFT2_ENVTARG","S354_QFC","S354_FC_SHIFT2_ENVTARG",
            "S308_QFC","S77_QFC","C10A_QFC",
            "S8_QFC","S150_QFC","S7_QFC","S6_QFC",
            "S8","S150","S7","S6")

# q.dat=data.frame()
# for(j in 1:length(Alts)){
#   dss_out=opendss(paste0(data.path,Alts[j],"/RSMBN_output.dss"))
#   for(i in 1:length(RSM.sites)){
#     paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2005/1DAY/SIMULATED/")
#     tmp=data.frame(getFullTSC(dss_out,paths))
#     tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
#     rownames(tmp)<-NULL
#     tmp=tmp[,c(2,1)]
#     colnames(tmp)=c("Date","Data.Value")
#     tmp$parameter="FLOW"
#     tmp$SITE=RSM.sites[i]
#     tmp$Alt=Alts[j]
#     q.dat=rbind(tmp,q.dat)
#     print(i)
#   }
# }
# for(j in 2:n.alts){
#   dss_out=opendss(paste0(data.path,Alts[j],"/RSMBN_output.dss"))  
#   
#   paths=paste0("/RSMBN/STA2TMC/FLOW/01JAN1965 - 01JAN2005/1DAY/SIMULATED/")  
#   tmp=data.frame(getFullTSC(dss_out,paths))
#   tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
#   rownames(tmp)<-NULL
#   tmp=tmp[,c(2,1)]
#   colnames(tmp)=c("Date","Data.Value")
#   tmp$parameter="FLOW"
#   tmp$SITE="STA2TMC"
#   tmp$Alt=Alts[j]
#   q.dat=rbind(tmp,q.dat)
#   print(j)
# }
# range(q.dat$Date)
# unique(q.dat$parameter)
# unique(q.dat$SITE)
# unique(q.dat$Alt)
# 
# q.dat=q.dat[order(q.dat$Alt,q.dat$SITE,q.dat$Date),]
# q.dat$CY=as.numeric(format(q.dat$Date,"%Y"))
# q.dat$month=as.numeric(format(q.dat$Date,"%m"))
# 
# save(q.dat,file=paste0(data.path,"q_dat.RData"))
load(paste0(data.path,"q_dat.RData"))


RSM.sites.ENP=c(paste0("S12",LETTERS[1:4]),"S333","S334",paste0("S355",LETTERS[1:2]),"S356","S335");#S333N
# rsmgl.alts=c("rsmgl/EARECB/output_110317_svn12388_rsm5205",
#              "rsmgl/EARFWO/output_110217_svn12388_rsm5205",
#              "rsmgl/C240/output_011418_svn12773_rsm5207")
# q.dat.GL=data.frame()
# for(j in 1:length(Alts)){
#   dss_out=opendss(paste0(data.path,rsmgl.alts[j],"/RSMGL_CEPP_output.dss"))
#   for(i in 1:length(RSM.sites.ENP)){
#     paths=paste0("/RSMGL_CEPP/",RSM.sites.ENP[i],"/FLOW/01JAN1965 - 01JAN2005/1DAY/SIMULATED/")
#     tmp=data.frame(getFullTSC(dss_out,paths))
#     tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
#     rownames(tmp)<-NULL
#     tmp=tmp[,c(2,1)]
#     colnames(tmp)=c("Date","Data.Value")
#     tmp$parameter="FLOW"
#     tmp$SITE=RSM.sites.ENP[i]
#     tmp$Alt=Alts[j]
#     q.dat.GL=rbind(tmp,q.dat.GL)
#     print(i)
#   }
# }
# range(q.dat.GL$Date)
# unique(q.dat.GL$parameter)
# unique(q.dat.GL$SITE)
# unique(q.dat.GL$Alt)

# q.dat.GL=q.dat.GL[order(q.dat.GL$Alt,q.dat.GL$SITE,q.dat.GL$Date),]
# q.dat.GL$CY=as.numeric(format(q.dat.GL$Date,"%Y"))
# q.dat.GL$month=as.numeric(format(q.dat.GL$Date,"%m"))

# save(q.dat.GL,file=paste0(data.path,"q_dat_GL.RData"))
load(paste0(data.path,"q_dat_GL.RData"))

q.dat.xtab=reshape2::dcast(q.dat,Alt+Date+CY+month~SITE,value.var="Data.Value",function(x)mean(x,na.rm=T))
head(q.dat.xtab)
q.dat.xtab$S79.14d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80.14d=with(q.dat.xtab,ave(S80,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S79.30d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,29),rollapply(x,width=30,FUN=function(x)mean(x,na.rm=T)))))
length(unique(q.dat.xtab$CY))
## SLE GW -----------------------------------------------------------------
## gw data in LOWRP data folder
sle.gw=read.table("C:/Julian_LaCie/_GitHub/LOWRP_ModelEval/Data/gw_flws.txt",header=F,col.names = c("Date","sle_gw"))
sle.gw$Date=with(sle.gw,date.fun(Date,form="%m/%d/%Y"))
q.dat.xtab=merge(q.dat.xtab,sle.gw,"Date",all.x=T)
q.dat.xtab=q.dat.xtab[order(q.dat.xtab$Alt,q.dat.xtab$Date),]

## flood control ----------------------------------------------------------
RSM.sites.region=data.frame(SITE=c("S351_QFC","S351_FC_SHIFT2_ENVTARG",
                                   "S354_QFC","S354_FC_SHIFT2_ENVTARG",
                                   "S77_QFC","S308_QFC","C10A_QFC"),
                            Region=c(rep("WCAs",4),"Cal",'StL',"LWLagoon"))

regq.dat=merge(subset(q.dat,SITE%in%RSM.sites.region$SITE),RSM.sites.region,"SITE")

regq.dat.CY=ddply(regq.dat,c("CY","Alt",'Region'),summarise,
                  TFlow.kAcft=sum(cfs.to.acftd(Data.Value),na.rm=T)/1000,
                  TFlow.km3=sum(cfs.to.km3d(Data.Value)))
regq.dat.CY$Alt_Region=with(regq.dat.CY,paste(Alt,Region,sep="_"))
regq.dat.CY$Alt=factor(regq.dat.CY$Alt,levels=Alts)
# regq.dat.CY.mean=reshape2::dcast(regq.dat.CY,Alt~Region,value.var = "TFlow.kAcft",mean)
# regq.dat.CY.mean=regq.dat.CY.mean[match(alts,regq.dat.CY.mean$Alt),]
# regq.dat.CY.mean=regq.dat.CY.mean[,c("Alt","WCAs","Cal","StL","LWLagoon")]
# regq.dat.CY.mean=regq.dat.CY.mean[match(Alts,regq.dat.CY.mean$Alt),]
regq.dat.CY.mean=reshape2::dcast(regq.dat.CY,Region~Alt,value.var = "TFlow.kAcft",mean)
regq.dat.CY.mean$PerDiff_FWO=with(regq.dat.CY.mean,((C240-FWO)/FWO)*100)
regq.dat.CY.mean$PerDiff_ECB=with(regq.dat.CY.mean,((C240-ECB)/ECB)*100)
# write.csv(regq.dat.CY.mean,paste0(export.path,"/LOK_regQ.csv"),row.names = F)

dput(regq.dat.CY.mean[match(c("WCAs","Cal","StL","LWLagoon"),regq.dat.CY.mean$Region),])


ddply(regq.dat.CY,c("Region"),summarise,
      stat=kruskal.test(TFlow.kAcft~Alt)$statistic,
      pval=round(kruskal.test(TFlow.kAcft~Alt)$p.value,3))

with(subset(regq.dat.CY,Region=="WCAs"),dunn.test::dunn.test(TFlow.kAcft,Alt))
with(subset(regq.dat.CY,Region=="Cal"),dunn.test::dunn.test(TFlow.kAcft,Alt))
with(subset(regq.dat.CY,Region=="StL"),dunn.test::dunn.test(TFlow.kAcft,Alt))
with(subset(regq.dat.CY,Region=="LWLagoon"),dunn.test::dunn.test(TFlow.kAcft,Alt))

# png(filename=paste0(plot.path,"AvgFloodControl2.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.25,0.5),oma=c(2,2,0.5,0.25),lwd=0.5);
layout(matrix(1:4,2,2,byrow=T))

ylim.val=c(0,2.25);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TFlow.km3~Alt,subset(regq.dat.CY,Region=="Cal"),
        ylim=ylim.val,col=adjustcolor(cols.alts,0.5),outline=F,ann=F,axes=F)
with(aggregate(TFlow.km3~Alt,subset(regq.dat.CY,Region=="Cal"),mean),
     points(1:3,TFlow.km3,pch=21,bg=adjustcolor("white",0.5),cex=1.25,lwd=0.1))
DT=with(subset(regq.dat.CY,Region=="Cal"),dunn.test::dunn.test(TFlow.kAcft,Alt))
DT.ltr=cldList(P.adjusted ~ comparison,data=DT,threshold = 0.05)
DT.ltr$Letter=toupper(DT.ltr$Letter)
DT.ltr=DT.ltr[order(match(DT.ltr$Group,c("ECB","FWO",'C24'))),]
text(1:5,x$stats[5,],DT.ltr$Letter,pos=3)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:3,1:3,NA);box(lwd=1)
mtext(side=3,line=-1.25,"  CRE",adj=0,font=4)

ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TFlow.km3~Alt,subset(regq.dat.CY,Region=="StL"),
          ylim=ylim.val,col=adjustcolor(cols.alts,0.5),outline=F,ann=F,axes=F)
with(aggregate(TFlow.km3~Alt,subset(regq.dat.CY,Region=="StL"),mean),
     points(1:3,TFlow.km3,pch=21,bg=adjustcolor("grey",0.5),cex=1.25,lwd=0.1))
DT=with(subset(regq.dat.CY,Region=="StL"),dunn.test::dunn.test(TFlow.kAcft,Alt))
DT.ltr=cldList(P.adjusted ~ comparison,data=DT,threshold = 0.05)
DT.ltr$Letter=toupper(DT.ltr$Letter)
DT.ltr=DT.ltr[order(match(DT.ltr$Group,c("ECB","FWO",'C24'))),]
text(1:5,x$stats[5,],DT.ltr$Letter,pos=3)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:3,1:3,NA);box(lwd=1)
mtext(side=3,line=-1.25,"  SLE",adj=0,font=4)

ylim.val=c(0,2.25);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TFlow.km3~Alt,subset(regq.dat.CY,Region=="WCAs"),
          ylim=ylim.val,col=adjustcolor(cols.alts,0.5),outline=F,ann=F,axes=F)
with(aggregate(TFlow.km3~Alt,subset(regq.dat.CY,Region=="WCAs"),mean),
     points(1:3,TFlow.km3,pch=21,bg=adjustcolor("white",0.5),cex=1.25,lwd=0.1))
DT=with(subset(regq.dat.CY,Region=="WCAs"),dunn.test::dunn.test(TFlow.kAcft,Alt))
DT.ltr=cldList(P.adjusted ~ comparison,data=DT,threshold = 0.05)
DT.ltr$Letter=toupper(DT.ltr$Letter)
DT.ltr=DT.ltr[order(match(DT.ltr$Group,c("ECB","FWO",'C24'))),]
text(1:5,x$stats[5,],DT.ltr$Letter,pos=3)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:3,1:3,Alts,line=-0.5);box(lwd=1)
mtext(side=3,line=-1.25,"  WCA",adj=0,font=4)

ylim.val=c(0,0.3);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TFlow.km3~Alt,subset(regq.dat.CY,Region=="LWLagoon"),
          ylim=ylim.val,col=adjustcolor(cols.alts,0.5),outline=F,ann=F,axes=F)
with(aggregate(TFlow.km3~Alt,subset(regq.dat.CY,Region=="LWLagoon"),mean),
     points(1:3,TFlow.km3,pch=21,bg=adjustcolor("grey",0.5),cex=1.25,lwd=0.1))
DT=with(subset(regq.dat.CY,Region=="LWLagoon"),dunn.test::dunn.test(TFlow.kAcft,Alt))
DT.ltr=cldList(P.adjusted ~ comparison,data=DT,threshold = 0.05)
DT.ltr$Letter=toupper(DT.ltr$Letter)
DT.ltr=DT.ltr[order(match(DT.ltr$Group,c("ECB","FWO",'C24'))),]
text(1:5,x$stats[5,],DT.ltr$Letter,pos=3)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:3,1:3,Alts,line=-0.5);box(lwd=1)
mtext(side=3,line=-1.25,"  LWL",adj=0,font=4)

mtext(side=1,line=0.5,outer=T,"Alternatives")
mtext(side=2,line=0.5,outer=T,"Annual Regulatory Discharge (km\u00B3 Yr\u207B\u00B9)")
dev.off()


regq.dat.CY=ddply(regq.dat,c("CY","Alt",'Region'),summarise,TFlow.kAcft=sum(cfs.to.acftd(Data.Value),na.rm=T)/1000)
regq.dat.CY.mean=reshape2::dcast(regq.dat.CY,Alt~Region,value.var = "TFlow.kAcft",mean)
regq.dat.CY.mean=regq.dat.CY.mean[match(alts,regq.dat.CY.mean$Alt),]
regq.dat.CY.mean=regq.dat.CY.mean[,c("Alt","WCAs","Cal","StL","LWLagoon")]
regq.dat.CY.mean=regq.dat.CY.mean[match(alts,regq.dat.CY.mean$Alt),]

tmp=regq.dat.CY.mean[,c("WCAs","Cal","StL","LWLagoon")]
rownames(tmp)<-alts
cols2=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))

ylim.val=c(0,850);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"AvgFloodControl.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(1,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),1,2,byrow=T),heights=c(1,0.4))

x=barplot(t(tmp),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp),beside=F,col=cols2,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts),add=T)
with(regq.dat.CY.mean,text(x,WCAs/2,round(WCAs,0),cex=0.75,col="white"))
with(regq.dat.CY.mean,text(x,WCAs+(((Cal+WCAs)-WCAs)/2),round(regq.dat.CY.mean$Cal,0),cex=0.75))
with(regq.dat.CY.mean,text(x,(WCAs+Cal)+(((Cal+WCAs+StL)-(Cal+WCAs))/2),round(regq.dat.CY.mean$StL,0),cex=0.75))
with(regq.dat.CY.mean,text(x,Cal+WCAs+StL+LWLagoon,round(regq.dat.CY.mean$LWLagoon,0),pos=3,offset=0.1,cex=0.75))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts,line=-0.5,las=1,cex=0.8);box(lwd=1)
mtext(side=2,line=3,"Discharge Volume (x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=1.5,"Alternatives")

# par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(-0.15,0.75,legend=c("EAA (S351 & S354)","Caloosahatchee River (S77)","St. Lucie River (S308)","Lake Worth Lagoon (S271)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols2,
       pt.cex=1.25,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)
text(1,0.2,"Mean annual flood control releases\nfrom Lake Okeechobee for the\n41 year (1965 - 2005)simulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()


RSM.sites.WCAs=data.frame(SITE=c("S8_QFC","S150_QFC","S7_QFC","S6_QFC","S8","S150","S7","S6"),
                          type=c(rep("QFC",4),rep("total",4)))

regq.dat.CY2=ddply(merge(subset(q.dat,SITE%in%RSM.sites.WCAs$SITE),RSM.sites.WCAs,"SITE"),
                  c("CY","Alt","type"),summarise,
                  TFlow.kAcft=sum(cfs.to.acftd(Data.Value),na.rm=T)/1000,
                  TFlow.km3=sum(cfs.to.km3d(Data.Value)))

# plot(QFC~total,dcast(regq.dat.CY2,CY+Alt~type,value.var = "TFlow.kAcft",mean))

regq.dat.CY.mean2=reshape2::dcast(regq.dat.CY2,type~Alt,value.var = "TFlow.kAcft",mean)
regq.dat.CY.mean2$PerDiff_FWO=with(regq.dat.CY.mean2,((C240-FWO)/FWO)*100)
regq.dat.CY.mean2$PerDiff_ECB=with(regq.dat.CY.mean2,((C240-ECB)/ECB)*100)
# write.csv(regq.dat.CY.mean2,paste0(export.path,"EAA_totalQ_WCAs.csv"),row.names = F)

regq.dat.CY.mean2=reshape2::dcast(regq.dat.CY2,Alt~type,value.var = "TFlow.kAcft",mean)
regq.dat.CY.mean2=regq.dat.CY.mean2[match(alts,regq.dat.CY.mean2$Alt),]

ylim.val=c(0,1050);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"DischargeWCAs.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(1,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

x=barplot(regq.dat.CY.mean2$total,beside=T,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(regq.dat.CY.mean2$total,beside=F,col=adjustcolor(cols.alts,0.5),ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts),add=T)
with(regq.dat.CY.mean2,text(x,total,round(regq.dat.CY.mean2$total,0),pos=3,offset=0.1,cex=0.75))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts,line=-0.5,las=1,cex=0.8);box(lwd=1)
mtext(side=2,line=3,"Discharge Volume (x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=1.5,"Alternatives")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(0.5,0.5,legend=c("WCA-2 & 3 (S6,S7,S150 & S8)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg="grey",
       pt.cex=1.25,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
text(1,-1,"Mean annual total flow from EAA for the\n41 year (1965 - 2005)simulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()


# ENP ---------------------------------------------------------------------
q.dat.xtab.ENP=reshape2::dcast(q.dat.GL,Alt+Date+CY+month~SITE,value.var="Data.Value",function(x)mean(x,na.rm=T))
q.dat.xtab.ENP$S333N=NA;# not in this modeling

q.dat.xtab.ENP$minS356_S335=apply(q.dat.xtab.ENP[,c('S356','S335')],1,min)
q.dat.xtab.ENP$WSRS.Q=rowSums(q.dat.xtab.ENP[,c("S12A","S12B","S12C","S12D")],na.rm=T)
q.dat.xtab.ENP$ESRS.Q=rowSums(q.dat.xtab.ENP[,c("S333","S355A","S355B","minS356_S335")],na.rm=T)
q.dat.xtab.ENP$S333R=with(q.dat.xtab.ENP,S333*(1-ifelse(ESRS.Q>0,ifelse(S334/ESRS.Q>1,1,S334/ESRS.Q),0)))
q.dat.xtab.ENP$S355A.adj=with(q.dat.xtab.ENP,S355A*(1-ifelse(ESRS.Q>0,ifelse(S334/ESRS.Q>1,1,S334/ESRS.Q),0)))
q.dat.xtab.ENP$S355B.adj=with(q.dat.xtab.ENP,S355B*(1-ifelse(ESRS.Q>0,ifelse(S334/ESRS.Q>1,1,S334/ESRS.Q),0)))
q.dat.xtab.ENP$S356.adj=with(q.dat.xtab.ENP,minS356_S335*(1-ifelse(ESRS.Q>0,ifelse(S334/ESRS.Q>1,1,S334/ESRS.Q),0)))
q.dat.xtab.ENP$TFlow=rowSums(q.dat.xtab.ENP[,c("S12A","S12B","S12C","S12D","S333","S333N","S355A","S355B","minS356_S335")],na.rm=T)

ENP.dat.CY=ddply(q.dat.xtab.ENP,c("Alt","CY"),summarise,
      WSRS.kacft=sum(cfs.to.acftd(WSRS.Q),na.rm=T)/1000,
      ESRS.kacft=sum(cfs.to.acftd(ESRS.Q),na.rm=T)/1000,
      TFlow.kacft=sum(cfs.to.acftd(TFlow),na.rm=T)/1000)
ENP.dat.CY.melt=reshape2::melt(ENP.dat.CY,id.vars=c("Alt","CY"))

ENP.dat.CY.mean=reshape2::dcast(ENP.dat.CY.melt,variable~Alt,value.var = "value",mean)
ENP.dat.CY.mean$PerDiff_FWO=with(ENP.dat.CY.mean,((C240-FWO)/FWO)*100)
ENP.dat.CY.mean$PerDiff_ECB=with(ENP.dat.CY.mean,((C240-ECB)/ECB)*100)

ENP.dat.CY.mean$C240[1]/ENP.dat.CY.mean$C240[3]
ENP.dat.CY.mean$C240[2]/ENP.dat.CY.mean$C240[3]

# RECOVER Salinity Envelope -----------------------------------------------
## CRE
q.dat.xtab$S79_QPFCSOURCE_LAKE.14d=with(q.dat.xtab,ave(S79_QPFCSOURCE_LAKE,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$CRE.low=with(q.dat.xtab,ifelse(S79.14d<750,1,0)) # RECOVER Low
q.dat.xtab$CRE.opt=with(q.dat.xtab,ifelse(S79.14d>=750&S79.14d<2100,1,0)) # RECOVER Optimum
q.dat.xtab$CRE.high=with(q.dat.xtab,ifelse(S79.14d>=2100&S79.14d<2600,1,0)) # RECOVER Stress
q.dat.xtab$CRE.dam=with(q.dat.xtab,ifelse(S79.14d>=2600,1,0)) # RECOVER Damaging

## SLE
q.dat.xtab$SLE.S80trib=rowSums(q.dat.xtab[,c("S80","TMC2EST","S48","S49","NSF2EST","sle_gw")],na.rm=T)
q.dat.xtab$SLE.S80trib.14d=with(q.dat.xtab,ave(SLE.S80trib,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80_QPFCSOURCE_LAKE=apply(q.dat.xtab[,c("S308","S80")],1,min,na.rm=T);# actual flow tag not simulated.Assumes the minimum amount of water from lake is passed to estuary
q.dat.xtab$S80_QPFCSOURCE_LAKE.14d=with(q.dat.xtab,ave(S80_QPFCSOURCE_LAKE,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S308_QFC.14d=with(q.dat.xtab,ave(S308_QFC,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))

# 2007 Sal Metric
q.dat.xtab$sltrib=rowSums(q.dat.xtab[,c("TMC2EST","S48","S49","NSF2EST","sle_gw",'STA2TMC')],na.rm=T)
q.dat.xtab$slbsn=with(q.dat.xtab,S80-S308_QFC+sltrib)
q.dat.xtab$slbsn.14d=with(q.dat.xtab,ave(slbsn,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$sltotal=with(q.dat.xtab,S80+sltrib)
q.dat.xtab$sltotal.14d=with(q.dat.xtab,ave(sltotal,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))

q.dat.xtab$SLE.low=with(q.dat.xtab,ifelse(SLE.S80trib.14d<150,1,0)) # RECOVER Low
q.dat.xtab$SLE.opt=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=150&SLE.S80trib.14d<1400,1,0)) # RECOVER Optimum
q.dat.xtab$SLE.high=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1400&SLE.S80trib.14d<1700,1,0)) # RECOVER stress
q.dat.xtab$SLE.dam=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1700,1,0)) # RECOVER damaging
q.dat.xtab$SLE.2000=with(q.dat.xtab,ifelse(sltotal.14d>2000,1,0))
q.dat.xtab$SLE.2000.bsn=with(q.dat.xtab,ifelse(slbsn.14d>2000,1,0))

##
q.dat.xtab$CRE.low.count=0
q.dat.xtab$CRE.opt.count=0
q.dat.xtab$CRE.opt.LOK.count=0
q.dat.xtab$CRE.opt.basin.count=0
q.dat.xtab$CRE.high.count=0
q.dat.xtab$CRE.high.LOK.count=0
q.dat.xtab$CRE.high.basin.count=0
q.dat.xtab$CRE.dam.count=0
q.dat.xtab$CRE.dam.LOK.count=0
q.dat.xtab$CRE.dam.basin.count=0

q.dat.xtab$SLE.low.count=0
q.dat.xtab$SLE.opt.count=0
q.dat.xtab$SLE.opt.LOK.count=0
q.dat.xtab$SLE.opt.basin.count=0
q.dat.xtab$SLE.high.count=0
q.dat.xtab$SLE.high.LOK.count=0
q.dat.xtab$SLE.high.basin.count=0
q.dat.xtab$SLE.dam.count=0
q.dat.xtab$SLE.dam.LOK.count=0
q.dat.xtab$SLE.dam.basin.count=0

###
q.dat.xtab2=data.frame()
for(j in 1:length(alts)){
  tmp=subset(q.dat.xtab,Alt==alts[j])
  for(i in 14:nrow(tmp)){
    ## CRE
    tmp$CRE.low.count[i]=with(tmp,ifelse(CRE.low[i]==1&sum(CRE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$CRE.opt.count[i]=with(tmp,ifelse(CRE.opt[i]==1&sum(CRE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.opt.LOK.count[i]=with(tmp,
                                  ifelse(CRE.opt.count[i]==1,
                                         ifelse((S79.14d[i]-S79_QPFCSOURCE_LAKE.14d[i])<=750,1,0),0))
    tmp$CRE.opt.basin.count[i]=with(tmp,CRE.opt.count[i]-CRE.opt.LOK.count[i])
    
    tmp$CRE.high.count[i]=with(tmp,ifelse(CRE.high[i]==1&sum(CRE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.high.LOK.count[i]=with(tmp,
                                   ifelse(CRE.high.count[i]==1,
                                          ifelse((S79.14d[i]-S79_QPFCSOURCE_LAKE.14d[i])<=2100,1,0),0))
    tmp$CRE.high.basin.count[i]=with(tmp,CRE.high.count[i]-CRE.high.LOK.count[i])
    tmp$CRE.dam.count[i]=with(tmp,ifelse(CRE.dam[i]==1&sum(CRE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.dam.LOK.count[i]=with(tmp,
                                  ifelse(CRE.dam.count[i]==1,
                                         ifelse((S79.14d[i]-S79_QPFCSOURCE_LAKE.14d[i])<=2600,1,0),0))
    tmp$CRE.dam.basin.count[i]=with(tmp,CRE.dam.count[i]-CRE.dam.LOK.count[i])
    
    ## SLE
    tmp$SLE.low.count[i]=with(tmp,ifelse(SLE.low[i]==1&sum(SLE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0)) 
    tmp$SLE.opt.count[i]=with(tmp,ifelse(SLE.opt[i]==1&sum(SLE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.opt.LOK.count[i]=with(tmp,
                                  ifelse(SLE.opt.count[i]==1,
                                         ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=150,1,0),0))
    tmp$SLE.opt.basin.count[i]=with(tmp,SLE.opt.count[i]-SLE.opt.LOK.count[i])
    tmp$SLE.high.count[i]=with(tmp,ifelse(SLE.high[i]==1&sum(SLE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high.LOK.count[i]=with(tmp,
                                   ifelse(SLE.high.count[i]==1,
                                          ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=1400,1,0),0))
    tmp$SLE.high.basin.count[i]=with(tmp,SLE.high.count[i]-SLE.high.LOK.count[i])
    tmp$SLE.dam.count[i]=with(tmp,ifelse(SLE.dam[i]==1&sum(SLE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.dam.LOK.count[i]=with(tmp,
                                  ifelse(SLE.dam.count[i]==1,
                                         ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=1700,1,0),0))
    tmp$SLE.dam.basin.count[i]=with(tmp,SLE.dam.count[i]-SLE.dam.LOK.count[i])
    }
  q.dat.xtab2=rbind(q.dat.xtab2,tmp)
  print(j)
}

# Old RECOVER Salinity envelope based on monthly Q
# SLE
q.dat.xtab$SLE.2000.count=0
q.dat.xtab$SLE.2000.count.LOK=0
q.dat.xtab$SLE.2000.count.bsn=0
q.dat.xtab3=data.frame()
for(j in 1:length(alts)){
  tmp=subset(q.dat.xtab,Alt==alts[j])
  for(i in 14:nrow(tmp)){
    tmp$SLE.2000.count[i]=with(tmp,ifelse(sum(SLE.2000[(i-13):i],na.rm=T)==14&sum(SLE.2000.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.2000.count.bsn[i]=with(tmp,ifelse(sum(SLE.2000.bsn[(i-13):i],na.rm=T)==14&sum(SLE.2000.count.bsn[(i-13):(i-1)],na.rm=T)==0,1,0))
  }
  q.dat.xtab3=rbind(q.dat.xtab3,tmp)
  print(j)
}

## sle_gw file might be different than LOWRP
SLE_SalEnv2000=ddply(q.dat.xtab3,"Alt",summarise,
                     SLE.2000.total=sum(SLE.2000.count,na.rm = T),
                     SLE.2000.bsn=sum(SLE.2000.count.bsn,na.rm = T))
SLE_SalEnv2000$SLE.2000.lok=with(SLE_SalEnv2000,SLE.2000.total-SLE.2000.bsn)

SLE.sal.env=ddply(q.dat.xtab,c("Alt","CY","month"),summarise,month.Q=mean(SLE.S80trib,na.rm=T))
SLE.sal.env$lowQ=with(SLE.sal.env,ifelse(month.Q<=350,1,0))
SLE.sal.env$highQ=with(SLE.sal.env,ifelse(month.Q>=2000&month.Q<3000,1,0))
SLE.sal.env$ext.highQ=with(SLE.sal.env,ifelse(month.Q>3000,1,0))
SLE.sal.env.sum=ddply(SLE.sal.env,"Alt",summarise,
                      N.low=sum(lowQ,na.rm=T),
                      N.highQ=sum(highQ,na.rm=T),
                      N.ext.highQ=sum(ext.highQ,na.rm=T))
SLE.sal.env.sum=SLE.sal.env.sum[match(alts,SLE.sal.env.sum$Alt),]
SLE.sal.env.sum

SLE.sal.env.sum2=merge(SLE.sal.env.sum,SLE_SalEnv2000[,c("Alt","SLE.2000.bsn","SLE.2000.lok")],"Alt")
vars=c("Alt", "N.low","SLE.2000.bsn", "SLE.2000.lok", "N.highQ", "N.ext.highQ")
SLE.sal.env.sum2=SLE.sal.env.sum2[match(alts,SLE.sal.env.sum2$Alt),vars]


# CRE
CRE.sal.env=ddply(q.dat.xtab,c("Alt","CY","month"),summarise,month.Q=mean(S79,na.rm=T))
CRE.sal.env$lowQ=with(CRE.sal.env,ifelse(month%in%c(10:12,1:7)&month.Q<=450,1,0))
CRE.sal.env$highQ=with(CRE.sal.env,ifelse(month.Q>2800,1,0))
CRE.sal.env$ext.highQ=with(CRE.sal.env,ifelse(month.Q>4500,1,0))

CRE.sal.env.sum=ddply(CRE.sal.env,c("Alt"),summarise,
                      N.lowQ=sum(lowQ),
                      N.highQ=sum(highQ),
                      N.ext.highQ=sum(ext.highQ))
CRE.sal.env.sum=CRE.sal.env.sum[match(alts,CRE.sal.env.sum$Alt),]

CRE.sal.env.consec=data.frame()
for(j in 1:length(alts)){
  tmp=subset(CRE.sal.env,Alt==alts[j])
  tmp$highQ.N=0
  for(i in 2:nrow(tmp)){
    tmp$highQ.N[i]=with(tmp,ifelse(highQ[i-1]==0&highQ[i]>0,1,
                                   ifelse(highQ[i-1]>0&highQ[i]>0,1,0)))
    
  }
  CRE.highQ=consec.startend(tmp$highQ.N>0)
  tmp$sum.highQ=0
  for(i in 1:length(CRE.highQ$ends)){
    tmp[CRE.highQ$ends[i],]$sum.highQ=with(tmp[c(CRE.highQ$starts[i]:CRE.highQ$ends[i]),],sum(highQ.N,na.rm=T))
  }
  CRE.sal.env.consec=rbind(tmp,CRE.sal.env.consec)
  print(j)
}
rslt.CREHighQ=ddply(CRE.sal.env.consec,c("Alt","sum.highQ"),summarise,count.event=N.obs(sum.highQ))

q.dat.xtab2$cum.CRE.opt.count=with(q.dat.xtab2,ave(CRE.opt.count,Alt,FUN = function(x) cumsum(x)))
q.dat.xtab2$cum.CRE.dam.LOK.count=with(q.dat.xtab2,ave(CRE.dam.LOK.count,Alt,FUN = function(x) cumsum(x)))

plot(cum.CRE.opt.count~Date,q.dat.xtab2,type="n")
lines(cum.CRE.opt.count~Date,subset(q.dat.xtab2,Alt==Alts[1]),col=cols.alts[1],lwd=2)
lines(cum.CRE.opt.count~Date,subset(q.dat.xtab2,Alt==Alts[2]),col="green",lwd=2,lty=1)
lines(cum.CRE.opt.count~Date,subset(q.dat.xtab2,Alt==Alts[3]),col=cols.alts[3],lwd=2)

# png(filename=paste0(plot.path,"example_CCount_CRE_LOKDAM.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,0.5),oma=c(2,2,0.5,0.25),lwd=0.5);
ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1965-01-01","2005-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

plot(cum.CRE.dam.LOK.count~Date,q.dat.xtab2,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
lines(cum.CRE.dam.LOK.count~Date,subset(q.dat.xtab2,Alt==Alts[1]),col=cols.alts[1],lwd=2)
lines(cum.CRE.dam.LOK.count~Date,subset(q.dat.xtab2,Alt==Alts[2]),col=cols.alts[1],lwd=2,lty=2)
lines(cum.CRE.dam.LOK.count~Date,subset(q.dat.xtab2,Alt==Alts[3]),col=cols.alts[3],lwd=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Cumulative Count 14-Day Event")
mtext(side=1,line=1.5,"Date (Year)")
mtext(side=3,adj=0,"CRE - Damaging from LOK (>2600 cfs) ")
legend("topleft",legend=alts,
       pch=NA,lty=c(1,2,1),lwd=c(2,2,2),
       col=c(cols.alts[1],cols.alts[1],cols.alts[3]),pt.bg=NA,
       pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()

vars=c(paste0("CRE.",c("low.count","opt.count","high.count","high.LOK.count","high.basin.count","dam.count","dam.LOK.count","dam.basin.count")),
       paste0("SLE.",c("low.count","opt.count","high.count","high.LOK.count","high.basin.count","dam.count","dam.LOK.count","dam.basin.count")))

SalEnv_count.melt=reshape2::melt(q.dat.xtab2[,c("Alt","Date","CY",vars)],id.vars = c("Alt","Date","CY"))
SalEnv_count=reshape2::dcast(SalEnv_count.melt,Alt~variable,value.var = "value",sum)
SalEnv_count=SalEnv_count[match(alts,SalEnv_count$Alt),]
SalEnv_count
SalEnv_count$SLE.stress_dam.LOK=with(SalEnv_count,SLE.high.LOK.count+SLE.dam.LOK.count)
SalEnv_count$CRE.stress_dam.LOK=with(SalEnv_count,CRE.high.LOK.count+CRE.dam.LOK.count)
SalEnv_count$per_CREstressdam.lok=with(SalEnv_count,((CRE.stress_dam.LOK-CRE.stress_dam.LOK[2])/CRE.stress_dam.LOK[2])*100)
SalEnv_count$per_SLEstressdam.lok=with(SalEnv_count,((SLE.stress_dam.LOK-SLE.stress_dam.LOK[2])/SLE.stress_dam.LOK[2])*100)
SalEnv_count

# https://stats.stackexchange.com/questions/205316/what-statistical-analysis-to-run-for-count-data-in-r
SalEnv.chi=data.frame()
env.thres=paste("CRE",c("low.count","opt.count","high.LOK.count","dam.LOK.count"),sep=".")
for(i in 1:length(env.thres)){

  for(j in 1:2){
tmp=SalEnv_count[c(j,3),env.thres[i]]
rslt.tmp=chisq.test(tmp)
rslt=data.frame(thres=env.thres[i],
                baseline=SalEnv_count$Alt[j],
                chisq=as.numeric(rslt.tmp$statistic),
                pval=rslt.tmp$p.value)
SalEnv.chi=rbind(SalEnv.chi,rslt)
}
}

env.thres=paste("SLE",c("low.count","opt.count","high.LOK.count","dam.LOK.count"),sep=".")
for(i in 1:length(env.thres)){
  
  for(j in 1:2){
    tmp=SalEnv_count[c(j,3),env.thres[i]]
    rslt.tmp=chisq.test(tmp)
    rslt=data.frame(thres=env.thres[i],
                    baseline=SalEnv_count$Alt[j],
                    chisq=as.numeric(rslt.tmp$statistic),
                    pval=rslt.tmp$p.value)
    SalEnv.chi=rbind(SalEnv.chi,rslt)
  }
}
SalEnv.chi

SalEnv.chi$region=substr(SalEnv.chi$thres,1,3)
SalEnv.chi$thres2=substr(SalEnv.chi$thres,5,20)

vars=c("thres2", "baseline", "chisq", "pval")
tmp.tbl=merge(subset(SalEnv.chi,region=="CRE")[,vars],
      subset(SalEnv.chi,region=="SLE")[,vars],
      c("thres2","baseline"),sort=F)
colnames(tmp.tbl)=c("thres2", "baseline", "chisq.CRE", "pval.CRE", "chisq.SLE", "pval.SLE")

tmp.tbl%>%  
flextable()%>%
  colformat_double(j="chisq.CRE",digits=2,na_str=" --- ",big.mark="")%>%
  colformat_double(j="pval.CRE",i=~pval.CRE>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="pval.CRE",i=~pval.CRE<0.05,value=as_paragraph('< 0.05'))%>%
  bold(j="pval.CRE",i=~pval.CRE>0.05)%>%
  colformat_double(j="chisq.SLE",digits=2,na_str=" --- ",big.mark="")%>%
  colformat_double(j="pval.SLE",i=~pval.SLE>=0.05,digits=2,na_str=" --- ",big.mark="")%>%
  compose(j="pval.SLE",i=~pval.SLE<0.05,value=as_paragraph('< 0.05'))%>%
  bold(j="pval.SLE",i=~pval.SLE>0.05)%>%
  set_header_labels("thres2"="Salinity\nEnvelope",
                    "baseline"="Basline",
                    "chisq.CRE"="\u03C7\u00B2", 
                    "pval.CRE"="\u03C1-value",
                    "chisq.SLE"="\u03C7\u00B2", 
                    "pval.SLE"="\u03C1-value")%>%
  add_header(
    "chisq.CRE"="CRE", 
    "pval.CRE"="CRE",
    "chisq.SLE"="SLE", 
    "pval.SLE"="SLE")%>%
  merge_h(part="header")%>%align(align="center",part="header")%>%
  merge_v(j="thres2")%>%
  hline(i=seq(2,8,2))%>%
  fix_border_issues()


# SalEnv_count2=reshape2::dcast(SalEnv_count.melt,Alt+CY~variable,value.var = "value",sum)
# glm.test=glm(CRE.opt.count~Alt,SalEnv_count2,family=poisson())
# glm.test=glm(CRE.high.LOK.count~Alt,SalEnv_count2,family=poisson())
# summary(glm.test)
# layout(matrix(1:4,2,2));plot(glm.test)
# hist(residuals(glm.test))

###

SalEnv_count.LOSOM=data.frame(
  Alt=c("ECB19","NA25f","PA25"),
  CRE.stress_dam.LOK=c(184+198,183+187,66+81),
  SLE.stress_dam.LOK=c(164+159,145+140,30+38),
  CRE.opt=c(469,588,765),
  SLE.opt=c(831,865,912)
)

SalEnv_count.LOWRP=data.frame(
  Alt=c("ECB","FWO","ASR"),
  CRE.stress_dam.LOK=c(86+114,61+58,63+63),
  SLE.stress_dam.LOK=c(60+102,64+63,57+59),
  CRE.opt=c(385,367,319),
  SLE.opt=c(785,899,916)
)

# png(filename=paste0(plot.path,"RECOVER_CRESLE_stress_dam.png"),width=5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1),oma=c(2,3,0.5,1),lwd=0.5);
ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=ylim.val;xmaj=ymaj;xmin=ymin
plot(CRE.stress_dam.LOK~SLE.stress_dam.LOK,SalEnv_count,ylim=xlim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=2,col=adjustcolor("grey",0.5))
abline(0,1)
points(CRE.stress_dam.LOK~SLE.stress_dam.LOK,SalEnv_count,pch=21,bg="indianred1",cex=1.25,lwd=0.1)
text(CRE.stress_dam.LOK~SLE.stress_dam.LOK,SalEnv_count,Alt,pos=1,cex=0.75,offset=0.5,col="indianred1")
points(CRE.stress_dam.LOK~SLE.stress_dam.LOK,SalEnv_count.LOSOM,pch=22,bg="forestgreen",cex=1.25,lwd=0.1)
text(CRE.stress_dam.LOK~SLE.stress_dam.LOK,SalEnv_count.LOSOM,Alt,pos=2,col="forestgreen",cex=0.75,offset=0.5)
points(CRE.stress_dam.LOK~SLE.stress_dam.LOK,SalEnv_count.LOWRP,pch=21,bg="dodgerblue1",cex=1.25,lwd=0.1)
text(CRE.stress_dam.LOK~SLE.stress_dam.LOK,SalEnv_count.LOWRP,Alt,pos=3,col="dodgerblue1",cex=0.75,offset=0.5)
axis_fun(2,ymaj,ymin,ymaj,line=-0.5)
axis_fun(1,xmaj,xmin,xmaj);box(lwd=1)
mtext(side=2,line=3,"CRE Stress & Dam LOK Flow Events")
mtext(side=1,line=2,"SLE Stress & Dam LOK Flow Events")
mtext(side=3,adj=0,"Based on 14-day event count")
legend("topleft",legend=c("CEPP PACR (1965 - 2005)","LOWRP (1965 - 2005)","LOSOM (1965 - 2016)"),
       pch=c(21,21,22),
       lty=c(0,0),lwd=0.1,
       col=c("black"),
       pt.bg=c("indianred1","dodgerblue1","forestgreen"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()
# png(filename=paste0(plot.path,"RECOVER_CRESLE_opt.png"),width=5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1),oma=c(2,3,0.5,1),lwd=0.5);
ylim.val=c(0,1100);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=ylim.val;xmaj=ymaj;xmin=ymin
plot(CRE.opt.count~SLE.opt.count,SalEnv_count,ylim=xlim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=2,col=adjustcolor("grey",0.5))
abline(0,1)
points(CRE.opt.count~SLE.opt.count,SalEnv_count,pch=21,bg="indianred1",cex=1.25,lwd=0.1)
text(CRE.opt.count~SLE.opt.count,SalEnv_count,Alt,pos=2,cex=0.75,offset=0.5,col="indianred1")
points(CRE.opt~SLE.opt,SalEnv_count.LOSOM,pch=22,bg="forestgreen",cex=1.25,lwd=0.1)
text(CRE.opt~SLE.opt,SalEnv_count.LOSOM,Alt,pos=2,col="forestgreen",cex=0.75,offset=0.5)
points(CRE.opt~SLE.opt,SalEnv_count.LOWRP,pch=21,bg="dodgerblue1",cex=1.25,lwd=0.1)
text(CRE.opt~SLE.opt,SalEnv_count.LOWRP,Alt,pos=4,col="dodgerblue1",cex=0.75,offset=0.5)
axis_fun(2,ymaj,ymin,ymaj,line=-0.5)
axis_fun(1,xmaj,xmin,xmaj);box(lwd=1)
mtext(side=2,line=3,"CRE Optimum Flow Events")
mtext(side=1,line=2,"SLE Optimum Flow Events")
mtext(side=3,adj=0,"Based on 14-day event count")
legend("topleft",legend=c("CEPP PACR (1965 - 2005)","LOWRP (1965 - 2005)","LOSOM (1965 - 2016)"),
       pch=c(21,21,22),
       lty=c(0,0),lwd=0.1,
       col=c("black"),
       pt.bg=c("indianred1","dodgerblue1","forestgreen"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()


vars.CRE=paste("CRE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high3.count"),sep=".")
CRE.SalEnv_count3=SalEnv_count[,c("Alt",vars.CRE)]
CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
# png(filename=paste0(plot.path,"RECOVER_CRE_SalEnv_2007PM.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:3),1,3,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(3,3,2,1),lwd=0.5);

CRE.labs.old=c("Low Flow\n(<450 cfs Basin & LOK)","High Flow\n(>2800 cfs Basin & LOK)","Extreme High Flow\n(>4500 cfs Basin & LOK)")
ymax=c(200,100,100)
yval=ymax/2
for(i in 2:4){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(CRE.sal.env.sum[,i],col=adjustcolor(cols.alts,0.5),ylim=ylim.val,axes=F,ann=F,space=0)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts,cex=1,las=1)
  # axis_fun(1,x,x,alts,cex=0.7,las=2)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs.old[i-1],cex=0.5)
  text(x,CRE.sal.env.sum[,i],
       round(CRE.sal.env.sum[,i],0),font=2,col="black",pos=3,cex=0.75,offset=0.25)
  if(i==2){mtext(side=2,line=2.5,"Number of Exceedances")}
}
mtext(side=1,outer=T,"Alternatives")
mtext(side=1,outer=T,line=1.5,adj=0,
      "2007 RECOVER Salinity Envelope Performance Metric based on mean monthly discharge at S-79",
      cex=0.5,font=3)
dev.off()


vars.CRE=paste("CRE",c("low.count","opt.count","high.LOK.count","dam.LOK.count"),sep=".")
vars.SLE=paste("SLE",c("low.count","opt.count","high.LOK.count","dam.LOK.count"),sep=".")
CRE.SalEnv_count3=SalEnv_count[,c("Alt",vars.CRE)]
SLE.SalEnv_count3=SalEnv_count[,c("Alt",vars.SLE)]
CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)")
SLE.labs=c("Low Flow\n(<150 cfs)","Optimum\n(150 - 1400 cfs)","Stress From LOK\n(1400 - 1700 cfs)","Damaging From LOK\n(>1700 cfs)")
# png(filename=paste0(plot.path,"RECOVER_CRE_SalEnv_.png"),width=7,height=3,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:4),1,4,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(3,3,2,1),lwd=0.5);

ymax=c(800,500,150,150)
yval=ymax/2
for(i in 2:5){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(CRE.SalEnv_count3[,i],col=adjustcolor(cols.alts,0.5),ylim=ylim.val,axes=F,ann=F,space=0)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts,cex=0.7,las=2)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count3[,i],
       round(CRE.SalEnv_count3[,i],0),font=2,col="black",pos=3,cex=0.75,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")
mtext(side=1,line=1.75,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"Count of 14-Day Periods")
dev.off()

# png(filename=paste0(plot.path,"RECOVER_SLE_SalEnv_.png"),width=7,height=3,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:4),1,4,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(3,3,2,1),lwd=0.5);

ymax=c(12,1000,150,150)
yval=ymax/2
for(i in 2:5){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(SLE.SalEnv_count3[,i],col=adjustcolor(cols.alts,0.5),ylim=ylim.val,axes=F,ann=F,space=0)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts,cex=0.7,las=2)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count3[,i],
       round(SLE.SalEnv_count3[,i],0),font=2,col="black",pos=3,cex=0.75,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=1.75,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"Count of 14-Day Periods")
dev.off()


SalEnv_count.FWO=reshape2::dcast(SalEnv_count.melt,variable~Alt,value.var = "value",sum)
SalEnv_count.FWO=SalEnv_count.FWO[,c("variable",alts)]
SalEnv_count.FWO$C240.PerFWO=with(SalEnv_count.FWO,((C240-FWO)/FWO)*100)

# png(filename=paste0(plot.path,"RECOVER_CRE_SalEnv_FWO.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1),oma=c(2,2,1,1),lwd=0.5);
ylim.val=c(-30,25);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(subset(SalEnv_count.FWO,variable%in%vars.CRE)$C240.PerFWO,
          ylim=ylim.val,space=0,
          axes=F,ann=F)
abline(h=0,lwd=1)
axis_fun(1,x,x,CRE.labs,line=0,cex=0.6)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
with(subset(SalEnv_count.FWO,variable%in%vars.CRE),
     text(x,C240.PerFWO,
          round(C240.PerFWO,1),
          pos=ifelse(C240.PerFWO<0,1,3),offset=0.2))
mtext(side=3,adj=0,"Caloosahatchee Estuary\nAlternative: C240")
mtext(side=1,line=2.5,outer=F,"Salinity Envelope Category")
mtext(side=2,line=2.5,outer=F,"% Difference relative to FWO")
dev.off()

# png(filename=paste0(plot.path,"RECOVER_CRE_SalEnv_FWO.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1),oma=c(2,2,1,1),lwd=0.5);
ylim.val=c(-50,25);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(subset(SalEnv_count.FWO,variable%in%vars.SLE)$C240.PerFWO,
          ylim=ylim.val,space=0,
          axes=F,ann=F)
abline(h=0,lwd=1)
axis_fun(1,x,x,SLE.labs,line=0,cex=0.6)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
with(subset(SalEnv_count.FWO,variable%in%vars.SLE),
     text(x,C240.PerFWO,
          round(C240.PerFWO,1),
          pos=ifelse(C240.PerFWO<0,1,3),offset=0.2))
mtext(side=3,adj=0,"St Lucie Estuary\nAlternative: C240")
mtext(side=1,line=2.5,outer=F,"Salinity Envelope Category")
mtext(side=2,line=2.5,outer=F,"% Difference relative to FWO")
dev.off()

# CRE MFL -----------------------------------------------------------------
q.dat.xtab$mfl.exceed=with(q.dat.xtab,ifelse(is.na(S79.30d)==T,0,ifelse(S79.30d<457,1,0)))
CRE.mfl.rslt=data.frame()
q.dat1.xtab.mfl=data.frame()
for(j in 1:n.alts){
  
  tmp=subset(q.dat.xtab,Alt==alts[j])
  ## Adapted from mflst_cre_v2.py
  for(i in 2:nrow(tmp)){
    if(tmp$mfl.exceed[i-1]==1&tmp$mfl.exceed[i]==0){
      tmp$exceed_end[i-1]=1 #found the last exceedance dates 
    }else{
      tmp$exceed_end[i-1]=0
    }
  }
  
  # subset(tmp,exceed_end==1)
  
  tmp$countdown=0
  tmp$exceed2=NA
  counts=0
  exc_n=0
  for(i in 30:nrow(tmp)){
    # rest counts
    if(tmp$mfl.exceed[i-1]==0&tmp$mfl.exceed[i]==1){
      counts=1
    }
    
    if(tmp$exceed_end[i]==1){
      if(tmp$countdown[i-1]<1){
        tmp$countdown[i]=365
      }else{
        tmp$countdown[i]=tmp$countdown[i-1]-1
        if(tmp$countdown[i]==0 & tmp$mfl.exceed[i]==1){
          tmp$countdown[i]=365
        }
      }
    }else{
      tmp$countdown[i]=tmp$countdown[i-1]-1
      counts=counts+1
      
      if(counts>366 & tmp$mfl.exceed[i]==1){
        tmp$countdown[i]=365
        counts=0
      }
      if(tmp$countdown[i]==0 & tmp$mfl.exceed[i]==1){
        tmp$countdown[i]=365
      }
    }
    
    #identify yearly violations
    if(tmp$countdown[i]<0){
      if(tmp$mfl.exceed[i]==1){
        if(tmp$mfl.exceed[i-1]!=1){
          tmp$exceed2[i]=1
          exc_n=exc_n+1}else{
            tmp$exceed2[i]=0
          }
      }else{tmp$exceed2[i]=0}
    }else{
      if(tmp$countdown[i]==365 & tmp$exceed_end[i]==0){
        tmp$exceed2[i]==1
        exc_n=exc_n+1
      }else{
        tmp$exceed2[i]=0
      }
    }
  }
  
  
  counts
  exc_n
  
  CRE.mfl.rslt=rbind(CRE.mfl.rslt,data.frame(Alt=alts[j],N.exceed=exc_n))
  q.dat1.xtab.mfl=rbind(q.dat1.xtab.mfl,tmp)
  print(j)
}

q.dat1.xtab.mfl$exc=with(q.dat1.xtab.mfl,ifelse(countdown<0&mfl.exceed==1,1,0))
q.dat1.xtab.mfl$plot_exc=with(q.dat1.xtab.mfl,ifelse(countdown<0&mfl.exceed==1,S79.30d,NA))
q.dat1.xtab.mfl$plot_exc365=with(q.dat1.xtab.mfl,ifelse(countdown>0&mfl.exceed==1,S79.30d,NA))

for(i in 1:n.alts){
  # png(filename=paste0(plot.path,"CRE_MFL_Alt_",alts[i],".png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
  layout(matrix(1:2,2,1),heights=c(1,0.2))
  par(family="serif",mar=c(1,3,0.75,1),oma=c(1.5,1,2.5,0.5));
  
  ylim.val=c(0,15000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  xlim.val=date.fun(c("1965-01-01","2006-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
  
  plot(S79.30d~Date,q.dat1.xtab.mfl,type="n",yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F)
  abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
  abline(h=457,col="brown",lty=2)
  with(subset(q.dat1.xtab.mfl,Alt==alts[i]),lines(Date,S79.30d,col="blue",lty=1.5))
  with(subset(q.dat1.xtab.mfl,Alt==alts[i]),lines(Date,plot_exc,col="orange",lty=1.5))
  with(subset(q.dat1.xtab.mfl,Alt==alts[i]),lines(Date,plot_exc365,col="grey",lty=1.5))
  axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=2,line=3,"Discharge (cfs)")
  mtext(side=1,line=1.5,"Year")
  mtext(side=3,paste0("MFL Recovery Water Body - Caloosahatchee River 30 Day Averege Flow at S79\n",
                      alts[i]," : ",subset(CRE.mfl.rslt,Alt==alts[i])$N.exceed,
                      " exceedance in 41 years of simualtion"))
  plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
  legend(0.5,-0.5,legend=c("30-day Moving Average","MFL Criteria (457 cfs)","Exceedance","Exceedance w/in 365 Days"),
         pch=NA,
         lty=c(1,2,1,1),lwd=2,
         col=c("blue","brown","orange","grey"),
         pt.bg=NA,
         pt.cex=1.5,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  dev.off()
}

CRE.mfl.rslt=CRE.mfl.rslt[match(alts,CRE.mfl.rslt$Alt),]
CRE.mfl.rslt$FWO_perchange=with(CRE.mfl.rslt,round(((N.exceed-N.exceed[2])/N.exceed[2])*100,2))

# png(filename=paste0(plot.path,"CREMFL_Alts.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,40);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(2,2,0.5,0.25),oma=c(1,2,0.75,1),lwd=0.5);

x=barplot(CRE.mfl.rslt$N.exceed,beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,length(alts)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(CRE.mfl.rslt$N.exceed,beside=F,ylim=ylim.val,col=adjustcolor(cols.alts,0.5),axes=F,ann=F,names.arg=rep(NA,length(alts)),add=T)
text(x,CRE.mfl.rslt$N.exceed,CRE.mfl.rslt$N.exceed,pos=3,offset=0.25)
text(x,CRE.mfl.rslt$N.exceed,paste0(CRE.mfl.rslt$FWO_perchange,"%"),pos=1,font=3)
axis_fun(1,x,x,alts,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"MFL Exceedances")
mtext(side=1,line=1.75,"Alternative")
mtext(side=3,adj=0,"Caloosahatchee MFL")
mtext(side=3,adj=1,"CY 1965 - 2005")
dev.off()


# LOK ---------------------------------------------------------------------
## Lake Stage -------------------------------------------------------------

lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,alts[i],"/RSMBN_output.dss"))  
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2005/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=alts[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}
unique(lakeO.stage$Alt)
range(lakeO.stage$Date)

# save(lakeO.stage,file=paste0(data.path,"LOK_stage.RData"))
load(paste0(data.path,"LOK_stage.RData"))

lakeO.stage$recess_7day=with(lakeO.stage,ave(STAGE,Alt,FUN=function(x) c(rep(NA,6),diff(x,lag=6))))

lakeO.stage$month=as.numeric(format(lakeO.stage$Date,'%m'))
lakeO.stage$day=as.numeric(format(lakeO.stage$Date,'%d'))
lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,'%Y'))
lakeO.stage$DoY=as.numeric(format(lakeO.stage$Date,'%j'))
lakeO.stage$WY=WY(lakeO.stage$Date)
lakeO.stage$low.stg=with(lakeO.stage,ifelse(STAGE<11,1,0))
lakeO.stage$vlow.stg=with(lakeO.stage,ifelse(STAGE<=10,1,0))
lakeO.stage$High.stg=with(lakeO.stage,ifelse(STAGE>16,1,0))
lakeO.stage$vHigh.stg=with(lakeO.stage,ifelse(STAGE>=17,1,0))

ann.peak=ddply(lakeO.stage,c("CY","Alt"),summarise,max.stg=max(STAGE,na.rm=T))
ann.peak$GT17=with(ann.peak,ifelse(round(max.stg,1)>=16.9,1,0))

ann.peak=merge(ann.peak,data.frame(Alt=alts,plot.y=1:3),"Alt")

days.POS=ddply(lakeO.stage,"Alt",summarise,N.days=N.obs(STAGE),
               sum.mod.high=sum(STAGE>15,na.rm=T),
               sum.low=sum(low.stg),
               sum.vlow=sum(vlow.stg),
               sum.High=sum(High.stg),
               sum.vHigh=sum(vHigh.stg))
days.POS=days.POS[match(alts,days.POS$Alt),]

labs=c("% LOK Stage >17 Ft NGVD29 (Exterme High)",
       "% LOK Stage >15 Ft NGVD29 (Moderate High)",
       "% LOK Stage <11 Ft NGVD29 (Moderate Low)",
       "% LOK Stage <10 Ft NGVD29 (Extreme Low)")
# png(filename=paste0(plot.path,"LOK_stg.png"),width=4,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(5,2,2,1),lwd=0.5);
layout(matrix(1:4,4,1,byrow=F))

ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.vHigh/14975)*100,col=adjustcolor(cols.alts,0.5),
          ylim=ylim.val,space=0,axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,NA,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,line=1.25,"LOK")
mtext(side=3,adj=0,labs[1],cex=0.7)
text(x,(days.POS$sum.vHigh/14975)*100,
     round((days.POS$sum.vHigh/14975)*100,1),font=2,col="black",pos=3,cex=1,offset=0.25)

ylim.val=c(0,40);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.mod.high/14975)*100,col=adjustcolor(cols.alts,0.5),
          ylim=ylim.val,space=0,axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,NA,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,labs[2],cex=0.7)
text(x,(days.POS$sum.mod.high/14975)*100,
     round((days.POS$sum.mod.high/14975)*100,1),font=2,col="black",pos=3,cex=1,offset=0.25)

ylim.val=c(0,14);by.y=7;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.low/14975)*100,col=adjustcolor(cols.alts,0.5),
          ylim=ylim.val,space=0,axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,NA,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,labs[3],cex=0.7)
text(x,(days.POS$sum.low/14975)*100,
     round((days.POS$sum.low/14975)*100,1),font=2,col="black",pos=3,cex=1,offset=0.25)

ylim.val=c(0,6);by.y=3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.vlow/14975)*100,col=adjustcolor(cols.alts,0.5),
          ylim=ylim.val,space=0,axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,labs[4],cex=0.7)
text(x,(days.POS$sum.vlow/14975)*100,
     format(round((days.POS$sum.vlow/14975)*100,1)),font=2,col="black",pos=3,cex=1,offset=0.25)
mtext(side=2,line=0.5,"Percent of Time",outer=T)
mtext(side=1,line=4,"Alternative",outer=F)
dev.off()


days.POS2=ddply(lakeO.stage,"Alt",summarise,N.days=N.obs(STAGE),
                stg.10=(sum(STAGE>10,na.rm=T)/N.days)*100,
                stg.11=(sum(STAGE>11,na.rm=T)/N.days)*100,
                stg.12=(sum(STAGE>12,na.rm=T)/N.days)*100,
                stg.13=(sum(STAGE>13,na.rm=T)/N.days)*100,
                stg.14=(sum(STAGE>14,na.rm=T)/N.days)*100,
                stg.15=(sum(STAGE>15,na.rm=T)/N.days)*100,
                stg.16=(sum(STAGE>16,na.rm=T)/N.days)*100,
                stg.17=(sum(STAGE>17,na.rm=T)/N.days)*100,
                stg.18=(sum(STAGE>18,na.rm=T)/N.days)*100
)

barplot(t(subset(days.POS2,Alt=="FWO")[,3:ncol(days.POS2)]),beside=T,horiz = T)
barplot(t(subset(days.POS2,Alt=="ASR")[,3:ncol(days.POS2)]),beside=T,horiz = T)
hist(subset(lakeO.stage,Alt=="FWO")$STAGE,breaks=seq(8,19,1))
x=hist(subset(lakeO.stage,Alt=="ASR")$STAGE,breaks=seq(8,19,1))
text(x$mids,x$counts,format(round((x$counts/14975)*100,1)),pos=3,offset=0.1)

# png(filename=paste0(plot.path,"LOK_stg_hist.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,1,1),lwd=0.1);
layout(matrix(1:3,3,1,byrow=F))

ylim.val=c(0,30);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(8,19);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
for(i in 1:length(alts)){
  x=hist(subset(lakeO.stage,Alt==alts[i])$STAGE,xaxs="i",yaxs="i",
         breaks=seq(8,19,1),ylim=(ylim.val/100)*14975,xlim=xlim.val,
         axes=F,ann=F,col=adjustcolor(cols.alts[i],0.5))
  text(x$mids,x$counts,format(round((x$counts/14975)*100,1)),pos=3,offset=0.2)
  if(i==3){axis_fun(1,xmaj,xmaj,xmaj,line=-0.5)}else{axis_fun(1,xmaj,xmaj,NA)}
  axis_fun(2,(ymaj/100)*14975,(ymin/100)*14975,ymaj)
  box(lwd=1)
  mtext(side=3,adj=0,line=-1.25,paste(" Alt: ",alts[i]))
}
mtext(side=2,outer=T,line=0.5,'Percent of Time')
mtext(side=1,line=2,"Stage (Ft, NGVD29)")
dev.off()

# Old RECOVER Stage Envelope ----------------------------------------------
stg.env.vals=read.csv("C:/Julian_LaCie/_GitHub/LOWRP_ModelEval/_resources/LOK_2007StgEnv_Da.csv")
stg.env.vals$Date=date.fun(stg.env.vals$Date,form="%m/%d/%Y")
stg.env.vals.da=ddply(stg.env.vals,"Jday",summarise,Env_Low=mean(Env_Low),Env_High=mean(Env_High))

plot(Env_Low~Jday,stg.env.vals.da,ylim=c(11,17),type="l")
lines(Env_High~Jday,stg.env.vals.da)

stg.env.07=lakeO.stage
stg.env.07=merge(stg.env.07,stg.env.vals.da,by.x="DoY",by.y="Jday",all.x=T)
stg.env.07=stg.env.07[order(stg.env.07$Alt,stg.env.07$Date),]

stg.env.07$dev_lower=with(stg.env.07,pmax(0,Env_Low-STAGE))
stg.env.07$dev_upper=with(stg.env.07,pmax(0,STAGE-Env_High))
stg.env.07$env=with(stg.env.07,ifelse(dev_upper+dev_lower>0,0,1))

stg.score.07=ddply(stg.env.07,"Alt",summarise,
                   N.env=(sum(env,na.rm=T)/14975)*100,
                   N.below.env=(sum(dev_lower>0)/14975)*100,
                   N.above.env=(sum(dev_upper>0)/14975)*100,
                   sum.dev_lower.ftdays=sum(dev_lower),
                   sum.dev_upper.ftdays=sum(dev_upper),
                   stg_env=(sum(env)/14975)*100,
                   ex.low=(sum(STAGE<10.0)/14975)*100,
                   ex.high=(sum(STAGE>17.0)/14975)*100)
stg.score.07$sum.dev_lower.ftwks=stg.score.07$sum.dev_lower/7
stg.score.07$sum.dev_upper.ftwks=stg.score.07$sum.dev_upper/7


stg.score.07$std.score.exlow=with(stg.score.07,((ex.low/100)*36*52)*-0.185+100)
stg.score.07$std.score.exhigh=with(stg.score.07,((ex.high/100)*36*52)*-0.253+100)
# 41-yr Equation
stg.score.07$std.score.lower=with(stg.score.07,ifelse(sum.dev_lower.ftwks<218.7,100,sum.dev_lower.ftwks*-0.05227+111.429))
stg.score.07$std.score.higher=with(stg.score.07,sum.dev_upper.ftwks*-0.0469+100)

stg.score.07$wgh.score.exlow=with(stg.score.07,(0.25*std.score.exlow))
stg.score.07$wgh.score.exhigh=with(stg.score.07,(0.50*std.score.exhigh))
stg.score.07$wgh.score.std.lower=with(stg.score.07,(0.10*std.score.lower))
stg.score.07$wgh.score.std.higher=with(stg.score.07,(0.15*std.score.higher))
stg.score.07$wgh.scr=rowSums(stg.score.07[,c("wgh.score.exlow",
                                             "wgh.score.exhigh",
                                             "wgh.score.std.lower",
                                             "wgh.score.std.higher")])
stg.score.07=stg.score.07[match(alts,stg.score.07$Alt),]


# png(filename=paste0(plot.path,"LOK_OldScore_total_parsed.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,105);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,1),oma=c(2.5,2,0.5,0.5),lwd=0.5);
layout(matrix(1:2,1,2),widths=c(1,0.5))

cols.stack=viridis::magma(4,0.5)
x=barplot(t(stg.score.07[,c("wgh.score.exlow",
                            "wgh.score.exhigh",
                            "wgh.score.std.lower",
                            "wgh.score.std.higher")]),
          col=cols.stack,ylim=ylim.val,
          border=adjustcolor("grey30",0.5),names.arg=rep(NA,n.alts),
          space=0.05,axes=F,ann=F)
text(x,stg.score.07$wgh.scr,round(stg.score.07$wgh.scr,2),pos=3,offset=0.2)
axis_fun(1,x,x,alts,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.5,"Standardized Score")
mtext(side=1,line=2,"Alternative")

plot(0:1,0:1,ann=F,axes=F,type="n")
mtext(side=3,line=-2,"Lake Okeechobee\nEnvelope Penalty Score\nAll Years")
legend("center",legend=rev(c("Extreme Low", "Extreme High","Below Envelope","Above Envelope")),
       pch=22,lty=0,lwd=0.1,pt.bg=rev(cols.stack),pt.cex=2,col=adjustcolor("grey30",0.5),
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title=" Performance Measure")
mtext(side=1,line=-1,adj=1,"Simulation Period of\nRecord (1965-2005). \n2007 RECOVER LOK PM",cex=0.75)
dev.off()





# Regulation Schedules ----------------------------------------------------
zones=c(paste("LOK",paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),sep="-"),
        paste("LOK",paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_"),sep="_"))
zone.alias=data.frame(zone=zones,
                      zone2=c(paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_")))
n.alts=length(alts)
reg.sch=data.frame()
for(j in 1:3){
  dss_out=opendss(paste0(data.path,alts[j],"/RSMBN_output.dss"))
  
  for(i in 1:length(zones)){
    paths=paste0("/RSMBN/",zones[i],"/STAGE/01JAN1965 - 01JAN2005/1DAY/INPUT/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$zone=zones[i]
    tmp$Alt=alts[j]
    reg.sch=rbind(tmp,reg.sch)
    print(i)
  }
}

unique(reg.sch$Alt)
unique(reg.sch$zone)

reg.sch$DOY=as.numeric(format(reg.sch$Date,"%j"))
reg.sch=merge(reg.sch,zone.alias,"zone")
reg.sch2=reshape2::dcast(reg.sch,Alt+DOY~zone2,value.var = "STAGE",mean)

head(lakeO.stage)
reg.sch3=reshape2::dcast(reg.sch,Alt+Date~zone2,value.var = "STAGE",mean)

vars=c("Alt","Date","STAGE")
lakeO.stage.reg=merge(lakeO.stage[,vars],
                      reg.sch3,c("Alt","Date"))


lakeO.stage.reg$above.ZoneD=with(lakeO.stage.reg,ifelse(STAGE>ZONE_C,1,0))
lakeO.stage.reg$within.ZoneD=with(lakeO.stage.reg,ifelse(STAGE<ZONE_C&STAGE>ZONE_D0,1,0))
lakeO.stage.reg$below.ZoneD=with(lakeO.stage.reg,ifelse(STAGE<ZONE_D0,1,0))

zone.freq=ddply(lakeO.stage.reg,"Alt",summarise,
                 above.zoneD=sum(STAGE>ZONE_C),
                 zoneD=sum(ifelse(STAGE<ZONE_C&STAGE>ZONE_D0,1,0)),
                 below.zoneD=sum(STAGE<ZONE_D0))

zone.freq=zone.freq[match(Alts,zone.freq$Alt),]
zone.freq[,2:4]=(zone.freq[,2:4]/14975)*100
zone.freq.all=zone.freq

vars=c("Date","Alt","above.ZoneD","within.ZoneD","below.ZoneD")
q.dat.xtab.zones=merge(q.dat.xtab,lakeO.stage.reg[,vars],c("Date","Alt"))

q.dat.xtab.zones$above.ZoneD.CRE=with(q.dat.xtab.zones,S79*above.ZoneD)
q.dat.xtab.zones$within.ZoneD.CRE=with(q.dat.xtab.zones,S79*within.ZoneD)
q.dat.xtab.zones$below.ZoneD.CRE=with(q.dat.xtab.zones,S79*below.ZoneD)
q.dat.xtab.zones$above.ZoneD.CRE.lake=with(q.dat.xtab.zones,S79_QPFCSOURCE_LAKE*above.ZoneD)
q.dat.xtab.zones$within.ZoneD.CRE.lake=with(q.dat.xtab.zones,S79_QPFCSOURCE_LAKE*within.ZoneD)
q.dat.xtab.zones$below.ZoneD.CRE.lake=with(q.dat.xtab.zones,S79_QPFCSOURCE_LAKE*below.ZoneD)

q.dat.xtab.zones$above.ZoneD.SLE=with(q.dat.xtab.zones,S80*above.ZoneD)
q.dat.xtab.zones$within.ZoneD.SLE=with(q.dat.xtab.zones,S80*within.ZoneD)
q.dat.xtab.zones$below.ZoneD.SLE=with(q.dat.xtab.zones,S80*below.ZoneD)
q.dat.xtab.zones$above.ZoneD.SLE.lake=with(q.dat.xtab.zones,S80_QPFCSOURCE_LAKE*above.ZoneD)
q.dat.xtab.zones$within.ZoneD.SLE.lake=with(q.dat.xtab.zones,S80_QPFCSOURCE_LAKE*within.ZoneD)
q.dat.xtab.zones$below.ZoneD.SLE.lake=with(q.dat.xtab.zones,S80_QPFCSOURCE_LAKE*below.ZoneD)

vars=c("Date","Alt","above.ZoneD.CRE","within.ZoneD.CRE","below.ZoneD.CRE")
q.dat.xtab.zones.melt=reshape2::melt(q.dat.xtab.zones[,vars],id.vars=vars[1:2])
# q.dat.xtab.zones.melt2=subset(q.dat.xtab.zones.melt,Alt%in%c("NA25",'260467'))

vars=c("above.ZoneD.CRE","within.ZoneD.CRE","below.ZoneD.CRE")
density.group.all=data.frame()

for(j in 1:length(alts)){
  for(i in 1:3){
    tmp=subset(q.dat.xtab.zones.melt,Alt==alts[j])
    rng.val=range(log(tmp[tmp$value!=0,]$value),na.rm=T)
    nx=length(subset(tmp,variable==vars[i])$value)
    w=rep(1/nx,nx)
    n.den=512#nx; #default is 512
    dens=density(log(subset(tmp,variable==vars[i]&value!=0)$value),
                 # weights=w,   
                 kernel="gaussian",
                 from=rng.val[1],
                 to=rng.val[2],
                 n = n.den)
    dens=data.frame(x=exp(dens$x),
                    y=dens$y,
                    scaled =  dens$y / max(dens$y, na.rm = TRUE),
                    ndensity = dens$y / max(dens$y, na.rm = TRUE),
                    count=dens$y*nx,
                    n=nx,
                    group=vars[i])
    dens$Alt=alts[j]
    density.group.all=rbind(density.group.all,dens)
  }
  print(j)
}

plot(y~x,density.group.all,log="x")
cols4=wesanderson::wes_palette("Zissou1", 3, type = "continuous")
# png(filename=paste0(plot.path,"S79Q_zones.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(2.5,2.5,1,0.5));
layout(matrix(c(1:3),1,3,byrow=F))

ylim.val=c(0,2.5);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,20000);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
for(i in 1:length(alts)){
  tmp=subset(density.group.all,Alt==alts[i])
  plot(y~x,tmp,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
  abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
  with(subset(tmp,group=="below.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
  with(subset(tmp,group=="within.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
  with(subset(tmp,group=="above.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
  abline(v=c(450,750,2100,2600),lty=2,lwd=0.75)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  if(i==1){mtext(side=2,line=2,"Density")}
  mtext(side=3,adj=0,alts[i])
  if(i==1){
    legend("topleft",legend=c("Above Zone D","Within Zone D","Below Zone D"),
           pch=c(22,22,22),
           lty=0,lwd=0.01,
           col=rev(cols4),
           pt.bg=adjustcolor(rev(cols4),0.5),
           pt.cex=1,ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=1,line=1,outer=T,"S-79 Daily Discharge (ft\u00B3 sec\u207B\u00B9)")
dev.off()
