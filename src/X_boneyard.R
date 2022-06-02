

mean.comp=ddply(regq.dat.CY,c("Alt","Region"),summarise,mean.val=mean(TFlow.kAcft),SE.val=SE(TFlow.kAcft))
mean.comp=merge(mean.comp,data.frame(Alt=Alts,plt.val=1:3),"Alt")
mean.comp=mean.comp[order(mean.comp$plt.val,mean.comp$Region),]

cols2=adjustcolor(rev(wesanderson::wes_palette("Zissou1",4,"continuous")),0.5)
region.vals=c("WCAs","Cal","StL","LWLagoon")
# png(filename=paste0(plot.path,"AvgFloodControl2.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.25,0.5),oma=c(1,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.4))

xlim.val=c(0.75,3.25)
ylim.val=c(0,600);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(mean.val~plt.val,mean.comp,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymaj,col="grey",lty=3,lwd=0.5)
for(i in 1:4){
  with(subset(mean.comp,Region==region.vals[i]),
       pt_line_error(plt.val,mean.val,SE.val,1,cols2[i],1.5,21,cols2[i],
                     length=0.025,cex=1.25,pt.lwd=0.5,er.lwd=1.25))
}
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:3,1:3,Alts,line=-0.5,las=1,cex=0.8);box(lwd=1)
mtext(side=2,line=2.5,"Discharge Volume (x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=1.5,"Alternatives")
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(0.5,0.25,legend=c("Everglades\n(S351 & S354)","Caloosahatchee River\n(S77)","St. Lucie River\n(S308)","Lake Worth Lagoon\n(S271/C10A)"),
       pch=21,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols2,
       pt.cex=1.25,ncol=2,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()