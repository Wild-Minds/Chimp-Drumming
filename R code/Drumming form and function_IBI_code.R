#Inter-beat interval plots for traveling-resting drums
setwd("~/Desktop/Drumming Individual differences Analyses/ibi plots")

#Prepare data
setwd("~/Desktop/Drumming Coding sheets/Waibira")
all.data=read.table(file="/Users/vestaeleuteri/Desktop/Drumming Coding sheets/Waibira/Drumming form and function_IBI_data.csv", header=T, sep=",", stringsAsFactors=T)
xx=table(all.data$Individual.)
xx=xx[xx>=5]
sel.data=droplevels(subset(all.data, Individual.%in%names(xx)))
sel.data=droplevels(subset(sel.data, Individual.!="UNK"))
str(sel.data)
vars=paste("Interbeat", 1:13, sep=".")
sel.data$context[sel.data$Context%in%c("Traveling", "Resting")]="travel_rest"
table(sel.data$Context, sel.data$context)
sel.data$context=factor(sel.data$context, levels=c("travel_rest"))
cols=c("black")

xx=table(sel.data$Individual.)
xx=xx[order(xx)]
plot.index=1

#Plot drumming bouts visualization plot (Figure 3)
par(mar=c(1, 0.2, 0.2, 0.2), mgp=c(1.7, 0.15, 0), tcl=-0.1, las=1)
plot(1, 1, type="n", yaxt="n", ann=F, xlim=range(apply(sel.data[, vars], 1, sum, na.rm=T)), ylim=c(52.5, 0.5), yaxs="i")
box()
added=0
while(added<52 & any(!is.na(xx))){
  sel=52-added-xx
  sel=sel[!is.na(sel)]
  sel=sel[sel>=0]
  if(length(sel)>0){
    sel=names(sel[which.min(sel)])
    sel.sel.data=subset(sel.data, Individual.==sel)
    sel.sel.data=sel.sel.data[rev(order(as.numeric(sel.sel.data$context), apply(!is.na(sel.sel.data[, vars]), 1, sum), decreasing=F)), ]
    points(x=rep(0, nrow(sel.sel.data)), y=added+1:nrow(sel.sel.data), pch=4, col=cols[as.numeric(sel.sel.data$context)])
    for(i in 1:nrow(sel.sel.data)){
      to.add=sel.sel.data[i, vars]
      to.add=to.add[!is.na(to.add)]
      if(length(to.add)>0){
        to.add=cumsum(to.add)
        segments(x0=0, x1=max(to.add), y0=added+i, y1=added+i, lty=3, col=cols[as.numeric(sel.sel.data$context[i])])
        points(x=to.add, y=rep(added+i, length(to.add)), pch=4, col=cols[as.numeric(sel.sel.data$context[i])])
      }
    }
    abline(h=added+nrow(sel.sel.data)+1, lty=2)
    text(labels=sel, x=-0.05, y=mean(c(added+1, added+nrow(sel.sel.data))), srt=90)
    added=added+nrow(sel.sel.data)+1
    xx[sel]=NA
  }else{
    added=53
  }
}
legend("bottomright", legend=rev(levels(sel.data$context)), text.col=rev(col), bty="n")
plot.index=plot.index+1
} 
setwd("~/Desktop/Drumming Individual differences Analyses/ibi plots/ibi x indv and context plots/travel-rest")
dev.copy2pdf(file="URS_travel-rest.pdf", out.type = "pdf") #saved each indvidual´s plot by rerunning from par to last } 


#Plot IBI Cumulative probability distribution plots (Figure 4)
setwd("~/Desktop/Drumming Coding sheets/Waibira")
all.data=read.table(file="/Users/vestaeleuteri/Desktop/Drumming Coding sheets/Waibira/Drumming form and function_IBI_data.csv", header=T, sep=",", stringsAsFactors=T)

#Prepare data
xx=table(all.data$Individual.)
xx=xx[xx>=5]
sel.data=droplevels(subset(all.data, Individual.%in%names(xx)))
str(sel.data)
levels(sel.data$Individual.)
levels(sel.data$Context)
vars=paste("Interbeat", 1:13, sep=".")
sel.data$context[sel.data$Context%in%c("Traveling", "Resting")]="travel_rest"
table(sel.data$Context, sel.data$context)
sel.data$context=factor(sel.data$context, levels=c("travel_rest"))

plot.mat=matrix(1:8, ncol=2, byrow=T) 
layout(plot.mat)
layout.show(max(plot.mat)) 

for(i in levels(sel.data$Individual.)){
  xx=subset(sel.data, Individual.==i)
  xx=as.vector(unlist(c(xx[, vars])))
  xx=xx[!is.na(xx)]
  cum.plot(nvec=log(xx), quants=NULL, xaxt="n")
  range(xx)
  axis(side=1, at=log(0.02*2^(0:6)), labels=0.02*2^(0:6))
  mtext(side=3, line=-1.2, text=i)
} 

setwd("~/Desktop/Drumming Individual differences Analyses/ibi plots/cumulative probability plots/travel-rest")
dev.copy2pdf(file="all_trav_rest.pdf", out.type = "pdf") 
