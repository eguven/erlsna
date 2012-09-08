args <- commandArgs(TRUE)
d1 = read.table(args[1])
time = as.Date.POSIXct(d1$V1)
incomingNetwork = d1$V3
outgoingNetwork = d1$V5
incomingAgent = d1$V7
outgoingAgent = d1$V9
network_range <- range(0, incomingNetwork, outgoingNetwork)
agent_range <- range(0, incomingAgent, outgoingAgent)
line_count = length(incomingNetwork)
png(filename=args[2], height=1000, width=1000, bg="white")

par(oma=c(1,1,1,1))

plot(time,outgoingNetwork,type="o", col="black", pch=21, lty=1,axes=FALSE, ann=FALSE, ylim=network_range)
lines(time,incomingNetwork,type="o",col="red",pch=22, lty=2)
#axis(1,las=1,at=6*0:line_count)
axis(2,las=1,at=128*0:network_range[2])

par(new=T)
plot(time,outgoingAgent,type="o", col="blue", pch=23, lty=3,axes=FALSE, ann=FALSE, ylim=agent_range)
lines(time,incomingAgent,type="o",col="green",pch=24, lty=4)
axis(4,las=1,at=8*0:agent_range[2])


axis(1,at=time,labels=format(time, "%b%d, %Y"))

title(main="Engagement", col.main="black", font.main=4)
title(xlab=paste("Time",args[3]), col.lab=rgb(0,0.5,0))
title(ylab="Messages", col.lab=rgb(0,0.5,0))
legend(x="top",legend=c("outgoingNetwork","incomingNetwork","outgoingAgent","incomingAgent"), cex=0.8, col=c("black","red","blue","green"), lty=c(1,2,3,4))
dev.off()
