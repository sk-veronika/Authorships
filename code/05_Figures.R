
setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")
d = read.csv("03_Data/Data_authorships_11.csv")
  names(d)
  dim(d)


###   Variables   ###

first = d$first_position
last = d$last_position
pos = d$author_position
other =(1-first)*(1-last)

g = as.factor(d$gender)
g = relevel(g,ref="male")
  table(g,useNA="ifany") 
#levels(g) = c("Male","Female")

aff = as.factor(d$affil_cat)
aff = relevel(aff,ref="Upper middle")
aff = relevel(aff,ref="High")
  table(aff,useNA="ifany")

yr = d$publicationyear		
yrx = yr - 2007
  summary(yr)


###   Outcome: Standardized Authorship Positions   ###

pos.std = NULL
for (i in 1:length(pos)){
  if (last[i]==1)  pos.std[i]=100
  if (first[i]==1) pos.std[i]=0
  if (other[i]==1) pos.std[i]=(pos[i]-1)*(100/(n[i]-1))
}



setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships/05_Figures/20_2023-02-14")


###  Figure2: Histograms of Authors' Positions on Article  ###

##   Figure 2A: Histogram of (Std) Position - by Gender

#B = 0:100
#B = 2*(0:50)
 B = 4*(0:26)-2  	## <- this one




####################
#png("Figure1A.png", width = 700, height = 700); par(cex=1.3)

h1  = hist(pos.std[g=="male"], breaks=B, col=colM[1], ylim=c(0,200),
	xlab="Percentile of authorship position (%)", main="")	#Histogram of Authorship Positions on Article - by Gender#(standardized)")
h2  = hist(pos.std[g=="female"]+0.6, breaks=B+0.6, col=colF[1], add=T)
#h2 = hist(pos.std[g=="female"]+0.3, breaks=B+0.3, col=colF[1], add=T)  # use when B=0:100
hist(pos.std[g=="male"], breaks=B, add=T,col=NULL)
text(7,170,"First"); text(93,170,"Last")
legend(40,200,xjust=0,yjust=1,legend=c("Female","Male"), fill=c(colF[1],colM[1]))
#legend(40,max(h1$counts),xjust=0,yjust=1,legend=c("Female","Male"), fill=c(colF[1],colM[1]))

#dev.off()

##   Figure 1C: Histogram of (Stand) Position - by Affiliation

#summary(aff) 

#png("Figure2C.png", width = 700, height = 700); par(cex=1.3)

h1  = hist(pos.std[aff=="High"], breaks=B, col=colAh[1], 
           xlab="Percentile of authorship position (%)",
           main="")#Histogram of Authorship Positions on Article - by Affiliation(standardized)")
h2  = hist(pos.std[aff=="Upper middle"]+0.5, breaks=B+0.5, col=colAm[1], add=T)
h3  = hist(pos.std[aff=="Lower"]+1, breaks=B+1, col=colAl[1], add=T)
text(7,170,"First"); text(93,170,"Last")
legend(40,200,xjust=0,yjust=1,legend=c("High income","Upper middle income","Lower income"), fill=c(colAh[1],colAm[1],colAl[1]))

#dev.off()
##################




png("Figure1.png", width = 700, height = 2*700) 
par(mfcol=c(2,1),cex=1.7) 

##   Figure 1B: Proportion of Female Authors per Position

h1  = hist(pos.std[g=="male"], breaks=B,plot=F)
h2  = hist(pos.std[g=="female"], breaks=B,plot=F)

pp = round(h2$counts/(h1$counts+h2$counts),2)
m1 = lm(pp~h1$mids,w=(h1$counts+h2$counts));summary(m1)
 a = m1$coef[1]; b = m1$coef[2]

#  Confidence band
#cbL = predict(m1, interval="confidence", level=0.95)[,2]
#cbU = predict(m1, interval="confidence", level=0.95)[,3]

#png("Figure1B.png", width = 700, height = 700); par(cex=cex)

plot(h1$mids, pp, cex=sqrt(h1$counts+h2$counts)/5, 
	lwd=2, col=colF[2], xlim=c(-2,102), ylim=c(0,1), 
	#main="Proportion of Female Authorships per (Standardized) Article Position", 
	xlab="Percentile of authorship position (%)",
	ylab="Proportion  of  female  authorships")
#polygon(x=c(h1$mids,rev(h1$mids)), y=c(cbL,rev(cbU)), col=colF[1], border=colF[1])
abline(h=.2*(1:4),col="grey",lty=5)
lines(x=c(0,100),y=c(a,a+100*b),col=colF[3],lwd=2)
points(h1$mids, pp, cex=sqrt(h1$counts+h2$counts)/5, lwd=2, col=colF[2])
text(2,pp[1]+0.15,"First
	position")
text(98,pp[length(pp)]+.15,"Last
	position")
#dev.off()


##   Figure 1D: Proportion of Authors from Low/Middle/High Income Countries per Position

h1  = hist(pos.std[aff=="High"], breaks=B,plot=F)
h2  = hist(pos.std[aff=="Upper middle"], breaks=B,plot=F)
h3  = hist(pos.std[aff=="Lower"], breaks=B,plot=F)

all.counts = h1$counts+h2$counts+h3$counts

pp1 = round(h1$counts/all.counts,2)
pp2 = round(h2$counts/all.counts,2)
pp3 = round(h3$counts/all.counts,2)

m1 = lm(pp1~h1$mids,w=(h1$counts+h2$counts+h3$counts)); a1 = m1$coef[1]; b1 = m1$coef[2]
m2 = lm(pp2~h1$mids,w=(h1$counts+h2$counts+h3$counts)); a2 = m2$coef[1]; b2 = m2$coef[2]
m3 = lm(pp3~h1$mids,w=(h1$counts+h2$counts+h3$counts)); a3 = m3$coef[1]; b3 = m3$coef[2]


#png("Figure2D.png", width = 700, height = 700); par(cex=cex)

plot(h1$mids, pp1, cex=sqrt(all.counts)/5, 
	lwd=2, col=colAh[1], xlim=c(-2,102), ylim=c(0,1), 
	main="",#Proportion of Low / Middle / High Income Authorships 
	#per (Standardized) Article Position", 
	xlab="Percentile of authorship position (%)",
	ylab="Proportion  of  income-group  authorships")
abline(h=.2*(0:4),col="grey",lty=5)
points(h1$mids, pp1, cex=sqrt(all.counts)/5, lwd=2, col=colAh[1])
points(h1$mids, pp2, cex=sqrt(all.counts)/5, lwd=2, col=colAm[1])
points(h1$mids, pp3, cex=sqrt(all.counts)/5, lwd=2, col=colAl[1])

text(2,pp1[1]+0.15,"First
	position")
text(98,pp1[length(pp)]+.15,"Last
	position")
legend(25,1,legend=c("High income","Upper middle income","Lower income"),pch=1,pt.cex=2,lwd=2,col=c(colAh[1],colAm[1],colAl[1]))

#   Fitted curves: Poly degree 3
fit1.3 = lm( pp1 ~ poly(h1$mids,3,raw=TRUE), weight=h1$counts)	  
 lines(h1$mids, predict(fit1.3), col=colAh[1],lwd=2 )			 
fit2.3 = lm( pp2 ~ poly(h2$mids,3,raw=TRUE), weight=h1$counts)
 lines(h1$mids, predict(fit2.3), col=colAm[1],lwd=2 )
fit3.3 = lm( pp3 ~ poly(h3$mids,3,raw=TRUE), weight=h1$counts)
 lines(h1$mids, predict(fit3.3), col=colAl[1],lwd=2)

dev.off()

#  Poly degree 2
#fit1.2 = lm( pp1 ~ poly(h1$mids,2,raw=TRUE), weight=h1$counts)	
# lines(h1$mids, predict(fit1.2), col=colAh[1],lwd=2)
#fit2.2 = lm( pp2 ~ poly(h2$mids,2,raw=TRUE), weight=h1$counts)
# lines(h1$mids, predict(fit2.2), col=colAm[1],lwd=2 )
#fit3.2 = lm( pp3 ~ poly(h3$mids,2,raw=TRUE), weight=h1$counts)
# lines(h1$mids, predict(fit3.2), col=colAl[1],lwd=2 )









###  Proportions of (first/last/other) authorships - per year  ###

#tx = table(g,yr); prop.table(tx,margin=2)[1,]
#tx = table(aff,yr); prop.table(tx,margin=2)

#n.art.y = table(first,yr)[2,]
n.first.y = table(first,yr)[2,]
n.last.y  = table(last,yr)[2,]
n.other.y = table(other,yr)[2,] 

tx = table(g[first==1], yr[first==1]); p.fem.first = prop.table(tx,margin=2)[2,]
tx = table(g[last==1],  yr[last==1]) ; p.fem.last  = prop.table(tx,margin=2)[2,]
tx = table(g[other==1], yr[other==1]); p.fem.other = prop.table(tx,margin=2)[2,]

tx = table(aff[first==1],yr[first==1]); tx; ptx=prop.table(tx,margin=2); #ptx 
p.hi.first = ptx[1,]; p.mid.first = ptx[2,]; p.low.first = ptx[3,]

tx = table(aff[last==1],yr[last==1]); tx; ptx=prop.table(tx,margin=2)
p.hi.last = ptx[1,]; p.mid.last = ptx[2,]; p.low.last = ptx[3,]

tx = table(aff[other==1],yr[other==1]); tx; ptx=prop.table(tx,margin=2)
p.hi.other = ptx[1,]; p.mid.other = ptx[2,]; p.low.other = ptx[3,]


yy = (2007:2020)



###   Figure 2: Proportions of authorships - per year   ###

png("Figure2.png", width = 2*700, height = 2*700) 
par(mfrow=c(2,2),cex=2)


##    Fig 2A: Female (first/last/other) authorships - per year 

colFr=colF[4]; colL=colF[3]; colO="grey"

#png("Figure2A.png", width = 700, height = 700); par(cex=1.5)

plot(yy,p.fem.other,col=colO,cex=0.3*sqrt(n.other.y),lwd=2,xlab="Year",xlim=c(2007,2020.2),ylim=c(0,1), 
	ylab="Proportion  of  female  authorships") #main="Proportion of Female Authorships per Year",
points(yy,p.fem.first,ylim=c(0,1),col=colFr,cex=.5*sqrt(n.first.y),lwd=2)
points(yy,p.fem.last, ylim=c(0,1),col=colL, cex=.5*sqrt(n.last.y), lwd=2)

mod3 = lm(p.fem.other~yy, w=sqrt(n.other.y)); mod3; a = mod3$coef[1];b=mod3$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),lwd=2,col=colO)
mod4 = lm(p.fem.first~yy, w=sqrt(n.first.y)); mod4; a = mod4$coef[1];b=mod4$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),col=colFr,lwd=2)
mod5 = lm(p.fem.last~yy, w=sqrt(n.last.y)); mod5; a = mod5$coef[1];b=mod5$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),col=colL,lwd=2)
legend(2007,1,legend=c("First authorships", "Last authorships","Other authorships"),pch=1,lwd=2,pt.cex=1.5,col=c(colFr,colL,colO) )

#dev.off()


##    Fig 2B: HIC (first/last/other) authorships - per year 

#png("Figure2B.png", width = 700, height = 700); par(cex=1.5)

plot(yy,p.hi.other,col=colO,cex=0.3*sqrt(n.other.y),lwd=2,xlab="Year",xlim=c(2007,2020.2),ylim=c(0,1), 
	ylab="Proportion  of  HIC  authorships") #main="Proportion of Female Authorships per Year",
points(yy,p.hi.first,ylim=c(0,1),col=colAh[2],cex=.5*sqrt(n.first.y),lwd=2)
points(yy,p.hi.last, ylim=c(0,1),col=colAh[3], cex=.5*sqrt(n.last.y), lwd=2)

mod3 = lm(p.hi.other~yy, w=sqrt(n.other.y)); mod3; a = mod3$coef[1];b=mod3$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),lwd=2,col=colO)
mod4 = lm(p.hi.first~yy, w=sqrt(n.first.y)); mod4; a = mod4$coef[1];b=mod4$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),col=colAh[2],lwd=2)
mod5 = lm(p.hi.last~yy, w=sqrt(n.last.y)); mod5; a = mod5$coef[1];b=mod5$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),col=colAh[3],lwd=2)
legend(2007,1,legend=c("First authorships", "Last authorships","Other authorships"),
	pch=1,lwd=2,pt.cex=1.5,col=c(colAh[2],colAh[3],colO)  )

#dev.off()


##    Fig 2C: MIC (first/last/other) authorships - per year 

#png("Figure2C.png", width = 700, height = 700); par(cex=1.5)

plot(yy,p.mid.other,col=colO,cex=0.3*sqrt(n.other.y),lwd=2,xlab="Year",xlim=c(2007,2020.2),ylim=c(0,1), 
	ylab="Proportion  of  UMIC  authorships") #main="Proportion of Female Authorships per Year",
points(yy,p.mid.first,ylim=c(0,1),col=colAm[2], cex=.5*sqrt(n.first.y),lwd=2)
points(yy,p.mid.last, ylim=c(0,1),col=colAm[3], cex=.5*sqrt(n.last.y), lwd=2)

mod3 = lm(p.mid.other~yy, w=sqrt(n.other.y)); mod3; a = mod3$coef[1];b=mod3$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),lwd=2,col=colO)
mod4 = lm(p.mid.first~yy, w=sqrt(n.first.y)); mod4; a = mod4$coef[1];b=mod4$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),col=colAm[2],lwd=2)
mod5 = lm(p.mid.last~yy, w=sqrt(n.last.y)); mod5; a = mod5$coef[1];b=mod5$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),col=colAm[3],lwd=2)
legend(2007,1,legend=c("First authorships", "Last authorships","Other authorships"),
	pch=1,lwd=2,pt.cex=1.5,col=c(colAm[2],colAm[3],colO))

#dev.off()


##    Fig 2D: LIC (first/last/other) authorships - per year 

#png("Figure2D.png", width = 700, height = 700); par(cex=1.5)

plot(yy,p.low.other,col=colO,cex=0.3*sqrt(n.other.y),lwd=2,xlab="Year",xlim=c(2007,2020.2),ylim=c(0,1), 
	ylab="Proportion  of  LIC  authorships") #main="Proportion of Female Authorships per Year",
points(yy,p.low.first,ylim=c(0,1),col=colAl[2],cex=.5*sqrt(n.first.y),lwd=2)
points(yy,p.low.last, ylim=c(0,1),col=colAl[3], cex=.5*sqrt(n.last.y), lwd=2)

mod3 = lm(p.low.other~yy, w=sqrt(n.other.y)); mod3; a = mod3$coef[1];b=mod3$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),lwd=2,col=colO)
mod4 = lm(p.low.first~yy, w=sqrt(n.first.y)); mod4; a = mod4$coef[1];b=mod4$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),col=colAl[2],lwd=2)
mod5 = lm(p.low.last~yy, w=sqrt(n.last.y)); mod5; a = mod5$coef[1];b=mod5$coef[2]
lines(x=c(2007,2020),y=c(a+2007*b,a+2020*b),col=colAl[3],lwd=2)
legend(2007,1,legend=c("First authorships", "Last authorships","Other authorships"),
	pch=1,lwd=2,pt.cex=1.5,col=c(colAl[2],colAl[3],colO)  )

dev.off()












