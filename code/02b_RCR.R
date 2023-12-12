
setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")
d = read.csv("03_Data/Data_authorships_10.csv")
  names(d)

g = as.factor(d$gender)
  table(g,useNA="ifany")
aff = as.factor(d$affil_cat)
aff = relevel(aff,ref="Upper middle")
aff = relevel(aff,ref="High")
  table(aff,useNA="ifany")
  

  
#######  IF, cit, RCR  ########

##   Median (IQR)
##   by Gender x Affiliation


IF = as.numeric(d$jif); summary(IF)
cit = as.numeric(d$cit);summary(cit)
RCR = as.numeric(d$RCR);summary(RCR)

gL = levels(g)
affL = levels(aff)

IF.f = CIT.f = RCR.f = IF.l = CIT.l = RCR.l = matrix(NA, 3,2)

for (i in 1:3){
for (j in 1:2){ 	#  i=2;j=2
 # summary(RCR[last==1&aff==affL[i]&g==gL[j]])  
 s = round(summary(IF[first==1&aff==affL[i]&g==gL[j]]),1)
 IF.f[i,j] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(cit[first==1&aff==affL[i]&g==gL[j]]),1)
 CIT.f[i,j] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(RCR[first==1&aff==affL[i]&g==gL[j]]),1)
 RCR.f[i,j] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
}}

for (i in 1:3){
for (j in 1:2){ 	# i=1;j=1
 s = round(summary(IF[last==1&aff==affL[i]&g==gL[j]]),1)
 IF.l[i,j] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(cit[last==1&aff==affL[i]&g==gL[j]]),1)
 CIT.l[i,j] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(RCR[last==1&aff==affL[i]&g==gL[j]]),1)
 RCR.l[i,j] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
}}

IF.f;CIT.f;RCR.f
IF.l;CIT.l;RCR.l

Tab = cbind( c(IF.f[,1],IF.l[,1]), c(CIT.f[,1],CIT.l[,1]), c(RCR.f[,1],RCR.l[,1]), c(IF.f[,2],IF.l[,2]), c(CIT.f[,2],CIT.l[,2]), c(RCR.f[,2],RCR.l[,2]) )
rownames(Tab)=c(affL,affL);colnames(Tab)=rep(c("JIF","Total citations","RCR"),2)
Tab

write.csv(Tab,"06_Results/Table_impact_2023-01-27.csv") 



###   Test of difference - no-parametric  ###
# library(dplyr)


##  Gender

##  Normality assumption
shapiro.test(RCR[last==1&g=="female"])     # N INVALID !!
shapiro.test(RCR[last==1&g=="male"])       # N INVALID !!

##  t-test
#summary(lm(RCR[last==1] ~ g[last==1]))
#t.test(RCR[last==1] ~ g[last==1], var.equal = TRUE)
#t.test(RCR[last==1] ~ g[last==1], var.equal = FALSE)
#t.test(RCR[last==1&g=="female"], RCR[last==1&g=="male"], var.equal = FALSE)

##  Wilcoxon (rank) test
wilcox.test(RCR[first==1] ~ g[first==1], exact = FALSE)
wilcox.test(RCR[last==1] ~ g[last==1], exact = FALSE)


##  Affiliation

##  N assumption
shapiro.test(RCR[last==1&aff==affL[3]])     ## N assumption - INVALID !!
shapiro.test(RCR[first==1&aff==affL[1]]) 

##  t-test
#t.test(RCR[last==1&aff==affL[1]], RCR[last==1&aff==affL[2]],var.equal = FALSE)
#t.test(RCR[last==1&aff==affL[1]], RCR[last==1&aff==affL[3]],var.equal = FALSE)
#t.test(RCR[last==1&aff==affL[3]], RCR[last==1&aff==affL[2]],var.equal = FALSE)

##  Kruskal-Wallis (rank) tests
kruskal.test(RCR[first==1] ~ aff[first==1])
kruskal.test(RCR[last==1] ~ aff[last==1])

##  Pair-wise comparisons
#wilcox.test(RCR[last==1&aff==affL[1]], RCR[last==1&aff==affL[2]])
#wilcox.test(RCR[last==1&aff==affL[1]], RCR[last==1&aff==affL[3]])
#wilcox.test(RCR[last==1&aff==affL[3]], RCR[last==1&aff==affL[2]])






####   RCR  ####
##     Mean (SD)

RCR = as.numeric(d$RCR);summary(RCR)
gL = levels(g)
affL = levels(aff)

RCRx = matrix(NA, 6,2)
for (i in 1:3){
  for (j in 1:2){ 	# i=1;j=1
    rcrx = RCR[first==1&aff==affL[i]&g==gL[j]]
    mx = round(mean(rcrx),1);mx
    sx = round(sd(rcrx),1);sx
    RCRx[i,j] = paste(mx," (",sx,")",sep="")
  }}
for (i in 1:3){
  for (j in 1:2){ 	# i=1;j=1
    rcrx = RCR[last==1&aff==affL[i]&g==gL[j]]
    mx = round(mean(rcrx),1);mx
    sx = round(sd(rcrx),1);sx
    RCRx[i+3,j] = paste(mx," (",sx,")",sep="")
  }}
RCRx

write.csv(RCRx,"06_Results/Table_RCR_2023-01-27.csv") 







###  Marginal summaries by Gender, Affiliation & Overall

##  Gender

M.g = matrix(NA, 2,6)
for (j in 1:2){ 	# i=1;j=2
 s = round(summary(IF[first==1&g==gL[j]]),1)
 M.g[1,1+(j-1)*3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(cit[first==1&g==gL[j]]),1)
 M.g[1,2+(j-1)*3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(RCR[first==1&g==gL[j]]),1)
 M.g[1,3+(j-1)*3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(IF[last==1&g==gL[j]]),1)
 M.g[2,1+(j-1)*3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(cit[last==1&g==gL[j]]),1)
 M.g[2,2+(j-1)*3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(RCR[last==1&g==gL[j]]),1)
 M.g[2,3+(j-1)*3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
}
rownames(M.g)=c("first","last");colnames(M.g)=rep(c("JIF","Total citations","RCR"),2)
M.g
write.csv(M.g,"06_Results/Table_impact_marG.csv") 

##  Affiliation

M.aff = matrix(NA, 6,3)
for (i in 1:3){
 s = round(summary(IF[first==1&aff==affL[i]]),1)
 M.aff[i,1] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(cit[first==1&aff==affL[i]]),1)
 M.aff[i,2] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(RCR[first==1&aff==affL[i]]),1)
 M.aff[i,3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(IF[last==1&aff==affL[i]]),1)
 M.aff[3+i,1] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(cit[last==1&aff==affL[i]]),1)
 M.aff[3+i,2] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(RCR[last==1&aff==affL[i]]),1)
 M.aff[3+i,3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
}
rownames(M.aff) = rep(affL,2);colnames(M.aff)=c("JIF","Total citations","RCR")
M.aff
write.csv(M.aff,"06_Results/Table_impact_marAff.csv") 

## Overall

M = matrix(NA, 2,3)
 s = round(summary(IF[first==1]),1)
 M[1,1]= paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(cit[first==1]),1)
 M[1,2]= paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(RCR[first==1]),1)
 M[1,3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(IF[last==1]),1)
 M[2,1]= paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(cit[last==1]),1)
 M[2,2]= paste(s[3]," (",s[2],"-",s[5],")",sep="")
 s = round(summary(RCR[last==1]),1)
 M[2,3] = paste(s[3]," (",s[2],"-",s[5],")",sep="")
rownames(M) =c("first","last") ;colnames(M)=c("JIF","Total citations","RCR")
M
write.csv(M,"06_Results/Table_impact_mar.csv") 





################################



