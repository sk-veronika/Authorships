
setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")
d = read.csv("03_Data/Data_authorships_11.csv")
  names(d)
  dim(d)

first = d$first_position
MR = d$MR
  table(MR[first==1],useNA="ifany")

  
  
  
###   Sensitivity analyses   ###
w = which(MR==0|MR==1)
w = which(MR==0)

w = which(MR==1)
w = which(MR==9)


d = d[w,]  

#########





###   Table 1  (Full sample)   ###

Mx = matrix(NA, 6,6)

nn = dim(d)[1];nn # 3421

art.id = d$pmid
aut.id = d$author_code

#aut.idu = unique(aut.id[aff=="High"]); length(aut.idu)
#aut.idu = unique(aut.id[aff=="Middle"]); length(aut.idu)
#aut.idu = unique(aut.id[aff=="Low"]); length(aut.idu)


N = length(unique(aut.id));N  # N = 1064  nr.of authors
M = length(unique(art.id));M  # M = 313   nr.of articles

Mx[1,1] = N
Mx[1,2] = nn

n = d$max;  summary(n) 
surn = d$surname

yr = d$publicationyear
g = as.factor(d$gender)
  table(g,useNA="ifany")
aff = as.factor(d$affil_cat)
aff = relevel(aff,ref="Upper middle")
aff = relevel(aff,ref="High")
  table(aff,useNA="ifany")

#IF = as.numeric(d$jif); summary(IF)
#RCR = as.numeric(d$RCR);summary(RCR)
#cit = as.numeric(d$cit);summary(cit)


pos = d$author_position
first = d$first_position
last = d$last_position
other = (1-first)*(1-last)

sum(first); 
sum(last);	
sum(first*last)

pos.std = NULL
for (i in 1:length(pos)){
  if (last[i]==1)  pos.std[i]=100
  if (first[i]==1) pos.std[i]=0
  if (other[i]==1) pos.std[i]=(pos[i]-1)*(100/(n[i]-1))
}
summary(pos.std)
sum(pos.std==0)
sum(pos.std==100)

lastx = last
last = 1*(pos.std==100); sum(last)		 # Assigning single-authorships first position 
fol = 1*(first==1|last==1); sum(fol)	 # First OR last authorship

Mx[1,4] = sum(first)
Mx[1,5] = sum(last)
Mx[1,6] = M


##  D(g, aff) among 313 first authorships
tx = table(first,g);   tx[2,]; px = prop.table(tx,margin=1)[2,];px
Mx[2,4] = paste(tx[2,1]," (",round(100*px[1],1), "%)",sep="") 
Mx[3,4] = paste(tx[2,2]," (",round(100*px[2],1), "%)",sep="") 

tx = table(first,aff); tx[2,]; px=prop.table(tx,margin=1)[2,];px
Mx[4,4] = paste(tx[2,1]," (",round(100*px[1],1), "%)",sep="") 
Mx[5,4] = paste(tx[2,2]," (",round(100*px[2],1), "%)",sep="")
Mx[6,4] = paste(tx[2,3]," (",round(100*px[3],1), "%)",sep="") 



##  D(g, aff) among 311 last authorships
tx = table(last,g);   tx[2,]; px=prop.table(tx,margin=1)[2,];px
Mx[2,5] = paste(tx[2,1]," (",round(100*px[1],1), "%)",sep="") 
Mx[3,5] = paste(tx[2,2]," (",round(100*px[2],1), "%)",sep="") 

tx = table(last,aff); tx[2,]; px=prop.table(tx,margin=1)[2,];px
Mx[4,5] = paste(tx[2,1]," (",round(100*px[1],1), "%)",sep="") 
Mx[5,5] = paste(tx[2,2]," (",round(100*px[2],1), "%)",sep="")
Mx[6,5] = paste(tx[2,3]," (",round(100*px[3],1), "%)",sep="") 

Mx


###  Descriptives  ###

##  Nr of authors per article

#  table(aff,useNA="ifany")
#  n = NULL

nx = nf = nm = nlic = nmic = nhic = nlmic = NULL
fol.f = fol.m = fol.lic = fol.mic = fol.hic = NULL
uID = unique(art.id)
M = length(unique(art.id));M  # M = 313   nr.of articles

for (i in 1:length(uID)){ # i=189
  id = uID[i]
  w = which(art.id==id)
  # n[w] = max(pos[w])
  nx[i] = max(pos[w])
  nf[i] = sum(g[w]=="female")
  nm[i] = sum(g[w]=="male")
  nlic[i] = sum(aff[w]=="Lower")
  nmic[i] = sum(aff[w]=="Upper middle")
  nhic[i] = sum(aff[w]=="High")
  fol.f[i] = 1*(sum(g[w][fol[w]==1]=="female")>0)
  fol.m[i] = 1*(sum(g[w][fol[w]==1]=="male")>0)
  fol.lic[i] = 1*(sum(aff[w][fol[w]==1]=="Lower")>0)
  fol.mic[i] = 1*(sum(aff[w][fol[w]==1]=="Upper middle")>0)
  fol.hic[i] = 1*(sum(aff[w][fol[w]==1]=="High")>0)
}
summary(nx)  	# nr. authors per article
FOL = c(sum(fol.hic),sum(fol.mic),sum(fol.lic)); FOL


sum(nf==0);sum(nm==0)
sum(nf>0&nm>0)

sum(nlic==0);sum(nmic==0);sum(nhic==0)
sum(nlic==0&nmic==0)

NOhic.pmid = uID[nhic==0]
#write.csv(NOhic.pmid,"06_Results/No_HIC_articles.csv")


##   Articles with first AND/OR last authors female/male, LIC/MIC/HIC

c(sum(fol.f),100*sum(fol.f)/M);c(sum(fol.m), 100*sum(fol.m)/M)
c(sum(fol.lic),sum(fol.lic)/M);c(sum(fol.mic),sum(fol.mic)/M);c(sum(fol.hic),sum(fol.hic)/M)


Mx[2,6] = paste(round(sum(fol.f)), " (", round(100*sum(fol.f)/M,1)  ,"%)",sep="")
Mx[3,6] = paste(round(sum(fol.m)), " (", round(100*sum(fol.m)/M,1)  ,"%)",sep="")
Mx[4,6] = paste(round(sum(fol.hic)), " (", round(100*sum(fol.hic)/M,1)  ,"%)",sep="")
Mx[5,6] = paste(round(sum(fol.mic)), " (", round(100*sum(fol.mic)/M,1)  ,"%)",sep="")
Mx[6,6] = paste(round(sum(fol.lic)), " (", round(100*sum(fol.lic)/M,1)  ,"%)",sep="")

Mx



##  Articles with only HIC authors 

hic.pmid = uID[nlic==0&nmic==0]
#write.csv(hic.pmid,"06_Results/HIC_articles.csv")

w.hic = which(d$pmid %in% hic.pmid)
o = order(d$pmid[w.hic])
  # d[w.hic[o],]  #c(3,4,6,8:10,123,14,16,17,35,124)]
aut.hic = d[w.hic[o],]  #c(3,4,6,10,123,14,124)]
# write.csv(aut.hic,"06_Results/HIC_articles_authors.csv")




##  Per Authorships

nn
sx = summary(as.factor(g));sx;  px=summary(as.factor(g))/nn;px
Mx[2,2] = paste(sx[1]," (",round(100*px[1],1), "%)",sep="") 
Mx[3,2] = paste(sx[2]," (",round(100*px[2],1), "%)",sep="") 
sx=summary(as.factor(aff));sx; px=summary(as.factor(aff))/nn;px
Mx[4,2] = paste(sx[1]," (",round(100*px[1],1), "%)",sep="") 
Mx[5,2] = paste(sx[2]," (",round(100*px[2],1), "%)",sep="")
Mx[6,2] = paste(sx[3]," (",round(100*px[3],1), "%)",sep="") 

#Mx


##  Per Author

aut.idu = unique(aut.id); length(aut.idu)
givn = d$given_name
surn = d$surname	
  #cbind(surn[1:30],givn[1:30])


n.art = surname = given = NULL
a.gen = factor(NULL,levels=levels(g))
a.aff = factor(NULL,levels=levels(aff))
a.first = a.last = NULL

wx.g = wx.aff = NULL
for (i in 1:N) {  # i=41  i=422  i=950
  wa = which(aut.id==aut.idu[i])
  n.art[i] = length(wa)    # i=7
  surname[i] = as.character(surn[wa][1])
  given[i] = as.character(givn[wa][1])
  a.first[i] = sum(first[wa])
  a.last[i] = sum(last[wa])
  #if(length(unique(g[wa]))>1) wx.g=c(wx.g,i)
  a.gen[i] = g[wa][1]
  tx = table(aff[wa])
  wx = which(as.numeric(tx)!=0)[1]
  a.aff[i] = levels(aff)[wx]  #aff[wa][1]
}

#cbind(surname[1:30],given[1:30])


#surname[wx.g] 
#surname[wx.aff]
#d2 = d[,c(3,4,6,120,7:10,123,124,14,16:17,15)]
#d2[which(aut.id %in% aut.idu[wx.g]),]


sx=summary(as.factor(a.gen));sx; px=summary(as.factor(a.gen))/N;px
Mx[2,1] = paste(sx[1]," (",round(100*px[1],1), "%)",sep="") 
Mx[3,1] = paste(sx[2]," (",round(100*px[2],1), "%)",sep="") 

sx=summary(as.factor(a.aff));sx; px=summary(as.factor(a.aff))/N;px
Mx[4,1] = paste(sx[1]," (",round(100*px[1],1), "%)",sep="") 
Mx[5,1] = paste(sx[2]," (",round(100*px[2],1), "%)",sep="")
Mx[6,1] = paste(sx[3]," (",round(100*px[3],1), "%)",sep="") 

  #Mx
  #table(a.aff,useNA="ifany")
  #summary(a.first)
  #summary(a.last)

##  Number of articles per author

summary(n.art[a.gen=="female"]);sd(n.art[a.gen=="female"])
summary(n.art[a.gen=="male"]);sd(n.art[a.gen=="male"])
summary(n.art[a.aff=="Upper middle"]);sd(n.art[a.aff=="Upper middle"])


mx=mean(n.art);mx; px=sd(n.art);px
Mx[1,3] = paste(round(mx,1)," (",round(px,1), ")",sep="") 

mxf=mean(n.art[a.gen=="female"]);mxf; pxf=sd(n.art[a.gen=="female"]);pxf
mxm=mean(n.art[a.gen=="male"]);mxm;   pxm=sd(n.art[a.gen=="male"]);pxm
Mx[2,3] = paste(round(mxf,1)," (",round(pxf,1), ")",sep="") 
Mx[3,3] = paste(round(mxm,1)," (",round(pxm,1), ")",sep="") 

mx=mean(n.art[a.aff=="High"]);mx; px=sd(n.art[a.aff=="High"]);px
Mx[4,3] = paste(round(mx,1)," (",round(px,1), ")",sep="") 
mx=mean(n.art[a.aff=="Upper middle"]);mx;px=sd(n.art[a.aff=="Upper middle"]);px
Mx[5,3] = paste(round(mx,1)," (",round(px,1), ")",sep="") 
mx=mean(n.art[a.aff=="Lower"]);mx;px=sd(n.art[a.aff=="Lower"]);px
Mx[6,3] = paste(round(mx,1)," (",round(px,1), ")",sep="") 

Mx

#write.csv(Mx,"06_Results/Table1_2022-01-26.csv")



###   Stratified (sub)Table 1   ###       

Mxs = matrix(NA, 6,6)


tx=table(a.aff,a.gen);tx; px=prop.table(tx,margin=1);px      # Authors
for(i in 1:3){
  Mxs[2*i-1,1] = paste (tx[i,1]," (", round(100*px[i,1],1),"%)",sep="")
  Mxs[2*i,1] = paste (tx[i,2]," (", round(100*px[i,2],1),"%)",sep="")
}

tx=table(aff,g);tx; px=prop.table(tx,margin=1);px            # Authorships
for(i in 1:3){
  Mxs[2*i-1,2] = paste (tx[i,1]," (", round(100*px[i,1],1),"%)",sep="")
  Mxs[2*i,2] = paste (tx[i,2]," (", round(100*px[i,2],1),"%)",sep="")
}

tx=table(first,aff,g)[2,,];tx; px=prop.table(tx,margin=1);px  # Fist
for(i in 1:3){
  Mxs[2*i-1,4] = paste (tx[i,1]," (", round(100*px[i,1],1),"%)",sep="")
  Mxs[2*i,4] = paste (tx[i,2]," (", round(100*px[i,2],1),"%)",sep="")
}

tx=table(last,aff,g)[2,,];tx; px=prop.table(tx,margin=1);px   # Last
for(i in 1:3){
  Mxs[2*i-1,5] = paste (tx[i,1]," (", round(100*px[i,1],1),"%)",sep="")
  Mxs[2*i,5] = paste (tx[i,2]," (", round(100*px[i,2],1),"%)",sep="")
}


##  FOL 

fol.lic.f = fol.mic.f = fol.hic.f = fol.lic.m = fol.mic.m = fol.hic.m = NULL
for (i in 1:length(uID)){ # i=20
  id = uID[i]
  w = which(art.id==id)
  fol.lic.f[i] = 1*(sum(aff[w][fol[w]==1]=="Lower"&g[w][fol[w]==1]=="female")>0)
  fol.mic.f[i] = 1*(sum(aff[w][fol[w]==1]=="Upper middle"&g[w][fol[w]==1]=="female")>0)
  fol.hic.f[i] = 1*(sum(aff[w][fol[w]==1]=="High"&g[w][fol[w]==1]=="female")>0)
  fol.lic.m[i] = 1*(sum(aff[w][fol[w]==1]=="Lower"&g[w][fol[w]==1]=="male")>0)
  fol.mic.m[i] = 1*(sum(aff[w][fol[w]==1]=="Upper middle"&g[w][fol[w]==1]=="male")>0)
  fol.hic.m[i] = 1*(sum(aff[w][fol[w]==1]=="High"&g[w][fol[w]==1]=="male")>0)
}

FOLx = matrix(c(sum(fol.hic.f),sum(fol.mic.f),sum(fol.lic.f),sum(fol.hic.m),sum(fol.mic.m),sum(fol.lic.m)),3,2);FOLx
#FOL = c(sum(fol.hic),sum(fol.mic),sum(fol.lic)); FOL
for(i in 1:3){
    Mxs[2*i-1,6] = paste (FOLx[i,1]," (", round((100*FOLx[i,1]/FOL[i]),1),"%)",sep="")
    Mxs[2*i,6] = paste (FOLx[i,2]," (", round((100*FOLx[i,2]/FOL[i]),1),"%)",sep="")
  }

Mxs


# N.articles

Mxs[1,3] = paste( round(mean(n.art[a.aff=="High"&a.gen=="female"]),1)," (", round(sd(n.art[a.aff=="High"&a.gen=="female"]),1),")" ,sep="")
Mxs[2,3] = paste( round(mean(n.art[a.aff=="High"&a.gen=="male"]),1)," (", round(sd(n.art[a.aff=="High"&a.gen=="male"]),1),")" ,sep="")

Mxs[3,3] = paste( round(mean(n.art[a.aff=="Upper middle"&a.gen=="female"]),1)," (", round(sd(n.art[a.aff=="Upper middle"&a.gen=="female"]),1),")" ,sep="")
Mxs[4,3] = paste( round(mean(n.art[a.aff=="Upper middle"&a.gen=="male"]),1)," (", round(sd(n.art[a.aff=="Upper middle"&a.gen=="male"]),1),")" ,sep="")

Mxs[5,3] = paste( round(mean(n.art[a.aff=="Lower"&a.gen=="female"]),1)," (", round(sd(n.art[a.aff=="Lower"&a.gen=="female"]),1),")" ,sep="")
Mxs[6,3] = paste( round(mean(n.art[a.aff=="Lower"&a.gen=="male"]),1)," (", round(sd(n.art[a.aff=="Lower"&a.gen=="male"]),1),")" ,sep="")

Mxs

#write.csv(Mxs,"06_Results/Table1_strat_2022-01-27.csv")


###   Merge main and stratified Table 1  ###

Mxx =  matrix(NA, 12,6)

Mxx[c(1:4,7,10),] = Mx
Mxx[c(5,6,8,9,11,12),] = Mxs

Mxx  




write.csv(Mxx,"06_Results/Table1_merged_2022-01-31.csv")


# Sensitivity analyses

write.csv(Mxx,"06_Results/Table1_merged_MR01_2022-01-31.csv")
write.csv(Mxx,"06_Results/Table1_merged_MR0_2022-01-31.csv")

write.csv(Mxx,"06_Results/Table1_merged_MR1_2022-01-31.csv")
write.csv(Mxx,"06_Results/Table1_merged_MR9_2022-01-31.csv")


##################################

##  List of top publishing authors

wtop = which(n.art>20)
o = rev(order(n.art[wtop]))
#wtop = which(a.last>5)
#o = rev(order(a.last[wtop]))

wo = wtop[o]
Top = cbind(surname,given,aut.idu,as.character(a.gen),n.art,a.first,a.last)[wo,]; Top

#write.csv(Top,"06_Results/Top_authors.csv")



  table(d$affil_main)
LIC = unique(d$affil_main[d$affil_cat=="Lower"]) 
MIC = unique(d$affil_main[d$affil_cat=="Upper middle"]) 
HIC = unique(d$affil_main[d$affil_cat=="High"]) 

length(HIC);length(MIC);length(LIC)
sort(HIC)
sort(MIC)
sort(LIC)
