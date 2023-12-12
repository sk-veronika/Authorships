
#  home
setwd("C:/Users/Veronika/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")

#  office
setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")


d = read.csv("03_Data/Data_authorships_11.csv")
  dim(d)	# 
  names(d) 


  
###   Sensitivity analysis - from here  ###
  

###   Variables   ###

cod = d$author_code
first = d$first_position
last = d$last_position
  sum(first)
  sum(last)
  sum(first*last)

pos = d$author_position
other =(1-first)*(1-last)

art.id = d$pmid
aut.id = d$author_code

n = d$max; summary(n) 		# Nr. of authors per article
N = length(unique(aut.id));N  # N = 1064  nr.of authors
M = length(unique(art.id));M  # M = 313   nr.of articles


g = as.factor(d$gender)
g = relevel(g,ref="male")
  table(g,useNA="ifany") 
 levels(g) = c("Male","Female")


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
summary(pos.std)
sum(pos.std==0)
sum(pos.std==100)

lastx = last
last = 1*(pos.std==100); sum(last)		# Assigning single-authorships first position 


pos.multi = factor(levels=c("mid","first","mid.low","mid.high","last"))
for (i in 1:length(pos)){
 if (pos.std[i]<100)  pos.multi[i]="mid.high"
 if (pos.std[i]<66.5) pos.multi[i]="mid"
 if (pos.std[i]<33.5) pos.multi[i]="mid.low"
 if (pos.std[i]==0)   pos.multi[i]="first"
 if (pos.std[i]==100) pos.multi[i]="last"
}
summary(pos.multi)[c(2,3,1,4,5)]




########################
#install.packages("lme4")
#install.packages("statmod")
#install.packages("nnet", repos = "http://cran.r-project.org")
#install.packages("mclogit")



###  Multinomial (logistic) models

#library(nnet)
#?multinom

library(lmtest)
library("mclogit")
#?mblogit



# Main model

multi.mod   = mblogit(pos.multi ~ g + aff , random=~ 1|aut.id)

# Univar
multi.mod.g = mblogit(pos.multi ~ g,   random=~ 1|aut.id) #, data = data[w,])
multi.mod.a = mblogit(pos.multi ~ aff, random=~ 1|aut.id) #, data = data[w,])


# Inter G x Aff
multi.mod.ga = mblogit(pos.multi ~ g + aff + g*aff , random=~ 1|aut.id)


# Interaction w Year
multi.mod.yg = mblogit(pos.multi ~ yrx*g + aff , random=~ 1|aut.id)
multi.mod.ya = mblogit(pos.multi ~ g + yrx*aff , random=~ 1|aut.id)


# Impact factors
#multi.mod.IFg = mblogit(pos.multi ~ IFs*g.mod + aff.mod , random=~ 1|aut.id)
#multi.mod.IFa = mblogit(pos.multi ~ g.mod + IFs*aff.mod , random=~ 1|aut.id)
#multi.mod.RCRg = mblogit(pos.multi ~ RCRs*g.mod + aff.mod , random=~ 1|aut.id)
#multi.mod.RCRa = mblogit(pos.multi ~ g.mod + RCRs*aff.mod , random=~ 1|aut.id)



###  Formating results 

##  Multivariable model
mod = multi.mod

## Univariable models
mod = multi.mod.g
mod = multi.mod.a


## Model with interaction
mod = multi.mod.ga

## Models with interaction with Year
mod = multi.mod.yg
mod = multi.mod.ya
#mod = multi.mod.uyg



summary(mod)
#names(summary(mod))


ccx = summary(mod)$coef 
cc  = round(cbind(exp(cbind(ccx[,1], ccx[,1]-1.96*ccx[,2], ccx[,1]+1.96*ccx[,2])),ccx[,4]),2)

Tab = NULL
 cc = cc[-(1:4),]
 ci = cc[,2:3]; 
 p = cc[,4]

 m = dim(cc)[1]; M = matrix(NA,m,3); MM = NA
 for (i in 1:m){  
  if (cc[i,1] == round(cc[i,1],1)) {M[i,1] = paste(cc[i,1],"0 ",sep="")} else {M[i,1] = paste(cc[i,1]," ",sep="")}
  if (ci[i,1] == round(ci[i,1],1)) {M[i,2] = paste("(",ci[i,1],"0 - ",sep="")} else {M[i,2] = paste("(",ci[i,1]," - ",sep="")}
  if (ci[i,2] == round(ci[i,2],1)) {M[i,3] = paste(ci[i,2],"0)",sep="")} else {M[i,3] = paste(ci[i,2],")",sep="")}
  MM[i] = paste(M[i,1],M[i,2],M[i,3],sep="")
 }  
 names(MM) = rownames(cc)
 Tab = cbind(MM,round(p,3)); 
 Tab[,2][Tab[,2]=="0"]="<0.001" # Tab
 Tab

 

 
###   Main analysis  ### 
 
 
write.csv(Tab,"06_Results/GMRM_main.csv")

write.csv(Tab,"06_Results/GMRM_uni_g.csv")
write.csv(Tab,"06_Results/GMRM_uni_aff.csv")

write.csv(Tab,"06_Results/GMRM_main_int.csv")

write.csv(Tab,"06_Results/GMRM_yearxg.csv")
write.csv(Tab,"06_Results/GMRM_yearxaff.csv")


#lrtest(multi.mod,multi.mod.g)
#lrtest(multi.mod,multi.mod.a)
#lrtest(multi.mod.g)
#lrtest(multi.mod.a)



##  Sensitivity analysis (subgroups of articles)  ##


write.csv(Tab,"06_Results/GMRM_main_MR01.csv") 
write.csv(Tab,"06_Results/GMRM_uni_g_MR01.csv")
write.csv(Tab,"06_Results/GMRM_uni_aff_MR01.csv")

write.csv(Tab,"06_Results/GMRM_main_MR0.csv") 
write.csv(Tab,"06_Results/GMRM_uni_g_MR0.csv")
write.csv(Tab,"06_Results/GMRM_uni_aff_MR0.csv")


write.csv(Tab,"06_Results/GMRM_main_MR1.csv") 
write.csv(Tab,"06_Results/GMRM_uni_g_MR1.csv")
write.csv(Tab,"06_Results/GMRM_uni_aff_MR1.csv")

write.csv(Tab,"06_Results/GMRM_main_MR9.csv") 
write.csv(Tab,"06_Results/GMRM_uni_g_MR9.csv")
write.csv(Tab,"06_Results/GMRM_uni_aff_MR9.csv")


##  Sensitivity analyses: minus PIs

#write.csv(Tab,"06_Results/GMRM_main_minusPIs.csv")
#write.csv(Tab,"06_Results/GMRM_main_minusPIs2.csv")
#write.csv(Tab,"06_Results/GMRM_main_minusPIs3.csv")








#####################################################



###  Multinomial GEE models --  ???  memory allocation issues

#install.packages("multgee")
#library("multgee")
#?nomLORgee
#multi.mod.gee   = nomLORgee(pos.multi ~ g + aff , id=aut.id)





##################################
####  Impact metrics   --  DELETE
IF = as.numeric(d$jif)
RCR = as.numeric(d$RCR)
summary(IF); sd(IF)
summary(RCR); sd(RCR)
IFs = IF/sd(IF)
RCRs = RCR/sd(RCR)
summary(IFs)
summary(RCRs)

####  Re-level gender & affiliation  -- DELETE 
g.mod = factor(levels=c("Male","Female"))
g.mod[which(g=="male")] = "Male"
g.mod[g=="female"] = "Female"
g.mod[g=="unknown"] = NA
summary(g.mod); summary(g)

aff.mod = factor(levels=c("High","Middle","Low"))
aff.mod[aff=="High"] = "High"
aff.mod[aff=="Middle"] = "Middle"
aff.mod[aff=="Low"] = "Low"
aff.mod[aff==""] = NA
summary(aff.mod); summary(aff)

a.gen = as.factor(a.gen)
a.aff = as.factor(a.aff)
summary(a.gen)
summary(a.aff)

a.aff.mod = factor(levels=c("High","Middle","Low"))
a.aff.mod[a.aff=="High"] = "High"
a.aff.mod[a.aff=="Middle"] = "Middle"
a.aff.mod[a.aff=="Low"] = "Low"
a.aff.mod[a.aff==""] = NA
summary(a.aff.mod)

a.gen.mod = factor(levels=c("Male","Female"))
a.gen.mod[which(a.gen=="male")] = "Male"
a.gen.mod[a.gen=="female"] = "Female"
a.gen.mod[a.gen=="unknown"] = NA
summary(a.gen.mod)
#########################################






###   Poisson models  ###
?glm 

Poi.mod.first = glm(a.first ~ a.gen.mod + a.aff.mod, family=poisson ) 
Poi.mod.last  = glm(a.last  ~ a.gen.mod + a.aff.mod, family=poisson ) 

mod = Poi.mod.first
mod = Poi.mod.last


summary(mod)  			# names(summary(mod))
cc = summary(mod)$coef; 
round(exp(cbind(cc[,1], cc[,1]-1.96*cc[,2], cc[,1]+1.96*cc[,2])),5)





###   Logistic reg models   ###
##    First vs all other    ##


library("lme4")


##   Gender 

#  GLM model wo random intercepts for authors (wo adj. for correlated observations)
GLM.mod1g  = glm(first ~ g.mod , family=binomial); summary(GLM.mod1g)

#  LMEM (rand.int. | authors)
LME.mod1g  = glmer(first ~ g.mod + (1|aut.id), family=binomial ) 
summary(LME.mod1g)

#  + rand.int. | article : Singularity issue (std.dev of article rand.int = 0) 
#    results (nearly) identical to LMEM wo article rand.int
LME.mod1g2  = glmer(first ~ g.mod + (1|aut.id) + (1|art.id), family=binomial ) 
summary(LME.mod1g2)

#  + interactions

LME.mod1g_yr  = glmer(first ~ g.mod*yrx + (1|aut.id), family=binomial )  
LME.mod1g_if  = glmer(first ~ g.mod*IFc + (1|aut.id), family=binomial ) 
LME.mod1g_oa  = glmer(first ~ g.mod*oa + (1|aut.id), family=binomial )  



##   Affiliation 

#  LMEM (rand.int. | authors)
LME.mod1a  = glmer(first ~ aff.mod + (1|aut.id), family=binomial )  

#  + interactions
LME.mod1a_yr  = glmer(first ~ aff.mod*yrx + (1|aut.id), family=binomial )  
LME.mod1a_if  = glmer(first ~ aff.mod*IFc + (1|aut.id), family=binomial )


## Year

LME.mod1y  = glmer(first ~ yrx + (1|aut.id), family=binomial ) 


##   Gender + Affiliation

#  LMEM (rand.int. | authors)
LME.mod2  = glmer(first ~ g.mod + aff.mod + (1|aut.id), family=binomial ) # +(1|art.id)
LME.mod2s  = glm(first ~ g.mod + aff.mod , family=binomial )

LME.mod2yr  = glmer(first ~ g.mod + aff.mod + yrx + (1|aut.id), family=binomial )

#  + interaction with year
LME.mod2g_yr  = glmer(first ~ g.mod*yrx + aff.mod + (1|aut.id), family=binomial )  
LME.mod2a_yr  = glmer(first ~ g.mod + aff.mod*yrx + (1|aut.id), family=binomial )  

#  + interaction with IF 
#IFc = (IF-10)/10
#LME.mod2g_ifc = glmer(first ~ g.mod*IFc + aff.mod + (1|aut.id), family=binomial )
 LME.mod2g_if  = glmer(first ~ g.mod*IF  + aff.mod + (1|aut.id), family=binomial ) 
#LME.mod2a_ifc = glmer(first ~ g.mod + aff.mod*IFc + (1|aut.id), family=binomial )
 LME.mod2a_if  = glmer(first ~ g.mod + aff.mod*IF + (1|aut.id), family=binomial )



mod = LME.mod1g
mod = LME.mod1a
mod = LME.mod1y
mod = LME.mod2
mod = LME.mod2s
mod = LME.mod2yr

# Interactions

mod = LME.mod1g_yr
mod = LME.mod1g_if  
mod = LME.mod1g_oa


mod = LME.mod1a_yr
mod = LME.mod1a_if
mod = LME.mod1a_oa


# Multi
mod = LME.mod2g_yr
#mod = LME.mod2g_ifc # !! 
mod = LME.mod2g_if

mod = LME.mod2a_yr
#mod = LME.mod2a_ifc
mod = LME.mod2a_if


summary(mod)  			#names(summary(mod))#summary(mod)$
cc = summary(mod)$coef; 
round(exp(cbind(cc[,1], cc[,1]-1.96*cc[,2], cc[,1]+1.96*cc[,2])),5)




# Centered & scaled IF : IFc = (IF-10)/10  [Lin com of G and IF]

# coef(CI) for G at IF = 10

v = summary(mod)$vcov
ccg.e = cc[2,1]+10*cc[6,1]
ccg.se = sqrt( v[2,2]+10*v[2,6]+100*v[6,6] )
 round(exp(c(ccg.e,ccg.e-1.96*ccg.se,ccg.e+1.96*ccg.se)),3)
 2*(1-pnorm(ccg.e/ccg.se))	# p-val

# coef(CI) for IF and IF*G
round(exp(c( 10*cc[3,1], 10*cc[3,1]-1.96*10*cc[3,2], 10*cc[3,1]+ 1.96*10*cc[3,2] )),3)
round(exp(c( 10*cc[6,1], 10*cc[6,1]-1.96*10*cc[6,2], 10*cc[6,1]+ 1.96*10*cc[6,2] )),3)


# coef(CI) for A at IF = 10

v = summary(mod)$vcov
cca.e = cc[3,1]+10*cc[6,1]
cca.se = sqrt( v[3,3]+10*v[3,6]+100*v[6,6] )
 round(exp(c(cca.e,cca.e-1.96*cca.se,cca.e+1.96*cca.se)),3)
 2*(pnorm(cca.e/cca.se))	# p-val
cca.e = cc[4,1]+10*cc[7,1]
cca.se = sqrt( v[4,4]+10*v[4,7]+100*v[7,7] )
 round(exp(c(cca.e,cca.e-1.96*cca.se,cca.e+1.96*cca.se)),3)
 2*(pnorm(cca.e/cca.se))	# p-val

# coef(CI) for IF and IF*A
round(exp(c( 10*cc[5,1], 10*cc[5,1]-1.96*10*cc[5,2], 10*cc[5,1]+ 1.96*10*cc[5,2] )),3)
round(exp(c( 10*cc[6,1], 10*cc[6,1]-1.96*10*cc[6,2], 10*cc[6,1]+ 1.96*10*cc[6,2] )),3)
round(exp(c( 10*cc[7,1], 10*cc[7,1]-1.96*10*cc[7,2], 10*cc[7,1]+ 1.96*10*cc[7,2] )),3)





###   Last authorship vs all other   ###

#  Univariable

LME.mod3g  = glmer(last ~ g.mod + (1|aut.id), family=binomial )
LME.mod3a  = glmer(last ~ aff.mod + (1|aut.id), family=binomial )
LME.mod3y  = glmer(last ~ yrx + (1|aut.id), family=binomial )

#  Multivariable

LME.mod4  = glmer(last ~ g.mod + aff.mod + (1|aut.id), family=binomial )
LME.mod4y  = glmer(last ~ g.mod + aff.mod + yrx + (1|aut.id), family=binomial )

# Inter. w Year
LME.mod3g_y  = glmer(last ~ g.mod*yrx + (1|aut.id), family=binomial )
LME.mod3a_y  = glmer(last ~ aff.mod*yrx + (1|aut.id), family=binomial )
LME.mod4g_y  = glmer(last ~ g.mod*yrx + aff.mod + (1|aut.id), family=binomial )
LME.mod4a_y  = glmer(last ~ g.mod + aff.mod*yrx + (1|aut.id), family=binomial )

# Inter. w IF
LME.mod3g_if  = glmer(last ~ g.mod*IFc + (1|aut.id), family=binomial )
LME.mod3a_if  = glmer(last ~ aff.mod*IFc + (1|aut.id), family=binomial )
LME.mod4g_if  = glmer(last ~ g.mod*IFc + aff.mod + (1|aut.id), family=binomial )
LME.mod4a_if  = glmer(last ~ g.mod + aff.mod*IFc + (1|aut.id), family=binomial )



mod = LME.mod3g
mod = LME.mod3a
mod = LME.mod3y

mod = LME.mod4
mod = LME.mod4y

mod = LME.mod3g_y
mod = LME.mod4g_y
mod = LME.mod3a_y
mod = LME.mod4a_y

mod = LME.mod3g_if
mod = LME.mod4g_if
mod = LME.mod3a_if
mod = LME.mod4a_if


cc = summary(mod)$coef 	# summary(mod)
round( cbind( exp(cbind(cc[,1], cc[,1]-1.96*cc[,2], cc[,1]+1.96*cc[,2])),cc[,4]), 3)












###   First + Last authorship vs all other (Affiliation)   ###

FL = 1-((first==0)*(last==0))

#LME.mod1g  = glmer(FL ~ g.mod + (1|aut.id), family=binomial ) 
LME.mod1a  = glmer(FL ~ aff.mod + (1|aut.id), family=binomial )  
#LME.mod2  = glmer(FL ~ g.mod + aff.mod + (1|aut.id), family=binomial ) 

mod = LME.mod1g
mod = LME.mod1a
mod = LME.mod2

summary(mod)
cc = summary(mod)$coef
round(exp(cbind(cc[,1], cc[,1]-1.96*cc[,2], cc[,1]+1.96*cc[,2])),3)






# lin comb of interaction coef  !!!!! ???

ccx = rbind(cc, cc[3,]+cc[4,]+c(0,2*(-0.743)*cc[3,1]*cc[4,1],0,0))		
round(exp(cbind(ccx[,1], ccx[,1]-1.96*ccx[,2], ccx[,1]+1.96*ccx[,2])),5)




###   First authorship vs all other except Last   ###

firstL = first[last==0]
g.modL = g.mod[last==0]
aff.modL = aff.mod[last==0]
aut.idL = aut.id[last==0]

LME.mod1g  = glmer(firstL ~ g.modL + (1|aut.idL), family=binomial ) 
LME.mod1a  = glmer(firstL ~ aff.modL + (1|aut.idL), family=binomial )  
LME.mod2  = glmer(firstL ~ g.modL + aff.modL + (1|aut.idL), family=binomial )  

###   Last authorship vs all other excpet First   ###

lastF = last[first==0]
g.modF = g.mod[first==0]
aff.modF = aff.mod[first==0]
aut.idF = aut.id[first==0]

LME.mod3g  = glmer(lastF ~ g.modF + (1|aut.idF), family=binomial )
LME.mod3a  = glmer(lastF ~ aff.modF + (1|aut.idF), family=binomial )
LME.mod3  = glmer(lastF ~ g.modF + aff.modF + (1|aut.idF), family=binomial )



mod = LME.mod1g
mod = LME.mod1a
mod = LME.mod2

mod = LME.mod3g
mod = LME.mod3a
mod = LME.mod3

summary(mod)
cc = summary(mod)$coef
round(exp(cbind(cc[,1], cc[,1]-1.96*cc[,2], cc[,1]+1.96*cc[,2])),3)







