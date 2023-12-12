###   Export simple database for Kostas   ###
setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")
d = read.csv("03_Data/Data_authorships.csv")
names(d)
d2 = d[,c(3,5,4,6,120,8:10,123,124,14,16:17,15)]; dim(d2)
colnames(d2)[c(9,10)] = c("affil_level","N_authors")
head(d2)[,-2]
names(d2)
o2 = order(d2[,11])
d3 = d2[o2,]; d3[1:20,-2]
o3 = order(d3[,1])
d4 = d3[o3,]; d4[1:20,-2]
write.csv(d4,"03_Data/Data_authorships_Konstantinos.csv")
########################################################


###  Data manipulation

##   Reorder by PMID and author position

setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")
d = read.csv("03_Data/Data_authorships.csv")
names(d)

o2 = order(d$author_position)
o3 = order(d$pmid[o2])
d = d[o2,][o3,]

write.csv(d,"03_Data/Data_authorships_2.csv")


##   Manual corrections

setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")
d = read.csv("03_Data/Data_authorships_2.csv")
names(d)

#   Wrong author list

cbind(d$author_code,d$given_name,d$surname)[d$surname=="technau",]

d$author_code[d$surname=="technau"]="14274"
d$given_name[d$surname=="technau"]="karl"

#   Double entry 

wx = which(d$pmid==17990236); nwx = length(wx)
wx = wx[nwx]
d = d[-wx,]
wx = which(d$pmid==17990236); nwx = length(wx)
d$max[d$pmid==17990236] = nwx
d$last_position[wx[nwx]] = 1

d2 = d[,c(3,4,6,120,8:10,123,124,14,16:17,15)-1]
d2[d$pmid==17990236,]

write.csv(d,"03_Data/Data_authorships_3.csv")



###  Merge with new extractions

setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")
d = read.csv("03_Data/Data_authorships_3.csv")
names(d)
n = dim(d)[1]

dx = read.csv("03_Data/new_extractions_221021.csv")
names(dx)

#   Check if names match 

wx = which(d$surname != dx$surname.1)
cbind(d$surname,dx$surname.1) [wx,]		 
pmidx = unique(d$pmid[wx])
cbind(d$surname,dx$surname.1) [d$pmid==pmidx[1],]


#   Match authors' order in the new dataset to the original 

art.id = d$pmid
uID = unique(art.id)
pmidx = NULL

d$comm = d$affil_pdf3 = d$affil_pdf2 = d$affil_pdf1 = rep(NA,n)

for (i in 1:length(uID)){ # i=1
  wx = which(d$pmid==uID[i])
  wx2 = which(dx$pmid==uID[i])
  #if (length(wx)!=length(wx2)) pmidx = c(pmidx,uID[i])
  for (j in 1:length(wx2)){
    wxx = which(d$surname[wx]==dx$surname.1[wx2[j]])
    d$affil_pdf1[wx][wxx] = dx$pdf_affil_country[wx2[j]] # dx$pdf_affil_country[wx2]
    d$affil_pdf2[wx][wxx] = dx$pdf_second_affil_country[wx2[j]] #dx$pdf_second_affil_country[wx2]
    d$affil_pdf3[wx][wxx] = dx$pdf_third_affil_country[wx2[j]]
    d$comm[wx][wxx] = dx$comments[wx2[j]]
  }
}

table(d$affil_pdf2, useNA = "ifany")
table(dx$pdf_second_affil_country, useNA = "ifany")
sum(d$affil_second_country!=d$affil_pdf2)

write.csv(d,"03_Data/Data_authorships_4.csv")




###   Fix affiliations

##    List of authors with 2+ affiliations - summary of all affiliations 

d = read.csv("03_Data/Data_authorships_4.csv")
names(d)

nx = names(table(d$comm)); nx		# comments
w2aff = which(d$comm %in% nx[4:7]); length(w2aff)
cbind(d$affil_pdf1,d$affil_pdf2,d$affil_pdf3)[w2aff,]
a2aff = unique(d$author_code[w2aff]); length(a2aff)

aff.all = matrix("",length(a2aff),7)
aff.all[,1] = a2aff

for (i in 1:length(a2aff)){ # i=1
  wx = which(d$author_code==a2aff[i])
  aff.all[i,2] = d$surname[wx][1]
  aff.all[i,3] = d$given_name[wx][1]
  affx = c(d$affil_pdf1[wx],d$affil_pdf2[wx],d$affil_pdf3[wx])
  nx = length(wx)
  t = table(affx); o = rev(order(t))
  t = t[o]
  wx = which(names(t)=="")
  if (length(wx)>0) t = t[-wx]
  t2 = rep("",4)
  t3 = t
  for (j in 1:length(t)) t3[j] = paste(names(t)[j]," (",t[j],")",sep="")
  t2[1:length(t)] = t3
  aff.all[i,4:7] = t2
}

o = order(aff.all[,2])
aff.all = aff.all[o,]
aff.all

write.csv(aff.all,"03_Data/Multiple_affiliations1.csv")


##    All records for authors with multiple affiliations - ordered by authors and years

d = read.csv("03_Data/Data_authorships_4.csv")
names(d)

nx = names(table(d$comm)); nx		# comments
w2aff = which(d$comm %in% nx[4:7]); length(w2aff)

d2 = subset(d,select=c(surname,given_name,author_code,gender,publicationyear,pmid,author_position,affil_pdf1,affil_pdf2,affil_pdf3,title))
names(d2)
d2 = d2[w2aff,]; dim(d2)
o2 = order(d2$publicationyear)
d2 = d2[o2,]
o3 = order(d2$author_code)
d3 = d2[o3,]
o4 = order(d3$surname)
d3 = d3[o4,]

write.csv(d3,"03_Data/Authors_mult_affiliations2.csv")


##    Create affilM to use in the analysis
#     + Fix income level group

d = read.csv("03_Data/Data_authorships_4.csv")
names(d)
d2 = read.csv("03_Data/Authors_mult_affiliations3.csv")
names(d2)

affilM = d$affil_pdf1
affil_MAIN = d2$affil_MAIN 

# Replace affilM with d2$affil_MAIN when multiple affil

for (i in 1:length(affil_MAIN)){
  wx = which(d$pmid==d2$pmid[i]&d$author_code==d2$author_code[i])
  if(length(wx)>0) affilM[wx] = affil_MAIN[i]
}

d$affil_main = affilM
table(affilM)
sum(d$affil_country!=d$affil_main)  # 218
names(d)
table(d$aff)


write.csv(d,"03_Data/Data_authorships_5.csv")


######################


###  Fix author codes

#    List of all authors by surname, given name and code

setwd("C:/Users/skrivankova/OneDrive - Universitaet Bern/Work/ISPM/IeDEA/Authorships")
d = read.csv("03_Data/Data_authorships_5.csv")
names(d)

d2 = subset(d, select=c(surname,given_name,author_code,gender,publicationyear,pmid,affil_pdf1,affil_pdf2,affil_pdf3,title))
names(d2)

o1 = order(d2$author_code)
d3 = d2[o1,]
o2 = order(d3$surname)
d4 = d3[o2,]

write.csv(d4,"03_Data/Author_codes.csv")


#   Review highlighted mistakes

d1 = read.csv("03_Data/Author_codes2.csv")
names(d1)
table(d1$author_code_wrong)
wx = which(d1$author_code_wrong %in% c(0,1,"?")); length(wx)
d2 = d1[wx,]
d2[,1:10]

write.csv(d2,"03_Data/Author_codes2_wrong_subset.csv")


#   Correct the wrong author codes  #   <<<<<<<<<   !!!!!!!!!


d = dx = read.csv("03_Data/Data_authorships_5.csv")
#d = dx = read.csv("03_Data/Author_codes.csv");dim(d)  names(d)
d3 = read.csv("03_Data/Author_codes3_wrong_subset.csv")    
names(d3)
d4 = d3[d3$author_code_wrong==1,]
d4[,1:10]

acw = d4$author_code
acn = d4$author_code_new

for (i in 1:length(acw)){
  wx = which(d$author_code==acw[i])
  if (length(wx)>0) d$author_code[wx] = acn[i]
}

d$author_code[which(d$author_code==230333 & d$pmid==28364561)]=2303331

wx = which(d$author_code!=dx$author_code) ;length(wx)
dxxx = subset(dx, select=c(surname,given_name,author_code,gender,publicationyear,pmid,affil_pdf1,affil_pdf2,affil_pdf3,title))
dxxx[wx, 1:8]  
dxx = subset(d, select=c(surname,given_name,author_code,gender,publicationyear,pmid,affil_pdf1,affil_pdf2,affil_pdf3,title))
dxx[wx, 1:8]  



###   Fix author names

d$given_name[which(d$author_code==522)]= "andrew"
d$given_name[which(d$author_code==5236)]= "mary-ann" 
d$given_name[which(d$author_code==157227)]= "brian"
d$given_name[which(d$author_code==30164)]= "alexa"
d$given_name[which(d$author_code==175153)]= "kouadio"
d$given_name[which(d$author_code==3830)]= "sam"


d$surname[which(d$author_code==784)]= "carter"
d$surname[which(d$author_code==175153)]= "kouakou"
d$surname[which(d$author_code==265263)]= "luthy"
d$surname[which(d$author_code==299403&d$publicationyear==2016)]= "bolton-moore"


###  Add missing authorship! 

unique(d$author_code[d$surname=="zwahlen"])  #  324359
names(d)

wx = which(d$pmid==27578823);length(wx)
nn = dim(d)[1];nn # 3420
dx = rbind(d,d[nn,])

dx[(wx[1]+3):(nn+1),] = d[(wx[1]+2):nn,]

dx$surname[(wx[1]+2)] = "zwahlen"
dx$given_name[(wx[1]+2)] = "marcel"
dx$gender[(wx[1]+2)] = "male"
dx$affil_main[(wx[1]+2)] = "Switzerland"
dx$aff_new[(wx[1]+2)] = "High"
dx$author_code[(wx[1]+2)] = 324359
dx$author_position[(wx[1]+2)] = 3
dx$first_position[(wx[1]+2)] = 0
dx$last_position[(wx[1]+2)] = 0
dx$affil_name[(wx[1]+2)] = dx$affil_city[(wx[1]+2)] =dx$affil_country[(wx[1]+2)] = NA
dx$Incomegroup[(wx[1]+2)] = dx$class_num[(wx[1]+2)]  = dx$aff[(wx[1]+2)] = dx$affil_pdf1[(wx[1]+2)]= dx$affil_pdf2[(wx[1]+2)]= dx$affil_pdf3[(wx[1]+2)]=NA

dx$author_position[wx]=1:length(wx)

d = dx

write.csv(d,"03_Data/Data_authorships_6.csv")



####   Fix the income level   #####

##  Save list of HIC/MIC/LIC countries

d = read.csv("03_Data/Data_authorships_4.csv")

table(d$aff)
LIC = unique(d$affil_country[d$aff=="Low"]) 
MIC = unique(d$affil_country[d$aff=="Middle"]) 
HIC = unique(d$affil_country[d$aff=="High"])  


##    Fix the income level based on new affiliation

d = read.csv("03_Data/Data_authorships_6.csv")

nn = dim(d)[1];nn # 3421
aff_new = rep(NA,nn)

for (i in 1:nn){
  if(d$affil_main[i] %in% LIC) aff_new[i] = "Low"
  if(d$affil_main[i] %in% MIC) aff_new[i] = "Middle"
  if(d$affil_main[i] %in% HIC) aff_new[i] = "High"
}

table(aff_new)
sum(is.na(aff_new))

wx = which(is.na(aff_new)); d$affil_main[wx]
aff_new[wx] = "Low"
sum(d$aff != aff_new,na.rm=T)

d$affil_cat = aff_new


##   Drop un-needed columns

names(d)
d = d[,-c(1:7,38:118)]

write.csv(d,"03_Data/Data_authorships_7.csv")

############################################




####   Add MR & rename affiliation  ####

d = read.csv("03_Data/Data_authorships_7.csv")
dx = read.csv("03_Data/list of articles/313 PMID DOI_MR v2_new.csv")

nn = dim(d)[1];nn
  names(d)
  table(d$affil_cat)
# w = which(d$affil_country!=d$affil_main);length(w)

  
##   Add MR
  
d$MR = rep(NA,nn)
for (i in 1:M){
  w = which(art.id==dx$PMID[i])
  d$MR[w] = dx$MR[i]
}  
#d$MR

names(d)
dxx = d[,c(2,4,14,56, 3,5,38,6, 54,55,13,15,16,42, 44,46,23)]
names(dxx)


##   Rename affiliation

dxx$affil_cat = factor(dxx$affil_cat)
levels(dxx$affil_cat) 
levels(dxx$affil_cat) = c("High","Lower","Upper middle")
table(dxx$affil_cat)

write.csv(dxx,"03_Data/Data_authorships_10.csv")


###  Order PMIDs

d = read.csv("03_Data/Data_authorships_10.csv")
  names(d)
  dim(d)

o2 = order(d$author_position)
o3 = order(d$pmid[o2])

d = d[o2,][o3,]
d[1:20,-3]
write.csv(d,"03_Data/Data_authorships_11.csv")



##############################################
##############################################


# Later: do the same for additional papers
# + find gender

# Merge all
