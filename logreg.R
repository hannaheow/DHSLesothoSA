
library("haven") #faster sas import function 

setwd("C:/Users/Hannah/Documents/spring2020/dhsproject/DHSLesothoSA-master")
saam = read_sas("SA adult men saam.sas7bdat")
saaw = read_sas("SA adult women saaw.sas7bdat")
lam = read_sas("lesotho adult men lam.sas7bdat") 
law = read_sas("lesotho adult women law.sas7bdat")
lh = read_sas("lesotho household lh.sas7bdat")
sah = read_sas("SA household sah.sas7bdat") #commented out bc file too large for github. export and reimport. 

#select for native sotho speakers only 
saam = saam[which(saam$MV045C == 5),] 
saaw = saaw[which(saaw$V045C == 5),]
sah = sah[which(sah$HV045C == 5),]


#select for urban only 
sah = sah[which(sah$HV025 == 1),] #commented out bc original sah file is too large. exported and reimported. 
lh = lh[which(lh$HV025 == 1),] #why does N on the 'rural' page of the excel match the N for HV025 == 1??? 

#code to export and reimport 
#library("rio")
#export(sah, "SAUrbanSothoHouseholds.sas7bdat")
#sah = read_sas("SAUrbanSothoHouseholds.sas7bdat")


#create id var 
sah$clusterhouse = paste0("s", sah$HV001, sah$HV002) #commented out bc exported/reimported file already contains this col
sah = sah[!duplicated(sah$clusterhouse),] 
lh$clusterhouse = paste0("l", lh$HV001, lh$HV002)
lh = lh[!duplicated(lh$clusterhouse),] #there are no duplicates at the household level - nice; 


#create memsleep 
lh$HV216 = ifelse (lh$HV216 == 0, 1, lh$HV216)
sah$HV216 = ifelse (sah$HV216 == 0, 1, sah$HV216)
lh$memsleep = lh$HV012/lh$HV216
sah$memsleep = sah$HV012/sah$HV216


##########################################################
#pca analysis 
##########################################################

#these vars are in both SA and lesotho datasets. need to turn categorical vars into dummy vars to make matrix to feed prcomp function
#How is memsleep coded???? appear binary in xlsx spreadsheets but that doesn't make sense.... 
#documentation says memsleep should be treated as continuous in pca 
pcavars = c('id', 'drinkingwater', 'toilet', 'cookfuel', 'floor', 'roof', 'wall', 'memsleep', 'electricity', 'radio', 'tv', 'landline', 'fridge', 'computer', 'watch', 'bike', 'mobilephone', 'motorcycle', 'animalcart', 'car') 

#clean up and rename vars
lhnames = c('clusterhouse', "HV201", "HV205", "HV226", "HV213", "HV215", "HV214", "memsleep", "HV206", "HV207", "HV208", "HV221", "HV209", "SH110J", "HV243B", "HV210", "HV243A", "HV211", "HV243C", "HV212")
lhwealth = lh[lhnames]
names(lhwealth) = pcavars

#only computer is different 
sahnames = c('clusterhouse', 'HV201', "HV205", "HV226", "HV213", "HV215", "HV214", "memsleep", "HV206", "HV207", "HV208", "HV221", "HV209", "HV243E", "HV243B", "HV210", "HV243A", "HV211", "HV243C", "HV212")
sahwealth = sah[sahnames]
names(sahwealth) = pcavars



library(Stack)
library(dummies)  
library(factoextra)

lshwealth = Stack(lhwealth, sahwealth)

dlh = cbind(lhwealth, dummy(lhwealth$drinkingwater, sep = "drinkingwater"), dummy(lhwealth$toilet, sep = "toilet"),dummy(lhwealth$cookfuel, sep = "cookfuel"),
            dummy(lhwealth$floor, sep = "floor"), dummy(lhwealth$roof, sep = "roof"), dummy(lhwealth$wall, sep = "wall"))
dsah = cbind(sahwealth, dummy(sahwealth$drinkingwater, sep = "drinkingwater"), dummy(sahwealth$toilet, sep = "toilet"),dummy(sahwealth$cookfuel, sep = "cookfuel"),
             dummy(sahwealth$floor, sep = "floor"), dummy(sahwealth$roof, sep = "roof"), dummy(sahwealth$wall, sep = "wall"))
dlsh = cbind(lshwealth, dummy(lshwealth$drinkingwater, sep = "drinkingwater"), dummy(lshwealth$toilet, sep = "toilet"),dummy(lshwealth$cookfuel, sep = "cookfuel"),
             dummy(lshwealth$floor, sep = "floor"), dummy(lshwealth$roof, sep = "roof"), dummy(lshwealth$wall, sep = "wall"))

lspca = prcomp(dlsh[,!names(dlsh) %in% c('id', 'drinkingwater', 'toilet', 'cookfuel', 'floor', 'roof', 'wall', 'memsleep')], rank = 1)
lpca = prcomp(dlh[,!names(dlh) %in% c('id', 'drinkingwater', 'toilet', 'cookfuel', 'floor', 'roof', 'wall', 'memsleep')], rank = 1)
spca = prcomp(dsah[,!names(dsah) %in% c('id', 'drinkingwater', 'toilet', 'cookfuel', 'floor', 'roof', 'wall', 'memsleep')], rank = 1)


locspecwealth = rbind(lpca$x, spca$x)

household = cbind(lspca$x, locspecwealth, lshwealth)
colnames(household)[1] = "wealth"
colnames(household)[2] = "locspecwealth"





########################################
#### CLEAN UP INDIVIDUAL DATASETS #####
#######################################


#age
saam$age = saam$MV012
saaw$age = saaw$V012
law$age = law$V012
lam$age = lam$MV012

#country 
saam$country = "SA"
saaw$country = "SA"
law$country = "Lesotho"
lam$country = "Lesotho"

#gender 
saam$gender = "male"
saaw$gender = "female"
law$gender = "female"  
lam$gender = "male"

#diabetes 
saaw$diabdiag = saaw$S1413F 
saaw$diabtreat = saaw$S1426
saam$diabdiag = saam$SM1108F
saam$diabtreat = saam$SM1121
law$diabdiag = law$S1012B
law$diabtreat = law$S1012C
lam$diabdiag = lam$SM812B 
lam$diabtreat = lam$SM812C 

#education
saam$eduyears = saam$MV133
saaw$eduyears = saaw$V133
law$eduyears = law$V133
lam$eduyears = lam$MV133

#make a single id var and remove duplicates/same household 
#confirmed that these are the correct id vars: https://dhsprogram.com/data/Merging-Datasets.cfm
saaw$id = paste0("s", saaw$V001, saaw$V002)
saam$id = paste0("s", saam$MV001, saam$MV002)
law$id = paste0("l", law$V001, law$V002)
lam$id = paste0("l", lam$MV001, lam$MV002)

saaw = saaw[!duplicated(saaw$id),] 
saam = saam[!duplicated(saam$id),] 
law = law[!duplicated(law$id),] 
lam = lam[!duplicated(lam$id),] 


s = Stack(saaw, saam)
l = Stack(law, lam)

keep = c("id", "gender", "country", "age", "eduyears", "diabdiag", "diabtreat")

individual = Stack(s, l) 
individual = individual[keep]


f = merge(household, individual, by = "id") 
#this is not quite right. should have a one-to-many relationship but something strange is happening. 
#needs to be good enough for now. will troubleshoot later. 




length(which(f$diabdiag == 1))
#only 62 total cases of diabetes in the entire dataset.... this might not be great. 
#what if we use edu, gender, country, age to predict wealth instead?  
#62/15 ~ 4df to spend on model -> additive only 


#age interacts with gender 
#cubic spline for age 

#initial model: cs for age, interaction between age and gender, cs for wealth, cs (or not) for edu, include all two-way interactions 
#wealth and country interaction (important), but include all for starters 

f$diabdiag[is.na(f$diabdiag)] = 0
logit = glm(diabdiag ~ age + eduyears + gender + country + wealth, data = f, family = "binomial")
#1df for gender
#1df for country
#1df for eduyears
#1df for age 
#1df for wealth 
#5df total 
#adding cs for age, eduyears, wealth would add df (dfxdfxdf)

summary(logit)
#age and wealth are significant - need cubic splines 
#gender, edu, country do not appear to be significant 


lin = lm(wealth~country+age+gender+eduyears, data = f)
summary(lin)
#all are significant except gender which makes sense bc wealth is by household. remove gender 
lin2 = lm(wealth~country +age+eduyears, data = f)
summary(lin2)
plot(lin2)



library(splines)
lincub = lm(wealth~country + ns(age, df = 4)+ ns(age, df = 4):country + ns(eduyears, df = 4) + ns(eduyears,df=4):country + ns(eduyears,df=4):ns(age,df=4), data = f)
