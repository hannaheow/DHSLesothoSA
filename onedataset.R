# outcome var: diabetes  
# wealth, age, country, gender, edu  
# logistic regression 




library("haven") #faster sas import function 

setwd("C:/Users/Hannah/Documents/2020/dhsproject/DHSLesothoSA-master")
saam = read_sas("SA adult men saam.sas7bdat")
saaw = read_sas("SA adult women saaw.sas7bdat")
lam = read_sas("lesotho adult men lam.sas7bdat") 
law = read_sas("lesotho adult women law.sas7bdat")
lh = read_sas("lesotho household lh.sas7bdat")
#sah = read_sas("SA household sah.sas7bdat") #commented out bc file too large for github. export and reimport. 

#select for native sotho speakers only 
saam = saam[which(saam$MV045C == 5),] 
saaw = saaw[which(saaw$V045C == 5),]
#sah = sah[which(sah$HV045C == 5),]


#select for urban only 
#sah = sah[which(sah$HV025 == 1),] #commented out bc original sah file is too large. exported and reimported. 
lh = lh[which(lh$HV025 == 1),] #why does N on the 'rural' page of the excel match the N for HV025 == 1??? 

#library("rio")
#export(sah, "SAUrbanSothoHouseholds.sas7bdat")
sah = read_sas("SAUrbanSothoHouseholds.sas7bdat")

#age
saam$age = saam$MV012
saaw$age = saaw$V012
law$age = law$V012
lam$age = lam$MV012

#date of birth 
saam$dob = saam$MV011
saaw$dob = saaw$V011
law$dob = law$V011
lam$dob = lam$MV011

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
saam$eduattain = saam$MV149
saam$eduyears = saam$MV133
saam$edulevel = saam$MV106
saam$eduyearsatlevel = saam$MV107
saaw$eduattain = saaw$V149
saaw$eduyears = saaw$V133
saaw$edulevel = saaw$V106
saaw$eduyearsatlevel = saaw$V107
law$eduattain = law$V149
law$eduyears = law$V133
law$edulevel = law$V106
law$eduyearsatlevel = law$V107
lam$eduattain = lam$MV149
lam$eduyears = lam$MV133
lam$edulevel = lam$MV106
lam$eduyearsatlevel = lam$MV107
#lesotho specific edu vars: S106, S105



#wealth
law$wealthindex = law$V190
law$wealthscore = law$V191
lam$wealthindex = lam$MV190
lam$wealthscore = lam$MV191
saam$wealthindex = saam$MV190
saam$wealthscore = saam$MV191
saaw$wealthindex = saaw$V190
saaw$wealthscore = saaw$V191



#subset data to keep only the important cols - less messy; 
keepw = c("wealthindex", 
          "wealthscore",
          "diabdiag",
          "diabtreat", 
          "gender", 
          "country", 
          "dob",
          "age", 
          "eduattain",
          "eduyears",
          "eduyearsatlevel",
          "edulevel", 
          "V001", 
          "V002")

keepm = c("wealthindex", 
          "wealthscore",
          "diabdiag",
          "diabtreat", 
          "gender", 
          "country", 
          "dob",
          "age", 
          "eduattain",
          "eduyears",
          "eduyearsatlevel",
          "edulevel", 
          "MV001", 
          "MV002")


saaws = saaw[keepw]
laws = law[keepw]
saams = saam[keepm]
lams = lam[keepm]


#make a single id var and remove duplicates/same household 
#confirmed that these are the correct id vars: https://dhsprogram.com/data/Merging-Datasets.cfm
saaws$clusterhouse = paste(saaws$V001, saaws$V002)
saams$clusterhouse = paste(saams$MV001, saams$MV002)
laws$clusterhouse = paste(laws$V001, laws$V002)
lams$clusterhouse = paste(lams$MV001, lams$MV002)

subsaaws = saaws[!duplicated(saaws$clusterhouse),] 
subsaams = saams[!duplicated(saams$clusterhouse),] 
subsaams[,c("MV001", "MV002")] = list(NULL) 
subsaaws[,c("V001", "V002")] = list(NULL) 

sublaws = laws[!duplicated(laws$clusterhouse),] 
sublams = lams[!duplicated(lams$clusterhouse),] 
sublams[,c("MV001", "MV002")] = list(NULL) 
sublaws[,c("V001", "V002")] = list(NULL) 

#create one dataset for each country (at individual/person level)
library(Stack)
s = Stack(subsaams, subsaaws)
l = Stack(sublams, sublaws)



# sah$clusterhouse = paste(sah$HV001, sah$HV002) #commented out bc exported/reimported file already contains this col
sah = sah[!duplicated(sah$clusterhouse),] 
lh$clusterhouse = paste(lh$HV001, lh$HV002)




lh = lh[!duplicated(lh$clusterhouse),] #there are no duplicates at the household level - nice; 




keepsah = c("HV201", #source of drinking water
           "HV205", #type of toilet facility 
           "HV226", #type of cooking fuel 
           "SH116A", #type of fuel used for heating
           "HV206", #electricity
           "HV207", #radio 
           "HV208", #television
           "HV221", #telephone (nonmobile)
           "HV243E", #computer
           "HV209", #refrigerator
           "SH121G", #vacuum 
           "SH121H", #microwave
           "SH121I", #electric/gas stove
           "SH121J", #Washing machine 
           "HV243B", #Has watch
           "HV243A", #Has mobile telephone  
           "HV210", #Has bicycle  
           "HV211", #Has motorcycle/scooter 
           "HV243C", #Has animal-drawn cart
           "HV212", #Has car/truck
           "HV243D", #Has boat with a motor
           "SH124A", #How is refuse collected or removed
           "SH141A", #Type of dwelling 
           "HV213", #Main floor material  
           "HV215", #Main roof material 
           "HV214", #Main wall material
           "HV246A", #Owns cattle 
           "HV246C", #Owns horses/ donkeys/ mules
           "HV246D", #Owns goats
           "HV246E", #Owns sheep
           "HV246G", #Owns pigs
           "HV246F", #Owns chickens/poultry
           "HV216", #Number of rooms used for sleeping
           "HV012", #Number of de jure members
           "clusterhouse"
           )

keeplh = c("HV201", #source of drinking water
          "HV205", #type of toilet facility
          "HV206", #electricity
          "SH110B", #Battery or generator
          "SH110C", #Solar panel
          "HV207", #radio 
          "HV208", #television 
          "HV243A", #Has mobile telephone  
          "HV221", #telephone (nonmobile)
          "HV209", #refrigerator
          "SH110I", #Bed/mattress
          "SH110J", #Computer
          "SH110K", #Internet access
          "HV226", #type of cooking fuel
          "HV213", #Main floor material  
          "HV215", #Main roof material 
          "HV214", #Main wall material
          "HV243B", #Has watch
          "HV210", #Has bicycle  
          "HV211", #Has motorcycle/scooter 
          "HV243C", #Has animal-drawn cart
          "HV212", #Has car/truck
          "HV247", #Has bank account
        #  "HV101", #relationship to head == 15, domestic employee 
          "HV244", #Owns land usable for agriculture
          "HV216", #Number of rooms used for sleeping
          "HV012", #Number of de jure members   
          "clusterhouse"
)


sah = sah[keepsah]
lh = lh[keeplh]
lh$HV216 = ifelse (lh$HV216 == 0, 1, lh$HV216)
sah$HV216 = ifelse (sah$HV216 == 0, 1, sah$HV216)
lh$memsleep = lh$HV012/lh$HV216
sah$memsleep = sah$HV012/sah$HV216

library(dplyr)
smerge = right_join(sah, s, by = "clusterhouse") #keep all obs at individual level (s and l files)
lmerge = right_join(lh, l, by = "clusterhouse")
sl = Stack(smerge, lmerge)


#create memsleep var for wealth index #commented bc it was created above 
#sl$memsleep = sl$HV012/sl$HV216


#recode SAS DOB var 
#make cmc dob interpretable: 
sl$birthyear = 1900+(sl$dob %/%12)
sl$birthmonth = sl$dob%%12
sl$birthmonth[sl$birthmonth == 0] = 12

#domestic staff??? 
#house - owns a house 
#check Ns 
#what to do about missings? 
#Why are vals missing? 
#fix memsleep var 




##########################################################
#pca analysis 
##########################################################

#these vars are in both SA and lesotho datasets. need to turn categorical vars into dummy vars to make matrix to feed prcomp function
#How is memsleep coded???? appear binary in xlsx spreadsheets but that doesn't make sense.... 
#documentation says memsleep should be treated as continuous in pca 
pcavars = c('drinkingwater', 'toilet', 'cookfuel', 'floor', 'roof', 'wall', 'memsleep', 'electricity', 'radio', 'tv', 'landline', 'fridge', 'computer', 'watch', 'bike', 'mobilephone', 'motorcycle', 'animalcart', 'car') 

#clean up and rename vars
lhnames = c("HV201", "HV205", "HV226", "HV213", "HV215", "HV214", "memsleep", "HV206", "HV207", "HV208", "HV221", "HV209", "SH110J", "HV243B", "HV210", "HV243A", "HV211", "HV243C", "HV212")
lh = lh[lhnames]
names(lh) = pcavars

#only computer is different 
sahnames = c("HV201", "HV205", "HV226", "HV213", "HV215", "HV214", "memsleep", "HV206", "HV207", "HV208", "HV221", "HV209", "HV243E", "HV243B", "HV210", "HV243A", "HV211", "HV243C", "HV212")
sah = sah[sahnames]
names(sah) = pcavars

ls = Stack(lh, sah)

library(dummies)  
library(factoextra)

dlh = cbind(lh, dummy(lh$drinkingwater, sep = "drinkingwater"), dummy(lh$toilet, sep = "toilet"),dummy(lh$cookfuel, sep = "cookfuel"),
                dummy(lh$floor, sep = "floor"), dummy(lh$roof, sep = "roof"), dummy(lh$wall, sep = "wall"))
dsah = cbind(sah, dummy(sah$drinkingwater, sep = "drinkingwater"), dummy(sah$toilet, sep = "toilet"),dummy(sah$cookfuel, sep = "cookfuel"),
            dummy(sah$floor, sep = "floor"), dummy(sah$roof, sep = "roof"), dummy(sah$wall, sep = "wall"))
dlsh = cbind(ls, dummy(ls$drinkingwater, sep = "drinkingwater"), dummy(ls$toilet, sep = "toilet"),dummy(ls$cookfuel, sep = "cookfuel"),
            dummy(ls$floor, sep = "floor"), dummy(ls$roof, sep = "roof"), dummy(ls$wall, sep = "wall"))

lspca = prcomp(dlsh[,!names(dlsh) %in% c('drinkingwater', 'toilet', 'cookfuel', 'floor', 'roof', 'wall', 'memsleep')])
lpca = prcomp(dlh[,!names(dlh) %in% c('drinkingwater', 'toilet', 'cookfuel', 'floor', 'roof', 'wall', 'memsleep')])
spca = prcomp(dsah[,!names(dsah) %in% c('drinkingwater', 'toilet', 'cookfuel', 'floor', 'roof', 'wall', 'memsleep')])

lspca$center

