


###############################################################
###### descriptive stats #################################################

# all of these are bernoulli so
# mean = p 
# stddev = sqrt(p(1-p))


# LESOTHO 
drinkingwater = lh %>% group_by(HV201) %>% summarize(n())
toilet = lh %>% group_by(HV205) %>% summarize(n())
cookfuel = lh %>% group_by(HV226) %>% summarize(n())
floor = lh %>% group_by(HV213) %>% summarize(n())
roof = lh %>% group_by(HV215) %>% summarize(n())
wall = lh %>% group_by(HV214) %>% summarize(n())

drinkingwater$mean = NA
drinkingwater$stddev = NA
for (i in 1:length(drinkingwater$HV201)) {
  n = drinkingwater$`n()`[i]
  p = n/(sum(drinkingwater$`n()`))
  drinkingwater$mean[i] = p
  drinkingwater$stddev[i] = sqrt(p*(1-p))
}

toilet$mean = NA
toilet$stddev = NA
for (i in 1:length(toilet$HV205)) {
  n = toilet$`n()`[i]
  p = n/(sum(toilet$`n()`))
  toilet$mean[i] = p
  toilet$stddev[i] = sqrt(p*(1-p))
}

cookfuel$mean = NA
cookfuel$stddev = NA
for (i in 1:length(cookfuel$HV226)) {
  n = cookfuel$`n()`[i]
  p = n/(sum(cookfuel$`n()`))
  cookfuel$mean[i] = p
  cookfuel$stddev[i] = sqrt(p*(1-p))
}

floor$mean = NA
floor$stddev = NA
for (i in 1:length(floor$HV213)) {
  n = floor$`n()`[i]
  p = n/(sum(floor$`n()`))
  floor$mean[i] = p
  floor$stddev[i] = sqrt(p*(1-p))
}

wall$mean = NA
wall$stddev = NA
for (i in 1:length(wall$HV214)) {
  n = wall$`n()`[i]
  p = n/(sum(wall$`n()`))
  wall$mean[i] = p
  wall$stddev[i] = sqrt(p*(1-p))
}


roof$mean = NA
roof$stddev = NA
for (i in 1:length(roof$HV215)) {
  n = roof$`n()`[i]
  p = n/(sum(roof$`n()`))
  roof$mean[i] = p
  roof$stddev[i] = sqrt(p*(1-p))
}

roof$desc = 'roof'
wall$desc = 'wall'
floor$desc = 'floor'
cookfuel$desc = 'cookfuel'
toilet$desc = 'toilet'
drinkingwater$desc = 'drinkingwater'

roof$numans = roof$HV215
wall$numans = wall$HV214
floor$numans = floor$HV213
cookfuel$numans = cookfuel$HV226
toilet$numans = toilet$HV205
drinkingwater$numans = drinkingwater$HV201


ldesc = Stack(drinkingwater,toilet)
ldesc = Stack(ldesc, cookfuel)
ldesc = Stack(ldesc, floor)
ldesc = Stack(ldesc, wall)
ldesc = Stack(ldesc, roof)

ldesc=ldesc[c('n()', 'mean', 'stddev', 'desc', 'numans')]

mean = NA
stddev = NA
binvars = c('HV206', 'SH110B', 'SH110C', 'HV207', 'HV208', 'HV243A', 'HV221', 'HV209', 'SH110I', 'SH110J', 'SH110K', 'HV243B', 'HV210', 'HV211', 'HV243C', 'HV212', 'HV247', 'HV244')
for (i in 1:length(binvars)){
  mean[i] = sum(lh[binvars[i]])/length(lh$HV206)
  stddev[i] = sqrt(mean[i]*(1-mean[i]))
}

desc = c('electricity', 'battery', 'solarpanel', 'radio', 'tv', 'mobilephone', 'landline', 'fridge', 'bed', 'computer', 'internet', 'watch', 'bike', 'motorcycle', 'animalcart', 'car', 'bank', 'land')

bindesc = data.frame(mean, stddev, desc)
bindesc

ldesc = Stack(ldesc, bindesc)
ldesc

