####################-----meta分析（分区域做）-----####################
rm(list=ls())
setwd("D:/毕业")
data <- read.csv("data_Class.csv")
data

#分区
data_A <- subset(data, cluster == "Zone1");data_A
data_B <- subset(data, cluster == "Zone2");data_B
data_C <- subset(data, cluster == "Zone3");data_C
data_D <- subset(data, cluster == "Zone4");data_D

####################-----Global-----####################
library(metafor)
library(metagear)
library(data.table)
data.treat <- data.table(treatment =  c('ALL',unique(data$Classification_1)))
data.treat

# what are labels
data.treat[treatment=='ALL',desc := 'All']
data.treat[treatment=='farmyard manure',desc := 'farmyard manure']
data.treat[treatment=='undefined',desc := 'undefined']
data.treat[treatment=='green manure',desc := 'green manure']
data.treat[treatment=='commodity organic fertilizer',desc := 'commodity organic fertilizer']
data.treat[treatment=='Humic acid organic fertilizer',desc := 'Humic acid organic fertilizer']
data.treat

# a list to store the coefficients
out2 = out3 = list()

# make a for loop to do a main analysis per treatment
for(i in data.treat$treatment){
  if(i=='ALL'){
    # run without selection to estimate overall mean
    r_nue <- rma.mv(yi,vi, data=data,random= list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    # run for selected treatment
    r_nue <- rma.mv(yi,vi, data=data[data$Classification_1==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  
  # save output in a list
  out2[[i]] <- data.table(mean = as.numeric((exp(r_nue$b)-1)*100),
                          se = as.numeric((exp(r_nue$se)-1)*100),
                          pval = round(as.numeric(r_nue$pval),4),
                          label =  paste0(data.treat[treatment==i,desc],' (n=',r_nue$k,')'))
}

# convert lists to vector
out2 <- rbindlist(out2)

# plot for NUE
forest(x = out2$mean, 
       sei = out2$se, 
       slab=out2$label, psize=0.9, cex=1, sortvar=out2$label, xlab="Change in pH(%)", header="Treatment", col="#CC0000", lwd=2)

#publication bias test
#begg’s test
ranktest(out2$mean, sei=out2$se)
#egger’s test
#用于检验发表偏移，如果P值大于0.05，则认为不存在发表偏倚；如果P值小于0.05，则认为存在发表偏倚
regtest(out2$mean, out2$se)

####################-----Zone1-----####################
data_A.treat <- data.table(treatment =  c('ALL',unique(data_A$Classification_1)))
data_A.treat

# what are labels
data_A.treat[treatment=='ALL',desc := 'All']
data_A.treat[treatment=='farmyard manure',desc := 'farmyard manure']
data_A.treat[treatment=='undefined',desc := 'undefined']
data_A.treat[treatment=='green manure',desc := 'green manure']
data_A.treat[treatment=='commodity organic fertilizer',desc := 'commodity organic fertilizer']
data_A.treat[treatment=='Humic acid organic fertilizer',desc := 'Humic acid organic fertilizer']
data_A.treat

# a list to store the coefficients
out2 = out3 = list()

# make a for loop to do a main analysis per treatment
for(i in data_A.treat$treatment){
  if(i=='ALL'){
    # run without selection to estimate overall mean
    r_nue <- rma.mv(yi,vi, data=data_A,random= list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    # run for selected treatment
    r_nue <- rma.mv(yi,vi, data=data_A[data_A$Classification_1==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  # save output in a list
  out2[[i]] <- data.table(mean = as.numeric((exp(r_nue$b)-1)*100),
                          se = as.numeric((exp(r_nue$se)-1)*100),
                          pval = round(as.numeric(r_nue$pval),4),
                          label =  paste0(data_A.treat[treatment==i,desc],' (n=',r_nue$k,')')
  )
}

# convert lists to vector
out2 <- rbindlist(out2)

# plot for NUE
forest(x = out2$mean, 
       sei = out2$se, 
       slab=out2$label, psize=0.9, cex=1, sortvar=out2$label, xlab="Change in pH(%)", header="Treatment", col="#CC0000", lwd=2)

#publication bias test
#begg’s test
ranktest(out2$mean, sei=out2$se)
#egger’s test
#用于检验发表偏移，如果P值大于0.05，则认为不存在发表偏倚；如果P值小于0.05，则认为存在发表偏倚
regtest(out2$mean, out2$se)




####################-----Zone2-----####################
data_B.treat <- data.table(treatment =  c('ALL',unique(data_B$Classification_1)))
data_B.treat

# what are labels
data_B.treat[treatment=='ALL',desc := 'All']
data_B.treat[treatment=='farmyard manure',desc := 'farmyard manure']
data_B.treat[treatment=='undefined',desc := 'undefined']
data_B.treat[treatment=='green manure',desc := 'green manure']
data_B.treat[treatment=='commodity organic fertilizer',desc := 'commodity organic fertilizer']
data_B.treat[treatment=='Humic acid organic fertilizer',desc := 'Humic acid organic fertilizer']
data_B.treat

# a list to store the coefficients
out2 = out3 = list()

# make a for loop to do a main analysis per treatment
for(i in data_B.treat$treatment){
  if(i=='ALL'){
    # run without selection to estimate overall mean
    r_nue <- rma.mv(yi,vi, data=data_B,random= list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    # run for selected treatment
    r_nue <- rma.mv(yi,vi, data=data_B[data_B$Classification_1==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  # save output in a list
  out2[[i]] <- data.table(mean = as.numeric((exp(r_nue$b)-1)*100),
                          se = as.numeric((exp(r_nue$se)-1)*100),
                          pval = round(as.numeric(r_nue$pval),4),
                          label =  paste0(data_B.treat[treatment==i,desc],' (n=',r_nue$k,')')
  )
}

# convert lists to vector
out2 <- rbindlist(out2)

# plot for NUE
forest(x = out2$mean, 
       sei = out2$se, 
       slab=out2$label, psize=0.9, cex=1, sortvar=out2$label, xlab="Change in pH(%)", header="Treatment", col="#CC0000", lwd=2)

#publication bias test
#begg’s test
ranktest(out2$mean, sei=out2$se)
#egger’s test
#用于检验发表偏移，如果P值大于0.05，则认为不存在发表偏倚；如果P值小于0.05，则认为存在发表偏倚
regtest(out2$mean, out2$se)

####################-----Zone3####################
data_C.treat <- data.table(treatment =  c('ALL',unique(data_C$Classification_1)))
data_C.treat

# what are labels
data_C.treat[treatment=='ALL',desc := 'All']
data_C.treat[treatment=='farmyard manure',desc := 'farmyard manure']
data_C.treat[treatment=='undefined',desc := 'undefined']
data_C.treat[treatment=='green manure',desc := 'green manure']
data_C.treat[treatment=='commodity organic fertilizer',desc := 'commodity organic fertilizer']
#data_C.treat[treatment=='Humic acid organic fertilizer',desc := 'Humic acid organic fertilizer']
data_C.treat

# a list to store the coefficients
out2 = out3 = list()

# make a for loop to do a main analysis per treatment
for(i in data_C.treat$treatment){
  if(i=='ALL'){
    # run without selection to estimate overall mean
    r_nue <- rma.mv(yi,vi, data=data_C,random= list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    # run for selected treatment
    r_nue <- rma.mv(yi,vi, data=data_C[data_C$Classification_1==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  # save output in a list
  out2[[i]] <- data.table(mean = as.numeric((exp(r_nue$b)-1)*100),
                          se = as.numeric((exp(r_nue$se)-1)*100),
                          pval = round(as.numeric(r_nue$pval),4),
                          label =  paste0(data_C.treat[treatment==i,desc],' (n=',r_nue$k,')')
  )
}

# convert lists to vector
out2 <- rbindlist(out2)

# plot for NUE
forest(x = out2$mean, 
       sei = out2$se, 
       slab=out2$label, psize=0.9, cex=1, sortvar=out2$label, xlab="Change in pH(%)", header="Treatment", col="#CC0000", lwd=2)

#publication bias test
#begg’s test
ranktest(out2$mean, sei=out2$se)
#egger’s test
#用于检验发表偏移，如果P值大于0.05，则认为不存在发表偏倚；如果P值小于0.05，则认为存在发表偏倚
regtest(out2$mean, out2$se)


####################-----Zone4-----####################
data_D.treat <- data.table(treatment =  c('ALL',unique(data_D$Classification_1)))
data_D.treat

# what are labels
data_D.treat[treatment=='ALL',desc := 'All']
data_D.treat[treatment=='farmyard manure',desc := 'farmyard manure']
#data_D.treat[treatment=='undefined',desc := 'undefined']
#data_D.treat[treatment=='green manure',desc := 'green manure']
data_D.treat[treatment=='commodity organic fertilizer',desc := 'commodity organic fertilizer']
data_D.treat[treatment=='Humic acid organic fertilizer',desc := 'Humic acid organic fertilizer']
data_D.treat

# a list to store the coefficients
out2 = out3 = list()

# make a for loop to do a main analysis per treatment
for(i in data_D.treat$treatment){
  if(i=='ALL'){
    # run without selection to estimate overall mean
    r_nue <- rma.mv(yi,vi, data=data_D,random= list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    # run for selected treatment
    r_nue <- rma.mv(yi,vi, data=data_D[data_D$Classification_1==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  # save output in a list
  out2[[i]] <- data.table(mean = as.numeric((exp(r_nue$b)-1)*100),
                          se = as.numeric((exp(r_nue$se)-1)*100),
                          pval = round(as.numeric(r_nue$pval),4),
                          label =  paste0(data_D.treat[treatment==i,desc],' (n=',r_nue$k,')')
  )
}

# convert lists to vector
out2 <- rbindlist(out2)

# plot for NUE
forest(x = out2$mean, 
       sei = out2$se, 
       slab=out2$label, psize=0.9, cex=1, sortvar=out2$label, xlab="Change in pH(%)", header="Treatment", col="#CC0000", lwd=2)

#publication bias test
#begg’s test
ranktest(out2$mean, sei=out2$se)
#egger’s test
#用于检验发表偏移，如果P值大于0.05，则认为不存在发表偏倚；如果P值小于0.05，则认为存在发表偏倚
regtest(out2$mean, out2$se)




####################全球及分区域meta分析(获取sourdata数据)########
rm(list=ls())
setwd("D:/毕业")
data <- read.csv("data_Class.csv")
data

data_A <- subset(data, cluster == "Zone1");data_A
data_B <- subset(data, cluster == "Zone2");data_B
data_C <- subset(data, cluster == "Zone3");data_C
data_D <- subset(data, cluster == "Zone4");data_D

####################-----Global-----####################
###################total###
fixed1<-rma(yi,vi, data=data, method="FE")
summary(fixed1)
coef(fixed1)
###################ph###
table(data$pH_class)
r1_pH<-rma(yi,vi, mods=~pH_class,data=data, method="REML")
summary(r1_pH)
r1_pH_1<-rma(yi,vi, mods=~pH_class-1,data=data, method="REML")
summary(r1_pH_1)
a<-coef(summary(r1_pH_1))
View(a)
#####################soc###
table(data$SOC_class)
r1_soc<-rma(yi,vi, mods=~SOC_class,data=data, method="REML")
summary(r1_soc)
r1_soc_1<-rma(yi,vi, mods=~SOC_class-1,data=data, method="REML")
summary(r1_soc_1)
a<-coef(summary(r1_soc_1))
View(a)
#####################tn###
table(data$TN_class)
r1_tn<-rma(yi,vi, mods=~TN_class,data=data, method="REML")
summary(r1_tn)
r1_tn_1<-rma(yi,vi, mods=~TN_class-1,data=data, method="REML")
summary(r1_tn_1)
a<-coef(summary(r1_tn_1))
View(a)
#####################ap###
table(data$AP_class)
r1_ap<-rma(yi,vi, mods=~AP_class,data=data, method="REML")
summary(r1_ap)
r1_ap_1<-rma(yi,vi, mods=~AP_class-1,data=data, method="REML")
summary(r1_ap_1)
a<-coef(summary(r1_ap_1))
View(a)
#####################clay###
table(data$clay_class)
r1_ak<-rma(yi,vi, mods=~clay_class,data=data, method="REML")
summary(r1_ak)
r1_ak_1<-rma(yi,vi, mods=~clay_class-1,data=data, method="REML")
summary(r1_ak_1)
a<-coef(summary(r1_ak_1))
View(a)
#####################map###
table(data$map_class)
r1_map<-rma(yi,vi, mods=~map_class,data=data, method="REML")
summary(r1_map)
r1_map_1<-rma(yi,vi, mods=~map_class-1,data=data, method="REML")
summary(r1_map_1)
a<-coef(summary(r1_map_1))
View(a)
#####################mat###
table(data$mat_class)
r1_mat<-rma(yi,vi, mods=~mat_class,data=data, method="REML")
summary(r1_mat)
r1_mat_1<-rma(yi,vi, mods=~mat_class-1,data=data, method="REML")
summary(r1_mat_1)
a<-coef(summary(r1_mat_1))
View(a)
#####################cropland_type###
table(data$Cropland_type)
r1_cropland_type<-rma(yi,vi, mods=~Cropland_type,data=data, method="REML")
summary(r1_cropland_type)
r1_cropland_type_1<-rma(yi,vi, mods=~Cropland_type-1,data=data, method="REML")
summary(r1_cropland_type_1)
a<-coef(summary(r1_cropland_type_1))
View(a)
#####################tillage_methods###
table(data$Tillage_methods)
r1_tillage_methods<-rma(yi,vi, mods=~Tillage_methods,data=data, method="REML")
summary(r1_tillage_methods)
r1_tillage_methods_1<-rma(yi,vi, mods=~Tillage_methods-1,data=data, method="REML")
summary(r1_tillage_methods_1)
a<-coef(summary(r1_tillage_methods_1))
View(a)
#####################time.1###
table(data$Time)
r1_time.1<-rma(yi,vi, mods=~Time,data=data, method="REML")
summary(r1_time.1)
r1_time.1_1<-rma(yi,vi, mods=~Time-1,data=data, method="REML")
summary(r1_time.1_1)
a<-coef(summary(r1_time.1_1))
View(a)
#####################Dosages###
table(data$Dosages)
r1_dosages.1<-rma(yi,vi, mods=~Dosages,data=data, method="REML")
summary(r1_dosages.1)
r1_dosages.1_1<-rma(yi,vi, mods=~Dosages-1,data=data, method="REML")
summary(r1_dosages.1_1)
a<-coef(summary(r1_dosages.1_1))
View(a)

####################-----Zone1-----#####################
###################total###
fixed1<-rma(yi,vi, data=data_A, method="FE")
summary(fixed1)
coef(fixed1)
###################ph###
table(data_A$pH_class)
r1_pH<-rma(yi,vi, mods=~pH_class,data=data_A, method="REML")
summary(r1_pH)
r1_pH_1<-rma(yi,vi, mods=~pH_class-1,data=data_A, method="REML")
summary(r1_pH_1)
a<-coef(summary(r1_pH_1))
View(a)
#####################soc###
table(data_A$SOC_class)
r1_soc<-rma(yi,vi, mods=~SOC_class,data=data_A, method="REML")
summary(r1_soc)
r1_soc_1<-rma(yi,vi, mods=~SOC_class-1,data=data_A, method="REML")
summary(r1_soc_1)
a<-coef(summary(r1_soc_1))
View(a)
#####################tn###
table(data_A$TN_class)
r1_tn<-rma(yi,vi, mods=~TN_class,data=data_A, method="REML")
summary(r1_tn)
r1_tn_1<-rma(yi,vi, mods=~TN_class-1,data=data_A, method="REML")
summary(r1_tn_1)
a<-coef(summary(r1_tn_1))
View(a)
#####################ap##
table(data_A$AP_class)
r1_ap<-rma(yi,vi, mods=~AP_class,data=data_A, method="REML")
summary(r1_ap)
r1_ap_1<-rma(yi,vi, mods=~AP_class-1,data=data_A, method="REML")
summary(r1_ap_1)
a<-coef(summary(r1_ap_1))
View(a)
#####################Clay###
table(data_A$clay_class)
r1_ak<-rma(yi,vi, mods=~clay_class,data=data_A, method="REML")
summary(r1_ak)
r1_ak_1<-rma(yi,vi, mods=~clay_class-1,data=data_A, method="REML")
summary(r1_ak_1)
a<-coef(summary(r1_ak_1))
View(a)
#####################map###
table(data_A$map_class)
r1_map<-rma(yi,vi, mods=~map_class,data=data_A, method="REML")
summary(r1_map)
r1_map_1<-rma(yi,vi, mods=~map_class-1,data=data_A, method="REML")
summary(r1_map_1)
a<-coef(summary(r1_map_1))
View(a)
#####################mat##
table(data_A$mat_class)
r1_mat<-rma(yi,vi, mods=~mat_class,data=data_A, method="REML")
summary(r1_mat)
r1_mat_1<-rma(yi,vi, mods=~mat_class-1,data=data_A, method="REML")
summary(r1_mat_1)
a<-coef(summary(r1_mat_1))
View(a)
#####################cropland_type###
table(data_A$Cropland_type)
r1_cropland_type<-rma(yi,vi, mods=~Cropland_type,data=data_A, method="REML")
summary(r1_cropland_type)
r1_cropland_type_1<-rma(yi,vi, mods=~Cropland_type-1,data=data_A, method="REML")
summary(r1_cropland_type_1)
a<-coef(summary(r1_cropland_type_1))
View(a)
#####################tillage_methods###
table(data_A$Tillage_methods)
r1_tillage_methods<-rma(yi,vi, mods=~Tillage_methods,data=data_A, method="REML")
summary(r1_tillage_methods)
r1_tillage_methods_1<-rma(yi,vi, mods=~Tillage_methods-1,data=data_A, method="REML")
summary(r1_tillage_methods_1)
a<-coef(summary(r1_tillage_methods_1))
View(a)
#####################Time###
table(data_A$Time)
r1_time.1<-rma(yi,vi, mods=~Time,data=data_A, method="REML")
summary(r1_time.1)
r1_time.1_1<-rma(yi,vi, mods=~Time-1,data=data_A, method="REML")
summary(r1_time.1_1)
a<-coef(summary(r1_time.1_1))
View(a)
#####################Dosages###
table(data_A$Dosages)
r1_dosages.1<-rma(yi,vi, mods=~Dosages,data=data_A, method="REML")
summary(r1_dosages.1)
r1_dosages.1_1<-rma(yi,vi, mods=~Dosages-1,data=data_A, method="REML")
summary(r1_dosages.1_1)
a<-coef(summary(r1_dosages.1_1))
View(a)

####################-----Zone2-----####################
###################total###
fixed1<-rma(yi,vi, data=data_B, method="FE")
summary(fixed1)
coef(fixed1)
###################ph###
table(data_B$pH_class)
r1_pH<-rma(yi,vi, mods=~pH_class,data=data_B, method="REML")
summary(r1_pH)
r1_pH_1<-rma(yi,vi, mods=~pH_class-1,data=data_B, method="REML")
summary(r1_pH_1)
a<-coef(summary(r1_pH_1))
View(a)
#####################soc###
table(data_B$SOC_class)
r1_soc<-rma(yi,vi, mods=~SOC_class,data=data_B, method="REML")
summary(r1_soc)
r1_soc_1<-rma(yi,vi, mods=~SOC_class-1,data=data_B, method="REML")
summary(r1_soc_1)
a<-coef(summary(r1_soc_1))
View(a)
#####################tn###
table(data_B$TN_class)
r1_tn<-rma(yi,vi, mods=~TN_class,data=data_B, method="REML")
summary(r1_tn)
r1_tn_1<-rma(yi,vi, mods=~TN_class-1,data=data_B, method="REML")
summary(r1_tn_1)
a<-coef(summary(r1_tn_1))
View(a)
#####################ap##
table(data_B$AP_class)
r1_ap<-rma(yi,vi, mods=~AP_class,data=data_B, method="REML")
summary(r1_ap)
r1_ap_1<-rma(yi,vi, mods=~AP_class-1,data=data_B, method="REML")
summary(r1_ap_1)
a<-coef(summary(r1_ap_1))
View(a)
#####################Clay###
table(data_B$clay_class)
r1_ak<-rma(yi,vi, mods=~clay_class,data=data_B, method="REML")
summary(r1_ak)
r1_ak_1<-rma(yi,vi, mods=~clay_class-1,data=data_B, method="REML")
summary(r1_ak_1)
a<-coef(summary(r1_ak_1))
View(a)
#####################map###
table(data_B$map_class)
r1_map<-rma(yi,vi, mods=~map_class,data=data_B, method="REML")
summary(r1_map)
r1_map_1<-rma(yi,vi, mods=~map_class-1,data=data_B, method="REML")
summary(r1_map_1)
a<-coef(summary(r1_map_1))
View(a)
#####################mat##
table(data_B$mat_class)
r1_mat<-rma(yi,vi, mods=~mat_class,data=data_B, method="REML")
summary(r1_mat)
r1_mat_1<-rma(yi,vi, mods=~mat_class-1,data=data_B, method="REML")
summary(r1_mat_1)
a<-coef(summary(r1_mat_1))
View(a)
#####################cropland_type###
table(data_B$Cropland_type)
r1_cropland_type<-rma(yi,vi, mods=~Cropland_type,data=data_B, method="REML")
summary(r1_cropland_type)
r1_cropland_type_1<-rma(yi,vi, mods=~Cropland_type-1,data=data_B, method="REML")
summary(r1_cropland_type_1)
a<-coef(summary(r1_cropland_type_1))
View(a)
#####################tillage_methods###
table(data_B$Tillage_methods)
r1_tillage_methods<-rma(yi,vi, mods=~Tillage_methods,data=data_B, method="REML")
summary(r1_tillage_methods)
r1_tillage_methods_1<-rma(yi,vi, mods=~Tillage_methods-1,data=data_B, method="REML")
summary(r1_tillage_methods_1)

a<-coef(summary(r1_tillage_methods_1))
View(a)
#####################Time###
table(data_B$Time)
r1_time.1<-rma(yi,vi, mods=~Time,data=data_B, method="REML")
summary(r1_time.1)
r1_time.1_1<-rma(yi,vi, mods=~Time-1,data=data_B, method="REML")
summary(r1_time.1_1)
a<-coef(summary(r1_time.1_1))
View(a)
#####################Dosages###
table(data_B$Dosages)
r1_dosages.1<-rma(yi,vi, mods=~Dosages,data=data_B, method="REML")
summary(r1_dosages.1)
r1_dosages.1_1<-rma(yi,vi, mods=~Dosages-1,data=data_B, method="REML")
summary(r1_dosages.1_1)
a<-coef(summary(r1_dosages.1_1))
View(a)


####################-----Zone3-----####################
###################total###
fixed1<-rma(yi,vi, data=data_C, method="FE")
summary(fixed1)
coef(fixed1)
###################ph###
table(data_C$pH_class)
r1_pH<-rma(yi,vi, mods=~pH_class,data=data_C, method="REML")
summary(r1_pH)
r1_pH_1<-rma(yi,vi, mods=~pH_class-1,data=data_C, method="REML")
summary(r1_pH_1)
a<-coef(summary(r1_pH_1))
View(a)
#####################soc###
table(data_C$SOC_class)
r1_soc<-rma(yi,vi, mods=~SOC_class,data=data_C, method="REML")
summary(r1_soc)
r1_soc_1<-rma(yi,vi, mods=~SOC_class-1,data=data_C, method="REML")
summary(r1_soc_1)
a<-coef(summary(r1_soc_1))
View(a)
#####################tn###
table(data_C$TN_class)
r1_tn<-rma(yi,vi, mods=~TN_class,data=data_C, method="REML")
summary(r1_tn)
r1_tn_1<-rma(yi,vi, mods=~TN_class-1,data=data_C, method="REML")
summary(r1_tn_1)
a<-coef(summary(r1_tn_1))
View(a)
#####################ap##
table(data_C$AP_class)
r1_ap<-rma(yi,vi, mods=~AP_class,data=data_C, method="REML")
summary(r1_ap)
r1_ap_1<-rma(yi,vi, mods=~AP_class-1,data=data_C, method="REML")
summary(r1_ap_1)
a<-coef(summary(r1_ap_1))
View(a)
#####################Clay###
table(data_C$clay_class)
r1_ak<-rma(yi,vi, mods=~clay_class,data=data_C, method="REML")
summary(r1_ak)
r1_ak_1<-rma(yi,vi, mods=~clay_class-1,data=data_C, method="REML")
summary(r1_ak_1)
a<-coef(summary(r1_ak_1))
View(a)
#####################map###
table(data_C$map_class)
r1_map<-rma(yi,vi, mods=~map_class,data=data_C, method="REML")
summary(r1_map)
r1_map_1<-rma(yi,vi, mods=~map_class-1,data=data_C, method="REML")
summary(r1_map_1)
a<-coef(summary(r1_map_1))
View(a)
#####################mat##
table(data_C$mat_class)
r1_mat<-rma(yi,vi, mods=~mat_class,data=data_C, method="REML")
summary(r1_mat)
r1_mat_1<-rma(yi,vi, mods=~mat_class-1,data=data_C, method="REML")
summary(r1_mat_1)
a<-coef(summary(r1_mat_1))
View(a)
#####################cropland_type###
table(data_C$Cropland_type)
r1_cropland_type<-rma(yi,vi, mods=~Cropland_type,data=data_C, method="REML")
summary(r1_cropland_type)
r1_cropland_type_1<-rma(yi,vi, mods=~Cropland_type-1,data=data_C, method="REML")
summary(r1_cropland_type_1)
a<-coef(summary(r1_cropland_type_1))
View(a)
#####################tillage_methods###
table(data_C$Tillage_methods)
r1_tillage_methods<-rma(yi,vi, mods=~Tillage_methods,data=data_C, method="REML")
summary(r1_tillage_methods)
r1_tillage_methods_1<-rma(yi,vi, mods=~Tillage_methods-1,data=data_C, method="REML")
summary(r1_tillage_methods_1)
a<-coef(summary(r1_tillage_methods_1))
View(a)
#####################Time###
table(data_C$Time)
r1_time.1<-rma(yi,vi, mods=~Time,data=data_C, method="REML")
summary(r1_time.1)
r1_time.1_1<-rma(yi,vi, mods=~Time-1,data=data_C, method="REML")
summary(r1_time.1_1)
a<-coef(summary(r1_time.1_1))
View(a)
#####################Dosages###
table(data_C$Dosages)
r1_dosages.1<-rma(yi,vi, mods=~Dosages,data=data_C, method="REML")
summary(r1_dosages.1)
r1_dosages.1_1<-rma(yi,vi, mods=~Dosages-1,data=data_C, method="REML")
summary(r1_dosages.1_1)
a<-coef(summary(r1_dosages.1_1))
View(a)

####################-----Zone4####################
###################total###
fixed1<-rma(yi,vi, data=data_D, method="FE")
summary(fixed1)
coef(fixed1)
###################ph###
table(data_D$pH_class)
r1_pH<-rma(yi,vi, mods=~pH_class,data=data_D, method="REML")
summary(r1_pH)
r1_pH_1<-rma(yi,vi, mods=~pH_class-1,data=data_D, method="REML")
summary(r1_pH_1)
a<-coef(summary(r1_pH_1))
View(a)
#####################soc###
table(data_D$SOC_class)
r1_soc<-rma(yi,vi, mods=~SOC_class,data=data_D, method="REML")
summary(r1_soc)
r1_soc_1<-rma(yi,vi, mods=~SOC_class-1,data=data_D, method="REML")
summary(r1_soc_1)
a<-coef(summary(r1_soc_1))
View(a)
#####################tn###
table(data_D$TN_class)
r1_tn<-rma(yi,vi, mods=~TN_class,data=data_D, method="REML")
summary(r1_tn)
r1_tn_1<-rma(yi,vi, mods=~TN_class-1,data=data_D, method="REML")
summary(r1_tn_1)
a<-coef(summary(r1_tn_1))
View(a)
#####################ap##
table(data_D$AP_class)
r1_ap<-rma(yi,vi, mods=~AP_class,data=data_D, method="REML")
summary(r1_ap)
r1_ap_1<-rma(yi,vi, mods=~AP_class-1,data=data_D, method="REML")
summary(r1_ap_1)
a<-coef(summary(r1_ap_1))
View(a)
#####################Clay###
table(data_D$clay_class)
r1_ak<-rma(yi,vi, mods=~clay_class,data=data_D, method="REML")
summary(r1_ak)
r1_ak_1<-rma(yi,vi, mods=~clay_class-1,data=data_D, method="REML")
summary(r1_ak_1)
a<-coef(summary(r1_ak_1))
View(a)
#####################map###
table(data_D$map_class)
r1_map<-rma(yi,vi, mods=~map_class,data=data_D, method="REML")
summary(r1_map)
r1_map_1<-rma(yi,vi, mods=~map_class-1,data=data_D, method="REML")
summary(r1_map_1)
a<-coef(summary(r1_map_1))
View(a)
#####################mat##
table(data_D$mat_class)
r1_mat<-rma(yi,vi, mods=~mat_class,data=data_D, method="REML")
summary(r1_mat)
r1_mat_1<-rma(yi,vi, mods=~mat_class-1,data=data_D, method="REML")
summary(r1_mat_1)
a<-coef(summary(r1_mat_1))
View(a)
#####################cropland_type###
table(data_D$Cropland_type)
r1_cropland_type<-rma(yi,vi, mods=~Cropland_type,data=data_D, method="REML")
summary(r1_cropland_type)
r1_cropland_type_1<-rma(yi,vi, mods=~Cropland_type-1,data=data_D, method="REML")
summary(r1_cropland_type_1)
a<-coef(summary(r1_cropland_type_1))
View(a)
#####################tillage_methods###
table(data_D$Tillage_methods)
r1_tillage_methods<-rma(yi,vi, mods=~Tillage_methods,data=data_D, method="REML")
summary(r1_tillage_methods)
r1_tillage_methods_1<-rma(yi,vi, mods=~Tillage_methods-1,data=data_D, method="REML")
summary(r1_tillage_methods_1)
a<-coef(summary(r1_tillage_methods_1))
View(a)
#####################Time###
table(data_D$Time)
r1_time.1<-rma(yi,vi, mods=~Time,data=data_D, method="REML")
summary(r1_time.1)
r1_time.1_1<-rma(yi,vi, mods=~Time-1,data=data_D, method="REML")
summary(r1_time.1_1)
a<-coef(summary(r1_time.1_1))
View(a)
#####################Dosages###
table(data_D$Dosages)
r1_dosages.1<-rma(yi,vi, mods=~Dosages,data=data_D, method="REML")
summary(r1_dosages.1)
r1_dosages.1_1<-rma(yi,vi, mods=~Dosages-1,data=data_D, method="REML")
summary(r1_dosages.1_1)
a<-coef(summary(r1_dosages.1_1))
View(a)
