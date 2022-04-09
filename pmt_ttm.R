#===============================================================================
#Title: Protection Motivation Theory - Trans Theoretical Model Framework 
#Author:Thulasi Vishwanath Harish
#Last updated: 7.12.2021
#Details: This R-script executes the methodology of the paper "IDENTIFYING THE DRIVERS OF 
#PRIVATE FLOOD PRECAUTIONARY MEASURES IN HO CHI MINH CITY, VIETNAM". It adopts the PMT-TTM framework and the methodology is described below:
#[1]Import data
#[2]Independent variables- data cleaning and normalization
#[3]Exploratory analysis
#   -implementation time plot
#   -implementation cost plot
#[4]Dependent variables - categorization of response variable bases on : proactive/ reactive and structural/non-structural
#[5]Lasso- Elastic net regression 
#[6]Model performance comparison
#[7]Feature importance
#   -extraction of variable coefficients from different models 
#   -creating a dataframe of weights (deviance)
#   -creating dataframes by aggregating coefficients from only ridge-lasso- elastic ney regression
#   -normalizing the absolute value of the above dataframes
#   -computing the weighted median
#   -plotting variable importance plots
#   -computing and plotting feature count
#===============================================================================

#Required library
library(modeest) 
library(fastDummies) 
library(BBmisc) 
library(MASS) 
library(bootStepAIC) 
library(gdata) 
library(data.table)
library(glmnet) 
library(gridExtra)
library(tibble)
library(broom)
library(modEvA)
library(modelr)
library(ggplot2)
library(tidyr) 
library(ggthemes)
library(pdp)
library(corrplot)
library(qpcR)
library(plyr)
library(spatstat)
library(dplyr)
library(wesanderson)
library(ggpubr)

#-------------------------------------------------------------------------------
#[1]Import Data
#-------------------------------------------------------------------------------

setwd("C:/G/TUM Study_Material/GFZ/precautionary drivers/Data")

data1 = read.csv("HH Survey Data_DECIDER2020_datavalue.csv")
lab1 = read.csv("HH Survey Data_DECIDER2020_lables.csv")

#-------------------------------------------------------------------------------
#[2]INDEPENDENT VARIABLES 
#-------------------------------------------------------------------------------

#Vulnerabilty 
vulnerable=as.data.frame(data1[,cbind("P3Q2.1","P3Q2.2","P3Q3.1")])
colnames(vulnerable)=c("flood last 10","flood next 10","economic loss")

#Severity
severity= as.data.frame(data1[,cbind("P3Q1.2","P3Q3.3","P3Q3.5","P3Q3.7")])
colnames(severity)=c("house impact","traffic","financial","health")

#Self-efficacy
self_eff= as.data.frame(data1[,cbind("P3Q2.6","P3Q3.2","P3Q3.4","P3Q3.8","P3Q3.9","P3Q3.11")])
colnames(self_eff)=c("house economy future","change livelihood","resist flood","repair house","relocate","financial support")

#trust in the government
trust= as.data.frame(data1[,cbind("P3Q1.1","P3Q1.3","P3Q1.4","P3Q1.5","P3Q1.6","P3Q1.7","P3Q2.4","P3Q2.5","P3Q3.10")]) 
colnames(trust)=c("city protection","flood warning","govt protection","govt damage","no help","flood neigh.","govt last 10","govt next 10","govt help")

#household profile
house_profile= as.data.frame(data1[,cbind("P4Q1.1","P4Q1.2","P4Q1.3","P4Q1.4","P4Q1.8","P4Q1.10","P4Q2.1","P4Q2.2")])
colnames(house_profile)=c("people","above 65","above 75","below 14","education","income","stay","constructed")
house_profile[house_profile==99|house_profile==98] = NA
house_profile$education[house_profile$education==8] = 5 #vocational training = bachelor degree
house_profile$stay= 2020 - house_profile$stay
house_profile$constructed = 2020 - house_profile$constructed

#past flood experience (only the serious events are included)
past_flood= as.data.frame(data1[,cbind("P1Q1", "P1Q2.3.2", "P1Q2.4.2", "P1Q2.5.2.0", "P1Q2.6.2", "P1Q2.8.2.10", "P1Q3.2.2", "P1Q3.3.2",
                                       "P1Q3.4.2", "P1Q3.5.2", "P1Q3.6.2", "P1Q3.7.2","P1Q4.2.2", "P1Q4.3.2", "P1Q4.4.2", "P1Q4.5.2", "P1Q7.2.2.98")])
past_flood[past_flood==99|past_flood==98]=NA
past_flood$'house damage'= rowMeans(past_flood[,7:12], na.rm = TRUE)
past_flood=past_flood[,-(7:12)]
past_flood$'valuable damage'=rowMeans(past_flood[,7:10], na.rm = TRUE)
past_flood=past_flood[,-(7:10)]
colnames(past_flood)=c("flood frequency","flood duration","flood height","no contamination", "flood velocity","no warning","no relief","house damage", "valuable damage")
past_flood$`flood duration`= as.numeric(gsub(",", ".", as.character(past_flood$`flood duration`)))

##combining all independent variables. Removing no data and replacing with mode
iv= cbind(vulnerable,severity, self_eff, trust, house_profile, past_flood)

#Eliminating the out liners for 
#flood duration, flood heights and govt last 10 years
iv$`flood duration`[iv$`flood duration`>200] = NA
iv$`flood height`[iv$`flood height`>200] = NA
iv$`govt last 10`[iv$`govt last 10`==88] = NA
iv$`govt next 10`[iv$`govt next 10`==88] = NA
iv[iv==99|iv==98] = NA
#replace missing values with mode
for(i in 1:ncol(iv)){
  iv[is.na(iv[,i]), i] <- mfv(iv[,i], na_rm= TRUE)
}

##normalizing independent variable for reactive households
iv_r= normalize(iv, method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")

##Past flood experience not considered for proactive households 
iv_p = iv_r[,-(31:39)]

#-------------------------------------------------------------------------------
#[3]EXPLORATORY ANALYSIS
#-------------------------------------------------------------------------------

#[1]Implementation Time
mea = cbind(data1[,c("P2Q1.4.implement", "P2Q1.7.implement", "P2Q1.1.implement", "P2Q1.2.implement", "P2Q1.3.implement", "P2Q1.5.implement", "P2Q1.6.implement")])
colnames(mea) = c("Elevate", "Install flood \n  protection", "Dry proofing \n valuables", "Mobile \n barriers", "Pumping \n equipment", "Water resistant \n materials", "Electricity control \n at higher level")
mea[mea==99] = NA
mea <- gather(mea, methods, time)

mea$methods <- factor(mea$methods, levels = c("Elevate", "Install flood \n  protection", "Dry proofing \n valuables", "Mobile \n barriers", "Pumping \n equipment", "Water resistant \n materials", "Electricity control \n at higher level"))

mea = mea %>% 
  mutate(group = case_when(mea$time == 5 ~ 'Did not implement'
                          ,mea$time == 4 ~ 'After both events'
                          ,mea$time == 2 ~ 'Before recent event'
                          ,mea$time == 3 ~ 'Before both events'
                          ,TRUE ~ 'Before serious event'
  ))

mea$group <- factor(mea$group, levels = c('Did not implement', 'After both events' , 'Before recent event', 'Before both events', 'Before serious event'))



ggplot(mea) +
  geom_bar(aes(x = methods, fill = as.factor(group)), 
           position = "stack", stat = "count") + 
  theme_hc()+
  labs(x="", y="Number of households", title = "Precautionary Measures Implemented")+ 
  theme(plot.title = element_text(hjust=0.5, size=16, face='bold'))+ 
  theme(axis.text=element_text(size=15),axis.title=element_text(size=16,face="bold"))+
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(), legend.text = element_text(size = 12))+
  scale_fill_manual(values=c("grey", "chocolate1","coral3","chartreuse3","chartreuse4"))

ggsave("C:/G/TUM Study_Material/GFZ/precautionary drivers/Results/measures_implemented.jpg", height = 20, width = 30, units = 'cm')


#[2]Implementation Cost
cost  = cbind(data1[,c("P2Q1.4.spend", "P2Q1.7.spend", "P2Q1.1.spend", "P2Q1.2.spend", "P2Q1.3.spend", "P2Q1.5.spend", "P2Q1.6.spend")])
cost[cost==99]= NA
cost[cost==0] = NA
colnames(cost) = c("Elevate", "Install flood \n  protection", "Dry proofing \n valuables", "Mobile \n barriers", "Pumping \n equipment", "Water resistant \n materials", "Electricity control \n at higher level")
for(i in 1:7){
  cost[,i] = as.numeric(gsub(",", ".", as.character(cost[,i])))
}
cost[cost==2]=NA
pre_cost = as.data.frame(rowSums(cost, na.rm = T))
cost = gather(cost, measure, VND)
cost = cost %>% drop_na(VND)
cost$measure <- factor(cost$measure, levels = c("Elevate", "Install flood \n  protection", "Dry proofing \n valuables", "Mobile \n barriers", "Pumping \n equipment", "Water resistant \n materials", "Electricity control \n at higher level"))

ggplot(cost, aes(x = measure, y = VND))+ geom_boxplot(fill = "darkolivegreen3")+
  scale_y_log10()+ theme_hc()+ labs( title = "Cost of Implementation", x = " ", y = "Cost per house in Million VND")+
  theme(plot.title = element_text(hjust=0.5, size=15, face='bold'))+ 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
ggsave("C:/G/TUM Study_Material/GFZ/precautionary drivers/Results/implementation_cost.jpg", height = 20, width = 30, units = 'cm')


#-------------------------------------------------------------------------------
#[4]DEPENDENT VARIABLES
#-------------------------------------------------------------------------------

##STRUCTURAL MEASURES
strct_measure= as.data.frame(data1[,cbind("P2Q1.4.implement","P2Q1.7.implement")])
colnames(strct_measure)=c("elevation_all","flood protection")

#Differentiation between proactive and reactive for structural measures 
strct_measure_pro= strct_measure
strct_measure_rea= strct_measure

for( i in 1:ncol(strct_measure)){
  strct_measure_pro[,i]= as.data.frame(sapply(strct_measure_pro[,i], function(x) ifelse(x==1||x==3,1,0))) 
  strct_measure_rea[,i]= as.data.frame(sapply(strct_measure_rea[,i], function(x) ifelse(x==2||x==4,1,0))) 
}

strct_measure_pro$sum= rowSums(strct_measure_pro)
strct_measure_pro$sum[strct_measure_pro$sum>0]=1

strct_measure_rea$sum= rowSums(strct_measure_rea)
strct_measure_rea$sum[strct_measure_rea$sum>0]=1

dv_strct= as.data.frame(cbind(strct_measure_pro$sum,strct_measure_rea$sum))
colnames(dv_strct)=c("strct_measure_pro","strct_measure_rea")


##NON-STRUCTURAL MEASURES
nonstrct_measure= as.data.frame(data1[,cbind("P2Q1.1.implement","P2Q1.2.implement","P2Q1.3.implement","P2Q1.5.implement","P2Q1.6.implement")])
colnames(nonstrct_measure)=c("dry proof", "water barrier", "pumping","water res","high istall elec.")

#Differentiation between proactive and reactive for non-structural measures 
nonstrct_measure_pro= nonstrct_measure
nonstrct_measure_rea= nonstrct_measure

for( i in 1:ncol(nonstrct_measure)){
  nonstrct_measure_pro[,i]= as.data.frame(sapply(nonstrct_measure_pro[,i], function(x) ifelse(x==1||x==3,1,0))) 
  nonstrct_measure_rea[,i]= as.data.frame(sapply(nonstrct_measure_rea[,i], function(x) ifelse(x==2||x==4,1,0))) 
}

nonstrct_measure_pro$sum= rowSums(nonstrct_measure_pro)
nonstrct_measure_pro$sum[nonstrct_measure_pro$sum>0]=1

nonstrct_measure_rea$sum= rowSums(nonstrct_measure_rea)
nonstrct_measure_rea$sum[nonstrct_measure_rea$sum>0]=1

dv_nonstrct= as.data.frame(cbind(nonstrct_measure_pro$sum,nonstrct_measure_rea$sum))
colnames(dv_nonstrct)=c("nonstrct_measure_pro", "nonstrct_measure_rea" )

#-------------------------------------------------------------------------------

#So far we have for type of cases:
#Case(i): structural-Proactive
#Case (ii): Structural- Reactive
#Case(iii): Non-Structural- Proactive
#Case(iv): Non- Structural- Reactive

#Now, we will look at different statistical methods for each of these cases


#-------------------------------------------------------------------------------
#[5]LASSO-NET REGRESSION
#-------------------------------------------------------------------------------

#cv.glmnet() function is used
#when alpha = 1 -> Lasso, alpha >0||<1 -> net-elastic
#lambda.1se is used to obtain the coefficients of the most suitable lambda 

#case1 
c1.list = list()
for( i in 0:10){
  fit.name = paste0("alpha", i/10)
  set.seed(20)
  c1.list[[fit.name]] = cv.glmnet(as.matrix(iv_p), dv_strct$strct_measure_pro , 
                                  alpha = i/10, family = "binomial", keep = TRUE)
}

c1.assess = list()
for(i in 1:11){
  assess.name = paste0("alpha", (i-1)/10)
  c1.assess[[assess.name]]= assess.glmnet(c1.list[[i]], newx = as.matrix(iv_p), 
                                          newy = dv_strct$strct_measure_pro, family = "binomial")
}

#case2
c2.list = list()
for( i in 0:10){
  fit.name = paste0("alpha", i/10)
  set.seed(20)
  c2.list[[fit.name]] = cv.glmnet(as.matrix(iv_r), dv_strct$strct_measure_rea , 
                                  alpha = i/10, family = "binomial", keep = TRUE)
}

c2.assess = list()
for(i in 1:11){
  assess.name = paste0("alpha", (i-1)/10)
  c2.assess[[assess.name]]= assess.glmnet(c2.list[[i]], newx = as.matrix(iv_r), 
                                          newy = dv_strct$strct_measure_rea, family = "binomial")
}

#case3
c3.list = list()
for( i in 0:10){
  fit.name = paste0("alpha", i/10)
  set.seed(20)
  c3.list[[fit.name]] = cv.glmnet(as.matrix(iv_p), dv_nonstrct$nonstrct_measure_pro , 
                                  alpha = i/10, family = "binomial", keep = TRUE)
}

c3.assess = list()
for(i in 1:11){
  assess.name = paste0("alpha", (i-1)/10)
  c3.assess[[assess.name]]= assess.glmnet(c3.list[[i]], newx = as.matrix(iv_p), 
                                          newy = dv_nonstrct$nonstrct_measure_pro, family = "binomial")
}

#case4
c4.list = list()
for( i in 0:10){
  fit.name = paste0("alpha", i/10)
  set.seed(20)
  c4.list[[fit.name]] = cv.glmnet(as.matrix(iv_r), dv_nonstrct$nonstrct_measure_rea , 
                                  alpha = i/10, family = "binomial", keep = TRUE)
}

c4.assess = list()
for(i in 1:11){
  assess.name = paste0("alpha", (i-1)/10)
  c4.assess[[assess.name]]= assess.glmnet(c4.list[[i]], newx = as.matrix(iv_r), 
                                          newy = dv_nonstrct$nonstrct_measure_rea, family = "binomial")
}

##Lasso
lasso.fit = list(c1.list$alpha1, c2.list$alpha1, c3.list$alpha1, c4.list$alpha1)

#acquiring best alpha and its corresponding deviance value 
alpha= data.frame(matrix( nrow = 11, ncol = 5))
alpha[,1] = names(c1.list)
for(i in 1:11){
  alpha[i,2] = c1.assess[[i]]$deviance
  alpha[i,3] = c2.assess[[i]]$deviance
  alpha[i,4] = c3.assess[[i]]$deviance
  alpha[i,5] = c4.assess[[i]]$deviance
}
colnames(alpha)= cbind('Deviance', 'case1', 'case2', 'case3', 'case4') 

alpha.min = data.frame(matrix(nrow = 2, ncol = 4))
colnames(alpha.min) = cbind('case1', 'case2', 'case3', 'case4')
for(i in 1:4){
  alpha.min[1,i] = min(alpha[,i+1])
  alpha.min[2,i] = alpha[which.min(alpha[,i+1]),1]
}

#net-elastic regression. MANUALLY enter the best alpha, if lowest deviance is for alpha = 0 or 1
#enter the alpha corresponding to the next lowest deviance from alpha.min
net.fit = list(c1.list$alpha0.5, c2.list$alpha0.4, c3.list$alpha0.9, c4.list$alpha0.1 )

#-------------------------------------------------------------------------------
#[6]MODEL PERFORMANCE COMPARISION
#-------------------------------------------------------------------------------

#calculate the deviance. rmse, auc and ROC curve performance metrics for all the above methods

#lasso
performance.lasso = as.data.frame(rbind(c(c1.assess$alpha1$deviance[[1]],c1.assess$alpha1$auc[[1]],sqrt(c1.assess$alpha1$mse[[1]])),
                                        c(c2.assess$alpha1$deviance[[1]],c2.assess$alpha1$auc[[1]],sqrt(c2.assess$alpha1$mse[[1]])),
                                        c(c3.assess$alpha1$deviance[[1]],c3.assess$alpha1$auc[[1]],sqrt(c3.assess$alpha1$mse[[1]])),
                                        c(c4.assess$alpha1$deviance[[1]],c4.assess$alpha1$auc[[1]],sqrt(c4.assess$alpha1$mse[[1]]))))
colnames(performance.lasso) = c("deviance", "AUC", "RMSE")

#net
performance.net = as.data.frame(rbind(c(c1.assess$alpha0.5$deviance[[1]],c1.assess$alpha0.5$auc[[1]],sqrt(c1.assess$alpha0.5$mse[[1]])),
                                      c(c2.assess$alpha0.5$deviance[[1]],c2.assess$alpha0.5$auc[[1]],sqrt(c2.assess$alpha0.3$mse[[1]])),
                                      c(c3.assess$alpha0.8$deviance[[1]],c3.assess$alpha0.8$auc[[1]],sqrt(c3.assess$alpha0.6$mse[[1]])),
                                      c(c4.assess$alpha0.9$deviance[[1]],c4.assess$alpha0.2$auc[[1]],sqrt(c4.assess$alpha0.1$mse[[1]]))))
colnames(performance.net) = c("deviance", "AUC", "RMSE")


#-------------------------------------------------------------------------------
#[7]FEATURE IMPORTANCE
#-------------------------------------------------------------------------------

#lasso regression 
var.lasso = cbindX(setDT(as.data.frame(as.matrix(coef(lasso.fit[[1]], s = "lambda.1se" ))), keep.rownames = TRUE)[],
                   setDT(as.data.frame(as.matrix(coef(lasso.fit[[2]] , s = "lambda.1se" ))), keep.rownames = TRUE)[],
                   setDT(as.data.frame(as.matrix(coef(lasso.fit[[3]] , s = "lambda.1se" ))), keep.rownames = TRUE)[],
                   setDT(as.data.frame(as.matrix(coef(lasso.fit[[4]] , s = "lambda.1se" ))), keep.rownames = TRUE)[])
colnames(var.lasso) = cbind('v1','c1','v2','c2','v3','c3','v4','c4')
var.lasso <-  as.data.frame(sapply(var.lasso, function(x) gsub("`", "", x)))

var.lasso.match = as.data.frame(matrix(nrow = 39, ncol = 4))
for(i in 1:4){
  var.lasso.match[,i] = var.lasso[match(colnames(iv_r), var.lasso[,(i*2)-1]),(i*2)]
}
var.lasso.match= as.data.frame(lapply(var.lasso.match, as.numeric))

#net regression
var.net = cbindX(setDT(as.data.frame(as.matrix(coef(net.fit[[1]], s = "lambda.1se" ))), keep.rownames = TRUE)[],
                 setDT(as.data.frame(as.matrix(coef(net.fit[[2]] , s = "lambda.1se" ))), keep.rownames = TRUE)[],
                 setDT(as.data.frame(as.matrix(coef(net.fit[[3]] , s = "lambda.1se" ))), keep.rownames = TRUE)[],
                 setDT(as.data.frame(as.matrix(coef(net.fit[[4]] , s = "lambda.1se" ))), keep.rownames = TRUE)[])
colnames(var.net) = cbind('v1','c1','v2','c2','v3','c3','v4','c4')
var.net <-  as.data.frame(sapply(var.net, function(x) gsub("`", "", x)))

var.net.match = as.data.frame(matrix(nrow = 39, ncol = 4))
for(i in 1:4){
  var.net.match[,i] = var.net[match(colnames(iv_r), var.net[,(i*2)-1]),(i*2)]
}
var.net.match= as.data.frame(lapply(var.net.match, as.numeric))

#weights
weight = as.data.frame(matrix(nrow = 2, ncol = 4))
weight[1,] = t(performance.lasso[,1])
weight[2,] = t(performance.net[,1])
rownames(weight) = c("lasso","net")
colnames(weight) = c("case1", "case2","case3","case4")

#Create four data frames containing the coefficients of variables 
# we are taking the absolute value since we are only interested in the magnitude
var.c1 = as.data.frame(abs(cbind( var.lasso.match$V1, var.net.match$V1)))
var.c2 = as.data.frame(abs(cbind( var.lasso.match$V2, var.net.match$V2)))
var.c3 = as.data.frame(abs(cbind( var.lasso.match$V3, var.net.match$V3)))
var.c4 = as.data.frame(abs(cbind( var.lasso.match$V4, var.net.match$V4)))

var.c1 = var.c1[1:30,]
var.c3 = var.c3[1:30,]

rownames(var.c1) = colnames(iv_p)
rownames(var.c2) = colnames(iv_r)
rownames(var.c3) = colnames(iv_p)
rownames(var.c4) = colnames(iv_r)

var.c1[is.na(var.c1)] <- 0
var.c2[is.na(var.c2)] <- 0
var.c3[is.na(var.c3)] <- 0
var.c4[is.na(var.c4)] <- 0

var.c1 = normalize( var.c1, method = "range", range = c(0.000001,1), margin = 2, on.constant = "quiet")
#because for lasso and net regression, the value of all coefficients are 0 and upon normalization they acquire a value of 0.5000005
var.c1$V1[var.c1$V1 == 0.5000005] = 0 
var.c1$V2[var.c1$V2 == 0.5000005] = 0 
var.c2 = normalize( var.c2, method = "range", range = c(0.000001,1), margin = 2, on.constant = "quiet")
var.c3 = normalize( var.c3, method = "range", range = c(0.000001,1), margin = 2, on.constant = "quiet")
var.c4 = normalize( var.c4, method = "range", range = c(0.000001,1), margin = 2, on.constant = "quiet")

#calculating the weighted median
var.c1$V3 = apply(var.c1[,c(1,2)],1,function(x){weighted.median(x,w=1/weight$case1[1:2])})
var.c2$V3 = apply(var.c2[,c(1,2)],1,function(x){weighted.median(x,w=1/weight$case2[1:2])})
var.c3$V3 = apply(var.c3[,c(1,2)],1,function(x){weighted.median(x,w=1/weight$case3[1:2])})
var.c4$V3 = apply(var.c4[,c(1,2)],1,function(x){weighted.median(x,w=1/weight$case4[1:2])})

###PLOT FEATURE IMPORTANCE###

#case 1
var.c1 <- tibble::rownames_to_column(var.c1, "row_names")
var.c1 = var.c1 %>% arrange(V3)
var.c1$row_names <- factor(var.c1$row_names, levels = var.c1$row_names)
var.c1 = var.c1[-(1 :(length(var.c1$V3) - 11)),]
var.c1 <- gather(var.c1, methods, coefficients , V1:V3 , factor_key=TRUE)
var.c1 = var.c1 %>% drop_na(coefficients)



p1 = ggplot() + 
  geom_line(data = var.c1, aes(x = coefficients, y = row_names, group = methods, color = methods))+
  geom_point(data = var.c1, aes(x = coefficients, y = row_names, group = methods, color = methods))+
  geom_vline(xintercept=0.5, linetype="dashed", color = "red")+
  theme_hc()+
  labs(x="Importance", y=" ", title = "Structural Proactive Households")+ xlim(c(0,1))+
  theme(plot.title = element_text(hjust=0.5, size=14, face='bold'))+ 
  theme(axis.text.y = element_text(size=13))+
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(), legend.text = element_text(size = 12))+
  scale_color_manual(labels = c("Lasso", "Net-Elastic","Weighted-Median"),
                     values = c("darkolivegreen3", "goldenrod1","black"))

#case 2
var.c2 <- tibble::rownames_to_column(var.c2, "row_names")
var.c2 = var.c2 %>% arrange(V3)
var.c2$row_names <- factor(var.c2$row_names, levels = var.c2$row_names)
var.c2 = var.c2[-(1 :(length(var.c2$V3) - 11)),]
var.c2 <- gather(var.c2, methods, coefficients , V1:V3 , factor_key=TRUE)
var.c2 = var.c2 %>% drop_na(coefficients)

p2= ggplot() + 
  geom_line(data = var.c2, aes(x = coefficients, y = row_names, group = methods, color = methods))+
  geom_point(data = var.c2, aes(x = coefficients, y = row_names, group = methods, color = methods))+
  geom_vline(xintercept=0.5, linetype="dashed", color = "red")+
  theme_hc()+
  labs(x="Importance", y=" ", title = "Structural Reactive Housholds")+ 
  theme(plot.title = element_text(hjust=0.5, size=14, face='bold'))+
  theme(axis.text.y = element_text(size=13))+
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(), legend.text = element_text(size = 12))+
  scale_color_manual(labels = c("Lasso", "Net-Elastic","Weighted-Median"),
                     values = c("darkolivegreen3", "goldenrod1","black"))

#case 3
var.c3 <- tibble::rownames_to_column(var.c3, "row_names")
var.c3 = var.c3 %>% arrange(V3)
var.c3$row_names <- factor(var.c3$row_names, levels = var.c3$row_names)
var.c3 = var.c3[-(1 :(length(var.c3$V3) - 11)),]
var.c3 <- gather(var.c3, methods, coefficients , V1:V3 , factor_key=TRUE)
var.c3 = var.c3 %>% drop_na(coefficients)

p3 = ggplot() + 
  geom_line(data = var.c3, aes(x = coefficients, y = row_names, group = methods, color = methods))+
  geom_point(data = var.c3, aes(x = coefficients, y = row_names, group = methods, color = methods))+
  geom_vline(xintercept=0.5, linetype="dashed", color = "red")+
  theme_hc()+
  labs(x="Importance", y=" ", title = "Non-structural Proactive Households")+ 
  theme(plot.title = element_text(hjust=0.5, size=14, face='bold'))+ 
  theme(axis.text.y = element_text(size=13))+
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(), legend.text = element_text(size = 12))+
  scale_color_manual(labels = c("Lasso", "Net-Elastic","Weighted-Median"),
                     values = c("darkolivegreen3", "goldenrod1","black"))

#case 4
var.c4 <- tibble::rownames_to_column(var.c4, "row_names")
var.c4 = var.c4 %>% arrange(V3)
var.c4$row_names <- factor(var.c4$row_names, levels = var.c4$row_names)
var.c4 = var.c4[-(1 :(length(var.c4$V3) - 11)),]
var.c4 <- gather(var.c4, methods, coefficients , V1:V3 , factor_key=TRUE)
var.c4 = var.c4 %>% drop_na(coefficients)

p4 = ggplot() + 
  geom_line(data = var.c4, aes(x = coefficients, y = row_names, group = methods, color = methods))+
  geom_point(data = var.c4, aes(x = coefficients, y = row_names, group = methods, color = methods))+
  geom_vline(xintercept=0.5, linetype="dashed", color = "red")+
  theme_hc()+
  labs(x="Importance", y=" ", title = "Non-structural Reactive Households")+ 
  theme(plot.title = element_text(hjust=0.5, size=14, face='bold'))+
  theme(axis.text.y = element_text(size=13))+
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(), legend.text = element_text(size = 12))+
  scale_color_manual(labels = c( "Lasso", "Net-Elastic","Weighted-Median"),
                     values = c("darkolivegreen3", "goldenrod1","black")) 

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, labels = c("(A)","(B)","(C)","(D)"), common.legend = TRUE, legend = "bottom")

ggsave("C:/G/TUM Study_Material/GFZ/precautionary drivers/Results/feature_importance_onlyimp.png", height = 40, width = 30, units = 'cm')

