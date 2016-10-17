#install.packages("plspm")
library(plspm)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(plsdepot)


KMData <- read.csv("F:/R/JASISTProject/exportdata/346_346_2.csv")

#KMData_1 <- KMData[!(KMData$CompleteTime <= 281),]
#KMData_2 <- KMData[!(KMData$CompleteTime <= 220),]
KMData_2 = KMData
dim(KMData_2)

attach(KMData_2)
#list(KMData_2)
head(KMData_2)

#How many rows and columns

summary(KMData_2[,1:2])

#---------Expectancy Component (EC)---------
#Knowledge Sharing Self-Efficacy (KSSE)
#Perceived Compatibility, PC

#---------Value Component (VC)---------
#Desire for Online Self-Presentation, PRN
#Perceived Relative Advantage (PRA)

#---------Affective Component (AC)---------
#Positive Anticipated Emotions (PAE)
#Negative Anticipated Emotions (NAE)

#---------DVs-------
#Involvement (INV)
#Engagement (ENG)
#Knowledge Contribution Intention (KCI)
#Continunace Community Commitment (CCC)
#Knowledge Utilizaiton (KU)
#-------DVs,Objective-------
#FrequencyPost
#FrequencyRead


#------------Controls-----
#OnlineYear
#PlatformYear
#Gender
#Age
#Education
#Job
#JobTime


#---------Useless Component---------
#Norm of Reciprocity, NR
#Interpersonal Turst (ITR)

#Create Dummy Variables
Gender.f = factor(Gender)
KMData_2$Gender_dummies = model.matrix(~Gender.f)
Education.f = factor(Education)
KMData_2$Educations_dummies = model.matrix(~Education.f)
Job.f = factor(Job)
KMData_2$Job_dummies = model.matrix(~Job.f)
#Build Model

#Build Manli's Model ----- Begin
#inner model
EC = rep(0,12)
VC = rep(0,12)
AC = rep(0,12)
KCI = rep(0,12)
CCC = rep(0,12)
KU = rep(0,12)
KSSE = rep(0,12)
PC = rep(0,12)
PRN = rep(0,12)
PRA = rep(0,12)
PAE = rep(0,12)
NAE = rep(0,12)


KCI[1] = 1
KCI[2] = 1
KCI[3] = 1
CCC[1] = 1
CCC[2] = 1
CCC[3] = 1
KU[1] = 1
KU[2] = 1
KU[3] = 1
KSSE[1]=1
PC[1]=1
PRN[2]=1
PRA[2]=1
PAE[3]=1
NAE[3]=1
model1_path = rbind(EC,VC,AC,KCI,CCC,KU,KSSE,PC,PRN,PRA,PAE,NAE)
colnames(model1_path) = rownames(model1_path)
innerplot(model1_path,box.size=0.05)
#outer model
model1_blocks = list(c("KSSE1","KSSE2","KSSE3","PC1","PC2","PC3","PC4"),c("PRN1","PRN2","PRN3","PRN4","PRA1","PRA2","PRA3"),c("PAE1","PAE2","PAE3","PAE4","PAE5","PAE6","PAE7","PAE8","PAE9","NAE1","NAE2","NAE3","NAE4","NAE5","NAE6","NAE7","NAE8","NAE9","NAE10","NAE11","NAE12"),c("KCI1","KCI2","KCI3"),c("CCC1","CCC2","CCC3"),c("KU1","KU2","KU3"),c("KSSE1","KSSE2","KSSE3"),c("PC1","PC2","PC3","PC4"),c("PRN1","PRN2","PRN3","PRN4"),c("PRA1","PRA2","PRA3"),c("PAE1","PAE2","PAE3","PAE4","PAE5","PAE6","PAE7","PAE8","PAE9"),c("NAE1","NAE2","NAE3","NAE4","NAE5","NAE6","NAE7","NAE8","NAE9","NAE10","NAE11","NAE12"))

model1_modes = rep("A",12)
#Build Research Model----------End
#apply plspm
model1_pls1 = plspm(KMData_2,model1_path,model1_blocks,modes=model1_modes)
plot(model1_pls1)
model1_pls1$path_coefs
model1_pls1$inner_model
model1_pls1$inner_summary #AVE is reported
model1_pls1$gof
plot(model1_pls1, arr.pos = 0.35)

#crobach's alpha ...
model1_pls1$unidim
plot(model1_pls1, what = "loadings")


#check outer model, loadings and cross-loadings
model1_pls1$outer_model
model1_pls1$crossloadings




#-----------Two-Stage Path Modeling Approach ,AC as moderator-------------
#get the latent variable scores in dataframe format
Scores = as.data.frame(model1_pls1$scores)
#create the interaction terms
Scores$InterAC1 = Scores$EC * Scores$AC
Scores$InterAC2 = Scores$VC * Scores$AC


head(Scores, n =5)

two_path_AC = matrix(c(
  rep(rep(0,14),5),
  rep(c(1,1,1,1,1,0,0,0,0,0,0,0,0,0),3),
  rep(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0),2),
  rep(c(0,1,0,0,0,0,0,0,0,0,0,0,0,0),2),
  rep(c(0,0,1,0,0,0,0,0,0,0,0,0,0,0),2)), nrow =14, ncol =14, byrow = TRUE
)

rownames(two_path_AC) = c("EC","VC","AC","InterAC1","InterAC2","KCI","CCC","KU","KSSE","PC","PRN","PRA","PAE","NAE")
colnames(two_path_AC) = rownames(two_path_AC) 
innerplot(two_path_AC,box.size=0.05)
two_blocks_AC = list("EC","VC","AC","InterAC1","InterAC2","KCI","CCC","KU","KSSE","PC","PRN","PRA","PAE","NAE")
two_modes_AC = rep("A",14)
two_pls_AC = plspm(Scores,two_path_AC,two_blocks_AC,modes=two_modes_AC,boot.val = TRUE, br =300)
round(two_pls_AC$boot$paths,11)
plot(two_pls_AC)
two_pls_AC$inner_model

