# script 2.R

#https://github.com/gregorioCPcG/As-clivagens-do-Brasil-uma-dimens-o-longitudinal/tree/Bases


library(readxl)
# diagnosis####

scree(moreno_clivagem_brasil1991[,26:42])
scree(moreno_clivagem_brasil1997[,26:41])
scree(moreno_clivagem_brasil2006[,26:40])
scree(moreno_clivagem_brasil2014[,26:40])
scree(moreno_clivagem_brasil2018[,26:42])

nfactors(moreno_clivagem_brasil1991[,26:42])
nfactors(moreno_clivagem_brasil1997[,26:41])
nfactors(moreno_clivagem_brasil2006[,26:40])
nfactors(moreno_clivagem_brasil2014[,26:40])
nfactors(moreno_clivagem_brasil2018[,26:42])

#Histogramas####
df1 <-data.frame(pf1.1.3[["scores"]])
df2 <- data.frame(pf2.1.3[["scores"]])
df2$MR1 <- df2$MR1*-1
df3 <- data.frame(pf3.1.3[["scores"]])
df4 <- data.frame(pf4.1.3[["scores"]])
df5 <- data.frame(pf5.1.3[["scores"]])

hist(df1$MR1, main="1991",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)
hist(df2$MR1, main="1997",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)
hist(df3$MR1, main="2006",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)
hist(df4$MR1, main="2014",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)
hist(df5$MR1, main="2018",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)

#FA
print(pf2.1.2$loadings,cutoff = 0.001)
pf2.1.1<- fa(moreno_clivagem_brasil1997[,26:41],nfactors = 3, rotate = "varimax")
print(pf2.1.1$loadings,cutoff = 0.001)
pf3.1.1<- fa(moreno_clivagem_brasil2006[,26:40],nfactors = 3, rotate = "varimax")
print(pf3.1.1$loadings,cutoff = 0.001)
print(pf3.1.2$loadings,cutoff = 0.001)
pf4.1.1<- fa(moreno_clivagem_brasil2014[,26:40],nfactors = 3, rotate = "varimax")
print(pf4.1.1$loadings,cutoff = 0.001)
print(pf4.1.2$loadings,cutoff = 0.001)
pf5.1.1<- fa(moreno_clivagem_brasil2018[,26:42],nfactors = 3, rotate = "varimax")
print(pf5.1.1$loadings,cutoff = 0.001)
print(pf5.1.2$loadings,cutoff = 0.001)

# (+manip) ######
library(sjPlot)
library(lm.beta)
library(dplyr)
library(memisc)
library(haven)
library(gridExtra)
WVS_MERGE_81_2020_BRAZIL <- read_sav("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS Lucas 13_10/WVS MERGE 81-2020 BRAZIL.sav")
df1 <- moreno_clivagem_brasil1991
df2 <- moreno_clivagem_brasil1997
df3 <- moreno_clivagem_brasil2006
df4 <- moreno_clivagem_brasil2014
df5 <- moreno_clivagem_brasil2018
df1$MR1 <- pf1.1.3[["scores"]]
df2$MR1 <- pf2.1.3[["scores"]]
df3$MR1 <- pf3.1.3[["scores"]]
df4$MR1 <- pf4.1.3[["scores"]]
df5$MR1 <- pf5.1.3[["scores"]]
df2$MR1 <- df2$MR1*-1
###
dftemp97 <- WVS_MERGE_81_2020_BRAZIL %>%
  filter(S002VS == 3)
str(dftemp97$X036)
df2$Job <- dftemp97$X036
dftemp2006 <- WVS_MERGE_81_2020_BRAZIL %>%
  filter(S002VS == 5)
str(dftemp2006$X036)
df3$Job <- dftemp2006$X036
df5$Job <- df5$X036E
df1$Job <- df1$X036
#dftemp2014$X036E# 2014 nao tem essa opção
#dftemp2014$X036# 2014 não tem essa opção


#dftemp2014$F025_WVS
library(dplyr)

df2014 <- read_excel("F00007581-WV6_Data_Brazil_Excel_v20201117.xlsx")
df2018 <- read_excel("F00010337-WVS_Wave_7_Brazil_Excel_v2.0.xlsx")
str(df2018$`Q289: Religious denominations - major groups`)
df4$Religion <- df2014$`V144G: Religious denominations - major groups`
df5$Religions <- df2018$`Q289: Religious denominations - major groups`
df1$Religion <- df1$F025old
df2$Religion <- df2$F025old
df3$Religion <- df3$F025old
rm(df2014, df2018)
#
df1$SEX <-df1$X001
df1$AGE <- df1$X003
df1$Education_Level <- df1$X025R
df1$Scale_of_Incomes <- df1$X047_WVS
df1$Settlement_size <- df1$X049
df1$Etnic_Group <- df1$X051
df1$PostMaterialistIndex <- df1$Y001
df1$Interest_in_Politics <- df1$E023
df2$SEX <-df2$X001
df2$AGE <- df2$X003
df2$Education_Level <- df2$X025R
df2$Scale_of_Incomes <- df2$X047_WVS
df2$Settlement_size <- df2$X049
df2$Etnic_Group <- df2$X051
df2$PostMaterialistIndex <- df2$Y001
df2$Interest_in_Politics <- df2$E023
df3$SEX <-df3$X001
df3$AGE <- df3$X003
df3$Education_Level <- df3$X025R
df3$Scale_of_Incomes <- df3$X047_WVS
df3$Settlement_size <- df3$X049
df3$Etnic_Group <- df3$X051
df3$PostMaterialistIndex <- df3$Y001
df3$Interest_in_Politics <- df3$E023
df4$SEX <-df4$X001
df4$AGE <- df4$X003
df4$Education_Level <- df4$X025R
df4$Scale_of_Incomes <- df4$X047_WVS
df4$Settlement_size <- df4$X049
df4$Etnic_Group <- df4$X051
df4$PostMaterialistIndex <- df4$Y001
df4$Interest_in_Politics <- df4$E023
df5$SEX <-df5$X001
df5$AGE <- df5$X003
df5$Education_Level <- df5$X025R
df5$Scale_of_Incomes <- df5$X047_WVS
df5$Settlement_size <- df5$X049
df5$Etnic_Group <- df5$X051
df5$PostMaterialistIndex <- df5$Y001
df5$Interest_in_Politics <- df5$E023

#
df5$Ideology <-df5$E033
df4$Ideology <-df4$E033
df2$Ideology <-df2$E033
df3$Ideology <-df3$E033
df1$Ideology <-df1$E033


df1$Partido <- df1$E179WVS
df2$Partido <- df2$E179WVS
df3$Partido <- df3$E179WVS
df4$Partido <- df4$E179WVS
df5$Partido <- df5$E179WVS


summary(df1)
df1 <-df1[,64:76]
summary(df1)

summary(df2)
df2 <-df2[,63:75]
summary(df2)

summary(df3)
df3 <-df3[,62:74]
summary(df3)

summary(df4)
df4 <-df4[,59:70]
summary(df4)

summary(df5)
df5 <-df5[,64:76]
summary(df5)


prop.table(table(df1$Job))*100
prop.table(table(df2$Job))*100
prop.table(table(df3$Job))*100
prop.table(table(df5$Job))*100
# JOB = 2014 a pergunta nao foi feita.
prop.table(table(df1$Religion))*100
prop.table(table(df2$Religion))*100
prop.table(table(df3$Religion))*100
prop.table(table(df4$Religion))*100# nao foi missificado
prop.table(table(df5$Religion))*100# nao foi missificado

df1$Religion2 <- memisc::recode(as.numeric(df1$Religion), 0 <- c(0),
                                1 <- c(64), 2 <-c(62,52), 3 <-c(12,49))
table(df1$Religion2)
df1$Religion2 <- memisc::recode(as.numeric(df1$Religion), 0 <- c(0),
                                1 <- c(64), 2 <-c(62,52), 3 <-c(12,49))
table(df1$Religion2)

table(df2$Religion)
df2$Religion2 <- memisc::recode(as.numeric(df2$Religion), 0 <- c(0),
                                1 <- c(64), 2 <-c(62), 3 <-c(42,83))
table(df2$Religion2)
table(df3$Religion)
df3$Religion2 <- memisc::recode(as.numeric(df3$Religion), 0 <- c(0),
                                1 <- c(64), 2 <-c(25), 3 <-c(12,42,52,53,54,62,73))
table(df3$Religion2)


table(df4$Religion)
df4$Religion2 <- memisc::recode(as.numeric(df4$Religion), 0 <- c(0),
                                1 <- c(1), 2 <-c(8), 3 <-c(-2,-1,2,7,9))
table(df4$Religion2)

table(df5$Religion)
df5$Religion2 <- memisc::recode(as.numeric(df5$Religion), 0 <- c(0),
                                1 <- c(1), 2 <-c(2), 3 <-c(-2,-1,4,8,9))
table(df5$Religion2)

tapply(df1$MR1,df1$Religion2, mean, na.rm = TRUE)
tapply(df2$MR1,df2$Religion2, mean, na.rm = TRUE)
tapply(df3$MR1,df3$Religion2, mean, na.rm = TRUE)
tapply(df4$MR1,df4$Religion2, mean, na.rm = TRUE)
tapply(df5$MR1,df5$Religion2, mean, na.rm = TRUE)

df1$Religion <- as.factor(df1$Religion2)
levels(df1$Religion)
levels(df1$Religion) <- c('Sem Religião','Católico','Evangélico','Outros')

df2$Religion <- as.factor(df2$Religion2)
levels(df2$Religion)
levels(df2$Religion) <- c('Sem Religião','Católico','Evangélico','Outros')

df3$Religion <- as.factor(df3$Religion2)
levels(df3$Religion)
levels(df3$Religion) <- c('Sem Religião','Católico','Evangélico','Outros')

df4$Religion <- as.factor(df4$Religion2)
levels(df4$Religion)
levels(df4$Religion) <- c('Sem Religião','Católico','Evangélico','Outros')

df5$Religion <- as.factor(df5$Religion2)
levels(df5$Religion)
levels(df5$Religion) <- c('Sem Religião','Católico','Evangélico','Outros')

tapply(df1$MR1,df1$Religion, mean, na.rm = TRUE)
tapply(df2$MR1,df2$Religion, mean, na.rm = TRUE)
tapply(df3$MR1,df3$Religion, mean, na.rm = TRUE)
tapply(df4$MR1,df4$Religion, mean, na.rm = TRUE)
tapply(df5$MR1,df5$Religion, mean, na.rm = TRUE)

table(df1$Job)
df1$Job2 <- memisc::recode(as.numeric(df1$Job), 1 <- c(21),
                           2 <- c(32), 4 <-c(34), 5 <-c(42), 6 <- c(23),7<-c(13,16,22,31,41,51,33))
table(df1$Job2)
table(df2$Job)
df2$Job2 <- memisc::recode(as.numeric(df2$Job), 1 <- c(21),
                           2 <- c(32), 4 <-c(34), 5 <-c(42), 6 <- c(23),7<-c(13,16,31,41,51,61,33))
table(df2$Job2)

table(df3$Job)
df3$Job2 <- memisc::recode(as.numeric(df3$Job), 1 <- c(21),
                           2 <- c(32), 4 <-c(34), 5 <-c(42), 6 <- c(23),7<-c(13,16,31,41,51,61,25,33))
table(df3$Job2)

table(df5$Job)
df5$Job2 <- memisc::recode(as.numeric(df5$Job), 1 <- c(1),
                           2 <- c(6), 4 <-c(8), 5 <-c(9), 6 <- c(2),7<-c(0,3,4,5,10,7))
table(df5$Job2)

df1 <- df1 %>%
  mutate(Profiss.Liberal = case_when(Job2 == 1 ~ 1,
                                     TRUE ~0)) %>%
  mutate(Braçal.Especializ = case_when(Job2 == 2 ~ 1,
                                       TRUE ~0))%>%
  mutate(Braçal = case_when(Job2 == 4 ~ 1,
                            TRUE ~0))%>%
  mutate(Trab.Rural = case_when(Job2 == 5 ~ 1,
                                TRUE ~0))%>%
  mutate(Gerente = case_when(Job == 6 ~ 1,
                             TRUE ~0))
df2 <- df2 %>%
  mutate(Profiss.Liberal = case_when(Job2 == 1 ~ 1,
                                     TRUE ~0)) %>%
  mutate(Braçal.Especializ = case_when(Job2 == 2 ~ 1,
                                       TRUE ~0))%>%
  mutate(Braçal = case_when(Job2 == 4 ~ 1,
                            TRUE ~0))%>%
  mutate(Trab.Rural = case_when(Job2 == 5 ~ 1,
                                TRUE ~0))%>%
  mutate(Gerente = case_when(Job == 6 ~ 1,
                             TRUE ~0))

df3 <- df3 %>%
  mutate(Profiss.Liberal = case_when(Job2 == 1 ~ 1,
                                     TRUE ~0)) %>%
  mutate(Braçal.Especializ = case_when(Job2 == 2 ~ 1,
                                       TRUE ~0))%>%
  mutate(Braçal = case_when(Job2 == 4 ~ 1,
                            TRUE ~0))%>%
  mutate(Trab.Rural = case_when(Job2 == 5 ~ 1,
                                TRUE ~0))%>%
  mutate(Gerente = case_when(Job == 6 ~ 1,
                             TRUE ~0))

df5 <- df5 %>%
  mutate(Profiss.Liberal = case_when(Job2 == 1 ~ 1,
                                     TRUE ~0)) %>%
  mutate(Braçal.Especializ = case_when(Job2 == 2 ~ 1,
                                       TRUE ~0))%>%
  mutate(Braçal = case_when(Job2 == 4 ~ 1,
                            TRUE ~0))%>%
  mutate(Trab.Rural = case_when(Job2 == 5 ~ 1,
                                TRUE ~0))%>%
  mutate(Gerente = case_when(Job == 6 ~ 1,
                             TRUE ~0))

df1 <- df1 %>%
  mutate(Sem_Religiao = case_when(Religion2 == 0 ~ 1,
                                  TRUE ~0)) %>%
  mutate(Catolico = case_when(Religion2 == 1 ~ 1,
                              TRUE ~0))

df2 <- df2 %>%
  mutate(Sem_Religiao = case_when(Religion2 == 0 ~ 1,
                                  TRUE ~0)) %>%
  mutate(Catolico = case_when(Religion2 == 1 ~ 1,
                              TRUE ~0))
df3 <- df3 %>%
  mutate(Sem_Religiao = case_when(Religion2 == 0 ~ 1,
                                  TRUE ~0)) %>%
  mutate(Catolico = case_when(Religion2 == 1 ~ 1,
                              TRUE ~0))
df4 <- df4 %>%
  mutate(Sem_Religiao = case_when(Religion2 == 0 ~ 1,
                                  TRUE ~0)) %>%
  mutate(Catolico = case_when(Religion2 == 1 ~ 1,
                              TRUE ~0))
df5 <- df5 %>%
  mutate(Sem_Religiao = case_when(Religion2 == 0 ~ 1,
                                  TRUE ~0)) %>%
  mutate(Catolico = case_when(Religion2 == 1 ~ 1,
                              TRUE ~0))
tapply(df1$MR1,df1$Job, mean, na.rm = TRUE)
tapply(df2$MR1,df2$Job, mean, na.rm = TRUE)
tapply(df3$MR1,df3$Job, mean, na.rm = TRUE)
tapply(df5$MR1,df5$Job, mean, na.rm = TRUE)




str(df2$Partido)
table(df5$Partido)
print(df2$Partido)#76002 é o PT


df1 <- df1 %>%
  mutate(PT = case_when(Partido == 76002 ~ 1,
                        TRUE ~0))
df1$PT <- as.factor(df1$PT)  
levels(df1$PT )
levels(df1$PT ) <- c('n', 'PT')
levels(df1$PT )

df2 <- df2 %>%
  mutate(PT = case_when(Partido == 76002 ~ 1,
                        TRUE ~0))
df2$PT <- as.factor(df2$PT)  
levels(df2$PT )
levels(df2$PT ) <- c('n', 'PT')
levels(df2$PT )


df3 <- df3 %>%
  mutate(PT = case_when(Partido == 76002 ~ 1,
                        TRUE ~0))
df3$PT <- as.factor(df3$PT)  
levels(df3$PT )
levels(df3$PT ) <- c('n', 'PT')
levels(df3$PT )

df5 <- df5 %>%
  mutate(PT = case_when(Partido == 76002 ~ 1,
                        TRUE ~0))
df5$PT <- as.factor(df5$PT)  
levels(df5$PT )
levels(df5$PT ) <- c('n', 'PT')
levels(df5$PT )

df4 <- df4 %>%
  mutate(PT = case_when(Partido == 76002 ~ 1,
                        TRUE ~0))
df4$PT <- as.factor(df4$PT)  
levels(df4$PT )
levels(df4$PT ) <- c('n', 'PT')
levels(df4$PT )
#reg
df1$Religion <-relevel(df1$Religion, "Sem Religião")
df2$Religion <-relevel(df2$Religion, "Sem Religião")
df3$Religion <-relevel(df3$Religion, "Sem Religião")
df4$Religion<-relevel(df4$Religion, "Sem Religião")
df5$Religion <-relevel(df5$Religion, "Sem Religião")

# regressoes  #### 
modelo11 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
               data=df1)
summary(modelo11)#beta
modelo11x <- lm.beta(modelo11)
#tab_model(modelo11x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo12 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
               data=df2)
summary(modelo12)#beta
modelo12x <- lm.beta(modelo12)
#tab_model(modelo12x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo13 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,data=df3)
summary(modelo13)#beta
modelo13x <- lm.beta(modelo13)
#tab_model(modelo13x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo14 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,data=df4)
summary(modelo14)#beta
modelo14x <- lm.beta(modelo14)
#tab_model(modelo14x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo15 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,data=df5)
summary(modelo15)#beta
modelo15x <- lm.beta(modelo15)

tab_model(modelo11,modelo12,modelo13,modelo14,modelo15,
          show.ci = F, auto.label = T, show.se = T,collapse.se = T,
          wrap.labels = 60, p.style = "stars")
tab_model(modelo11x,modelo12x,modelo13x,modelo14x,modelo15x,
          show.ci = F, auto.label = T, show.se = T,collapse.se = T,
          wrap.labels = 60, p.style = "stars")

plot_models(modelo11, modelo12,modelo11, modelo12, modelo13, modelo14, modelo15, rm.terms=c("AGE","Settlement_size","SEX",
                                           "Interest_in_Politics",
                                           "Scale_of_Incomes","Etnic_Group"))
a<- plot_models(modelo11, modelo12, modelo13, modelo14, modelo15,
            legend.title = "Onda",m.labels = c("1991", "1997", "2006","2014",
                                               "2018"),rm.terms=c("AGE","Settlement_size","SEX",
                                                                  "Interest_in_Politics",
                                                                  "Scale_of_Incomes","Etnic_Group"),
            axis.labels=c("Direita","PT","Pós-Materialismo","Education Level"),show.values = FALSE, 
            show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
            p.threshold = c(0.05, 0.01, 0.001), 
            vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=F)+
  theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))

a + labs(subtitle = "Results of full regression models",
         caption = "The graph takes into consideration the complete regression model
         Results from Age, Income Scale, Political Interest,
         Gender, and Etnics are omitted from the graph;
         the effects of these variables can be viewed in Appendix A.",
         y="Estimates (Fundamentalism)") #Edit

# another regression with religion ####
modelo11b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,
               data=df1)
summary(modelo11b)#beta
modelo11bx <- lm.beta(modelo11b)
#tab_model(modelo11x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo12b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,
               data=df2)
summary(modelo12b)#beta
modelo12bx <- lm.beta(modelo12b)
#tab_model(modelo12x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo13b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,data=df3)
summary(modelo13b)#beta
modelo13bx <- lm.beta(modelo13b)
#tab_model(modelo13x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo14b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,data=df4)
summary(modelo14b)#beta
modelo14bx <- lm.beta(modelo14b)
#tab_model(modelo14x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo15b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,data=df5)
summary(modelo15b)#beta
modelo15bx <- lm.beta(modelo15b)

tab_model(modelo11b,modelo12b,modelo13b,modelo14b,modelo15b,
          show.ci = F, auto.label = T, show.se = T,collapse.se = T,
          wrap.labels = 60, p.style = "stars")
tab_model(modelo11bx,modelo12bx,modelo13bx,modelo14bx,modelo15bx,
          show.ci = F, auto.label = T, show.se = T,collapse.se = T,
          wrap.labels = 60, p.style = "stars")





## diagnosis
library(lmtest)
library(car)
library(sandwich)
library(psych)
library(lm.beta)

vif(modelo11)
vif(modelo12)
vif(modelo13)
vif(modelo14)
vif(modelo15)



residualPlots(modelo11, fitted=TRUE, typ="rstudent", test=TRUE)
residualPlots(modelo12, fitted=TRUE, typ="rstudent", test=F)
residualPlots(modelo13, fitted=TRUE, typ="rstudent", test=TRUE)
residualPlots(modelo14, fitted=TRUE, typ="rstudent", test=TRUE)
residualPlots(modelo15, fitted=TRUE, typ="rstudent", test=TRUE)



#appendix c models with PSDB



table(df1$Partido, useNA = "always")

df1 <- df1 %>%
  mutate(moreparties = case_when(Partido == 76002 ~ 1,Partido ==76003 ~ 2,Partido ==-50 ~ 3
                                 TRUE ~0))
df1$moreparties  <- as.factor(df1$moreparties )  
levels(df1$moreparties  )
levels(df1$moreparties  ) <- c('Others/None', 'PT','PSDB')
levels(df1$moreparties  )
df1$moreparties <-relevel(df1$moreparties, "Others/None")



table(df2$Partido, useNA = "always")

df2 <- df2 %>%
  mutate(moreparties = case_when(Partido == 76002 ~ 1,Partido ==76003 ~ 2,
                                 TRUE ~0))
df2$moreparties  <- as.factor(df2$moreparties )  
levels(df2$moreparties  )
levels(df2$moreparties  ) <- c('Others/None', 'PT','PSDB')
levels(df2$moreparties  )
df2$moreparties <-relevel(df2$moreparties, "Others/None")


table(df3$Partido, useNA = "always")

df3 <- df3 %>%
  mutate(moreparties = case_when(Partido == 76002 ~ 1,Partido ==76003 ~ 2,
                                 TRUE ~0))
df3$moreparties  <- as.factor(df3$moreparties )  
levels(df3$moreparties  )
levels(df3$moreparties  ) <- c('Others/None', 'PT','PSDB')
levels(df3$moreparties  )
df3$moreparties <-relevel(df3$moreparties, "Others/None")


table(df4$Partido, useNA = "always")

df4 <- df4 %>%
  mutate(moreparties = case_when(Partido == 76002 ~ 1,Partido ==76003 ~ 2,
                                 TRUE ~0))
df4$moreparties  <- as.factor(df4$moreparties )  
levels(df4$moreparties  )
levels(df4$moreparties  ) <- c('Others/None', 'PT','PSDB')
levels(df4$moreparties  )
df4$moreparties <-relevel(df4$moreparties, "Others/None")


table(df5$Partido, useNA = "always")

df5 <- df5 %>%
  mutate(moreparties = case_when(Partido == 76002 ~ 1,Partido ==76003 ~ 2,
                                 TRUE ~0))
df5$moreparties  <- as.factor(df5$moreparties )  
levels(df5$moreparties  )
levels(df5$moreparties  ) <- c('Others/None', 'PT','PSDB')
levels(df5$moreparties  )
df5$moreparties <-relevel(df5$moreparties, "Others/None")


modelo11ff <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+moreparties+Ideology,
                 data=df1)

modelo12ff <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+moreparties+Ideology,
                 data=df2)

modelo13ff <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+moreparties+Ideology,data=df3)

modelo14ff <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+moreparties+Ideology,data=df4)

modelo15ff <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+moreparties+Ideology,data=df5)


#results

tab_model(modelo11ff,modelo12ff, modelo13ff, modelo14ff, modelo15ff)


library(marginaleffects)
a<-plot_cap(modelo11ff, condition=c("moreparties"),conf_level = .9)
a <- a+labs(y="Fundamentalism",title="Wave 2 - 1991")+theme_minimal()
a
b<-plot_cap(modelo12ff, condition=c("moreparties"),conf_level = .9)
b <- b+labs(y="Fundamentalism",title="Wave 3 - 1997")+theme_minimal()
b
c<-plot_cap(modelo13ff, condition=c("moreparties"),conf_level = .9)
c <- c+labs(y="Fundamentalism",title="Wave 5 - 2006")+theme_minimal()
c
d<-plot_cap(modelo14ff, condition=c("moreparties"),conf_level = .9)
d <- d+labs(y="Fundamentalism",title="Wave 6 - 2014")+theme_minimal()
d
e<-plot_cap(modelo15ff, condition=c("moreparties"),conf_level = .9)
e <- e+labs(y="Fundamentalism",title="Wave 7 - 2018")+theme_minimal()
e
library(gridExtra)
grid.arrange(a,b,c,d,e)


#histograms
df1 <-data.frame(pf1.1.3[["scores"]])
df2 <- data.frame(pf2.1.3[["scores"]])
df2$MR1 <- df2$MR1*-1
df3 <- data.frame(pf3.1.3[["scores"]])
df4 <- data.frame(pf4.1.3[["scores"]])
df5 <- data.frame(pf5.1.3[["scores"]])

hist(df1$MR1, main="1991",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)
hist(df2$MR1, main="1997",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)
hist(df3$MR1, main="2006",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)
hist(df4$MR1, main="2014",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)
hist(df5$MR1, main="2018",xlab="Fundamentalismo",ylab="", col="darkmagenta",breaks=80)



library(ggdist)
library(tidyquant)
library(tidyverse)

df1$ano <- "1991"
df2$ano <- "1997"
df3$ano <- "2006"
df4$ano <- "2014"
df5$ano <- "2018"

dfzao <- full_join(df1,df2)
dfzao <- full_join(df3,dfzao)
dfzao <- full_join(df4, dfzao)
dfzao <- full_join(df5, dfzao)
table(dfzao$ano)
#rodar todos os scripts antes
dfzao %>%
  #dplyr::filter(cyl %in% c(4,6,8)) %>%
  ggplot(aes(x=ano, y=MR1, fill=ano))+ggdist::stat_halfeye(adjust=0.5,
                                                           justification = -.2,
                                                           .width=0,
                                                           point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+
  theme_tq() +
  labs(tilte= "",
       subtitle = "",
       x = "",
       y= "Liberalismo/Fundamentalismo", caption="valores positivos indicam maior fundamentalismo",
       fill= "Ano",
  ) + coord_flip()


# IRT
library(tidyverse)
library(labelled)
df1 <- remove_labels(df1)
df2 <- remove_labels(df2)
df3 <- remove_labels(df3)
df4 <- remove_labels(df4)
df5 <- remove_labels(df5)

df11 <- moreno_clivagem_brasil1991[,26:42]
df12 <- moreno_clivagem_brasil1997[,26:41]
df13 <- moreno_clivagem_brasil2006[,26:40]
df14 <- moreno_clivagem_brasil2014[,26:40]
df15 <- moreno_clivagem_brasil2018[,26:42]

library(mirt)
mirtCluster()
library(psych)

TRI91 <- mirt(df11,1)
summary(TRI91, suppress = .31)
TRI91.2 <- mirt(df11,2)
summary(TRI91.2, suppress = .31)
M2(TRI91,na.rm=T)
M2(TRI91.2,na.rm=T)
anova(TRI91,TRI91.2)
TRI97 <- mirt(df12,1)
summary(TRI97, suppress = .31)
TRI97.2 <- mirt(df12,2)
summary(TRI97.2, suppress = .31)
M2(TRI97,na.rm=T)
M2(TRI97.2,na.rm=T)
anova(TRI91,TRI91.2)
TRI2006 <- mirt(df13,1)
summary(TRI2006, suppress = .31)
TRI2006.2 <- mirt(df13,2)# passou de 500 iterações, nao utilizar
summary(TRI2006.2, suppress = .31)
M2(TRI2006,na.rm=T)
#M2(TRI2006.2,na.rm=T)
TRI2014 <- mirt(df14,1)
summary(TRI2014, suppress = .31)
TRI2014.2 <- mirt(df14,2)
summary(TRI2014.2, suppress = .31)
M2(TRI2014,na.rm=T)
M2(TRI2014.2,na.rm=T)
anova(TRI2014, TRI2014.2)
TRI2018 <- mirt(df15,1)
summary(TRI2018, suppress = .31)
TRI2018.2 <- mirt(df15,2)
summary(TRI2018.2, suppress = .31)
M2(TRI2018,na.rm=T)
M2(TRI2018.2,na.rm=T)
a1 <- fscores(TRI91)
a1#pra verificar
df1$mirt <- a1[,1]
summary(df1) 
hist(df1$mirt)#praverificar
a2 <- fscores(TRI97)
df2$mirt <- a2[,1]
hist(df2$mirt)#praverificar
df2$mirt <- df2$mirt*-1#arrumar pra fundamentalismo ficar positivo
hist(df2$mirt)#praverificar
a3 <- fscores(TRI2006)
df3$mirt <- a3[,1]
hist(df3$mirt)#praverificar
df3$mirt <- df3$mirt*-1#arrumar pra fundamentalismo ficar positivo
hist(df3$mirt)#praverificar
a4 <- fscores(TRI2014)
df4$mirt <- a4[,1]
hist(df4$mirt)#praverificar
df4$mirt <- df4$mirt*-1#arrumar pra fundamentalismo ficar positivo
a5 <- fscores(TRI2018)
df5$mirt <- a5[,1]
hist(df5$mirt)#praverificar

rm(df11,df12,df13,df14,df15)

library(sjPlot)

mirtModel1 <- lm(mirt ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
                 data=df1)

mirtModel2 <- lm(mirt ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
                 data=df2)

mirtModel3 <- lm(mirt ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
                 data=df3)

mirtModel4 <- lm(mirt ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
                 data=df4)

mirtModel5 <- lm(mirt ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
                 data=df5)

tab_model(mirtModel1,mirtModel2,mirtModel3,mirtModel4,mirtModel5,
          show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

# confirmatory

df11 <- moreno_clivagem_brasil1991[,26:42]
df12 <- moreno_clivagem_brasil1997[,26:41]
df13 <- moreno_clivagem_brasil2006[,26:40]
df14 <- moreno_clivagem_brasil2014[,26:40]
df15 <- moreno_clivagem_brasil2018[,26:42]
library(lavaan)
library(lavaanPlot)

# 

model<-'
fundamentalista1991=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit1<-cfa(model,data=df11)
print(fitMeasures(fit1, c("gfi")))#>0.95
summary(fit1,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = 
                                               "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))


model<-'
fundamentalista1997=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit2<-cfa(model,data=df12)
print(fitMeasures(fit2, c("gfi")))#>0.95
summary(fit2,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit2, node_options = list(shape = "box", fontname = 
                                               "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))

model<-'
fundamentalista2006=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit3<-cfa(model,data=df13)
print(fitMeasures(fit3, c("gfi")))#>0.95
#summary(fit3,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit3, node_options = list(shape = "box", fontname = 
                                               "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))

model<-'
fundamentalista2014=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit4<-cfa(model,data=df14)
print(fitMeasures(fit4, c("gfi")))#>0.95
#summary(fit4,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit4, node_options = list(shape = "box", fontname = 
                                               "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))
model<-'
fundamentalista2018=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit5<-cfa(model,data=df15)
print(fitMeasures(fit5, c("gfi")))#>0.95
#summary(fit5,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit5, node_options = list(shape = "box", fontname = 
                                               "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))


#more ####
#91
df1[,18:34] <- moreno_clivagem_brasil1991[,26:42]
df1$E018 <- moreno_clivagem_brasil1991$E018
df1$Education_Level <- as.numeric(df1$Education_Level)
df1$SEX <- as.numeric(df1$SEX)
model<-'
fundamentalista1991=~F028+F034+F063+F118+F120+F121+G006
fundamentalista1991~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology
'
fit11<-cfa(model,data=df1)
print(fitMeasures(fit11, c("gfi")))#>0.95
summary(fit11,fit.measures=TRUE,standardized=TRUE)#comparar com o original
lavaanPlot(model = fit11, node_options = list(shape = "box", fontname = 
                                                "Helvetica"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = c("regression"))


#97
df2[,26:41] <- moreno_clivagem_brasil1997[,26:41]
#df1$E018 <- moreno_clivagem_brasil1991$E018
df2$Education_Level <- as.numeric(df2$Education_Level)
df2$SEX <- as.numeric(df2$SEX)
model<-'
fundamentalista1997=~F028+F034+F063+F118+F120+F121+G006
fundamentalista1997~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology
'
fit12<-cfa(model,data=df2)
print(fitMeasures(fit12, c("gfi")))#>0.95
summary(fit12,fit.measures=TRUE,standardized=TRUE)#comparar com o original
lavaanPlot(model = fit12, node_options = list(shape = "box", fontname = 
                                                "Helvetica"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = c("regression"))

#2006
df3[,26:46] <- moreno_clivagem_brasil2006[,26:40]
#df1$E018 <- moreno_clivagem_brasil1991$E018
df3$Education_Level <- as.numeric(df3$Education_Level)
df3$SEX <- as.numeric(df3$SEX)
model<-'
fundamentalista2006=~F028+F034+F063+F118+F120+F121+G006
fundamentalista2006~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology
'
fit13<-cfa(model,data=df3)
print(fitMeasures(fit13, c("gfi")))#>0.95
summary(fit13,fit.measures=TRUE,standardized=TRUE)#comparar com o original
lavaanPlot(model = fit13, node_options = list(shape = "box", fontname = 
                                                "Helvetica"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = c("regression"))



#
#2014
df4[,18:32] <- moreno_clivagem_brasil2014[,26:40]
#df1$E018 <- moreno_clivagem_brasil1991$E018
df4$Education_Level <- as.numeric(df4$Education_Level)
df4$SEX <- as.numeric(df4$SEX)
model<-'
fundamentalista2014=~F028+F034+F063+F118+F120+F121+G006
fundamentalista2014~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology
'
fit14<-cfa(model,data=df4)
print(fitMeasures(fit14, c("gfi")))#>0.95
summary(fit14,fit.measures=TRUE,standardized=TRUE)#comparar com o original
lavaanPlot(model = fit14, node_options = list(shape = "box", fontname = 
                                                "Helvetica"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = c("regression"))

#
#2018
df5[,27:43] <- moreno_clivagem_brasil2018[,26:42]
#df1$E018 <- moreno_clivagem_brasil1991$E018
df5$Education_Level <- as.numeric(df5$Education_Level)
df5$SEX <- as.numeric(df5$SEX)
model<-'
fundamentalista2018=~F028+F034+F063+F118+F120+F121+G006
fundamentalista2018~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology
'
fit15<-cfa(model,data=df5)
print(fitMeasures(fit15, c("gfi")))#>0.95
summary(fit15,fit.measures=TRUE,standardized=TRUE)#comparar com o original
lavaanPlot(model = fit15, node_options = list(shape = "box", fontname = 
                                                "Helvetica"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = c("regression"))


