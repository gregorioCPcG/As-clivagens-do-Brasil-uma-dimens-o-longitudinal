# script 2.R
# baixar todos os bancos antes na mesma pasta
#https://github.com/gregorioCPcG/As-clivagens-do-Brasil-uma-dimens-o-longitudinal/tree/Bases
#Rodar script1.RMD antes!


# diagnósticos####

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
# mais fatoriais
#mais fatoriais
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
WVS_MERGE_81_2020_BRAZIL <- read_sav("/WVS MERGE 81-2020 BRAZIL.sav")
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


#
dftemp2014$F025_WVS
library(readxl)
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
summary(modelo11)#valores estimados
modelo11x <- lm.beta(modelo11)
#tab_model(modelo11x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo12 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
               data=df2)
summary(modelo12)#valores estimados
modelo12x <- lm.beta(modelo12)
#tab_model(modelo12x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo13 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,data=df3)
summary(modelo13)#valores estimados
modelo13x <- lm.beta(modelo13)
#tab_model(modelo13x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo14 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,data=df4)
summary(modelo14)#valores estimados
modelo14x <- lm.beta(modelo14)
#tab_model(modelo14x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo15 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,data=df5)
summary(modelo15)#valores estimados
modelo15x <- lm.beta(modelo15)

tab_model(modelo11,modelo12,modelo13,modelo14,modelo15,
          show.ci = F, auto.label = T, show.se = T,collapse.se = T,
          wrap.labels = 60, p.style = "stars")
tab_model(modelo11x,modelo12x,modelo13x,modelo14x,modelo15x,
          show.ci = F, auto.label = T, show.se = T,collapse.se = T,
          wrap.labels = 60, p.style = "stars")

plot_models(modelo11, modelo12, rm.terms=c("AGE","Settlement_size","Education_Level",
                                           "Interest_in_Politics",
                                           "Scale_of_Incomes","Etnic_Group"))
plot_models(modelo11, modelo12, modelo13, modelo14, modelo15,
            legend.title = "Onda",m.labels = c("1991", "1997", "2006","2014",
                                               "2018"),rm.terms=c("AGE","Settlement_size","Education_Level",
                                                                  "Interest_in_Politics",
                                                                  "Scale_of_Incomes","Etnic_Group"),
            axis.labels=c("Direita","PT","Pós-Materialismo","Mulher"),show.values = FALSE, 
            show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
            p.threshold = c(0.05, 0.01, 0.001), 
            vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=F)+
  theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))


# another regression with religion ####
modelo11b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,
               data=df1)
summary(modelo11b)#valores estimados
modelo11bx <- lm.beta(modelo11b)
#tab_model(modelo11x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo12b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,
               data=df2)
summary(modelo12b)#valores estimados
modelo12bx <- lm.beta(modelo12b)
#tab_model(modelo12x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo13b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,data=df3)
summary(modelo13b)#valores estimados
modelo13bx <- lm.beta(modelo13b)
#tab_model(modelo13x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo14b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,data=df4)
summary(modelo14b)#valores estimados
modelo14bx <- lm.beta(modelo14b)
#tab_model(modelo14x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo15b <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology+Religion,data=df5)
summary(modelo15b)#valores estimados
modelo15bx <- lm.beta(modelo15b)

tab_model(modelo11b,modelo12b,modelo13b,modelo14b,modelo15b,
          show.ci = F, auto.label = T, show.se = T,collapse.se = T,
          wrap.labels = 60, p.style = "stars")
tab_model(modelo11bx,modelo12bx,modelo13bx,modelo14bx,modelo15bx,
          show.ci = F, auto.label = T, show.se = T,collapse.se = T,
          wrap.labels = 60, p.style = "stars")
#tabelas de médias do score#####
# GENDER
#1991
tapply(df1$MR1,df1$SEX, mean, na.rm = TRUE)
yy1 <- c(-0.07, 0.05)# consertar abaixo tambem
xx1 <- c("MASC","FEM")
grafxx1 <- data.frame(xx1,yy1)

SEX91 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.25,0.25)) +
  ggtitle("                    1991") +
  xlab("Gênero") + 
  ylab("Fundamentalista")

SEX91

#1997

tapply(df2$MR1,df2$SEX, mean, na.rm = TRUE)
yy1 <- c(-0.01,-0.04)# 
grafxx1 <- data.frame(xx1,yy1)

SEX97 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.25,0.25)) +
  ggtitle("                    1997") +
  xlab("Gênero") + 
  ylab("Fundamentalista")

SEX97

#2006
tapply(df3$MR1,df3$SEX, mean, na.rm = TRUE)
yy1 <- c(-0.03,0.05)# 
grafxx1 <- data.frame(xx1,yy1)

SEX2006 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.25,0.25)) +
  ggtitle("                    2006") +
  xlab("Gênero") + 
  ylab("Fundamentalista")

SEX2006

#2014

tapply(df4$MR1,df4$SEX, mean, na.rm = TRUE)
yy1 <- c(-0.1,0.07)# 
grafxx1 <- data.frame(xx1,yy1)

SEX14 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.25,0.25)) +
  ggtitle("                    2014") +
  xlab("Gênero") + 
  ylab("Fundamentalista")

SEX14


# 2018

tapply(df5$MR1,df5$SEX, mean, na.rm = TRUE)
yy1 <- c(-0.009,0.06)# 
grafxx1 <- data.frame(xx1,yy1)

SEX18 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.25,0.25)) +
  ggtitle("                    2018") +
  xlab("Gênero") + 
  ylab("Fundamentalista")

SEX18
grid.arrange(SEX91, SEX97, SEX2006, SEX14, SEX18, nrow=2)#exemplos

# post materialist index
#1991
xx1 <- c("0","1","2","3","4","5")
tapply(df1$MR1,df1$PostMaterialistIndex, mean, na.rm = TRUE)

yy1 <- c(0.25, 0.22,0.06,-.18,-.46,-.62)# consertar abaixo tambem

grafxx1 <- data.frame(xx1,yy1)

pm91 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1,1)) +
  ggtitle("                    1991") +
  xlab("Post-Materialist Index") + 
  ylab("Fundamentalista")

pm91

#97
tapply(df2$MR1,df2$PostMaterialistIndex, mean, na.rm = TRUE)

yy1 <- c(0.16, 0.12,0.02,-.04,-.37,-.79)# consertar abaixo tambem

grafxx1 <- data.frame(xx1,yy1)

pm97 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1,1)) +
  ggtitle("                    1997") +
  xlab("Post-Materialist Index") + 
  ylab("Fundamentalista")

pm97

#2006
tapply(df3$MR1,df3$PostMaterialistIndex, mean, na.rm = TRUE)

yy1 <- c(0.17, 0.08,0.01,-.02,-.24,0.02)# consertar abaixo tambem

grafxx1 <- data.frame(xx1,yy1)

pm2006 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1,1)) +
  ggtitle("                    2006") +
  xlab("Post-Materialist Index") + 
  ylab("Fundamentalista")

pm2006
#2014
tapply(df4$MR1,df4$PostMaterialistIndex, mean, na.rm = TRUE)

yy1 <- c(0.22, 0.007,0.02,0.03,-.22,-1.72)# consertar abaixo tambem

grafxx1 <- data.frame(xx1,yy1)

pm2014 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1.73,1)) +
  ggtitle("                    2014") +
  xlab("Post-Materialist Index") + 
  ylab("Fundamentalista")
pm2014

#2018
tapply(df5$MR1,df5$PostMaterialistIndex, mean, na.rm = TRUE)

yy1 <- c(0.17, 0.15,0.17,0.01,-.34,-.82)# consertar abaixo tambem

grafxx1 <- data.frame(xx1,yy1)

pm2018 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1,1)) +
  ggtitle("                    2018") +
  xlab("Post-Materialist Index") + 
  ylab("Fundamentalista")

pm2018

grid.arrange(pm91, pm97, pm2006, pm2014, pm2018, ncol=2)
# além das já presentes em script1.RMD foram feitas outras abaixo.





## diagnosis das regressao
library(lmtest)
library(car)
library(sandwich)
library(psych)
library(lm.beta)

vif(modelo11)#colineari... limiar 10
vif(modelo12)#colineari... limiar 10
vif(modelo13)#colineari... limiar 10
vif(modelo14)#colineari... limiar 10
vif(modelo15)#colineari... limiar 10



residualPlots(modelo11, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo12, fitted=TRUE, typ="rstudent", test=F)# nao pode ser linear...- resultados bãos
residualPlots(modelo13, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo14, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo15, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos

# predictions ####

# plots extras

df1$Education_Level <- as.factor(df1$Education_Level)
df2$Education_Level <- as.factor(df2$Education_Level)
df3$Education_Level <- as.factor(df3$Education_Level)
df5$Education_Level <- as.factor(df5$Education_Level)
df4$Education_Level <- as.factor(df4$Education_Level)
levels(df1$Education_Level) <- c('1','2','3')
levels(df2$Education_Level ) <- c('1','2','3')
levels(df3$Education_Level ) <- c('1','2','3')
levels(df5$Education_Level) <- c('1','2','3')
levels(df4$Education_Level) <- c('1','2','3')

df1$SEX <- as.factor(df1$SEX)
df2$SEX <- as.factor(df2$SEX)
df3$SEX <- as.factor(df3$SEX)
df5$SEX <- as.factor(df5$SEX)
df4$SEX <- as.factor(df4$SEX)
table(df4$SEX)
levels(df1$SEX) <- c('MASC','FEM')
levels(df2$SEX ) <- c('MASC','FEM')
levels(df3$SEX ) <- c('MASC','FEM')
levels(df5$SEX) <- c('MASC','FEM')
levels(df4$SEX) <- c('MASC','FEM')
library(marginaleffects)
library(ggeffects)
df1$Ideologia <- memisc::recode(as.numeric(df1$Ideology),  1 <- c(1,2,3),
                                2 <- c(4,5,6,7), 3 <-c(8,9,10))
df2$Ideologia <- memisc::recode(as.numeric(df2$Ideology),  1 <- c(1,2,3),
                                2 <- c(4,5,6,7), 3 <-c(8,9,10))
df3$Ideologia <- memisc::recode(as.numeric(df3$Ideology),  1 <- c(1,2,3),
                                2 <- c(4,5,6,7), 3 <-c(8,9,10))
df4$Ideologia <- memisc::recode(as.numeric(df4$Ideology),  1 <- c(1,2,3),
                                2 <- c(4,5,6,7), 3 <-c(8,9,10))
df5$Ideologia <- memisc::recode(as.numeric(df5$Ideology),  1 <- c(1,2,3),
                                2 <- c(4,5,6,7), 3 <-c(8,9,10))

df1$Ideologia <- as.factor(df1$Ideologia)
levels(df1$Ideologia) <- c('Esquerda','Centro','Direita')
df2$Ideologia <- as.factor(df2$Ideologia)
levels(df2$Ideologia) <- c('Esquerda','Centro','Direita')
df3$Ideologia <- as.factor(df3$Ideologia)
levels(df3$Ideologia) <- c('Esquerda','Centro','Direita')
df4$Ideologia <- as.factor(df4$Ideologia)
levels(df4$Ideologia) <- c('Esquerda','Centro','Direita')
df5$Ideologia <- as.factor(df5$Ideologia)
levels(df5$Ideologia) <- c('Esquerda','Centro','Direita')


modelo11c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideologia,
                data=df1)
modelo12c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideologia,
                data=df2)
modelo13c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideologia,data=df3)
modelo14c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideologia,data=df4)
modelo15c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideologia,data=df5)

a<- plot_cap(modelo11c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
a
b<-plot_cap(modelo12c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
c<-plot_cap(modelo13c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
d<-plot_cap(modelo14c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
e<-plot_cap(modelo15c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
grid.arrange(a,b,c,d,e, nrow=2)#exemplos

modelo11d <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
                data=df1)
modelo12d <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,
                data=df2)
modelo13d <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,data=df3)
modelo14d <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,data=df4)
modelo15d <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+Ideology,data=df5)
a<- plot_cap(modelo11d, condition = c("Ideology","SEX"))#
a
b<-plot_cap(modelo12d, condition = c("Ideology","SEX"))#
c<-plot_cap(modelo13d, condition = c("Ideology","SEX"))#
d<-plot_cap(modelo14d, condition = c("Ideology","SEX"))#
e<-plot_cap(modelo15d, condition = c("Ideology","SEX"))#
grid.arrange(a,b,c,d,e, nrow=2)#exemplos

p <- ggpredict(modelo11c, c("Ideologia","SEX","Settlement_size"),ci.lvl = 0.90)
plot(p)
p <- ggpredict(modelo11112, c("Ideologia","SEX","Settlement_size"),ci.lvl = 0.90) 
plot(p)
p <- ggpredict(modelo11113, c("Ideologia","SEX","Settlement_size"),ci.lvl = 0.90) 
plot(p)
p <- ggpredict(modelo11114, c("Ideologia","SEX","Settlement_size"),ci.lvl = 0.90) 
plot(p)
p <- ggpredict(modelo11115, c("Ideologia","SEX","Settlement_size"),ci.lvl = 0.90) 
plot(p)


predictions(modelo11c, newdata = datagrid(Settlement_size= 1, SEX="FEM",Ideologia="Direita" ))#mulher de cidade pequena e de direita

predictions(modelo11c, newdata = datagrid(Settlement_size= 8, SEX="FEM",Ideologia="Esquerda" ))#mulher de cidade grande e de esquerda
predictions(modelo11c, newdata = datagrid(Settlement_size= 1, SEX="MASC",Ideologia="Direita" ))#homem de cidade pequena e de direita

predictions(modelo11c, newdata = datagrid(Settlement_size= 8, SEX="MASC",Ideologia="Esquerda" ))#homem de cidade grande e de esquerda
#
predictions(modelo12c, newdata = datagrid(Settlement_size= 1, SEX="FEM",Ideologia="Direita" ))#mulher de cidade pequena e de direita

predictions(modelo12c, newdata = datagrid(Settlement_size= 8, SEX="FEM",Ideologia="Esquerda" ))#mulher de cidade grande e de esquerda
predictions(modelo12c, newdata = datagrid(Settlement_size= 1, SEX="MASC",Ideologia="Direita" ))#homem de cidade pequena e de direita

predictions(modelo12c, newdata = datagrid(Settlement_size= 8, SEX="MASC",Ideologia="Esquerda" ))#homem de cidade grande e de esquerda
#
predictions(modelo13c, newdata = datagrid(Settlement_size= 1, SEX="FEM",Ideologia="Direita" ))#mulher de cidade pequena e de direita

predictions(modelo13c, newdata = datagrid(Settlement_size= 8, SEX="FEM",Ideologia="Esquerda"))#mulher de cidade grande e de esquerda
predictions(modelo13c, newdata = datagrid(Settlement_size= 1, SEX="MASC",Ideologia="Direita"))#homem de cidade pequena e de direita

predictions(modelo13c, newdata = datagrid(Settlement_size= 8, SEX="MASC",Ideologia="Esquerda" ))#homem de cidade grande e de esquerda
#

predictions(modelo14c, newdata = datagrid(Settlement_size= 1, SEX="FEM",Ideologia="Direita"))#mulher de cidade pequena e de direita

predictions(modelo14c, newdata = datagrid(Settlement_size= 8, SEX="FEM",Ideologia="Esquerda" ))#mulher de cidade grande e de esquerda
predictions(modelo14c, newdata = datagrid(Settlement_size= 1, SEX="MASC",Ideologia="Direita" ))#homem de cidade pequena e de direita

predictions(modelo14c, newdata = datagrid(Settlement_size= 8, SEX="MASC",Ideologia="Esquerda"))#homem de cidade grande e de esquerda

#

predictions(modelo15c, newdata = datagrid(Settlement_size= 1, SEX="FEM",Ideologia="Direita"))#mulher de cidade pequena e de direita

predictions(modelo15c, newdata = datagrid(Settlement_size= 8, SEX="FEM",Ideologia="Esquerda" ))#mulher de cidade grande e de esquerda
predictions(modelo15c, newdata = datagrid(Settlement_size= 1, SEX="MASC",Ideologia="Direita"))#homem de cidade pequena e de direita

predictions(modelo15c, newdata = datagrid(Settlement_size= 8, SEX="MASC",Ideologia="Esquerda" ))#homem de cidade grande e de esquerda

