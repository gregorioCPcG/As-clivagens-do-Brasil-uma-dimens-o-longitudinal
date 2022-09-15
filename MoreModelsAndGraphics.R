library(sjPlot)
library(lm.beta)
library(ggplot2)
library(dplyr)
library(memisc)


# rodar "script" e "mais tabelas" antes.
library(haven)

library(gridExtra)
WVS_MERGE_81_2020_BRAZIL <- read_sav("D:/ATUALIZA_PASTA_d/WVS Lucas 13_10/WVS MERGE 81-2020 BRAZIL.sav")
head(WVS_MERGE_81_2020_BRAZIL)
table(WVS_MERGE_81_2020_BRAZIL$S002VS)
####

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

#categories
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


tapply(df1$MR1,df1$PT, mean, na.rm = TRUE)
tapply(df2$MR1,df2$PT, mean, na.rm = TRUE)
tapply(df3$MR1,df3$PT, mean, na.rm = TRUE)
tapply(df4$MR1,df4$PT, mean, na.rm = TRUE)
tapply(df5$MR1,df5$PT, mean, na.rm = TRUE)

print(df1$Ideology)# MANTER

table(df1$PT)

library(lm.beta)

modelo11 <- lm(MR1 ~ SEX+AGE+Education_Level+
                Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,
              data=df1)
summary(modelo11)#valores estimados
modelo11x <- lm.beta(modelo11)
tab_model(modelo11x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo12 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,
               data=df2)
summary(modelo12)#valores estimados
modelo12x <- lm.beta(modelo12)
#tab_model(modelo12x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo13 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,data=df3)
summary(modelo13)#valores estimados
modelo13x <- lm.beta(modelo13)
#tab_model(modelo13x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo14 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico,data=df4)
summary(modelo14)#valores estimados
modelo14x <- lm.beta(modelo14)
#tab_model(modelo14x, show.ci = F, auto.label = T, show.se = T,collapse.se = T, wrap.labels = 60, p.style = "stars")

modelo15 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,data=df5)
summary(modelo15)#valores estimados
modelo15x <- lm.beta(modelo15)

tab_model(modelo11x,modelo12x,modelo13x,modelo14x,modelo15x,
          show.ci = F, auto.label = T, show.se = T,collapse.se = T,
          wrap.labels = 60, p.style = "stars")

plot_models(modelo11, modelo12, modelo13, modelo14, modelo15,
            legend.title = "Onda",m.labels = c("1991", "1997", "2006","2014",
                         "2018"),show.values = FALSE, 
            show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
            p.threshold = c(0.05, 0.01, 0.001), 
            vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=T)+
    theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))

modelo111 <-lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico,data=df1)
modelo112 <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Sem_Religiao+
                  Catolico,data=df2)
modelo113 <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Sem_Religiao+
                  Catolico,data=df3)
modelo115 <-lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico,data=df5)

plot_models(modelo111, modelo112, modelo113, modelo14, modelo115,
            legend.title = "Onda",m.labels = c("1991", "1997", "2006","2014",
                                               "2018"),show.values = FALSE, 
            show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
            p.threshold = c(0.05, 0.01, 0.001), 
            vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=T)+
  theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))


plot_models(modelo111, modelo112, modelo113, modelo14, modelo115,
            legend.title = "Onda",m.labels = c("1991", "1997", "2006","2014",
                                               "2018"),show.values = FALSE, 
            show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
            p.threshold = c(0.05, 0.01, 0.001), 
            vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=T)+
  theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))


plot_models(modelo11, modelo12, modelo13, modelo14, modelo15,
            legend.title = "Onda",m.labels = c("1991", "1997", "2006","2014",
                                               "2018"),show.values = FALSE, 
            show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
            p.threshold = c(0.05, 0.01, 0.001), 
            vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=F)+
  theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))

plot_models(modelo11, modelo12, modelo13, modelo14, modelo15)

plot_models(modelo11, modelo12, modelo13,modelo14,  modelo15,
            legend.title = "Onda",axis.labels = c("Profiss.Lib.","Trab.Rural","Braçal Especializ.", "Braçal",
                                                  "Católico","Sem Religião","Auto posic. Direita",
                                                  "PT","Post-Materialist Index","Mulher"),
            rm.terms = c("AGE","Education_Level","Scale_of_Incomes","Settlement_size",
                         "Etnic_Group","Interest_in_Politics"),m.labels = c("1991", "1997", "2006","2014",
                                               "2018"),auto.label=F,show.values = FALSE, 
            show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
            p.threshold = c(0.05, 0.01, 0.001), 
            vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=F)+
  theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))

# incorporar as 4 novas

library(marginaleffects)


table(df1$Religion)
df1$Religion <-relevel(df1$Religion, "Evangélico")
df2$Religion <-relevel(df2$Religion, "Evangélico")
df3$Religion <-relevel(df3$Religion, "Evangélico")
df4$Religion<-relevel(df4$Religion, "Evangélico")
df5$Religion <-relevel(df5$Religion, "Evangélico")
table(df1$Job2)
df1$Work <- as.factor(df1$Job2)
df2$Work <- as.factor(df2$Job2)
df3$Work <- as.factor(df3$Job2)
df5$Work <- as.factor(df5$Job2)
levels(df1$Work) <- c('Profiss.Liberal','Braçal.Especializado','Braçal','Trab.Rural','Gerência','Outros')
levels(df2$Work ) <- c('Profiss.Liberal','Braçal.Especializado','Braçal','Trab.Rural','Gerência','Outros')
levels(df3$Work ) <- c('Profiss.Liberal','Braçal.Especializado','Braçal','Trab.Rural','Gerência','Outros')
levels(df5$Work) <- c('Profiss.Liberal','Braçal.Especializado','Braçal','Trab.Rural','Gerência','Outros')

library(ggeffects)


modelo11b <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Work+Religion,data=df1)
modelo12b <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Work+Religion,data=df2)
modelo13b <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Work+Religion,data=df3)
modelo14b <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Religion,data=df4)
modelo15b <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Work+Religion,data=df5)


b1bb <- plot_cap(modelo11b, condition = c("PT"))
b1bb
b2bb <- plot_cap(modelo12b, condition = c("PT"))
b3bb <- plot_cap(modelo13b, condition = c("PT"))
b4bb <- plot_cap(modelo14b, condition = c("PT"))
b5bb <- plot_cap(modelo15b, condition = c("PT"))

grid.arrange(b1bb, b2bb, b3bb, b4bb, b5bb,nrow=2)

ab1bb <- plot_cap(modelo11b, condition = c("Religion"))
ab1bb
ab2bb <- plot_cap(modelo12b, condition = c("Religion"))
ab3bb <- plot_cap(modelo13b, condition = c("Religion"))
ab4bb <- plot_cap(modelo14b, condition = c("Religion"))
ab5bb <- plot_cap(modelo15b, condition = c("Religion"))
grid.arrange(ab1bb, ab2bb, ab3bb, ab4bb, ab5bb,nrow=2)



ab1b <- plot_cap(modelo11b, condition = c("Ideology"))
ab1b
ab2b <- plot_cap(modelo12b, condition = c("Ideology"))
ab3b <- plot_cap(modelo13b, condition = c("Ideology"))
ab4b <- plot_cap(modelo14b, condition = c("Ideology"))
ab5b <- plot_cap(modelo15b, condition = c("Ideology"))
grid.arrange(ab1b, ab2b, ab3b, ab4b, ab5b,nrow=2)



bab1bb <- plot_cap(modelo11b, condition = c("Work"))
bab1bb
bab2bb <- plot_cap(modelo12b, condition = c("Work"))
bab3bb <- plot_cap(modelo13b, condition = c("Work"))
bab5bb <- plot_cap(modelo15b, condition = c("Work"))
grid.arrange(bab1bb, bab2bb, bab3bb, bab5bb,nrow=2)




## diagnosis
library(lmtest)
library(car)
library(sandwich)
library(psych)
library(lm.beta)

vif(modelo1)#colineari... limiar 10
vif(modelo2)
vif(modelo3)
vif(modelo4)
vif(modelo5)
vif(modelo11)
vif(modelo12)
vif(modelo13)
vif(modelo14)
vif(modelo15)
vif(modelo111)
vif(modelo112)
vif(modelo113)
vif(modelo115)

avPlots(modelo1)
residualPlots(modelo1, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo2, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo3, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo4, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo5, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo11, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo12, fitted=TRUE, typ="rstudent", test=F)# nao pode ser linear...- resultados bãos
residualPlots(modelo13, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo14, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos
residualPlots(modelo15, fitted=TRUE, typ="rstudent", test=TRUE)# nao pode ser linear...- resultados bãos


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
modelo1111 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,
               data=df1)

modelo1112 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,
               data=df2)



modelo1113 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,data=df3)



modelo1114 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico,data=df4)


modelo1115 <- lm(MR1 ~ SEX+AGE+Education_Level+
                 Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                 Ideology+Sem_Religiao+
                 Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,data=df5)

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
levels(df1$Ideologia) <- c('Esquerda','Centro','Direita')
df3$Ideologia <- as.factor(df3$Ideologia)
levels(df1$Ideologia) <- c('Esquerda','Centro','Direita')
df4$Ideologia <- as.factor(df4$Ideologia)
levels(df1$Ideologia) <- c('Esquerda','Centro','Direita')
df5$Ideologia <- as.factor(df5$Ideologia)
levels(df1$Ideologia) <- c('Esquerda','Centro','Direita')

modelo11111 <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                   Ideologia+Sem_Religiao+
                   Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,
                 data=df1)

modelo11112 <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                   Ideologia+Sem_Religiao+
                   Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,
                 data=df2)



modelo11113 <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                   Ideologia+Sem_Religiao+
                   Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,data=df3)



modelo11114 <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                   Ideologia+Sem_Religiao+
                   Catolico,data=df4)


modelo11115 <- lm(MR1 ~ SEX+AGE+Education_Level+
                   Scale_of_Incomes+Settlement_size+
                   Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                   Ideologia+Sem_Religiao+
                   Catolico+Braçal+Braçal.Especializ+Trab.Rural+Profiss.Liberal,data=df5)

a<- plot_cap(modelo1111, condition = c("Ideology","SEX"))#
a
b<-plot_cap(modelo1112, condition = c("Ideology","SEX"))#
c<-plot_cap(modelo1113, condition = c("Ideology","SEX"))#
d<-plot_cap(modelo1114, condition = c("Ideology","SEX"))#
e<-plot_cap(modelo1115, condition = c("Ideology","SEX"))#
grid.arrange(a,b,c,d,e, nrow=2)#exemplos

a<- plot_cap(modelo11111, condition = c("Ideologia"))#
a
b<-plot_cap(modelo11112, condition = c("Ideologia","SEX"))#
c<-plot_cap(modelo11113, condition = c("Ideologia","SEX"))#
d<-plot_cap(modelo11114, condition = c("Ideologia","SEX"))#
e<-plot_cap(modelo11115, condition = c("Ideologia","SEX"))#
grid.arrange(a,b,c,d,e, nrow=2)#exemplos

df1$Ideology <- as.factor(df1$Ideology)
df2$Ideology <- as.factor(df2$Ideology)
df3$Ideology <- as.factor(df3$Ideology)
df5$Ideology <- as.factor(df5$Ideology)
df4$Ideology <- as.factor(df4$Ideology)
levels(df1$Ideology) <- c('1','2','3','4','5','6','7','8','9','10','')
levels(df2$Ideology ) <- c('1','2','3','4','5','6','7','8','9','10','')
levels(df3$Ideology ) <- c('1','2','3','4','5','6','7','8','9','10','')
levels(df5$Ideology) <- c('1','2','3','4','5','6','7','8','9','10','')
levels(df4$Ideology) <- c('1','2','3','4','5','6','7','8','9','10','')

df1$PostMaterialistIndex <- as.factor(df1$PostMaterialistIndex)
df2$PostMaterialistIndex <- as.factor(df2$PostMaterialistIndex)
df3$PostMaterialistIndex <- as.factor(df3$PostMaterialistIndex)
df5$PostMaterialistIndex <- as.factor(df5$PostMaterialistIndex)
df4$PostMaterialistIndex <- as.factor(df4$PostMaterialistIndex)
levels(df1$PostMaterialistIndex) <- c('0','1','2','3','4','5')
levels(df2$PostMaterialistIndex ) <- c('0','1','2','3','4','5')
levels(df3$PostMaterialistIndex ) <- c('0','1','2','3','4','5')
levels(df5$PostMaterialistIndex) <- c('0','1','2','3','4','5')
levels(df4$PostMaterialistIndex) <- c('0','1','2','3','4','5')

df1 <- df1[ df1$Religion != "Outros", , drop=FALSE]; df1$Religion <- factor(df1$Religion); summary(df1$Religion)
df2 <- df2[ df2$Religion != "Outros", , drop=FALSE]; df2$Religion <- factor(df2$Religion); summary(df2$Religion)
df3 <- df3[ df3$Religion != "Outros", , drop=FALSE]; df3$Religion <- factor(df3$Religion); summary(df3$Religion)
df4 <- df4[ df4$Religion != "Outros", , drop=FALSE]; df4$Religion <- factor(df4$Religion); summary(df4$Religion)
df5 <- df5[ df5$Religion != "Outros", , drop=FALSE]; df5$Religion <- factor(df5$Religion); summary(df5$Religion)


modelo11c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Work+Religion,data=df1)
modelo12c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Work+Religion,data=df2)
modelo13c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Work+Religion,data=df3)
modelo14c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Religion,data=df4)
modelo15c <- lm(MR1 ~ SEX+AGE+Education_Level+
                  Scale_of_Incomes+Settlement_size+
                  Etnic_Group+PostMaterialistIndex+Interest_in_Politics+PT+
                  Ideology+Work+Religion,data=df5)
modelo

plot_models(modelo11c, modelo12c, modelo13c, modelo15c,
            legend.title = "Onda",m.labels = c("1991", "1997", "2006",
                                               "2018"), rm.terms ="FEM", show.values = FALSE, 
            show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
            p.threshold = c(0.05, 0.01, 0.001), 
            vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=F)+ 
  theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))

a<- plot_cap(modelo11c, condition = c("Religion","Education_Level"))#boa
b<-plot_cap(modelo12c, condition = c("Religion","Education_Level"))#boa
c<-plot_cap(modelo13c, condition = c("Religion","Education_Level"))#boa
d<-plot_cap(modelo14c, condition = c("Religion","Education_Level"))#boa
e<-plot_cap(modelo15c, condition = c("Religion","Education_Level"))#boa

grid.arrange(a,b,c,d,e, nrow=2)#exemplos


a<- plot_cap(modelo11c, condition = c("Religion","Ideology"))#boa
a
b<-plot_cap(modelo12c, condition = c("Religion","Ideology"))#boa
c<-plot_cap(modelo13c, condition = c("Religion","Ideology"))#boa
d<-plot_cap(modelo14c, condition = c("Religion","Ideology"))#boa
e<-plot_cap(modelo15c, condition = c("Religion","Ideology"))#boa
grid.arrange(a,b,c,d,e, nrow=2)#exemplos


a<- plot_cap(modelo11c, condition = c("Education_Level","Ideology"))#
a
b<-plot_cap(modelo12c, condition = c("Education_Level","Ideology"))#
c<-plot_cap(modelo13c, condition = c("Education_Level","Ideology"))#
d<-plot_cap(modelo14c, condition = c("Education_Level","Ideology"))#
e<-plot_cap(modelo15c, condition = c("Education_Level","Ideology"))#
grid.arrange(a,b,c,d,e, nrow=2)#exemplos


a<- plot_cap(modelo11c, condition = c("Education_Level","PT"))#
a
b<-plot_cap(modelo12c, condition = c("Education_Level","PT"))#
c<-plot_cap(modelo13c, condition = c("Education_Level","PT"))#
d<-plot_cap(modelo14c, condition = c("Education_Level","PT"))#
e<-plot_cap(modelo15c, condition = c("Education_Level","PT"))#
grid.arrange(a,b,c,d,e, nrow=2)#exemplos


a<- plot_cap(modelo11c, condition = c("PostMaterialistIndex","PT"),conf_level = 0.9)#
a
b<-plot_cap(modelo12c, condition = c("PostMaterialistIndex","PT"),conf_level = 0.9)#
c<-plot_cap(modelo13c, condition = c("PostMaterialistIndex","PT"),conf_level = 0.9)#
d<-plot_cap(modelo14c, condition = c("PostMaterialistIndex","PT"),conf_level = 0.9)#
e<-plot_cap(modelo15c, condition = c("PostMaterialistIndex","PT"),conf_level = 0.9)#
grid.arrange(a,b,c,d,e, nrow=2)#exemplos

p <- ggpredict(modelo11c, c("SEX","Work"))
p
plot(p)


# WORK
#1991
tapply(df1$MR1,df1$Work, mean, na.rm = TRUE)
yy1 <- c(-0.29,0.04,0.22,0.3,-0.3,-0.15)# consertar abaixo tambem
xx1 <- c("Prof.Liberal",
         "BraçalEspecializ.","Braçal","Trab.Rural",
         "Gerência","Outros")
xx1 <- factor(xx1,levels=c("BraçalEspecializ.","Braçal","Trab.Rural","Prof.Liberal",
                           "Gerência","Outros"))
grafxx1 <- data.frame(xx1,yy1)

trab91 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.35,0.4)) +
  ggtitle("Nível de fundamentalismo por trabalho (1991)") +
  xlab("Trabalho") + 
  ylab("Fundamentalista (factor.score)")

trab91

#1997
tapply(df2$MR1,df2$Work, mean, na.rm = TRUE)
yy1 <- c(-0.20,-.02,.15,.07,-.14,-.08)# 
grafxx1 <- data.frame(xx1,yy1)

trab97 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.35,0.4)) +
  ggtitle("Nível de fundamentalismo por trabalho (1997)") +
  xlab("Trabalho") + 
  ylab("Fundamentalista (factor.score)")

trab97

#2006
tapply(df3$MR1,df3$Work, mean, na.rm = TRUE)
yy1 <- c(-0.27,0.07666625,0.08,0.12,-0.23,0.01)# 
grafxx1 <- data.frame(xx1,yy1)

trab2006 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.35,0.4)) +
  ggtitle("Nível de fundamentalismo por trabalho (2006)") +
  xlab("Trabalho") + 
  ylab("Fundamentalista (factor.score)")

trab91
trab97
trab2006

tapply(df5$MR1,df5$Work, mean, na.rm = TRUE)

yy1 <- c(-0.26,0.08,0.17,0.302,0.04,0.01)# consertar abaixo tambem
grafxx1 <- data.frame(xx1,yy1)

trab2018 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.35,0.4)) +
  ggtitle("Nível de fundamentalismo por trabalho (2018)") +
  xlab("Trabalho") + 
  ylab("Fundamentalista (factor.score)")

trab2018

grid.arrange(trab91,trab97,trab2006,trab2018, nrow=2)#exemplos

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

# tradicional gender size of town

a<- plot_cap(modelo11c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
a
b<-plot_cap(modelo12c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
c<-plot_cap(modelo13c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
d<-plot_cap(modelo14c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
e<-plot_cap(modelo15c, condition = c("Settlement_size","SEX"),conf_level = 0.9)#
grid.arrange(a,b,c,d,e, nrow=2)#exemplos


a<- plot_cap(modelo1111, condition = c("Ideology","SEX"))#
a
b<-plot_cap(modelo1112, condition = c("Ideology","SEX"))#
c<-plot_cap(modelo1113, condition = c("Ideology","SEX"))#
d<-plot_cap(modelo1114, condition = c("Ideology","SEX"))#
e<-plot_cap(modelo1115, condition = c("Ideology","SEX"))#
grid.arrange(a,b,c,d,e, nrow=2)#exemplos

p <- ggpredict(modelo1111, c("Settlement_size","Ideology","SEX"))
plot(p)
