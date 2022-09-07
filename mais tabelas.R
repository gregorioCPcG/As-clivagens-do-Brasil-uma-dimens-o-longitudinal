# mais tabelas que não constam no script
library(ggplot2)
#rodar todo o script.R antes ####
df1 <- data.frame(pf1.1.3[["scores"]])
df2 <- data.frame(pf2.1.3[["scores"]])
df2$MR1 <- df2$MR1*-1
df3 <- data.frame(pf3.1.3[["scores"]])
df4 <- data.frame(pf4.1.3[["scores"]])
df5 <- data.frame(pf5.1.3[["scores"]])
# religião ####
#religiao 1997 (do script)
a97 <- ggplot(graf2.4, aes(y = y2.4, x = x2.4)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1.2,0.3)) +
  ggtitle("Nível de fundamentalismo por 
religião (1997)") +
  xlab("Religião") + 
  ylab("Fundamentalista (factor.score)")

# 1991 religião
df1$relig <- moreno_clivagem_brasil1991$F025old
summary(df1)
summary(as.factor(df1$relig))
library(memisc)
df1$relig <- memisc::recode(as.numeric(df1$relig), 1 <- c(64),0 <- c(0),2 <- c(62,12,49,52))
summary(as.factor(df1$relig))
df1$relig <- as.factor(df1$relig)
df1 <- na.omit(df1)
by(df1$MR1, df1$relig, mean)
# none --0.6828138
#catolico 0.03
#outras 0.39

yx <- c(-0.6828,0.03,0.0395)
xx <- c("1.Sem Rel","2.Catolico","3.Outra")
xgraf <- data.frame(xx,yx)

a91 <- ggplot(xgraf, aes(y = yx, x = xx)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1.2,0.3)) +
  ggtitle("Nível de fundamentalismo por 
religião (1991)") +
  xlab("Religião") + 
  ylab("Fundamentalista (factor.score)") 

a91
# 2006 religião

df3$relig <- moreno_clivagem_brasil2006$F025old
summary(df3)
summary(as.factor(df3$relig))

df3$relig <- memisc::recode(as.numeric(df3$relig), 1 <- c(64),
                            2 <- c(25), 0 <- c(0),3 <- c(12,42,52,53,54,62,73))
summary(as.factor(df3$relig))
df3$relig <- as.factor(df3$relig)
df3 <- na.omit(df3)
by(df3$MR1, df3$relig, mean)
# none --0.6000
#Evangelico - 0.37
#catolico 0.02
#outras -0.21

yx <- c(-0.6,0.37,0.02,-0.21)
xx <- c("1.Sem Rel","2.Evangelico","3.Catolico","4.Outra")
xgraf <- data.frame(xx,yx)

a06 <- ggplot(xgraf, aes(y = yx, x = xx)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1.2,0.4)) +
  ggtitle("Nível de fundamentalismo por 
religião (2006)") +
  xlab("Religião") + 
  ylab("Fundamentalista (factor.score)") 
a06


library(gridExtra)
grid.arrange(a91, a97, ncol=2)# show
library(readxl)
df2014 <- read_excel("F00007581-WV6_Data_Brazil_Excel_v20201117.xlsx")
summary(df4)
df4$relig <- df2014$`V144G: Religious denominations - major groups`

summary(as.factor(df4$relig))

df4$relig <- memisc::recode(as.numeric(df4$relig), 1 <- c(1),
                            2 <- c(8), 0 <- c(0),3 <- c(2,3,4,5,7,9),
                            NA <- c(-2,-1))
summary(as.factor(df4$relig))
df4$relig <- as.factor(df4$relig)
df4 <- na.omit(df4)
by(df4$MR1, df4$relig, mean)
# none -0.5835
#Evangelico - 0.3469
#catolico 0.0125
#outras -0.044

yx <- c(-0.5835,0.3469,0.0125,-0.04)
xx <- c("1.Sem Rel","2.Evangelico","3.Catolico","4.Outra")
xgraf <- data.frame(xx,yx)

a14 <- ggplot(xgraf, aes(y = yx, x = xx)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1.2,0.4)) +
  ggtitle("Nível de fundamentalismo por 
religião (2014)") +
  xlab("Religião") + 
  ylab("Fundamentalista (factor.score)") 

a14

a06


# 2018

df2018 <- read_excel("F00010337-WVS_Wave_7_Brazil_Excel_v2.0.xlsx")
table(df2018$`Q289: Religious denominations - major groups`)
df5$relig <- df2018$`Q289: Religious denominations - major groups`

summary(as.factor(df5$relig))

df5$relig <- memisc::recode(as.numeric(df5$relig), 1 <- c(1),
                            2 <- c(2), 0 <- c(0),3 <- c(3,4,5,7,8,9),
                            NA <- c(-2,-1))

summary(as.factor(df5$relig))
df5$relig <- as.factor(df5$relig)
df5 <- na.omit(df5)
by(df5$MR1, df5$relig, mean)
# none -0.669
#Evangelico - 0.359
#catolico 0.091
#outras -0.25

yx <- c(-0.669,0.359,0.0915,-0.252)
xx <- c("1.Sem Rel","2.Evangelico","3.Catolico","4.Outra")
xgraf <- data.frame(xx,yx)

a18 <- ggplot(xgraf, aes(y = yx, x = xx)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-1.2,0.4)) +
  ggtitle("Nível de fundamentalismo por 
religião (2018)") +
  xlab("Religião") + 
  ylab("Fundamentalista (factor.score)") 

a18


grid.arrange(a91, a97, ncol=2)
grid.arrange(a06, ncol=2)
grid.arrange(a14,a18, ncol=2)


# profissão> #####
#2018
y5.3 <- c(0.0009856193,-0.2732692,-0.08185384,-0.07666758,-0.01011711,-0.07485658,
          0.1353012,0.1847684,0.3028295,-0.1325546)
x5.3 <- c("Nunca Trab","Téc", "Adm","Clerical","Vendas",
          "Serviços","Semi-Qualif.","Braçal","Trab.Rural","Prop.Rural")
x5.3 <- factor(x5.3, levels=c("Adm","Vendas",
                              "Serviços","Téc","Semi-Qualif.",
                              "Braçal","Trab.Rural","Prop.Rural","Nunca Trab","Clerical"))
graf5.3 <- data.frame(x5.3,y5.3)
trab2018 <- ggplot(graf5.3, aes(y = y5.3, x = x5.3)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.3,0.4)) +
  ggtitle("Nível de fundamentalismo por trabalho (2018)") +
  xlab("Trabalho") + 
  ylab("Fundamentalista (factor.score)")
trab2018
# rodar de novo
df1 <- data.frame(pf1.1.3[["scores"]])
df2 <- data.frame(pf2.1.3[["scores"]])
df2$MR1 <- df2$MR1*-1
df3 <- data.frame(pf3.1.3[["scores"]])
df4 <- data.frame(pf4.1.3[["scores"]])
df5 <- data.frame(pf5.1.3[["scores"]])

# 91
table(moreno_clivagem_brasil1991$X036)
df91 <- read_excel("F00008257-WV2_Data_Brazil_Excel_v1.6.xlsx")
table(df91$`V359: Profession/job`)
df1$prof <- df91$`V359: Profession/job`

prop.table(table(df1$prof))
df1 <- na.omit(df1)
by(df1$MR1, df1$prof, mean)
#1 ADM = 0.14
#3 Prof.Liberal = -0.32
#4 Escritório = -0.26 
#6 Téc = -0.31
#7 Semi qual. braçal = 0.04
#8 Braçal = 0.31
#9 Prop.Rural = 0.21
#10 Trab. Rural = 0.30
# 12 comerciante = -0.12
yy1 <- c(.14,-.32,-.26,-.31,.04,.31,.21,.30,-.12)# consertar abaixo tambem
xx1 <- c("Adm","Prof.Liberal","Escritório",
         "Téc","SemiQualifBraçal","Braçal","Prop.Rural",
         "Trab.Rural","Comerciante")
xx1 <- factor(xx1,levels=c("Adm","Prof.Liberal","Escritório",
                           "Téc","SemiQualifBraçal","Braçal","Prop.Rural",
                           "Trab.Rural","Comerciante"))
grafxx1 <- data.frame(xx1,yy1)

trab91 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.35,0.4)) +
  ggtitle("Nível de fundamentalismo por trabalho (1991)") +
  xlab("Trabalho") + 
  ylab("Fundamentalista (factor.score)")
trab91

# 06
df3 <- data.frame(pf3.1.3[["scores"]])
df06 <- read_excel("F00007825-WV5_Data_Brazil_Excel_v20201117.xlsx")
table(df06$`V242: Profession/job`)
df3$trab <- df06$`V242: Profession/job`

df3 <- na.omit(df3)
by(df3$MR1, df3$trab, mean)
#1 ADM = 0.04
#3 Profissional liberal = -0.30
# 5 Trabalhador de escritório )= -0.13
# 7 Trabalhador manual especializado = 0.03
# 8 Trabalhador manual semi-especializado = 0.08
# 9 Trabalhador braçal não-especializado = 0.06
# 10 Fazendeiro = -0.006
# 11 Trabalhador rural = 0.10

yy1 <- c(0.04,-0.30,-0.13,0.03,.08,0.06,-0.006,.10)# consertar abaixo tambem
xx1 <- c("Adm","Prof.Liberal","Escritório",
         "Téc","SemiQualifBraçal","Braçal","Prop.Rural",
         "Trab.Rural")
xx1 <- factor(xx1,levels=c("Adm","Prof.Liberal","Escritório",
                           "Téc","SemiQualifBraçal","Braçal","Prop.Rural",
                           "Trab.Rural"))
grafxx1 <- data.frame(xx1,yy1)

trab06 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.35,0.4)) +
  ggtitle("Nível de fundamentalismo por trabalho (2006)") +
  xlab("Trabalho") + 
  ylab("Fundamentalista (factor.score)")
trab06

#
trab91
trab06
trab2018

# 97
df2 <- data.frame(pf2.1.3[["scores"]])
df2$MR1 <- df2$MR1*-1
df97 <- read_excel("F00008090-WV3_Data_Brazil_Excel_v20221107.xlsx")
table(df97$`V221: Profession/job`)
df2$trab <- df97$`V221: Profession/job`

df2 <- na.omit(df2)
by(df2$MR1, df2$trab, mean)
#1 ADM = 0.08
#3 Profissional liberal = -0.16
# 4 Trabalhador de escritório )= -0.13
# 7 Trabalhador manual especializado = -.22
# 8 Trabalhador manual semi-especializado = -0.005
# 9 Trabalhador braçal não-especializado = -0.001
# 10 Fazendeiro = 0.15
# 11 Trabalhador rural = 0.24

yy1 <- c(0.08,-0.16,-0.13,-0.22,-.005,-.001,.15,.24)# consertar abaixo tambem
xx1 <- c("Adm","Prof.Liberal","Escritório",
         "Téc","SemiQualifBraçal","Braçal","Prop.Rural",
         "Trab.Rural")
xx1 <- factor(xx1,levels=c("Adm","Prof.Liberal","Escritório",
                           "Téc","SemiQualifBraçal","Braçal","Prop.Rural",
                           "Trab.Rural"))
grafxx1 <- data.frame(xx1,yy1)

trab97 <- ggplot(grafxx1, aes(y = yy1, x = xx1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  scale_y_continuous(limits = c(-0.35,0.4)) +
  ggtitle("Nível de fundamentalismo por trabalho (1997)") +
  xlab("Trabalho") + 
  ylab("Fundamentalista (factor.score)")

trab97

# 14
#?

grid.arrange(trab91, trab06, trab2018, nrow=3)
#97 nos apêndices, em 14 nao achei a questão

# regressões - 
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
#
df1$SEX <-df1$X001
df1$AGE <- df1$X003
df1$Education_Level <- df1$X025R
df1$Employment_Status <- df1$X028
df1$Scale_of_Incomes <- df1$X047_WVS
df1$Settlement_size <- df1$X049
df1$Etnic_Group <- df1$X051
df1$PostMaterialistIndex <- df1$Y001
df1$Interest_in_Politics <- df1$E023
modelo1 <- lm(MR1 ~ SEX+AGE+Education_Level+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df1)
summary(modelo1)#valores estimados
library(coefplot)
library(lm.beta)
coefplot(modelo1x, interactive=TRUE, intercept=FALSE)
library(sjPlot)
modelo1x <- lm.beta(modelo1)
tab_model(modelo1x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")#valores padronizados
summary(testeh1.5, standardized = TRUE)#ambos

#
df2$SEX <-df2$X001
df2$AGE <- df2$X003
df2$Education_Level <- df2$X025R
df2$Employment_Status <- df2$X028
df2$Scale_of_Incomes <- df2$X047_WVS
df2$Settlement_size <- df2$X049
df2$Etnic_Group <- df2$X051
df2$PostMaterialistIndex <- df2$Y001
df2$Interest_in_Politics <- df2$E023
modelo2 <- lm(MR1 ~ SEX+AGE+Education_Level+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df2)
summary(modelo2)
modelo2x <- lm.beta(modelo2)
tab_model(modelo2x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

summary(testeh2.5, standardized = TRUE)#verificar se deu igual

coefplot(modelo2x, interactive=TRUE, intercept=FALSE)
coefplot(modelo2, interactive=TRUE, intercept=FALSE)

#
df3$SEX <-df3$X001
df3$AGE <- df3$X003
df3$Education_Level <- df3$X025R
df3$Employment_Status <- df3$X028
df3$Scale_of_Incomes <- df3$X047_WVS
df3$Settlement_size <- df3$X049
df3$Etnic_Group <- df3$X051
df3$PostMaterialistIndex <- df3$Y001
df3$Interest_in_Politics <- df3$E023
modelo3 <- lm(MR1 ~ SEX+AGE+Education_Level+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df3)
summary(modelo3)
modelo3x <- lm.beta(modelo3)
tab_model(modelo3x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
summary(testeh3.5, standardized = TRUE)#verificar se deu igual

#
df4$SEX <-df4$X001
df4$AGE <- df4$X003
df4$Education_Level <- df4$X025R
df4$Employment_Status <- df4$X028
df4$Scale_of_Incomes <- df4$X047_WVS
df4$Settlement_size <- df4$X049
df4$Etnic_Group <- df4$X051
df4$PostMaterialistIndex <- df4$Y001
df4$Interest_in_Politics <- df4$E023
modelo4 <- lm(MR1 ~ SEX+AGE+Education_Level+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df4)
summary(modelo4)
modelo4x <- lm.beta(modelo4)
tab_model(modelo4x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

summary(testeh4.5, standardized = TRUE)#verificar se deu igual


coefplot(modelo4x, interactive=TRUE, intercept=FALSE)
#
df5$SEX <-df5$X001
df5$AGE <- df5$X003
df5$Education_Level <- df5$X025R
df5$Employment_Status <- df5$X028
df5$Scale_of_Incomes <- df5$X047_WVS
df5$Settlement_size <- df5$X049
df5$Etnic_Group <- df5$X051
df5$PostMaterialistIndex <- df5$Y001
df5$Interest_in_Politics <- df5$E023
modelo5 <- lm(MR1 ~ SEX+AGE+Education_Level+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df5)
summary(modelo5)
modelo5x <- lm.beta(modelo5)
tab_model(modelo5x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
summary(testeh3.5, standardized = TRUE)#verificar se deu igual
summary(modelo5)

coefplot(modelo5x, interactive=TRUE, intercept=FALSE)

summary(testeh5.5, standardized = TRUE)#verificar se deu igual

# mais opçoes de plot

library(ggplot2)
library(ggsci)
library(ggprism)
library(jtools)
library(ggstance)

plotgx <- plot_coefs(modelo1x, modelo2x,modelo3x, modelo4x, modelo5x, 
                         model.names = c("1991","1997","2006", "2014", "2018"),
                         legend.title = "Anos",
                         inner_ci_level = .9,
                         point.shape = FALSE,
                     rescale.distributions=TRUE)
plotgx


plotg <- plot_coefs(modelo1, modelo2,modelo3, modelo4, modelo5, 
                     model.names = c("1991","1997","2006", "2014", "2018"),
                     legend.title = "Anos",
                     inner_ci_level = .9,
                     point.shape = FALSE,
                     rescale.distributions=TRUE)
plotg

library(marginaleffects)

a1 <- plot_cap(modelo1, condition = c("AGE"))
a2 <- plot_cap(modelo2, condition = c("AGE"))
a3 <- plot_cap(modelo3, condition = c("AGE"))
a4 <- plot_cap(modelo4, condition = c("AGE"))
a5 <- plot_cap(modelo5, condition = c("AGE"))

grid.arrange(a1,a2,a3,a4,a5, nrow=2)
summary(modelo1)

df1$EducationLevel <- as.factor(df1$Education_Level)
df2$EducationLevel <- as.factor(df2$Education_Level)
df3$EducationLevel <- as.factor(df3$Education_Level)
df4$EducationLevel <- as.factor(df4$Education_Level)
df5$EducationLevel <- as.factor(df5$Education_Level)
modelo1b <- lm(MR1 ~ SEX+AGE+EducationLevel+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df1)
modelo2b <- lm(MR1 ~ SEX+AGE+EducationLevel+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df2)
modelo3b<- lm(MR1 ~ SEX+AGE+EducationLevel+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df3)
modelo4b <- lm(MR1 ~ SEX+AGE+EducationLevel+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df4)
modelo5b <- lm(MR1 ~ SEX+AGE+EducationLevel+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialistIndex+Interest_in_Politics,
              data=df5)
summary(modelo1)
summary(modelo1b)# pra testar

b1 <- plot_cap(modelo1b, condition = c("EducationLevel"))
b2 <- plot_cap(modelo2b, condition = c("EducationLevel"))
b3 <- plot_cap(modelo3b, condition = c("EducationLevel"))
b4 <- plot_cap(modelo4b, condition = c("EducationLevel"))
b5 <- plot_cap(modelo5b, condition = c("EducationLevel"))

grid.arrange(b1,b2,b3,b4,b5, nrow=2)
table(as.factor(moreno_clivagem_brasil2018$X051))
print(moreno_clivagem_brasil2018$X051)


df5$Branco <- as.factor(df5$X051)
summary(df5$Branco)
levels(df5$Branco) <- c('Não-Branco','Branco')
modelo5bc <- lm(MR1 ~ SEX+AGE+EducationLevel+
                 Employment_Status+Scale_of_Incomes+Settlement_size+
                 Branco+PostMaterialistIndex+Interest_in_Politics,
               data=df5)
plot_cap(modelo5bc, condition = c("Branco"))


df1$PostMaterialist <- as.factor(df1$PostMaterialistIndex)
df2$PostMaterialist <- as.factor(df2$PostMaterialistIndex)
df3$PostMaterialist <- as.factor(df3$PostMaterialistIndex)
df4$PostMaterialist <- as.factor(df4$PostMaterialistIndex)
df5$PostMaterialist <- as.factor(df5$PostMaterialistIndex)
modelo1bb <- lm(MR1 ~ SEX+AGE+EducationLevel+
                 Employment_Status+Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialist+Interest_in_Politics,
               data=df1)
modelo2bb <- lm(MR1 ~ SEX+AGE+EducationLevel+
                 Employment_Status+Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialist+Interest_in_Politics,
               data=df2)
modelo3bb<- lm(MR1 ~ SEX+AGE+EducationLevel+
                Employment_Status+Scale_of_Incomes+Settlement_size+
                Etnic_Group+PostMaterialist+Interest_in_Politics,
              data=df3)
modelo4bb <- lm(MR1 ~ SEX+AGE+EducationLevel+
                 Employment_Status+Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialist+Interest_in_Politics,
               data=df4)
modelo5bb <- lm(MR1 ~ SEX+AGE+EducationLevel+
                 Employment_Status+Scale_of_Incomes+Settlement_size+
                 Etnic_Group+PostMaterialist+Interest_in_Politics,
               data=df5)
summary(modelo1)
summary(modelo1bb)# pra testar

b1b <- plot_cap(modelo1bb, condition = c("PostMaterialist"))
b2b <- plot_cap(modelo2bb, condition = c("PostMaterialist"))
b3b <- plot_cap(modelo3bb, condition = c("PostMaterialist"))
b4b <- plot_cap(modelo4bb, condition = c("PostMaterialist"))
b5b <- plot_cap(modelo5bb, condition = c("PostMaterialist"))

grid.arrange(b1b,b2b,b3b,b4b,b5b, nrow=2)
summary(df1$Settlement_size)
summary(df1$Interest_in_Politics)




grid.arrange(b1bb,b2bb,b3bb,b4bb,b5bb, nrow=2)





b1bb <- plot_cap(modelo1bb, condition = c("Settlement_size"))
b2bb <- plot_cap(modelo2bb, condition = c("Settlement_size"))
b3bb <- plot_cap(modelo3bb, condition = c("Settlement_size"))
b4bb <- plot_cap(modelo4bb, condition = c("Settlement_size"))
b5bb <- plot_cap(modelo5bb, condition = c("Settlement_size"))

grid.arrange(b1bb,b2bb,b3bb,b4bb,b5bb, nrow=2)


df1$Interest_inPolitics <- as.factor(df1$Interest_in_Politics)
df2$Interest_inPolitics <- as.factor(df2$Interest_in_Politics)
df3$Interest_inPolitics <- as.factor(df3$Interest_in_Politics)
df4$Interest_inPolitics <- as.factor(df4$Interest_in_Politics)
df5$Interest_inPolitics <- as.factor(df5$Interest_in_Politics)

modelo1ba <- lm(MR1 ~ SEX+AGE+EducationLevel+
                  Employment_Status+Scale_of_Incomes+SettlementSize+
                  Etnic_Group+PostMaterialistIndex+Interest_inPolitics,
                data=df1)
modelo2ba <- lm(MR1 ~ SEX+AGE+EducationLevel+
                  Employment_Status+Scale_of_Incomes+SettlementSize+
                  Etnic_Group+PostMaterialistIndex+Interest_inPolitics,
                data=df2)
modelo3ba<- lm(MR1 ~ SEX+AGE+EducationLevel+
                 Employment_Status+Scale_of_Incomes+SettlementSize+
                 Etnic_Group+PostMaterialistIndex+Interest_inPolitics,
               data=df3)
modelo4ba <- lm(MR1 ~ SEX+AGE+EducationLevel+
                  Employment_Status+Scale_of_Incomes+SettlementSize+
                  Etnic_Group+PostMaterialistIndex+Interest_inPolitics,
                data=df4)
modelo5ba <- lm(MR1 ~ SEX+AGE+EducationLevel+
                  Employment_Status+Scale_of_Incomes+SettlementSize+
                  Etnic_Group+PostMaterialistIndex+Interest_inPolitics,
                data=df5)

b1bb <- plot_cap(modelo1ba, condition = c("Interest_inPolitics"))
b2bb <- plot_cap(modelo2ba, condition = c("Interest_inPolitics"))
b3bb <- plot_cap(modelo3ba, condition = c("Interest_inPolitics"))
b4bb <- plot_cap(modelo4ba, condition = c("Interest_inPolitics"))
b5bb <- plot_cap(modelo5ba, condition = c("Interest_inPolitics"))

grid.arrange(b1bb,b2bb,b3bb,b4bb,b5bb, nrow=2)
