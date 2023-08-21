#afc só Brasil
#rodar script 1 e script 2 paper anpocs primeiro-ok
df11 <- moreno_clivagem_brasil1991[,26:42]
df12 <- moreno_clivagem_brasil1997[,26:41]
df13 <- moreno_clivagem_brasil2006[,26:40]
df14 <- moreno_clivagem_brasil2014[,26:40]
df15 <- moreno_clivagem_brasil2018[,26:42]
library(lavaan)
library(lavaanPlot)

# primeiro a confirmação  ####

model<-'
fundamentalista1991=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit1<-cfa(model,data=df11)
print(fitMeasures(fit1, c("gfi")))#acima de 0.95
summary(fit1,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = 
                                              "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))


model<-'
fundamentalista1997=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit2<-cfa(model,data=df12)
print(fitMeasures(fit2, c("gfi")))#acima de 0.95
summary(fit2,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit2, node_options = list(shape = "box", fontname = 
                                               "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))

model<-'
fundamentalista2006=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit3<-cfa(model,data=df13)
print(fitMeasures(fit3, c("gfi")))#acima de 0.95
#summary(fit3,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit3, node_options = list(shape = "box", fontname = 
                                               "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))

model<-'
fundamentalista2014=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit4<-cfa(model,data=df14)
print(fitMeasures(fit4, c("gfi")))#acima de 0.95
#summary(fit4,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit4, node_options = list(shape = "box", fontname = 
                                               "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))
model<-'
fundamentalista2018=~E018+F028+F034+F063+F118+F120+F121+G006
'
fit5<-cfa(model,data=df15)
print(fitMeasures(fit5, c("gfi")))#acima de 0.95
#summary(fit5,fit.measures=TRUE,standardized=TRUE)
lavaanPlot(model = fit5, node_options = list(shape = "box", fontname = 
                                               "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
             TRUE,stars = c(""))


#ok deu agora outra ideia- ampliar ####
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
print(fitMeasures(fit11, c("gfi")))#acima de 0.95
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
print(fitMeasures(fit12, c("gfi")))#acima de 0.95
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
print(fitMeasures(fit13, c("gfi")))#acima de 0.95
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
print(fitMeasures(fit14, c("gfi")))#acima de 0.95
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
print(fitMeasures(fit15, c("gfi")))#acima de 0.95
summary(fit15,fit.measures=TRUE,standardized=TRUE)#comparar com o original
lavaanPlot(model = fit15, node_options = list(shape = "box", fontname = 
                                                "Helvetica"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = c("regression"))

