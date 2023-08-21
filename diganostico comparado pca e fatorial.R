library(psych)
scree(moreno_clivagem_brasil1991[,26:42])
scree(moreno_clivagem_brasil1997[,26:41])
scree(moreno_clivagem_brasil2006[,26:40])
scree(moreno_clivagem_brasil2014[,26:40])
scree(moreno_clivagem_brasil2018[,26:42])


fa.plot(pf3.1.3, cut = 0.9)

#TRI
library(mirt)
mirtCluster()
mod1 <- mirt(moreno_clivagem_brasil1991[,26:42], na.rm=TRUE)
summary(mod1, suppress = .31)
M2(mod1, na.rm=TRUE)
mod2 <- mirt(moreno_clivagem_brasil1997[,26:41], na.rm=TRUE)
summary(mod2, suppress = .31)
M2(mod2, na.rm=TRUE)
mod3<- mirt(moreno_clivagem_brasil2006[,26:40], na.rm=TRUE)
summary(mod3, suppress = .31)
M2(mod3, na.rm=TRUE)
mod4 <- mirt(moreno_clivagem_brasil2014[,26:40], na.rm=TRUE)
summary(mod4, suppress = .31)
M2(mod4, na.rm=TRUE)
mod5 <- mirt(moreno_clivagem_brasil2018[,26:42], na.rm=TRUE)
summary(mod5, suppress = .31)
M2(mod5, na.rm=TRUE)


df91 <-moreno_clivagem_brasil1991[,26:42]
df97 <-moreno_clivagem_brasil1997[,26:41]
df2006<-moreno_clivagem_brasil2006[,26:40]
df2014<-moreno_clivagem_brasil2014[,26:40]
df2018<-moreno_clivagem_brasil2018[,26:42]
library(mice)
library(labelled)
df91 <- remove_labels(df91)
imp <- mice(df91, seed=23109)# repetir o mesmo seed usado na argentina(e em qualquer país)
df91 <-complete(imp, 1)
rm(imp)
mod1 <- mirt(df91)
summary(mod1)
M2(mod1)

df97 <- remove_labels(df97)
imp <- mice(df97, seed=23109)# repetir o mesmo seed usado na argentina(e em qualquer país)
df97 <-complete(imp, 1)
rm(imp)
mod2 <- mirt(df97)
summary(mod2)
M2(mod2)

df2006 <- remove_labels(df2006)
imp <- mice(df2006, seed=23109)# repetir o mesmo seed usado na argentina(e em qualquer país)
df2006 <-complete(imp, 1)
rm(imp)
mod3 <- mirt(df2006)
summary(mod3)
M2(mod3)

df2014 <- remove_labels(df2014)
imp <- mice(df2014, seed=23109)# repetir o mesmo seed usado na argentina(e em qualquer país)
df2014 <-complete(imp, 1)
rm(imp)
mod4 <- mirt(df2014)
summary(mod4)
M2(mod4)

df2018 <- remove_labels(df2018)
imp <- mice(df2018, seed=23109)# repetir o mesmo seed usado na argentina(e em qualquer país)
df2018 <-complete(imp, 1)
rm(imp)
mod5 <- mirt(df2018)
summary(mod5)
M2(mod5)

