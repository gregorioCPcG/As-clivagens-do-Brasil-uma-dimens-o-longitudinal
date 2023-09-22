#####################

#WVS1991 https://www.worldvaluessurvey.org/WVSDocumentationWV2.jsp (SELECT BRAZIL 1991)
#WVS1997 https://www.worldvaluessurvey.org/WVSDocumentationWV3.jsp (SELECT BRAZIL 1997)
#WVS2006 https://www.worldvaluessurvey.org/WVSDocumentationWV5.jsp (SELECT BRAZIL 2006)
#WVS2014 https://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp (SELECT BRAZIL 2014)
#WVS2018 https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp (SELECT BRAZIL 2018)

#####################

###1991###


##Install packages

library(readxl)
library(dplyr)
library(lavaan)
library(ggplot2)
library(semPlot)
library(psych)
library(GPArotation)
library(mirt)
library(ggrepel)

##Download Dataframe

Dataframe <- read_excel("~/Banco de Dados/WVS/Dados/F00008257-WV2_Data_Brazil_Excel_v1.6.xlsx")

##Check variables structure

#Jobs for man

table(Dataframe$`V128: Jobs scarce: Men should have more right to a job than women`)

Dataframe$V128 <- recode(Dataframe$`V128: Jobs scarce: Men should have more right to a job than women`, `1` = 3, `3` = 1)

Dataframe$V128 <- ifelse(Dataframe$V128 <= -1, NA, Dataframe$V128)

table(Dataframe$V128)

Dataframe$V1 <- Dataframe$V128

#jobs for own nationality

table(Dataframe$`V130: Jobs scarce: Employers should give priority to (nation) people than immigrants`)

Dataframe$V130 <- recode(Dataframe$`V130: Jobs scarce: Employers should give priority to (nation) people than immigrants`, `1` = 3, `3` = 1)

Dataframe$V130 <- ifelse(Dataframe$V130 <= -1, NA, Dataframe$V130)

table(Dataframe$V130)

Dataframe$V2 <- Dataframe$V130

#Political reform

table(Dataframe$`V339: Political reform is moving too rapidly`)

Dataframe$V339 <- recode(Dataframe$`V339: Political reform is moving too rapidly`, `1` = 5, `2` = 4, `4` = 2, `5` = 1)

Dataframe$V339 <- ifelse(Dataframe$V339 <= -1, NA, Dataframe$V339)

table(Dataframe$V339)

#Respect for authority

table(Dataframe$`V268: Future changes: Greater respect for authority`)

Dataframe$V268 <- recode(Dataframe$`V268: Future changes: Greater respect for authority`, `1` = 3, `3` = 1)

Dataframe$V268 <- ifelse(Dataframe$V268 <= -1, NA, Dataframe$V268)

table(Dataframe$V268)

Dataframe$V7 <- Dataframe$V268

#Importance of God

table(Dataframe$`V176: How important is God in your life`)

Dataframe$V176 <- ifelse(Dataframe$`V176: How important is God in your life` <= -1, NA, Dataframe$`V176: How important is God in your life`)

table(Dataframe$V176)

Dataframe$V3 <- Dataframe$V176

#Homosexuality

table(Dataframe$`V307: Justifiable: Homosexuality`)

Dataframe$V307 <- ifelse(Dataframe$`V307: Justifiable: Homosexuality` <= -1, NA, Dataframe$`V307: Justifiable: Homosexuality`)

table(Dataframe$V307)

Dataframe$V8 <- Dataframe$V307

#Prostitution

table(Dataframe$`V308: Justifiable: Prostitution`)

Dataframe$V308 <- ifelse(Dataframe$`V308: Justifiable: Prostitution` <= -1, NA, Dataframe$`V308: Justifiable: Prostitution`)

table(Dataframe$V308)

Dataframe$V9 <- Dataframe$V308

#Abortion

table(Dataframe$`V309: Justifiable: Abortion`)

Dataframe$V309 <- ifelse(Dataframe$`V309: Justifiable: Abortion` <= -1, NA, Dataframe$`V309: Justifiable: Abortion`)

table(Dataframe$V309)

Dataframe$V10 <- Dataframe$V309

#Divorce

table(Dataframe$`V310: Justifiable: Divorce`)

Dataframe$V310 <- ifelse(Dataframe$`V310: Justifiable: Divorce` <= -1, NA, Dataframe$`V310: Justifiable: Divorce`)

table(Dataframe$V310)

Dataframe$V11 <- Dataframe$V310

#Fighting against police

table(Dataframe$`V311: Justifiable: Fighting with the police`)

Dataframe$V311 <- ifelse(Dataframe$`V311: Justifiable: Fighting with the police` <= -1, NA, Dataframe$`V311: Justifiable: Fighting with the police`)

table(Dataframe$V311)

#National pride

table(Dataframe$`V322: How proud of nationality`)

Dataframe$V322 <- recode(Dataframe$`V322: How proud of nationality`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V322 <- ifelse(Dataframe$V322 <= -1, NA, Dataframe$V322)

table(Dataframe$V322)

Dataframe$V12 <- Dataframe$V322

#Societal change

table(Dataframe$`V249: Basic kinds of attitudes concerning society`)

Dataframe$V249 <- ifelse(Dataframe$`V249: Basic kinds of attitudes concerning society` <= -1, NA, Dataframe$`V249: Basic kinds of attitudes concerning society`)

table(Dataframe$V249)

#Income equality

table(Dataframe$`V250: Income equality`)

Dataframe$V250 <- ifelse(Dataframe$`V250: Income equality` <= -1, NA, Dataframe$`V250: Income equality`)

table(Dataframe$V250)

Dataframe$V4 <- Dataframe$V250

#Private/public ownership

table(Dataframe$`V251: Private vs state ownership of business`)

Dataframe$V251 <- ifelse(Dataframe$`V251: Private vs state ownership of business` <= -1, NA, Dataframe$`V251: Private vs state ownership of business`)

table(Dataframe$V251)

Dataframe$V5 <- Dataframe$V251

#Government responsibility

table(Dataframe$`V252: Government responsibility`)

Dataframe$V252 <- ifelse(Dataframe$`V252: Government responsibility` <= -1, NA, Dataframe$`V252: Government responsibility`)

table(Dataframe$V252)

Dataframe$V13 <- Dataframe$V252

#Competition

table(Dataframe$`V254: Competition good or harmful`)

Dataframe$V254 <- ifelse(Dataframe$`V254: Competition good or harmful` <= -1, NA, Dataframe$`V254: Competition good or harmful`)

table(Dataframe$V254)

Dataframe$V6 <- Dataframe$V254

#Government open

table(Dataframe$`V336: Our government should be made much more open to the public`)

Dataframe$V336 <- recode(Dataframe$`V336: Our government should be made much more open to the public`, `1` = 5, `2` = 4, `4` = 2, `5` = 1)

Dataframe$V336 <- ifelse(Dataframe$V336 <= -1, NA, Dataframe$V336)

table(Dataframe$V336)

#Ideology

table(Dataframe$`V248: Self positioning in political scale`)

Dataframe$V248 <- ifelse(Dataframe$`V248: Self positioning in political scale` <= -1, NA, Dataframe$`V248: Self positioning in political scale`)

table(Dataframe$V248)

Dataframe$V14 <- Dataframe$V248

#Post-materialism

table(Dataframe$`Y001: Post-Materialist index 12-item`)

Dataframe$Y001 <- ifelse(Dataframe$`Y001: Post-Materialist index 12-item` <= -1, NA, Dataframe$`Y001: Post-Materialist index 12-item`)

table(Dataframe$Y001)

Dataframe$V16 <- Dataframe$Y001

#Party

table(Dataframe$`V351: Which party would you vote for: first choice`)

Dataframe$V351 <- ifelse(Dataframe$`V351: Which party would you vote for: first choice` == 76002, 1, 0)

table(Dataframe$V351)

Dataframe$V15 <- Dataframe$V351

Dataframe$P1 <- Dataframe$`V351: Which party would you vote for: first choice`

#Party2

table(Dataframe$`V352: Which party would you vote for: second choice`)

Dataframe$V352 <- ifelse(Dataframe$`V352: Which party would you vote for: second choice` == 76002, 1, 0)

table(Dataframe$V352)

#Church attendance

table(Dataframe$`V147: How often do you attend religious services`)

Dataframe$V147 <- recode(Dataframe$`V147: How often do you attend religious services`, `1` = 8, `2` = 7, `3` = 6, `4` = 5, `5` = 4, `6` = 3, `7` = 2, `8` = 1)

Dataframe$V18 <- Dataframe$V147

#Education

table(Dataframe$`V375: Highest educational level attained`)

Dataframe$V375 <- ifelse(Dataframe$`V375: Highest educational level attained` <= -1, NA, Dataframe$`V375: Highest educational level attained`)

table(Dataframe$V375)

Dataframe$V17 <- Dataframe$V375

#Age

table(Dataframe$`V355: Age`)

Dataframe$V355 <- Dataframe$`V355: Age`

Dataframe$V19 <- Dataframe$V355

#Size town

table(Dataframe$`V368: Size of town`)

Dataframe$V368 <- Dataframe$`V368: Size of town`

Dataframe$V20 <- Dataframe$V368

#Sex

table(Dataframe$`V353: Sex`)

Dataframe$V353 <- Dataframe$`V353: Sex`

Dataframe$V21 <- Dataframe$V353

#Interest in politics

table(Dataframe$`V241: Interest in politics`)

Dataframe$V241<- recode(Dataframe$`V241: Interest in politics`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V241 <- ifelse(Dataframe$V241 <= -1, NA, Dataframe$V241)

table(Dataframe$V241)

Dataframe$V22 <- Dataframe$V241

#Income

table(Dataframe$`V363: Scale of incomes`)

Dataframe$V363 <- ifelse(Dataframe$`V363: Scale of incomes` <= -1, NA, Dataframe$`V363: Scale of incomes`)

table(Dataframe$V363)

Dataframe$V23 <- Dataframe$V363

#Race

table(Dataframe$`V369: Ethnic group`)

Dataframe$V369 <- ifelse(Dataframe$`V369: Ethnic group` == 76001, 1, 0)

table(Dataframe$V369)

Dataframe$V24 <- Dataframe$V369

Dataframe_dimension_reduction <- Dataframe[, c("V128","V130","V176","V249","V250","V251","V254","V268","V307","V308","V309","V310","V311","V322","V336","V339","V252","V248","Y001","V351","V352","V147","P1")]

Dataframe_dimension_reduction_1991 <- Dataframe[, c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13")]

df1991r <- Dataframe[, c("V128","V130","V176","V249","V250","V251","V254","V268","V307","V308","V309","V310","V311","V322","V336","V339","V252","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24")]

##Run PCA

pca_unconstrained <- principal(Dataframe_dimension_reduction[,1:17], nfactors = 3, rotate = "varimax")

print(pca_unconstrained)

##Run FA

fa_unconstrained <- fa(Dataframe_dimension_reduction, nfactors = 1, rotate = "varimax")

print(fa_unconstrained)

fa_constrained <- fa(Dataframe_dimension_reduction[,1:17], nfactors = 1, rotate = "varimax")

scores_Dataframe_dimension_reduction <- factor.scores(Dataframe_dimension_reduction[,1:17],fa_constrained)
scores_Dataframe_dimension_reduction <- scores_Dataframe_dimension_reduction$scores
Dataframe_dimension_reduction <- cbind(Dataframe_dimension_reduction,scores_Dataframe_dimension_reduction)

print(fa_constrained)

summary(Dataframe_dimension_reduction)

#Confirmatory Factor Analysis

modelo_1991 <- 'fundamentalist =~ V176 + V307 + V308 + V309 + V310
authoritarian =~ V128+V130+V254+V268+V311+V322+V336+V339
economic =~ V249+V250+V251+V252'

teste1991 <- sem(modelo_1991, data = Dataframe_dimension_reduction)
summary(teste1991, standardized = TRUE, fit.measures = TRUE)
fitmeasures(teste1991, c("chisq", "df", "pvalue", "cfi","tli", "rmsea","SRMR"))

#Run IRT

irt_unconstrained <- mirt(Dataframe_dimension_reduction, 3)

summary(irt_unconstrained, rotate = 'varimax')

##POSMATERIALISM INDEX

brasil.Y001.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 0| Dataframe_dimension_reduction$Y001 == 1,]

mean(brasil.Y001.0$MR1, na.rm = TRUE)
mean(brasil.Y001.0$V248, na.rm = TRUE)

brasil.Y001.2 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 2| Dataframe_dimension_reduction$Y001 == 3,]

mean(brasil.Y001.2$MR1, na.rm = TRUE)
mean(brasil.Y001.2$V248, na.rm = TRUE)

brasil.Y001.4 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 4| Dataframe_dimension_reduction$Y001 == 5,]

mean(brasil.Y001.4$MR1, na.rm = TRUE)
mean(brasil.Y001.4$V248, na.rm = TRUE)

#PARTY

brasil.V351.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V351 == 1,]

mean(brasil.V351.1$MR1, na.rm = TRUE)
mean(brasil.V351.1$V248, na.rm = TRUE)

brasil.V351.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V351 == 0,]

mean(brasil.V351.0$MR1, na.rm = TRUE)
mean(brasil.V351.0$V248, na.rm = TRUE)

#PARTY2

brasil.V352.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V352 == 1,]

mean(brasil.V352.1$MR1, na.rm = TRUE)
mean(brasil.V352.1$V248, na.rm = TRUE)

brasil.V352.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V352 == 0,]

mean(brasil.V352.0$MR1, na.rm = TRUE)
mean(brasil.V352.0$V248, na.rm = TRUE)

#Church Attendence

brasil.V147.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V147 == 1 | Dataframe_dimension_reduction$V147 == 2 | Dataframe_dimension_reduction$V147 == 3,]

mean(brasil.V147.1$MR1, na.rm = TRUE)
mean(brasil.V147.1$V248, na.rm = TRUE) 

brasil.V147.4 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V147 == 4 | Dataframe_dimension_reduction$V147 == 5,]

mean(brasil.V147.4$MR1, na.rm = TRUE)
mean(brasil.V147.4$V248, na.rm = TRUE)

brasil.V147.6 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V147 == 6 | Dataframe_dimension_reduction$V147 == 7 | Dataframe_dimension_reduction$V147 == 8,]

mean(brasil.V147.6$MR1, na.rm = TRUE)
mean(brasil.V147.6$V248, na.rm = TRUE)

df1991r$V17 <- recode(df1991r$V17, `1` = 1, `3` = 1, `5` = 2, `9` = 3)


bolinhas <- data.frame(
  Fundamentalist = c(0.2363424,-0.0691595,-0.4776807,-0.4391234,0.03003411,-0.1061604,-0.02040333,0.2224926,-0.1215009,-0.3007865,-0.61455148,-0.07084764,0.33864283),
  Category = c("Materialist","Mixed","Postmaterialist","PT (first vote)","Non-PT (first vote)","PT (second vote)","Non-PT (second vote)","High Church Att.","Medium Church Att.","Low Church Att.","High Education", "Medium Education","Low Education"),
  Ideology = c(6.16129,5.345251,4.329268,3.766667,5.683093,4.597403,5.499297,5.791724,5.344828,5.075,4.874459,5.216646,6.256250)
)


p <- ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "1991")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))
p

#Partys

table(Dataframe$P1)

media_1991 <- aggregate(cbind(MR1, V248) ~ P1, data = Dataframe_dimension_reduction, FUN = mean)

bolinhas <- data.frame(
  Fundamentalist = c(0.09792843,-0.47997139,-0.57192123,-0.27586527),
  Category = c("MDB","PT","PSDB","PDT"),
  Ideology = c(6.096774,3.809249,5.378378,4.267857)
)

ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "1991")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))

###1997###

##Download Dataframe

Dataframe <- read_excel("~/Banco de Dados/WVS/Dados/F00008090-WV3_Data_Brazil_Excel_v20221107.xlsx")

##Check variables structure

#Jobs for man

table(Dataframe$`V61: Jobs scarce: Men should have more right to a job than women`)

Dataframe$V61 <- recode(Dataframe$`V61: Jobs scarce: Men should have more right to a job than women`, `1` = 3, `3` = 1)

Dataframe$V61 <- ifelse(Dataframe$V61 <= -1, NA, Dataframe$V61)

table(Dataframe$V61)

Dataframe$V1 <- Dataframe$V61

#jobs for own nationality

table(Dataframe$`V63: Jobs scarce: Employers should give priority to (nation) people than immigrants`)

Dataframe$V63 <- recode(Dataframe$`V63: Jobs scarce: Employers should give priority to (nation) people than immigrants`, `1` = 3, `3` = 1)

Dataframe$V63 <- ifelse(Dataframe$V63 <= -1, NA, Dataframe$V63)

table(Dataframe$V63)

Dataframe$V2 <- Dataframe$V63

#Importance of God

table(Dataframe$`V190: Importance of God in your life`)

Dataframe$V190 <- ifelse(Dataframe$`V190: Importance of God in your life` <= -1, NA, Dataframe$`V190: Importance of God in your life`)

table(Dataframe$V190)

Dataframe$V3 <- Dataframe$V190

#Societal change

table(Dataframe$`V124: Basic kinds of attitudes concerning society`)

Dataframe$V124 <- ifelse(Dataframe$`V124: Basic kinds of attitudes concerning society` <= -1, NA, Dataframe$`V124: Basic kinds of attitudes concerning society`)

table(Dataframe$V124)

#Income equality

table(Dataframe$`V125: Income equality`)

Dataframe$V125 <- ifelse(Dataframe$`V125: Income equality` <= -1, NA, Dataframe$`V125: Income equality`)

table(Dataframe$V125)

Dataframe$V4 <- Dataframe$V125

#Private/public ownership

table(Dataframe$`V126: Private vs state ownership of business`)

Dataframe$V126 <- ifelse(Dataframe$`V126: Private vs state ownership of business` <= -1, NA, Dataframe$`V126: Private vs state ownership of business`)

table(Dataframe$V126)

Dataframe$V5 <- Dataframe$V126

#Competition

table(Dataframe$`V128: Competition good or harmful`)

Dataframe$V128 <- ifelse(Dataframe$`V128: Competition good or harmful` <= -1, NA, Dataframe$`V128: Competition good or harmful`)

table(Dataframe$V128)

Dataframe$V6 <- Dataframe$V128

#Respect for authority

table(Dataframe$`V114: Future changes: Greater respect for authority`)

Dataframe$V114 <- recode(Dataframe$`V114: Future changes: Greater respect for authority`, `1` = 3, `3` = 1)

Dataframe$V114 <- ifelse(Dataframe$V114 <= -1, NA, Dataframe$V114)

table(Dataframe$V114)

Dataframe$V7 <- Dataframe$V114

#Homosexuality

table(Dataframe$`V197: Justifiable: Homosexuality`)

Dataframe$V197 <- ifelse(Dataframe$`V197: Justifiable: Homosexuality` <= -1, NA, Dataframe$`V197: Justifiable: Homosexuality`)

table(Dataframe$V197)

Dataframe$V8 <- Dataframe$V197

#Prostitution

table(Dataframe$`V198: Justifiable: Prostitution`)

Dataframe$V198 <- ifelse(Dataframe$`V198: Justifiable: Prostitution` <= -1, NA, Dataframe$`V198: Justifiable: Prostitution`)

table(Dataframe$V198)

Dataframe$V9 <- Dataframe$V198

#Abortion

table(Dataframe$`V199: Justifiable: Abortion`)

Dataframe$V199 <- ifelse(Dataframe$`V199: Justifiable: Abortion` <= -1, NA, Dataframe$`V199: Justifiable: Abortion`)

table(Dataframe$V199)

Dataframe$V10 <- Dataframe$V199

#Divorce

table(Dataframe$`V200: Justifiable: Divorce`)

Dataframe$V200 <- ifelse(Dataframe$`V200: Justifiable: Divorce` <= -1, NA, Dataframe$`V200: Justifiable: Divorce`)

table(Dataframe$V200)

Dataframe$V11 <- Dataframe$V200

#National pride

table(Dataframe$`V205: How proud of nationality`)

Dataframe$V205 <- recode(Dataframe$`V205: How proud of nationality`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V205 <- ifelse(Dataframe$V205 <= -1, NA, Dataframe$V205)

Dataframe$V205 <- ifelse(Dataframe$V205 >= 5, NA, Dataframe$V205)

table(Dataframe$V205)

Dataframe$V12 <- Dataframe$V205

#Government responsibility

table(Dataframe$`V127: Government responsibility`)

Dataframe$V127 <- ifelse(Dataframe$`V127: Government responsibility` <= -1, NA, Dataframe$`V127: Government responsibility`)

table(Dataframe$V127)

Dataframe$V13 <- Dataframe$V127

#Immigrants policy

table(Dataframe$`V134: Immigrant policy`)

Dataframe$V134 <- ifelse(Dataframe$`V134: Immigrant policy` <= -1, NA, Dataframe$`V134: Immigrant policy`)

table(Dataframe$V134)

#Strong leader

table(Dataframe$`V154: Political system: Having a strong leader`)

Dataframe$V154 <- recode(Dataframe$`V154: Political system: Having a strong leader`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V154 <- ifelse(Dataframe$V154 <= -1, NA, Dataframe$V154)

table(Dataframe$V154)

#Experts

table(Dataframe$`V155: Political system: Having experts make decisions`)

Dataframe$V155 <- recode(Dataframe$`V155: Political system: Having experts make decisions`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V155 <- ifelse(Dataframe$V155 <= -1, NA, Dataframe$V155)

table(Dataframe$V155)

#Army rule

table(Dataframe$`V156: Political system: Having the army rule`)

Dataframe$V156 <- recode(Dataframe$`V156: Political system: Having the army rule`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V156 <- ifelse(Dataframe$V156 <= -1, NA, Dataframe$V156)

table(Dataframe$V156)

#Democracy

table(Dataframe$`V157: Political system: Having a democratic political system`)

Dataframe$V157 <- recode(Dataframe$`V157: Political system: Having a democratic political system`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V157 <- ifelse(Dataframe$V157 <= -1, NA, Dataframe$V157)

table(Dataframe$V157)

#Order vs Freedom

table(Dataframe$`V159: Government order vs. freedom`)

Dataframe$V159 <- ifelse(Dataframe$`V159: Government order vs. freedom` <= -1, NA, Dataframe$`V159: Government order vs. freedom`)

table(Dataframe$V159)

#Democracy economy

table(Dataframe$`V160: In democracy, the economic system runs badly`)

Dataframe$V160 <- recode(Dataframe$`V160: In democracy, the economic system runs badly`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V160 <- ifelse(Dataframe$V160 <= -1, NA, Dataframe$V160)

table(Dataframe$V160)

#Democracy Stability

table(Dataframe$`V161: Democracies are indecisive and have too much squabbling`)

Dataframe$V161 <- recode(Dataframe$`V161: Democracies are indecisive and have too much squabbling`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V161 <- ifelse(Dataframe$V161 <= -1, NA, Dataframe$V161)

table(Dataframe$V161)

#Democracy Order

table(Dataframe$`V162: Democracies aren´t good at maintaining order`)

Dataframe$V162 <- recode(Dataframe$`V162: Democracies aren´t good at maintaining order`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V162 <- ifelse(Dataframe$V162 <= -1, NA, Dataframe$V162)

table(Dataframe$V162)

#Democracy Better

table(Dataframe$`V163: Democracy may have problems but is better`)

Dataframe$V163 <- recode(Dataframe$`V163: Democracy may have problems but is better`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V163 <- ifelse(Dataframe$V163 <= -1, NA, Dataframe$V163)

table(Dataframe$V163)

#Ideology

table(Dataframe$`V123: Self positioning in political scale`)

Dataframe$V123 <- ifelse(Dataframe$`V123: Self positioning in political scale` <= -1, NA, Dataframe$`V123: Self positioning in political scale`)

table(Dataframe$V123)

Dataframe$V14 <- Dataframe$V123

#Post-materialism

table(Dataframe$`Y001: Post-Materialist index 12-item`)

Dataframe$Y001 <- ifelse(Dataframe$`Y001: Post-Materialist index 12-item` <= -1, NA, Dataframe$`Y001: Post-Materialist index 12-item`)

table(Dataframe$Y001)

Dataframe$V16 <- Dataframe$Y001

#Party

table(Dataframe$`V210: Which party would you vote for: First choice`)

Dataframe$V210 <- ifelse(Dataframe$`V210: Which party would you vote for: First choice` == 76002, 1, 0)

table(Dataframe$V210)

Dataframe$V15 <- Dataframe$V210

Dataframe$P1 <- Dataframe$`V210: Which party would you vote for: First choice`

#Party2

table(Dataframe$`V211: Which party would you vote for: Second choice`)

Dataframe$V211 <- ifelse(Dataframe$`V211: Which party would you vote for: Second choice` == 76002, 1, 0)

table(Dataframe$V211)

#PartyNever

table(Dataframe$`V212: Party that would never vote`)

Dataframe$V212 <- ifelse(Dataframe$`V212: Party that would never vote` == 76002, 1, 0)

table(Dataframe$V212)

#Church attendance

table(Dataframe$`V181: How often do you attend religious services`)

Dataframe$V181 <- recode(Dataframe$`V181: How often do you attend religious services`, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1)

Dataframe$V181 <- ifelse(Dataframe$V181 <= -1, NA, Dataframe$V181)

table(Dataframe$V181)

Dataframe$V18 <- Dataframe$V181

#Education

table(Dataframe$`V217: Highest educational level attained`)

Dataframe$V217 <- Dataframe$`V217: Highest educational level attained`

Dataframe$V17 <- Dataframe$V217

#Age

table(Dataframe$`V216: Age`)

Dataframe$V216 <- Dataframe$`V216: Age`

Dataframe$V19 <- Dataframe$V216

#Size town

table(Dataframe$`V232: Size of town`)

Dataframe$V232 <- Dataframe$`V232: Size of town`

Dataframe$V20 <- Dataframe$V232

#Sex

table(Dataframe$`V214: Sex`)

Dataframe$V214 <- Dataframe$`V214: Sex`

Dataframe$V21 <- Dataframe$V214

#Interest in politics

table(Dataframe$`V117: Interest in politics`)

Dataframe$V117 <- recode(Dataframe$`V117: Interest in politics`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V117 <- ifelse(Dataframe$V117 <= -1, NA, Dataframe$V117)

table(Dataframe$V117)

Dataframe$V22 <- Dataframe$V117

#Income

table(Dataframe$`V227CS: Income (CS)`)

Dataframe$V227 <- ifelse(Dataframe$`V227CS: Income (CS)` <= -1, NA, Dataframe$`V227CS: Income (CS)`)

table(Dataframe$V227)

Dataframe$V23 <- Dataframe$V227

#Race

table(Dataframe$`V233: Ethnic group`)

Dataframe$V233 <- ifelse(Dataframe$`V233: Ethnic group` == 76001, 1, 0)

table(Dataframe$V233)

Dataframe$V24 <- Dataframe$V233

Dataframe_dimension_reduction <- Dataframe[, c("V61","V63","V190","V124","V125","V126","V128","V114","V197","V198","V199","V200","V205","V127","V134","V154","V155","V156","V157","V159","V160","V161","V162","V163","V123","Y001","V210","V211","V212","V181")]

Dataframe_dimension_reduction_1997 <- Dataframe[, c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13")]

df1997r <- Dataframe[, c("V61","V63","V190","V124","V125","V126","V128","V114","V197","V198","V199","V200","V205","V127","V134","V154","V155","V156","V157","V159","V160","V161","V162","V163","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24")]

##Run PCA

Dataframe_dimension_reduction <- Dataframe[, c("V61","V63","V190","V124","V125","V126","V128","V114","V197","V198","V199","V200","V205","V127","V134","V154","V155","V156","V157","V159","V160","V161","V162","V163","V123","Y001","V210","V211","V212","V181","P1")]

pca_unconstrained <- principal(Dataframe_dimension_reduction, nfactors = 3, rotate = "varimax")

print(pca_unconstrained)

##Run FA

fa_unconstrained <- fa(Dataframe_dimension_reduction, nfactors = 3, rotate = "varimax")

print(fa_unconstrained)

fa_constrained <- fa(Dataframe_dimension_reduction[,1:24], nfactors = 3, rotate = "varimax")

scores_Dataframe_dimension_reduction <- factor.scores(Dataframe_dimension_reduction[,1:24],fa_constrained)
scores_Dataframe_dimension_reduction <- scores_Dataframe_dimension_reduction$scores
Dataframe_dimension_reduction <- cbind(Dataframe_dimension_reduction,scores_Dataframe_dimension_reduction)


print(fa_constrained)

#CFA

modelo_1997 <- 'fundamentalist =~ V190 + V197+V198+V199+V200
authoritarian =~ V61+V63+V124+V114+V205+V134+V154+V155+V156+V157+V159+V160+V161+V162+V163
economic =~ V125+V126+V127+V128'

teste1997 <- sem(modelo_1997, data = Dataframe_dimension_reduction)
summary(teste1997, standardized = TRUE, fit.measures = TRUE)
fitmeasures(teste1997, c("chisq", "df", "pvalue", "cfi","tli", "rmsea","SRMR"))

#Run IRT

irt_unconstrained <- mirt(Dataframe_dimension_reduction, 4)

summary(irt_unconstrained, rotate = 'varimax'

##POSMATERIALISM INDEX

brasil.Y001.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 0| Dataframe_dimension_reduction$Y001 == 1,]

mean(brasil.Y001.0$MR1, na.rm = TRUE)
mean(brasil.Y001.0$V123, na.rm = TRUE)

brasil.Y001.2 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 2| Dataframe_dimension_reduction$Y001 == 3,]

mean(brasil.Y001.2$MR1, na.rm = TRUE)
mean(brasil.Y001.2$V123, na.rm = TRUE)

brasil.Y001.4 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 4| Dataframe_dimension_reduction$Y001 == 5,]

mean(brasil.Y001.4$MR1, na.rm = TRUE)
mean(brasil.Y001.4$V123, na.rm = TRUE)

#PARTY

brasil.V210.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V210 == 1,]

mean(brasil.V210.1$MR1, na.rm = TRUE)
mean(brasil.V210.1$V123, na.rm = TRUE)

brasil.210.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V210 == 0,]

mean(brasil.210.0$MR1, na.rm = TRUE)
mean(brasil.210.0$V123, na.rm = TRUE)

#PARTY2

brasil.V211.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V211 == 1,]

mean(brasil.V211.1$MR1, na.rm = TRUE)
mean(brasil.V211.1$V123, na.rm = TRUE)

brasil.V211.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V211 == 0,]

mean(brasil.V211.0$MR1, na.rm = TRUE)
mean(brasil.V211.0$V123, na.rm = TRUE)

#ANTIPT

brasil.V212.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V212 == 1,]

mean(brasil.V212.1$MR1, na.rm = TRUE)
mean(brasil.V212.1$V123, na.rm = TRUE)

#Church Attendence

brasil.V181.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V181 == 1 | Dataframe_dimension_reduction$V181 == 2,]

mean(brasil.V181.1$MR1, na.rm = TRUE)
mean(brasil.V181.1$V123, na.rm = TRUE) 

brasil.V181.3 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V181 == 3 | Dataframe_dimension_reduction$V181 == 4 | Dataframe_dimension_reduction$V181 == 5,]

mean(brasil.V181.3$MR1, na.rm = TRUE)
mean(brasil.V181.3$V123, na.rm = TRUE)

brasil.V181.6 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V181 == 6 | Dataframe_dimension_reduction$V181 == 7,]

mean(brasil.V181.6$MR1, na.rm = TRUE)
mean(brasil.V181.6$V123, na.rm = TRUE)

df1997r$V17 <- recode(df1997r$V17, `1` = 1, `2` = 1, `3` = 1, `4` = 2, `5` = 2,
                      `6` = 2, `7` = 3, `8` = 3, `9` = 3)


bolinhas <- data.frame(
  Fundamentalist = c(0.2340066,-0.03918892,-0.4795985,-0.2213392,0.04466119,0.03017604,-0.03094939,0.1032098,0.1832185,-0.09004045,-0.5206493,-0.4392755,0.1000833,0.2188241),
  Category = c("Materialist","Mixed","Postmaterialist","PT (first vote)","Non-PT (first vote)","PT (second vote)","Non-PT (second vote)","Anti-PT","High Church Att.","Medium Church Att.","Low Church Att.","High Education", "Medium Education","Low Education"),
  Ideology = c(5.992453,6.076285,4.722689,4.942149,6.208719,6,5.88914,6.533981,6.201681,5.795775,5.197183,5.50000,5.71159,6.50000)
)


p <- ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "1997")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))

p

#Partys

table(Dataframe$P1)

media_1997 <- aggregate(cbind(MR1, V123) ~ P1, data = Dataframe_dimension_reduction, FUN = mean)

bolinhas <- data.frame(
  Fundamentalist = c(0.14496338,-0.22269937,0.08987986,0.10747506,0.17719653,
                     -0.03360972,0.15667339),
  Category = c("MDB","PT","PSDB","PDT","PPB","PFL","PTB"),
  Ideology = c(6.357664,4.916300,6.858974,5.179487,7.000000,6.826087,6.462963)
)

ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "1997")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))


###2006###

##Download Dataframe

Dataframe <- read_excel("~/Banco de Dados/WVS/Dados/F00007825-WV5_Data_Brazil_Excel_v20201117.xlsx")

##Check variables structure

#Jobs for man

table(Dataframe$`V44: Jobs scarce: Men should have more right to a job than women`)

Dataframe$V44 <- recode(Dataframe$`V44: Jobs scarce: Men should have more right to a job than women`, `1` = 3, `3` = 1)

Dataframe$V44 <- ifelse(Dataframe$V44 <= -1, NA, Dataframe$V44)

table(Dataframe$V44)

Dataframe$V1 <- Dataframe$V44

#jobs for own nationality

table(Dataframe$`V45: Jobs scarce: Employers should give priority to (nation) people than immigrants`)

Dataframe$V45 <- recode(Dataframe$`V45: Jobs scarce: Employers should give priority to (nation) people than immigrants`, `1` = 3, `3` = 1)

Dataframe$V45 <- ifelse(Dataframe$V45 <= -1, NA, Dataframe$V45)

table(Dataframe$V45)

Dataframe$V2 <- Dataframe$V45

#Importance of God

table(Dataframe$`V192: How important is god in your life`)

Dataframe$V192 <- ifelse(Dataframe$`V192: How important is god in your life` <= -1, NA, Dataframe$`V192: How important is god in your life`)

table(Dataframe$V192)

Dataframe$V3 <- Dataframe$V192

#Income equality

table(Dataframe$`V116: Income equality`)

Dataframe$V116 <- ifelse(Dataframe$`V116: Income equality` <= -1, NA, Dataframe$`V116: Income equality`)

table(Dataframe$V116)

Dataframe$V4 <- Dataframe$V116

#Private/public ownership

table(Dataframe$`V117: Private vs state ownership of business`)

Dataframe$V117 <- ifelse(Dataframe$`V117: Private vs state ownership of business` <= -1, NA, Dataframe$`V117: Private vs state ownership of business`)

table(Dataframe$V117)

Dataframe$V5 <- Dataframe$V117

#Competition

table(Dataframe$`V119: Competition good or harmful`)

Dataframe$V119 <- ifelse(Dataframe$`V119: Competition good or harmful` <= -1, NA, Dataframe$`V119: Competition good or harmful`)

table(Dataframe$V119)

Dataframe$V6 <- Dataframe$V119

#Respect for authority

table(Dataframe$`V78: Future changes: Greater respect for authority`)

Dataframe$V78 <- recode(Dataframe$`V78: Future changes: Greater respect for authority`, `1` = 3, `3` = 1)

Dataframe$V78 <- ifelse(Dataframe$V78 <= -1, NA, Dataframe$V78)

table(Dataframe$V78)

Dataframe$V7 <- Dataframe$V78

#Homosexuality

table(Dataframe$`V202: Justifiable: Homosexuality`)

Dataframe$V202 <- ifelse(Dataframe$`V202: Justifiable: Homosexuality` <= -1, NA, Dataframe$`V202: Justifiable: Homosexuality`)

table(Dataframe$V202)

Dataframe$V8 <- Dataframe$V202

#Prostitution

table(Dataframe$`V203: Justifiable: Prostitution`)

Dataframe$V203 <- ifelse(Dataframe$`V203: Justifiable: Prostitution` <= -1, NA, Dataframe$`V203: Justifiable: Prostitution`)

table(Dataframe$V203)

Dataframe$V9 <- Dataframe$V203

#Abortion

table(Dataframe$`V204: Justifiable: Abortion`)

Dataframe$V204 <- ifelse(Dataframe$`V204: Justifiable: Abortion` <= -1, NA, Dataframe$`V204: Justifiable: Abortion`)

table(Dataframe$V204)

Dataframe$V10 <- Dataframe$V204

#Divorce

table(Dataframe$`V205: Justifiable: Divorce`)

Dataframe$V205 <- ifelse(Dataframe$`V205: Justifiable: Divorce` <= -1, NA, Dataframe$`V205: Justifiable: Divorce`)

table(Dataframe$V205)

Dataframe$V11 <- Dataframe$V205

#National pride

table(Dataframe$`V209: How proud of nationality`)

Dataframe$V209 <- recode(Dataframe$`V209: How proud of nationality`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V209 <- ifelse(Dataframe$V209 <= -1, NA, Dataframe$V209)

Dataframe$V209 <- ifelse(Dataframe$V209 >= 5, NA, Dataframe$V209)

table(Dataframe$V209)

Dataframe$V12 <- Dataframe$V209

#Government responsibility

table(Dataframe$`V118: Government responsibility`)

Dataframe$V118 <- ifelse(Dataframe$`V118: Government responsibility` <= -1, NA, Dataframe$`V118: Government responsibility`)

table(Dataframe$V118)

Dataframe$V13 <- Dataframe$V118

#Immigrants policy

table(Dataframe$`V124: Immigrant policy`)

Dataframe$V124 <- ifelse(Dataframe$`V124: Immigrant policy` <= -1, NA, Dataframe$`V124: Immigrant policy`)

table(Dataframe$V124)

#Strong leader

table(Dataframe$`V148: Political system: Having a strong leader`)

Dataframe$V148 <- recode(Dataframe$`V148: Political system: Having a strong leader`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V148 <- ifelse(Dataframe$V148 <= -1, NA, Dataframe$V148)

table(Dataframe$V148)

#Experts

table(Dataframe$`V149: Political system: Having experts make decisions`)

Dataframe$V149 <- recode(Dataframe$`V149: Political system: Having experts make decisions`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V149 <- ifelse(Dataframe$V149 <= -1, NA, Dataframe$V149)

table(Dataframe$V149)

#Army rule

table(Dataframe$`V150: Political system: Having the army rule`)

Dataframe$V150 <- recode(Dataframe$`V150: Political system: Having the army rule`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V150 <- ifelse(Dataframe$V150 <= -1, NA, Dataframe$V150)

table(Dataframe$V150)

#Democracy

table(Dataframe$`V151: Political system: Having a democratic political system`)

Dataframe$V151 <- recode(Dataframe$`V151: Political system: Having a democratic political system`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V151 <- ifelse(Dataframe$V151 <= -1, NA, Dataframe$V151)

table(Dataframe$V151)

#Ideology

table(Dataframe$`V114: Self positioning in political scale`)

Dataframe$V114 <- ifelse(Dataframe$`V114: Self positioning in political scale` <= -1, NA, Dataframe$`V114: Self positioning in political scale`)

table(Dataframe$V114)

Dataframe$V14 <- Dataframe$V114

#Post-materialism

table(Dataframe$`Y001: Post-Materialist index 12-item`)

Dataframe$Y001 <- ifelse(Dataframe$`Y001: Post-Materialist index 12-item` <= -1, NA, Dataframe$`Y001: Post-Materialist index 12-item`)

table(Dataframe$Y001)

Dataframe$V16 <- Dataframe$Y001

#Party

table(Dataframe$`V231: Which party would you vote: first choice`)

Dataframe$V231 <- ifelse(Dataframe$`V231: Which party would you vote: first choice` == 76002, 1, 0)

table(Dataframe$V231)

Dataframe$V15 <- Dataframe$V231

Dataframe$P1 <- Dataframe$`V231: Which party would you vote: first choice`

#Party2

table(Dataframe$`V232: Which party would you vote: second choice`)

Dataframe$V232 <- ifelse(Dataframe$`V232: Which party would you vote: second choice` == 76002, 1, 0)

table(Dataframe$V232)

#PartyNever

table(Dataframe$`V233: Party that would never vote`)

Dataframe$V233 <- ifelse(Dataframe$`V233: Party that would never vote` == 76002, 1, 0)

table(Dataframe$V233)

#Church attendance

table(Dataframe$`V186: How often do you attend religious services`)

Dataframe$V186 <- recode(Dataframe$`V186: How often do you attend religious services`, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1)

Dataframe$V186 <- ifelse(Dataframe$V186 <= -1, NA, Dataframe$V186)

table(Dataframe$V186)

Dataframe$V18 <- Dataframe$V186

#Education

table(Dataframe$`V238: Highest educational level: Respondent`)

Dataframe$V238 <- ifelse(Dataframe$`V238: Highest educational level: Respondent` <= -1, NA, Dataframe$`V238: Highest educational level: Respondent`)

table(Dataframe$V238)

Dataframe$V17 <- Dataframe$V238

#Age

table(Dataframe$`V237: Age`)

Dataframe$V237 <- Dataframe$`V237: Age`

Dataframe$V19 <- Dataframe$V237

#Size town

table(Dataframe$`V255: Size of town`)

Dataframe$V255 <- Dataframe$`V255: Size of town`

Dataframe$V20 <- Dataframe$V255

#Sex

table(Dataframe$`V235: Sex`)

Dataframe$V235 <- Dataframe$`V235: Sex`

Dataframe$V21 <- Dataframe$V235

#Interest in politics

table(Dataframe$`V95: Interest in politics`)

Dataframe$V95 <- recode(Dataframe$`V95: Interest in politics`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V95 <- ifelse(Dataframe$V95 <= -1, NA, Dataframe$V95)

table(Dataframe$V95)

Dataframe$V22 <- Dataframe$V95

#Income

table(Dataframe$`V253: Scale of incomes`)

Dataframe$V253 <- ifelse(Dataframe$`V253: Scale of incomes` <= -1, NA, Dataframe$`V253: Scale of incomes`)

table(Dataframe$V253)

Dataframe$V23 <- Dataframe$V253

#Race

table(Dataframe$`V256: Ethnic group`)

Dataframe$V256 <- ifelse(Dataframe$`V256: Ethnic group` == 1400, 1, 0)

table(Dataframe$V256)

Dataframe$V24 <- Dataframe$V256

Dataframe_dimension_reduction <- Dataframe[, c("V44","V45","V192","V116","V117","V119","V78","V202","V203","V204","V205","V209","V118","V124","V148","V149","V150","V151","V114","Y001","V231","V232","V233","V186","P1")]

Dataframe_dimension_reduction_2006 <- Dataframe[, c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13")]

df2006r <- Dataframe[, c("V44","V45","V192","V116","V117","V119","V78","V202","V203","V204","V205","V209","V118","V124","V148","V149","V150","V151","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24")]



##Run PCA

pca_unconstrained <- principal(Dataframe_dimension_reduction, nfactors = 3, rotate = "varimax")

print(pca_unconstrained)

##Run FA

fa_unconstrained <- fa(Dataframe_dimension_reduction, nfactors = 1, rotate = "varimax")

print(fa_unconstrained)

fa_constrained <- fa(Dataframe_dimension_reduction[,1:18], nfactors = 1, rotate = "varimax")

scores_Dataframe_dimension_reduction <- factor.scores(Dataframe_dimension_reduction[,1:18],fa_constrained)
scores_Dataframe_dimension_reduction <- scores_Dataframe_dimension_reduction$scores
Dataframe_dimension_reduction <- cbind(Dataframe_dimension_reduction,scores_Dataframe_dimension_reduction)


print(fa_constrained)

#CFA

modelo_2006 <- 'fundamentalist =~ V192+V202+V203+V204+V205
authoritarian =~ V44+V45+V78+V209+V124+V148+V149+V150+V151
economic =~ V116+V117+V118+V119'

teste2006 <- sem(modelo_2006, data = Dataframe_dimension_reduction)
summary(teste2006, standardized = TRUE, fit.measures = TRUE)
fitmeasures(teste2006, c("chisq", "df", "pvalue", "cfi","tli", "rmsea","SRMR"))

#Run IRT

irt_unconstrained <- mirt(Dataframe_dimension_reduction, 2)

summary(irt_unconstrained, rotate = 'varimax')

##POSMATERIALISM INDEX

brasil.Y001.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 0| Dataframe_dimension_reduction$Y001 == 1,]

mean(brasil.Y001.0$MR1, na.rm = TRUE)
mean(brasil.Y001.0$V114, na.rm = TRUE)

brasil.Y001.2 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 2| Dataframe_dimension_reduction$Y001 == 3,]

mean(brasil.Y001.2$MR1, na.rm = TRUE)
mean(brasil.Y001.2$V114, na.rm = TRUE)

brasil.Y001.4 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 4| Dataframe_dimension_reduction$Y001 == 5,]

mean(brasil.Y001.4$MR1, na.rm = TRUE)
mean(brasil.Y001.4$V114, na.rm = TRUE)

#PARTY

brasil.V231.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V231 == 1,]

mean(brasil.V231.1$MR1, na.rm = TRUE)
mean(brasil.V231.1$V114, na.rm = TRUE)

brasil.231.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V231 == 0,]

mean(brasil.231.0$MR1, na.rm = TRUE)
mean(brasil.231.0$V114, na.rm = TRUE)

#PARTY2

brasil.V232.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V232 == 1,]

mean(brasil.V232.1$MR1, na.rm = TRUE)
mean(brasil.V232.1$V114, na.rm = TRUE)

brasil.V232.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V232 == 0,]

mean(brasil.V232.0$MR1, na.rm = TRUE)
mean(brasil.V232.0$V114, na.rm = TRUE)

#ANTIPT

brasil.V233.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V233 == 1,]

mean(brasil.V233.1$MR1, na.rm = TRUE)
mean(brasil.V233.1$V114, na.rm = TRUE)

#Church Attendence

brasil.V186.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V186 == 1 | Dataframe_dimension_reduction$V186 == 2,]

mean(brasil.V186.1$MR1, na.rm = TRUE)
mean(brasil.V186.1$V114, na.rm = TRUE) 

brasil.V186.3 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V186 == 3 | Dataframe_dimension_reduction$V186 == 4 | Dataframe_dimension_reduction$V186 == 5,]

mean(brasil.V186.3$MR1, na.rm = TRUE)
mean(brasil.V186.3$V114, na.rm = TRUE)

brasil.V186.6 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V186 == 6 | Dataframe_dimension_reduction$V186 == 7,]

mean(brasil.V186.6$MR1, na.rm = TRUE)
mean(brasil.V186.6$V114, na.rm = TRUE)

df2006r$V17 <- recode(df2006r$V17, `1` = 1, `2` = 1, `3` = 1, `4` = 2, `5` = 2,
                      `6` = 2, `7` = 3, `8` = 3, `9` = 3)

media_ed <- aggregate(cbind(MR1, V14) ~ V17, data = df2006r, FUN = mean)

bolinhas <- data.frame(
  Fundamentalist = c(0.09349702,-0.04767336,-0.3674175,0.0008914103,-0.038484,0.01362589,-0.02624751,-0.1108109,0.1079238,-0.08292418,-0.2053031,-0.45284361,-0.04743023,0.15339790),
  Category = c("Materialist","Mixed","Postmaterialist","PT (first vote)","Non-PT (first vote)","PT (second vote)","Non-PT (second vote)","Anti-PT","High Church Att.","Medium Church Att.","Low Church Att.","High Education", "Medium Education","Low Education"),
  Ideology = c(5.627792,5.389677,5.050847,5.22037,5.568675,5.919192,5.393391,5.622568,5.564565,5.539634,5.115591,5.095238,5.296984,5.671028)
)


p <- ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "2006")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))
p

#Partys

table(Dataframe$P1)

media_2006 <- aggregate(cbind(MR1, V114) ~ P1, data = Dataframe_dimension_reduction, FUN = mean)

bolinhas <- data.frame(
  Fundamentalist = c(0.14904043,0.01414857,-0.13618869,0.07588276,0.04951222),
  Category = c("MDB","PT","PSDB","PDT","PFL"),
  Ideology = c(5.797203,5.189956,5.929577,5.633333,5.860465)
)

ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "2006")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))

###2014###

##Download Dataframe

Dataframe <- read_excel("~/Banco de Dados/WVS/Dados/F00007581-WV6_Data_Brazil_Excel_v20201117.xlsx")

##Check variables structure

#Jobs for man

table(Dataframe$`V45: Jobs scarce: Men should have more right to a job than women`)

Dataframe$V45 <- recode(Dataframe$`V45: Jobs scarce: Men should have more right to a job than women`, `1` = 3, `3` = 1)

Dataframe$V45 <- ifelse(Dataframe$V45 <= -1, NA, Dataframe$V45)

table(Dataframe$V45)

Dataframe$V1 <- Dataframe$V45

#jobs for own nationality

table(Dataframe$`V46: Jobs scarce: Employers should give priority to (nation) people than immigrants`)

Dataframe$V46 <- recode(Dataframe$`V46: Jobs scarce: Employers should give priority to (nation) people than immigrants`, `1` = 3, `3` = 1)

Dataframe$V46 <- ifelse(Dataframe$V46 <= -1, NA, Dataframe$V46)

table(Dataframe$V46)

Dataframe$V2 <- Dataframe$V46

#Importance of God

table(Dataframe$`V152: How important is God in your life`)

Dataframe$V152 <- ifelse(Dataframe$`V152: How important is God in your life` <= -1, NA, Dataframe$`V152: How important is God in your life`)

table(Dataframe$V152)

Dataframe$V3 <- Dataframe$V152

#Income equality

table(Dataframe$`V96: Income equality`)

Dataframe$V96 <- ifelse(Dataframe$`V96: Income equality` <= -1, NA, Dataframe$`V96: Income equality`)

table(Dataframe$V96)

Dataframe$V4 <- Dataframe$V96

#Private/public ownership

table(Dataframe$`V97: Private vs state ownership of business`)

Dataframe$V97 <- ifelse(Dataframe$`V97: Private vs state ownership of business` <= -1, NA, Dataframe$`V97: Private vs state ownership of business`)

table(Dataframe$V97)

Dataframe$V5 <- Dataframe$V97

#Competition

table(Dataframe$`V99: Competition good or harmful`)

Dataframe$V99 <- ifelse(Dataframe$`V99: Competition good or harmful` <= -1, NA, Dataframe$`V99: Competition good or harmful`)

table(Dataframe$V99)

Dataframe$V6 <- Dataframe$V99

#Respect for authority

table(Dataframe$`V69: Future changes: Greater respect for authority`)

Dataframe$V69 <- recode(Dataframe$`V69: Future changes: Greater respect for authority`, `1` = 3, `3` = 1)

Dataframe$V69 <- ifelse(Dataframe$V69 <= -1, NA, Dataframe$V69)

table(Dataframe$V69)

Dataframe$V7 <- Dataframe$V69

#Homosexuality

table(Dataframe$`V203: Justifiable: Homosexuality`)

Dataframe$V203 <- ifelse(Dataframe$`V203: Justifiable: Homosexuality` <= -1, NA, Dataframe$`V203: Justifiable: Homosexuality`)

table(Dataframe$V203)

Dataframe$V8 <- Dataframe$V203

#Prostitution

table(Dataframe$`V203A: Justifiable: Prostitution`)

Dataframe$V203A <- ifelse(Dataframe$`V203A: Justifiable: Prostitution` <= -1, NA, Dataframe$`V203A: Justifiable: Prostitution`)

table(Dataframe$V203A)

Dataframe$V9 <- Dataframe$V203A

#Abortion

table(Dataframe$`V204: Justifiable: Abortion`)

Dataframe$V204 <- ifelse(Dataframe$`V204: Justifiable: Abortion` <= -1, NA, Dataframe$`V204: Justifiable: Abortion`)

table(Dataframe$V204)

Dataframe$V10 <- Dataframe$V204

#Divorce

table(Dataframe$`V205: Justifiable: Divorce`)

Dataframe$V205 <- ifelse(Dataframe$`V205: Justifiable: Divorce` <= -1, NA, Dataframe$`V205: Justifiable: Divorce`)

table(Dataframe$V205)

Dataframe$V11 <- Dataframe$V205

#National pride

table(Dataframe$`V211: How proud of nationality`)

Dataframe$V211 <- recode(Dataframe$`V211: How proud of nationality`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V211 <- ifelse(Dataframe$V211 <= -1, NA, Dataframe$V211)

Dataframe$V211 <- ifelse(Dataframe$V211 >= 5, NA, Dataframe$V211)

table(Dataframe$V211)

Dataframe$V12 <- Dataframe$V211

#Government responsibility

table(Dataframe$`V98: Government responsibility`)

Dataframe$V98 <- ifelse(Dataframe$`V98: Government responsibility` <= -1, NA, Dataframe$`V98: Government responsibility`)

table(Dataframe$V98)

Dataframe$V13 <- Dataframe$V98

#Strong leader

table(Dataframe$`V127: Political system: Having a strong leader`)

Dataframe$V127 <- recode(Dataframe$`V127: Political system: Having a strong leader`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V127 <- ifelse(Dataframe$V127 <= -1, NA, Dataframe$V127)

table(Dataframe$V127)

#Experts

table(Dataframe$`V128: Political system: Having experts make decisions`)

Dataframe$V128 <- recode(Dataframe$`V128: Political system: Having experts make decisions`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V128 <- ifelse(Dataframe$V128 <= -1, NA, Dataframe$V128)

table(Dataframe$V128)

#Army rule

table(Dataframe$`V129: Political system: Having the army rule`)

Dataframe$V129 <- recode(Dataframe$`V129: Political system: Having the army rule`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V129 <- ifelse(Dataframe$V129 <= -1, NA, Dataframe$V129)

table(Dataframe$V129)

#Democracy

table(Dataframe$`V130: Political system: Having a democratic political system`)

Dataframe$V130 <- recode(Dataframe$`V130: Political system: Having a democratic political system`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V130 <- ifelse(Dataframe$V130 <= -1, NA, Dataframe$V130)

table(Dataframe$V130)

#Ideology

table(Dataframe$`V95: Self positioning in political scale`)

Dataframe$V95 <- ifelse(Dataframe$`V95: Self positioning in political scale` <= -1, NA, Dataframe$`V95: Self positioning in political scale`)

table(Dataframe$V95)

Dataframe$V14 <- Dataframe$V95

#Post-materialism

table(Dataframe$`Y001: Post-Materialist index 12-item`)

Dataframe$Y001 <- ifelse(Dataframe$`Y001: Post-Materialist index 12-item` <= -1, NA, Dataframe$`Y001: Post-Materialist index 12-item`)

table(Dataframe$Y001)

Dataframe$V16 <- Dataframe$Y001

#Party

table(Dataframe$`V228: Which party would you vote for if there were a national election tomorrow`)

Dataframe$V228 <- ifelse(Dataframe$`V228: Which party would you vote for if there were a national election tomorrow` == 76002, 1, 0)

table(Dataframe$V228)

Dataframe$V15 <- Dataframe$V228

Dataframe$P1 <- Dataframe$`V228: Which party would you vote for if there were a national election tomorrow`

#Church attendance

table(Dataframe$`V145: How often do you attend religious services`)

Dataframe$V145 <- recode(Dataframe$`V145: How often do you attend religious services`, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1)

Dataframe$V145 <- ifelse(Dataframe$V145 <= -1, NA, Dataframe$V145)

table(Dataframe$V145)

Dataframe$V18 <- Dataframe$V145

#Education

table(Dataframe$`V248: Highest educational level attained`)

Dataframe$V248 <- ifelse(Dataframe$`V248: Highest educational level attained` <= -1, NA, Dataframe$`V248: Highest educational level attained`)

table(Dataframe$V248)

Dataframe$V17 <- Dataframe$V248

#Age

table(Dataframe$`V242: Age`)

Dataframe$V242 <- Dataframe$`V242: Age`

Dataframe$V19 <- Dataframe$V242

#Size town

table(Dataframe$`V253: Size of town`)

Dataframe$V253 <- ifelse(Dataframe$`V253: Size of town` <= -1, NA, Dataframe$`V253: Size of town`)

Dataframe$V20 <- Dataframe$V253

#Sex

table(Dataframe$`V240: Sex`)

Dataframe$V240 <- Dataframe$`V240: Sex`

Dataframe$V21 <- Dataframe$V240

#Interest in politics

table(Dataframe$`V84: Interest in politics`)

Dataframe$V84 <- recode(Dataframe$`V84: Interest in politics`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V84 <- ifelse(Dataframe$V84 <= -1, NA, Dataframe$V84)

table(Dataframe$V84)

Dataframe$V22 <- Dataframe$V84

#Income

table(Dataframe$`V239: Scale of incomes`)

Dataframe$V239 <- ifelse(Dataframe$`V239: Scale of incomes` <= -1, NA, Dataframe$`V239: Scale of incomes`)

table(Dataframe$V239)

Dataframe$V23 <- Dataframe$V239

#Race

table(Dataframe$`V254: Ethnic group`)

Dataframe$V254 <- ifelse(Dataframe$`V254: Ethnic group` == 1400, 1, 0)

table(Dataframe$V254)

Dataframe$V24 <- Dataframe$V254

Dataframe_dimension_reduction <- Dataframe[, c("V45","V46","V152","V96","V97","V99","V69","V203","V203A","V204","V205","V211","V98","V127","V128","V129","V130","V95","Y001","V228","V145","P1")]

Dataframe_dimension_reduction_2014 <- Dataframe[, c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13")]

df2014r <- Dataframe[, c("V45","V46","V152","V96","V97","V99","V69","V203","V203A","V204","V205","V211","V98","V127","V128","V129","V130","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24")]

##Run PCA

pca_unconstrained <- principal(Dataframe_dimension_reduction, nfactors = 3, rotate = "varimax")

print(pca_unconstrained)

##Run FA

fa_unconstrained <- fa(Dataframe_dimension_reduction, nfactors = 1, rotate = "varimax")

print(fa_unconstrained)

fa_constrained <- fa(Dataframe_dimension_reduction[,1:17], nfactors = 1, rotate = "varimax")

scores_Dataframe_dimension_reduction <- factor.scores(Dataframe_dimension_reduction[,1:17],fa_constrained)
scores_Dataframe_dimension_reduction <- scores_Dataframe_dimension_reduction$scores
Dataframe_dimension_reduction <- cbind(Dataframe_dimension_reduction,scores_Dataframe_dimension_reduction)

print(fa_constrained)

#CFA

modelo_2014 <- 'fundamentalist =~ V152+V203+V203A+V204+V205
authoritarian =~ V45+V46+V69+V211+V127+V128+V129+V130
economic =~ V96+V97+V99+V98'

teste2014 <- sem(modelo_2014, data = Dataframe_dimension_reduction)
summary(teste2014, standardized = TRUE, fit.measures = TRUE)
fitmeasures(teste2014, c("chisq", "df", "pvalue", "cfi","tli", "rmsea","SRMR"))

#Run IRT

irt_unconstrained <- mirt(Dataframe_dimension_reduction, 2)

summary(irt_unconstrained, rotate = 'varimax')

##POSMATERIALISM INDEX

brasil.Y001.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 0| Dataframe_dimension_reduction$Y001 == 1,]

mean(brasil.Y001.0$MR1, na.rm = TRUE)
mean(brasil.Y001.0$V95, na.rm = TRUE)

brasil.Y001.2 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 2| Dataframe_dimension_reduction$Y001 == 3,]

mean(brasil.Y001.2$MR1, na.rm = TRUE)
mean(brasil.Y001.2$V95, na.rm = TRUE)

brasil.Y001.4 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 4| Dataframe_dimension_reduction$Y001 == 5,]

mean(brasil.Y001.4$MR1, na.rm = TRUE)
mean(brasil.Y001.4$V95, na.rm = TRUE)

#PARTY

brasil.V228.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V228 == 1,]

mean(brasil.V228.1$MR1, na.rm = TRUE)
mean(brasil.V228.1$V95, na.rm = TRUE)

brasil.228.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V228 == 0,]

mean(brasil.228.0$MR1, na.rm = TRUE)
mean(brasil.228.0$V95, na.rm = TRUE)


#Church Attendence

brasil.V145.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V145 == 1 | Dataframe_dimension_reduction$V145 == 2,]

mean(brasil.V145.1$MR1, na.rm = TRUE)
mean(brasil.V145.1$V95, na.rm = TRUE) 

brasil.V145.3 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V145 == 3 | Dataframe_dimension_reduction$V145 == 4 | Dataframe_dimension_reduction$V145 == 5,]

mean(brasil.V145.3$MR1, na.rm = TRUE)
mean(brasil.V145.3$V95, na.rm = TRUE)

brasil.V145.6 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$V145 == 6 | Dataframe_dimension_reduction$V145 == 7,]

mean(brasil.V145.6$MR1, na.rm = TRUE)
mean(brasil.V145.6$V95, na.rm = TRUE)

df2014r$V17 <- recode(df2014r$V17, `1` = 1, `2` = 1, `3` = 1, `4` = 2, `5` = 2,
                      `6` = 2, `7` = 3, `8` = 3, `9` = 3)


bolinhas <- data.frame(
  Fundamentalist = c(0.02839747,0.002355039,-0.340635,-0.1233625,-0.01246395,0.1535815,-0.1278226,-0.2868364,-0.13824034,-0.04753775,0.10362779),
  Category = c("Materialist","Mixed","Postmaterialist","PT (first vote)","Non-PT (first vote)","High Church Att.","Medium Church Att.","Low Church Att.","High Education", "Medium Education","Low Education"),
  Ideology = c(5.644654,5.277519,5.28866,5.615789,5.324649,5.568067,5.451264,4.905537,4.997753,4.983740,5.744318)
)


p <- ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "2014")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))
p

#Partys

table(Dataframe$P1)

media_2014 <- aggregate(cbind(MR1, V95) ~ P1, data = Dataframe_dimension_reduction, FUN = mean)

bolinhas <- data.frame(
  Fundamentalist = c(0.182343970,-0.148000090,0.159438515),
  Category = c("MDB","PT","PSDB"),
  Ideology = c(6.041667,5.417219,5.211538)
)

ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "2014")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))

###2018###

##Download Dataframe

Dataframe <- read_excel("~/Banco de Dados/WVS/Dados/F00013178-WVS_Wave_7_Brazil_Excel_v5.0.xlsx")

##Check variables structure

#Jobs for man

table(Dataframe$`Q33: Jobs scarce: Men should have more right to a job than women`)

Dataframe$Q33 <- recode(Dataframe$`Q33: Jobs scarce: Men should have more right to a job than women`, `2` = 4, `4` = 2)

Dataframe$Q33 <- ifelse(Dataframe$Q33 <= -1, NA, Dataframe$Q33)

table(Dataframe$Q33)

Dataframe$V1 <- Dataframe$Q33

#jobs for own nationality

table(Dataframe$`Q34: Jobs scarce: Employers should give priority to (nation) people than immigrants`)

Dataframe$Q34 <- recode(Dataframe$`Q34: Jobs scarce: Employers should give priority to (nation) people than immigrants`, `2` = 4, `4` = 2)

Dataframe$Q34 <- ifelse(Dataframe$Q34 <= -1, NA, Dataframe$Q34)

table(Dataframe$Q34)

Dataframe$V2 <- Dataframe$Q34

#Importance of God

table(Dataframe$`Q164: Importance of God`)

Dataframe$Q164 <- ifelse(Dataframe$`Q164: Importance of God` <= -1, NA, Dataframe$`Q164: Importance of God`)

table(Dataframe$Q164)

Dataframe$V3 <- Dataframe$Q164

#Societal change

table(Dataframe$`Q42: Basic kinds of attitudes concerning society`)

Dataframe$Q42 <- ifelse(Dataframe$`Q42: Basic kinds of attitudes concerning society` <= -1, NA, Dataframe$`Q42: Basic kinds of attitudes concerning society`)

table(Dataframe$Q42)

#Income equality

table(Dataframe$`Q106: Incomes should be made more equal vs There should be greater incentives for individual effort`)

Dataframe$Q106 <- ifelse(Dataframe$`Q106: Incomes should be made more equal vs There should be greater incentives for individual effort` <= -1, NA, Dataframe$`Q106: Incomes should be made more equal vs There should be greater incentives for individual effort`)

table(Dataframe$Q106)

Dataframe$V4 <- Dataframe$Q106

#Private/public ownership

table(Dataframe$`Q107: Private vs state ownership of business`)

Dataframe$Q107 <- ifelse(Dataframe$`Q107: Private vs state ownership of business` <= -1, NA, Dataframe$`Q107: Private vs state ownership of business`)

table(Dataframe$Q107)

Dataframe$V5 <- Dataframe$Q107

#Competition

table(Dataframe$`Q109: Competition good or harmful`)

Dataframe$Q109 <- ifelse(Dataframe$`Q109: Competition good or harmful` <= -1, NA, Dataframe$`Q109: Competition good or harmful`)

table(Dataframe$Q109)

Dataframe$V6 <- Dataframe$Q109

#Respect for authority

table(Dataframe$`Q45: Future changes: Greater respect for authority`)

Dataframe$Q45 <- recode(Dataframe$`Q45: Future changes: Greater respect for authority`, `1` = 3, `3` = 1)

Dataframe$Q45 <- ifelse(Dataframe$Q45 <= -1, NA, Dataframe$Q45)

table(Dataframe$Q45)

Dataframe$V7 <- Dataframe$Q45

#Homosexuality

table(Dataframe$`Q182: Justifiable: Homosexuality`)

Dataframe$Q182 <- ifelse(Dataframe$`Q182: Justifiable: Homosexuality` <= -1, NA, Dataframe$`Q182: Justifiable: Homosexuality`)

table(Dataframe$Q182)

Dataframe$V8 <- Dataframe$Q182

#Prostitution

table(Dataframe$`Q183: Justifiable: Prostitution`)

Dataframe$Q183 <- ifelse(Dataframe$`Q183: Justifiable: Prostitution` <= -1, NA, Dataframe$`Q183: Justifiable: Prostitution`)

table(Dataframe$Q183)

Dataframe$V9 <- Dataframe$Q183

#Abortion

table(Dataframe$`Q184: Justifiable: Abortion`)

Dataframe$Q184 <- ifelse(Dataframe$`Q184: Justifiable: Abortion` <= -1, NA, Dataframe$`Q184: Justifiable: Abortion`)

table(Dataframe$Q184)

Dataframe$V10 <- Dataframe$Q184

#Divorce

table(Dataframe$`Q185: Justifiable: Divorce`)

Dataframe$Q185 <- ifelse(Dataframe$`Q185: Justifiable: Divorce` <= -1, NA, Dataframe$`Q185: Justifiable: Divorce`)

table(Dataframe$Q185)

Dataframe$V11 <- Dataframe$Q185

#National pride

table(Dataframe$`Q254: National pride`)

Dataframe$Q254 <- recode(Dataframe$`Q254: National pride`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q254 <- ifelse(Dataframe$Q254 <= -1, NA, Dataframe$Q254)

Dataframe$Q254 <- ifelse(Dataframe$Q254 >= 5, NA, Dataframe$Q254)

table(Dataframe$Q254)

Dataframe$V12 <- Dataframe$Q254

#Government responsibility

table(Dataframe$`Q108: Government's vs individual's responsibility`)

Dataframe$Q108 <- ifelse(Dataframe$`Q108: Government's vs individual's responsibility` <= -1, NA, Dataframe$`Q108: Government's vs individual's responsibility`)

table(Dataframe$Q108)

Dataframe$V13 <- Dataframe$Q108

#Immigration policy

table(Dataframe$`Q130: Immigration policy preference`)

Dataframe$Q130 <- ifelse(Dataframe$`Q130: Immigration policy preference` <= -1, NA, Dataframe$`Q130: Immigration policy preference`)

table(Dataframe$Q130)

#Strong leader

table(Dataframe$`Q235: Political system: Having a strong leader who does not have to bother with parliament and elections`)

Dataframe$Q235 <- recode(Dataframe$`Q235: Political system: Having a strong leader who does not have to bother with parliament and elections`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q235 <- ifelse(Dataframe$Q235 <= -1, NA, Dataframe$Q235)

table(Dataframe$Q235)

#Experts

table(Dataframe$`Q236: Political system: Having experts, not government, make decisions according to what they think is best for the country`)

Dataframe$Q236 <- recode(Dataframe$`Q236: Political system: Having experts, not government, make decisions according to what they think is best for the country`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q236 <- ifelse(Dataframe$Q236 <= -1, NA, Dataframe$Q236)

table(Dataframe$Q236)

#Army rule

table(Dataframe$`Q237: Political system: Having the army rule`)

Dataframe$Q237 <- recode(Dataframe$`Q237: Political system: Having the army rule`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q237 <- ifelse(Dataframe$Q237 <= -1, NA, Dataframe$Q237)

table(Dataframe$Q237)

#Democracy

table(Dataframe$`Q238: Political system: Having a democratic political system`)

Dataframe$Q238 <- recode(Dataframe$`Q238: Political system: Having a democratic political system`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q238 <- ifelse(Dataframe$Q238 <= -1, NA, Dataframe$Q238)

table(Dataframe$Q238)

#Ideology

table(Dataframe$`Q240: Left-right political scale`)

Dataframe$Q240 <- ifelse(Dataframe$`Q240: Left-right political scale` <= -1, NA, Dataframe$`Q240: Left-right political scale`)

table(Dataframe$Q240)

Dataframe$V14 <- Dataframe$Q240

#Post-materialism

table(Dataframe$`Y001: Post-Materialist index 12-item`)

Dataframe$Y001 <- ifelse(Dataframe$`Y001: Post-Materialist index 12-item` <= -1, NA, Dataframe$`Y001: Post-Materialist index 12-item`)

table(Dataframe$Y001)

Dataframe$V16 <- Dataframe$Y001

#Party

table(Dataframe$`Q223: Which party would you vote for if there were a national election tomorrow`)

Dataframe$Q223 <- ifelse(Dataframe$`Q223: Which party would you vote for if there were a national election tomorrow` == 76002, 1, 0)

table(Dataframe$Q223)

Dataframe$V15 <- Dataframe$Q223

Dataframe$P1 <- Dataframe$`Q223: Which party would you vote for if there were a national election tomorrow`

#Church attendance

table(Dataframe$`Q171: How often do you attend religious services`)

Dataframe$Q171 <- recode(Dataframe$`Q171: How often do you attend religious services`, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1)

Dataframe$Q171 <- ifelse(Dataframe$Q171 <= -1, NA, Dataframe$Q171)

table(Dataframe$Q171)

Dataframe$V18 <- Dataframe$Q171

#Education

table(Dataframe$`Q275: Highest educational level: Respondent [ISCED 2011]`)

Dataframe$Q275 <- ifelse(Dataframe$`Q275: Highest educational level: Respondent [ISCED 2011]` <= -1, NA, Dataframe$`Q275: Highest educational level: Respondent [ISCED 2011]`)

table(Dataframe$Q275)

Dataframe$V17 <- Dataframe$Q275

#Age

table(Dataframe$`Q262: Age`)

Dataframe$Q262 <- Dataframe$`Q262: Age`

Dataframe$V19 <- Dataframe$Q262

#Size town

table(Dataframe$`G_TOWNSIZE: Settlement size_8 groups`)

Dataframe$G_TOWNSIZE <- Dataframe$`G_TOWNSIZE: Settlement size_8 groups`

Dataframe$V20 <- Dataframe$G_TOWNSIZE

#Sex

table(Dataframe$`Q260: Sex`)

Dataframe$Q260 <- Dataframe$`Q260: Sex`

Dataframe$V21 <- Dataframe$Q260

#Interest in politics

table(Dataframe$`Q199: Interest in politics`)

Dataframe$Q199 <- recode(Dataframe$`Q199: Interest in politics`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q199 <- ifelse(Dataframe$Q199 <= -1, NA, Dataframe$Q199)

table(Dataframe$Q199)

Dataframe$V22 <- Dataframe$Q199

#Income

table(Dataframe$`Q288: Scale of incomes`)

Dataframe$Q288 <- ifelse(Dataframe$`Q288: Scale of incomes` <= -1, NA, Dataframe$`Q288: Scale of incomes`)

table(Dataframe$Q288)

Dataframe$V23 <- Dataframe$Q288

#Race

table(Dataframe$`Q290: Ethnic group`)

Dataframe$Q290 <- ifelse(Dataframe$`Q290: Ethnic group` == 76001, 1, 0)

table(Dataframe$Q290)

Dataframe$V24 <- Dataframe$Q290

Dataframe_dimension_reduction <- Dataframe[, c("Q33","Q34","Q164","Q42","Q106","Q107","Q109","Q45","Q182","Q183","Q184","Q185","Q254","Q108","Q130","Q235","Q236","Q237","Q238","Q240","Y001","Q223","Q171","P1")]

Dataframe_dimension_reduction_2018 <- Dataframe[, c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13")]

df2018r <- Dataframe[, c("Q33","Q34","Q164","Q42","Q106","Q107","Q109","Q45","Q182","Q183","Q184","Q185","Q254","Q108","Q130","Q235","Q236","Q237","Q238","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24")]

##Run PCA


pca_unconstrained <- principal(Dataframe_dimension_reduction, nfactors = 4, rotate = "varimax")

print(pca_unconstrained)

##Run FA

fa_unconstrained <- fa(Dataframe_dimension_reduction, nfactors = 1, rotate = "varimax")

print(fa_unconstrained)

fa_constrained <- fa(Dataframe_dimension_reduction[,1:19], nfactors = 1, rotate = "varimax")

scores_Dataframe_dimension_reduction <- factor.scores(Dataframe_dimension_reduction[,1:19],fa_constrained)
scores_Dataframe_dimension_reduction <- scores_Dataframe_dimension_reduction$scores
Dataframe_dimension_reduction <- cbind(Dataframe_dimension_reduction,scores_Dataframe_dimension_reduction)

print(fa_constrained)

#CFA

modelo_2018 <- 'fundamentalist =~ Q164+Q182+Q183+Q184+Q185
authoritarian =~ Q33+Q34+Q42+Q45+Q254+Q130+Q235+Q236+Q237+Q238
economic =~ Q106+Q107+Q108+Q109'

teste2018 <- sem(modelo_2018, data = Dataframe_dimension_reduction)
summary(teste2018, standardized = TRUE, fit.measures = TRUE)
fitmeasures(teste2018, c("chisq", "df", "pvalue", "cfi","tli", "rmsea","SRMR"))

#Run IRT

irt_unconstrained <- mirt(Dataframe_dimension_reduction, 1)

summary(irt_unconstrained, rotate = 'varimax')

##POSMATERIALISM INDEX

brasil.Y001.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 0| Dataframe_dimension_reduction$Y001 == 1,]

mean(brasil.Y001.0$MR1, na.rm = TRUE)
mean(brasil.Y001.0$Q240, na.rm = TRUE)

brasil.Y001.2 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 2| Dataframe_dimension_reduction$Y001 == 3,]

mean(brasil.Y001.2$MR1, na.rm = TRUE)
mean(brasil.Y001.2$Q240, na.rm = TRUE)

brasil.Y001.4 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Y001 == 4| Dataframe_dimension_reduction$Y001 == 5,]

mean(brasil.Y001.4$MR1, na.rm = TRUE)
mean(brasil.Y001.4$Q240, na.rm = TRUE)

#PARTY

brasil.Q223.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Q223 == 1,]

mean(brasil.Q223.1$MR1, na.rm = TRUE)
mean(brasil.Q223.1$Q240, na.rm = TRUE)

brasil.Q223.0 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Q223 == 0,]

mean(brasil.Q223.0$MR1, na.rm = TRUE)
mean(brasil.Q223.0$Q240, na.rm = TRUE)


#Church Attendence

brasil.Q171.1 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Q171 == 1 | Dataframe_dimension_reduction$Q171 == 2,]

mean(brasil.Q171.1$MR1, na.rm = TRUE)
mean(brasil.Q171.1$Q240, na.rm = TRUE) 

brasil.Q171.3 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Q171 == 3 | Dataframe_dimension_reduction$Q171 == 4 | Dataframe_dimension_reduction$Q171 == 5,]

mean(brasil.Q171.3$MR1, na.rm = TRUE)
mean(brasil.Q171.3$Q240, na.rm = TRUE)

brasil.Q171.6 <- Dataframe_dimension_reduction[Dataframe_dimension_reduction$Q171 == 6 | Dataframe_dimension_reduction$Q171 == 7,]

mean(brasil.Q171.6$MR1, na.rm = TRUE)
mean(brasil.Q171.6$Q240, na.rm = TRUE)

df2018r$V17 <- recode(df2018r$V17, `0` = 1, `1` = 1, `2` = 1, `3` = 2, `4` = 2, `5` = 2,
                      `6` = 3, `7` = 3, `8` = 3)


bolinhas <- data.frame(
  Fundamentalist = c(0.25574,0.05527921,-0.5916306,-0.01862526,0.003975542,0.1809248,-0.02323409,-0.2619612,-0.42091570,-0.02767414,0.12385853),
  Category = c("Materialist","Mixed","Postmaterialist","PT (first vote)","Non-PT (first vote)","High Church Att.","Medium Church Att.","Low Church Att.","High Education", "Medium Education","Low Education"),
  Ideology = c(5.965251,5.441696,4.060345,4.521127,5.708831,5.48996,5.354478,5.327635,5.256944,5.282958,5.612903)
)


p <- ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "2018")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))
p

#Partys

table(Dataframe$P1)

media_2018 <- aggregate(cbind(MR1, Q240) ~ P1, data = Dataframe_dimension_reduction, FUN = mean)

bolinhas <- data.frame(
  Fundamentalist = c(-0.06355458,0.13877743),
  Category = c("PT","PSDB"),
  Ideology = c(4.622222,7.051282)
)

ggplot(bolinhas, aes(x = Ideology, y = Fundamentalist)) +
  geom_point() +
  geom_text_repel(aes(label = Category), max.overlaps = Inf) +
  labs(x = "Ideology", y = "Fundamentalism", title = "2018")+
  theme_classic()+
  scale_x_continuous(limits = c(3, 7.2), breaks = seq(3, 7, by = 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.4), breaks = seq(-0.7, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x))

###LONGITUDINAL ANALYSIS###

df1991 <- Dataframe_dimension_reduction_1991
df1997 <- Dataframe_dimension_reduction_1997
df2006 <- Dataframe_dimension_reduction_2006
df2014 <- Dataframe_dimension_reduction_2014
df2018 <- Dataframe_dimension_reduction_2018

dataframes <- list(
  df1991 = df1991,
  df1997 = df1997,
  df2006 = df2006,
  df2014 = df2014,
  df2018 = df2018
)

combined_df <- bind_rows(
  .id = "ano",
  dataframes
)

fa_unconstrained <- fa(combined_df[,2:14], nfactors = 1, rotate = "varimax")

print(fa_unconstrained)

scores_combined_df <- factor.scores(combined_df[,2:14],fa_unconstrained)
scores_combined_df <- scores_combined_df$scores
combined_df <- cbind(combined_df,scores_combined_df)

combined_df$MR1 <- combined_df$MR1 * (-1)

summary(combined_df)#tutorial so com rc1
combined_df$anof <- as.factor(combined_df$ano)#criar ano factor

combined_df$anof <- relevel(combined_df$anof, "df2014")#2005 como factor
model4 <- lm(MR1~anof, data=combined_df)
summary(model4)
combined_df$anof <- relevel(combined_df$anof, "df2006")#2005 como factor
model3 <- lm(MR1~anof, data=combined_df)
summary(model3)
combined_df$anof <- relevel(combined_df$anof, "df1997")#2005 como factor
model2 <- lm(MR1~anof, data=combined_df)
summary(model2)

combined_df$anof <- relevel(combined_df$anof, "df1991")#2005 como factor
model1 <- lm(MR1~anof, data=combined_df)
summary(model1)


library(sjPlot)
tab_model(model1, model2, model3, model4, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


library(marginaleffects)



figure1 <- plot_cap(model1, condition ="anof", conf_level=.95)+
  labs(y="Fundamentalism (Estimates)",x="Wave")+
  theme_bw()



#install.packages("ggdist")
library(ggdist)
#install.packages("tidyquant")
library(tidyquant)
library(tidyverse)

figure2 <- combined_df %>%
  ggplot(aes(x=MR1, fill=anof))+
  ggdist::stat_halfeye(adjust=0.5,justification = -.2, .width=0, point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=.4, binwidth=.01) +
  scale_fill_tq()+ facet_wrap(anof~.)+
  theme_tq() +
  labs(title= "",
       subtitle = "",
       x = "",
       y= "",
       fill= "",
  ) #+ coord_flip() 

library(gridExtra)

grid.arrange(figure1, figure2, ncol = 2)

###REGRESSION###

fa_constrained <- fa(df1991r[,1:17], nfactors = 1, rotate = "varimax")

scores_df1991r <- factor.scores(df1991r[,1:17],fa_constrained)
scores_df1991r <- scores_df1991r$scores
df1991r <- cbind(df1991r,scores_df1991r)

df1991r$MR1 <- df1991r$MR1 * (-1)

hist(df1991r$MR1, 
     main = "1991",
     xlab = "Fundamentalism",
     ylab = "Frequency",
     col = "blue",  
     border = "black",
     xlim = c((-4), 2)
)


modelo1991 <- lm(MR1~V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24, data=df1991r)

fa_constrained <- fa(df1997r[,1:24], nfactors = 1, rotate = "varimax")

scores_df1997r <- factor.scores(df1997r[,1:24],fa_constrained)
scores_df1997r <- scores_df1997r$scores
df1997r <- cbind(df1997r,scores_df1997r)

modelo1997 <- lm(MR1~V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24, data=df1997r)

hist(df1997r$MR1, 
     main = "1997",
     xlab = "Fundamentalism",
     ylab = "Frequency",
     col = "blue",  
     border = "black",
     xlim = c((-4), 2)
)

fa_constrained <- fa(df2006r[,1:18], nfactors = 1, rotate = "varimax")

scores_df2006r <- factor.scores(df2006r[,1:18],fa_constrained)
scores_df2006r <- scores_df2006r$scores
df2006r <- cbind(df2006r,scores_df2006r)

df2006r$MR1 <- df2006r$MR1 * (-1)

hist(df2006r$MR1, 
     main = "2006",
     xlab = "Fundamentalism",
     ylab = "Frequency",
     col = "blue",  
     border = "black",
     xlim = c((-4), 2)
)

modelo2006 <- lm(MR1~V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24, data=df2006r)

fa_constrained <- fa(df2014r[,1:17], nfactors = 1, rotate = "varimax")

scores_df2014r <- factor.scores(df2014r[,1:17],fa_constrained)
scores_df2014r <- scores_df2014r$scores
df2014r <- cbind(df2014r,scores_df2014r)

df2014r$MR1 <- df2014r$MR1 * (-1)

hist(df2014r$MR1, 
     main = "2014",
     xlab = "Fundamentalism",
     ylab = "Frequency",
     col = "blue",  
     border = "black",
     xlim = c((-4), 2)
)

modelo2014 <- lm(MR1~V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24, data=df2014r)

fa_constrained <- fa(df2018r[,1:19], nfactors = 1, rotate = "varimax")

scores_df2018r <- factor.scores(df2018r[,1:19],fa_constrained)
scores_df2018r <- scores_df2018r$scores
df2018r <- cbind(df2018r,scores_df2018r)

df2018r$MR1 <- df2018r$MR1 * (-1)

hist(df2018r$MR1, 
     main = "2018",
     xlab = "Fundamentalism",
     ylab = "Frequency",
     col = "blue",  
     border = "black",
     xlim = c((-4), 2)
)

modelo2018 <- lm(MR1~V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24, data=df2018r)

library(sjPlot)

a<- plot_models(modelo1991, modelo1997, modelo2006, modelo2014, modelo2018,
                legend.title = "Onda",m.labels = c("1991", "1997", "2006","2014",
                                                   "2018"),rm.terms=c("V19","V20","V21","V22","V23","V24"),
                axis.labels=c("Att Religious Svc","Education Level","Postmaterialism","PT","Right-wing"),show.values = FALSE, 
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

b<- plot_models(modelo1991, modelo1997, modelo2006, modelo2014, modelo2018,
                legend.title = "Onda",m.labels = c("1991", "1997", "2006","2014",
                                                   "2018"),
                axis.labels=c("White","Income","Interest in politics","Woman","Size town","Age","Att Religious Svc","Education Level","Postmaterialism","PT","Right-wing"),show.values = FALSE, 
                show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
                p.threshold = c(0.05, 0.01, 0.001), 
                vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=F)+
  theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))

b + labs(y="Estimates (Fundamentalism)") #Edit
