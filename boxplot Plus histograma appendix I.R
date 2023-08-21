# histogramas
# fonte : https://twitter.com/mdancho84/status/1583437200792117254?t=4iHtufTG_m0ZhMFzC7fp0w&s=08


#install.packages("ggdist")
library(ggdist)
#install.packages("tidyquant")
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
