
# obs rodar os script ´primeiro
print(pf1.1.3$loadings,cutoff = 0.001)
print(pf2.1.3$loadings,cutoff = 0.001)
print(pf3.1.3$loadings,cutoff = 0.001)
print(pf4.1.3$loadings,cutoff = 0.001)
print(pf5.1.3$loadings,cutoff = 0.001)

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