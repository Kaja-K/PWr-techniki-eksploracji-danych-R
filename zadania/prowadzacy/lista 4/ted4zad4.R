library(cluster)
pl=na.omit(pluton)
pls=scale(pl)
library(factoextra)
fviz_nbclust(pls, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype=2)+
  labs(subtitle = "Metoda łokcia")
km.pl=kmeans(pls, 3, iter.max = 20, nstart = 25)
fviz_cluster(km.pl, data = pl, 
    palette = c("blue", "red","brown"), 
    geom =c("point", "text"), 
    ellipse.type = "convex",ggtheme = theme_bw())