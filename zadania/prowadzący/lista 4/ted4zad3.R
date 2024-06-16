library(cluster)
names(votes.repub)
vote=votes.repub[,c("X1960","X1964","X1968","X1972","X1976")]
vote=na.omit(vote)
votes=scale(vote)
library(factoextra)
fviz_nbclust(votes, kmeans, method = "wss")+
  geom_vline(xintercept = 7, linetype=2)+
  labs(subtitle = "Metoda łokcia")
km.votes=kmeans(votes, 7, iter.max = 20, nstart = 25)
fviz_cluster(km.votes, data = vote, 
 palette = c("blue", "red","green","grey","brown","black", "orange"), 
 geom =c("point", "text"), 
 ellipse.type = "convex",ggtheme = theme_bw())

vote=votes.repub[,c("X1960","X1976")]
vote=na.omit(vote)
votes=scale(vote)
library(factoextra)
fviz_nbclust(votes, kmeans, method = "wss")+
  geom_vline(xintercept = 6, linetype=2)+
  labs(subtitle = "Metoda łokcia")
km.votes=kmeans(votes, 6, iter.max = 20, nstart = 25)
fviz_cluster(km.votes, data = vote, palette = c("blue", "red","green","grey","brown","black"), geom =c("point", "text"), ellipse.type = "convex",ggtheme = theme_bw())
