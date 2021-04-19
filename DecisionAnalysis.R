
install.packages("chillR")
install.packages("igraph")
library(igraph)

#Plot 1
nodes<-data.frame(id=c("1: Economic disadvantages","2: Structural problems","3: Restructuring","4: Better economic situation"),
                  color=c("white"))
edges<-data.frame(from=c("1: Economic disadvantages","2: Structural problems","3: Restructuring","4: Better economic situation"), 
                  to=c("2: Structural problems","3: Restructuring","4: Better economic situation", "1: Economic disadvantages"),
                  weight=c(1,2,3,4))

Alltogether<-graph_from_data_frame(edges,directed=F,vertices=nodes)

plot(Alltogether,directed=FALSE, direction="climb", main="Economic options in terms of gender differences on german farms")

#Plot 2

nodes<-data.frame(id=c("1: Low pensions","2: No paid work","3: Hire/ make females co-owners/ establish special pension system for farmers","4: Live improves"),
                  color=c("white"))
edges<-data.frame(from=c("1: Low pensions","2: No paid work","3: Hire/ make females co-owners/ establish special pension system for farmers","4: Live improves"), 
                  to=c("2: No paid work","3: Hire/ make females co-owners/ establish special pension system for farmers","4: Live improves", "1: Low pensions"),
                  weight=c(1,2,3,4))

Alltogether<-graph_from_data_frame(edges,directed=F,vertices=nodes)

plot(Alltogether,directed=FALSE, direction="climb", main="Pension situation of female farmers in germany")