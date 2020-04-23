
library(igraph)
#install.packages('readr')
library(readr)

setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Social_Network/")

fox.nodes <- read_csv(file = 'fox_nodes_cl.csv', col_types = cols(
  node1 = col_character(),
    node2 = col_character(),
    directed = col_logical()))


fox.edges <- read_csv(file = 'fox_edges_cl.csv', col_types = cols(
  name = col_character()))

fox.nodes$node1 <- factor(fox.nodes$node1)
fox.nodes$node2 <- factor(fox.nodes$node2)
fox.edges$name <- factor(fox.edges$name)

library(dplyr)
library(tidyr)

fox.edges <- fox.edges %>% 
  mutate(RowNum = row_number())

fox.graph <- graph_from_data_frame(d = fox.nodes, vertices = fox.edges, directed = T)

summary(fox.graph)

#install.packages('Rfacebook')

library(Rfacebook)
library(gridExtra)
library(ggplot2)

fox.df <- data.frame(id=V(fox.graph)$name,
                    name=V(fox.graph)$label,
                    category=V(fox.graph)$category,
                    fans=as.numeric(V(fox.graph)$fan_count),
                    talking_about=as.numeric(V(fox.graph)$talking_about_count),
                    post_activity=as.numeric(V(fox.graph)$post_activity),
                    stringsAsFactors=FALSE)

degrees <- degree(fox.graph, mode="total")
degrees_df <- data.frame(ID=V(fox.graph)$name,
                         Name=V(fox.graph)$label,
                         Degree=as.vector(degrees))

tkplot(fox.graph,
       vertex.size = 10,
       vertex.color="orange",
       vertex.frame.color= "white",
       vertex.label.color = "black",
       vertex.label.family = "sans",
       edge.width=0.2,
       edge.arrow.size=0,
       edge.color="grey",
       edge.curved=TRUE,
       layout = layout.fruchterman.reingold)

# Degree 

degree_fox <- degree(fox.graph, mode="total")
degree_fox_df <- data.frame(Name=V(fox.graph)$label,
                            Degree=as.vector(degree_fox))
degree_fox_df <- degree_fox_df[order(degree_fox_df$Degree, decreasing=TRUE),]


# Betweenness 

betweenness_fox <- betweenness(fox.graph)
betweenness_fox_df <- data.frame(Name=V(fox.graph)$label,
                                 Betweenness=as.vector(betweenness_fox))
betweenness_fox_df <- betweenness_fox_df[order(betweenness_fox_df$Betweenness, decreasing=TRUE),]

# Closeness

closeness_fox <- closeness(fox.graph, mode = 'total')
closeness_fox_df <- data.frame(Name=V(fox.graph)$label,
                            Closeness=as.vector(closeness_fox))
closeness_fox_df <- closeness_fox_df[order(closeness_fox_df$Closeness, decreasing=TRUE),]


View(head(degree_fox_df, 10)) 
View(head(closeness_fox_df, 10)) # Host Brain Kilmeade and Fox News of course - Fox news graph 
View(head(betweenness_fox_df, 10)) # Duck Dynasty and Larry the Cable Guy

# Density 

edge_density(fox.graph) #.059 seems low

# Transitivity

transitivity(fox.graph) #20.47 so there is about a 1/5 chance they are connected
# IE if you watch fox and friends, and you are connected to Fox News, there is a 1 in 5 chance you also 
# follow Larry the Cable Guy...fucking idiotic. 

# Correlation Between Degree and Betweenness # Give labels to the dots
# Where is Donald Trump - Does he not use facebook?

rsq <- format(cor(degree_fox, betweenness_fox)^2, digits=3)
corr_plot <- ggplot(fox.df, aes(x=degree_fox, y=betweenness_fox))+ theme_bw() +
  geom_jitter(alpha=1/2) +
  geom_text(aes(label=name),hjust=0, vjust=0) +
  scale_y_log10() +
  labs(x="Degree", y="Betweenness") +
  annotate("text", label=paste("R-sq =", rsq), x=+Inf, y=1, hjust=1)
corr_plot

# Ounumbered Fox News is Fox News for women



#### Get NPR 

npr.nodes <- read_csv(file = 'npr_links_cl.csv', col_types = cols(
  node1 = col_character(),
  node2 = col_character(),
  directed = col_logical()))


npr.edges <- read_csv(file = 'npr_edges_cl.csv', col_types = cols(
  name = col_character()))

npr.edges <- npr.edges %>% 
  mutate(RowNum = row_number())

npr.graph <- graph_from_data_frame(d = npr.nodes, vertices = npr.edges, directed = T)

summary(npr.graph)

library(Rfacebook)
library(gridExtra)
library(ggplot2)


#### NPR Social Network Chart 

npr.df <- data.frame(id=V(npr.graph)$name,
                     name=V(npr.graph)$label,
                     category=V(npr.graph)$category,
                     fans=as.numeric(V(npr.graph)$fan_count),
                     talking_about=as.numeric(V(npr.graph)$talking_about_count),
                     post_activity=as.numeric(V(npr.graph)$post_activity),
                     stringsAsFactors=FALSE)

degrees <- degree(npr.graph, mode="total")
degrees_df <- data.frame(ID=V(npr.graph)$name,
                         Name=V(npr.graph)$label,
                         Degree=as.vector(degrees))

tkplot(npr.graph,
       vertex.size = 10,
       vertex.color="orange",
       vertex.frame.color= "white",
       vertex.label.color = "black",
       vertex.label.family = "sans",
       edge.width=0.2,
       edge.arrow.size=0,
       edge.color="grey",
       edge.curved=TRUE,
       layout = layout.fruchterman.reingold)

# Degree 

degree_npr <- degree(npr.graph, mode="total")
degree_npr_df <- data.frame(Name=V(npr.graph)$label,
                            Degree=as.vector(degree_npr))
degree_npr_df <- degree_npr_df[order(degree_npr_df$Degree, decreasing=TRUE),]


# Betweenness 

betweenness_npr <- betweenness(npr.graph)
betweenness_npr_df <- data.frame(Name=V(npr.graph)$label,
                                 Betweenness=as.vector(betweenness_npr))
betweenness_npr_df <- betweenness_npr_df[order(betweenness_npr_df$Betweenness, decreasing=TRUE),]

# Closeness

closeness_npr <- closeness(npr.graph, mode = 'total')
closeness_npr_df <- data.frame(Name=V(npr.graph)$label,
                               Closeness=as.vector(closeness_npr))
closeness_npr_df <- closeness_npr_df[order(closeness_npr_df$Closeness, decreasing=TRUE),]


View(head(degree_npr_df, 10)) 
View(head(closeness_npr_df, 10)) #  
View(head(betweenness_npr_df, 10)) # 

# Density 

edge_density(npr.graph) #.127

# Transitivity

transitivity(npr.graph) # .317

# Correlation Between Degree and Betweenness # Give labels to the dots


rsq <- format(cor(degree_npr, betweenness_npr)^2, digits=3)
corr_plot <- ggplot(npr.df, aes(x=degree_npr, y=betweenness_npr))+ theme_bw() +
  geom_jitter(alpha=1/2) +
  geom_text(aes(label=name),hjust=0, vjust=0) +
  scale_y_log10() +
  labs(x="Degree", y="Betweenness") +
  annotate("text", label=paste("R-sq =", rsq), x=+Inf, y=1, hjust=1)
corr_plot


# NPR DF Analysis 

library(dplyr)
library(tidyr)

# Append the summary statistics onto them as columns? 

npr.df$Source = "NPR"
fox.df$Source = "Fox and Friends"

s.media.master <- bind_rows(npr.df, fox.df)

npr.df.combined <- merge(npr.df, betweenness_npr_df, by.x = c('name'), by.y = c("Name"))
npr.df.combined <- merge(npr.df.combined, closeness_npr_df, by.x = c('name'), by.y = c("Name"))
npr.df.combined <- merge(npr.df.combined, degree_npr_df, by.x = c('name'), by.y = c("Name"))

npr.df.combined$EdgeDensity <- edge_density(npr.graph)
npr.df.combined$Transitivity <- transitivity(npr.graph)


fox.df.combined <- merge(fox.df, betweenness_fox_df, by.x = c('name'), by.y = c("Name"))
fox.df.combined <- merge(fox.df.combined, closeness_fox_df, by.x = c('name'), by.y = c("Name"))
fox.df.combined <- merge(fox.df.combined, degree_fox_df, by.x = c('name'), by.y = c("Name"))

fox.df.combined$EdgeDensity <- edge_density(fox.graph)
fox.df.combined$Transitivity <- transitivity(fox.graph)

s.media.master.combined <- bind_rows(npr.df.combined, fox.df.combined)

write.csv(s.media.master.combined, file = 'network_npr_fox_master.csv')

##### Network graphs 

plot(fox.graph, edge.arrow.size = .4, edge.curved = 1)

#deg <- degree(fox.graph, mode = "all")
#V(fox.graph)$color <- colrs[V(fox.graph)$media.type]
#V(fox.graph)$size <- V(fox.graph)$fan_count*0.1

#V(fox.graph)$label <- NA

#E(fox.graph)$arrow.size <- .2
#E(fox.graph)$edge.color <- "gray80"

graph_attr(fox.graph, "layout") <- layout_with_lgl
plot(fox.graph)

#plot(fox.graph, edge.arrow.size = .4, edge.curved = 1)

l <- layout_in_circle(fox.graph)
plot(fox.graph, layout=l)

l <- layout_on_sphere(fox.graph)
plot(fox.graph, layout=l)

l <- layout_with_fr(fox.graph)
plot(fox.graph,edge.arrow.size = .4, layout=l)

l <- layout_with_kk(fox.graph)
plot(fox.graph, edge.arrow.size = .4,layout=l)

l <- layout_nicely(fox.graph)
plot(fox.graph, edge.arrow.size = .4,layout=l)

l <- layout_as_star(fox.graph)
plot(fox.graph, edge.arrow.size = .4,layout=l)


# Fox
clp <- cluster_optimal(fox.graph)
class(clp)

plot(clp, fox.graph)
V(fox.graph)$community <- clp$membership

deg <- degree(fox.graph, mode="all")

colrs <- c("lightblue", "tomato", "gold", "yellowgreen")
plot(fox.graph, vertex.color=colrs[V(fox.graph)$community], 
     layout = l, vertex.label=V(fox.graph)$RowNum, 
     edge.arrow.size = .4)

layout.group <- data.frame(V(fox.graph)$community, V(fox.graph)$RowNum, 
              V(fox.graph)$label, deg)

layout.group.sort <- layout.group %>%
  arrange(V(fox.graph)$community, desc(deg))

setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Social_Network/")
write.csv(layout.group.sort, file = 'fox_clusters.csv')

# NPR
clp2 <- cluster_optimal(npr.graph)
V(npr.graph)$community <- clp2$membership
deg2 <- degree(npr.graph, mode="all")

colrs <- c("lightblue", "tomato", "gold", "yellowgreen")

l <- layout_with_fr(npr.graph)
#l <- layout_with_kk(npr.graph)
plot(npr.graph, vertex.color=colrs[V(npr.graph)$community], 
     layout = l, vertex.label=V(npr.graph)$RowNum,
     edge.arrow.size = .4)

layout.group2 <- data.frame(V(npr.graph)$community, V(npr.graph)$RowNum, 
                           V(npr.graph)$label, deg2)

layout.group.sort2 <- layout.group %>%
  arrange(V(npr.graph)$community, desc(deg2))

setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Social_Network/")
write.csv(layout.group2, file = 'npr_clusters.csv')
