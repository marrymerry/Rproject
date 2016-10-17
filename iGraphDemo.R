#install.packages("igraph")
library(igraph)



coauthornetwork.xls <- read.csv("./exportdata/coauthornetwork.csv")
tail(coauthornetwork.xls)

#set graph class
coauthornetwork.xls.graph <- graph.data.frame(coauthornetwork.xls)

plot(coauthornetwork.xls.graph)

summary(coauthornetwork.xls.graph)
vcount(coauthornetwork.xls.graph)
ecount(coauthornetwork.xls.graph)

get.edge.attribute(coauthornetwork.xls.graph, 'times')
#set layout. different functions
times.set.layout <- layout.fruchterman.reingold(coauthornetwork.xls.graph)

coauthornetwork.xls.graph <- set.vertex.attribute(coauthornetwork.xls.graph, "name2", value=c(NA))

#get author attributes
authorattributes.xls <- read.csv("./exportdata/authorattributes.csv")
#set each author's attribute,
#publications record how many publications they publish
for (i in V(coauthornetwork.xls.graph)){
  #set.vertex.attribute(graph, name, index=V(graph), value)
  authorname.tmp <- V(coauthornetwork.xls.graph)[i]
  #authorname.tmp$name get value
  publication.value <- authorattributes.xls[authorattributes.xls$author==authorname.tmp$name,c('publications')]
  coauthornetwork.xls.graph <- set.vertex.attribute(coauthornetwork.xls.graph, "publications", index=i, publication.value)
  
  #set the vertex attribute name2, if publication > 10, set it name NOT NULL
  if(publication.value >= 10)
    coauthornetwork.xls.graph <- set.vertex.attribute(coauthornetwork.xls.graph, "name2", index=i,authorname.tmp$name )
  
}





#set times color, this is the about "edge", not attribute
times.color <- get.edge.attribute(coauthornetwork.xls.graph,"times")
colors = c('Grey', 'Red', 'Blue', 'Yellow', 'Green')
times.color[times.color > 4]  = colors[5]
times.color[times.color == 1] = colors[1]
times.color[times.color == 2] = colors[2]
times.color[times.color == 3] = colors[3]
times.color[times.color == 4] = colors[4]

#set publication color, this is the about  attribute "publications"
publications.color <- get.vertex.attribute(coauthornetwork.xls.graph,"publications")
colors = c('Red', 'Blue', 'Yellow','Grey')
publications.color.result <- c(rep(0,length(publications.color)))
publications.color.result[publications.color > 20]  = colors[1]
publications.color.result[(publications.color <= 20) & (publications.color > 10)]  = colors[2]
publications.color.result[(publications.color <= 10) & (publications.color > 5)]  = colors[3]
publications.color.result[publications.color <= 5]  = colors[4]

#now let's set the vertex size by publications
publications.vertex.sizes = get.vertex.attribute(coauthornetwork.xls.graph,"publications")
sizesused = c(5, 4, 3, 2)
publications.vertex.sizes.result <- c(rep(0,length(publications.vertex.sizes)))
publications.vertex.sizes.result[publications.vertex.sizes > 20]  = sizesused[1]
publications.vertex.sizes.result[(publications.vertex.sizes <= 20) & (publications.vertex.sizes > 10)]  = sizesused[2]
publications.vertex.sizes.result[(publications.vertex.sizes <= 10) & (publications.vertex.sizes > 5)]  = sizesused[3]
publications.vertex.sizes.result[publications.vertex.sizes <= 5]  = sizesused[4]


pdf("./exportdata/coauthor.pdf")

plot(coauthornetwork.xls.graph,vertex.label=NA,edge.arrow.size=.2,vertex.size=1,vertex.label=0.2)

plot(coauthornetwork.xls.graph,layout=times.set.layout,vertex.label=NA,edge.arrow.size=.2,vertex.size=1,vertex.label=0.2)

#--------------Vertex Relative----------
#set color of publications
plot(coauthornetwork.xls.graph,layout=times.set.layout,vertex.color=publications.color.result,vertex.label=V(coauthornetwork.xls.graph)$name2,edge.arrow.size=.2,vertex.size=2.5,vertex.label=0.5) # please note, people who publish more than 10 papers are presented..



plot(coauthornetwork.xls.graph,layout=times.set.layout,vertex.color=publications.color.result,vertex.label=NA,edge.arrow.size=.2,vertex.size=publications.vertex.sizes.result,vertex.label=0.5)
#--------------End Vertex Relative----------



#set color of edge
plot(coauthornetwork.xls.graph,layout=times.set.layout,edge.color=times.color,vertex.label=NA,edge.arrow.size=.2,vertex.size=1,vertex.label=0.2)

plot(coauthornetwork.xls.graph, vertex.size=2, 
     vertex.color="green", vertex.frame.color="red", edge.color="grey",  
     edge.arrow.size=0.01, rescale=TRUE,vertex.label=NA, vertex.label.dist=0.0,
     vertex.label.cex=0.5, add=FALSE,   vertex.label.font=.001)

dev.off()

#let' calculate several features

#closeness
write.csv(closeness(coauthornetwork.xls.graph),"./exportdata/closeness.csv")


closeness(coauthornetwork.xls.graph, mode="in")
closeness(coauthornetwork.xls.graph, mode="out")
closeness(coauthornetwork.xls.graph, mode="all")
betweenness(coauthornetwork.xls.graph)


#let's calculate weak tie

ego.density = function(g)
   {
          l.ego.graphs              = graph.neighborhood(g,1)
          ego.ecount                = sapply(l.ego.graphs, ecount)
          ego.vcount                = sapply(l.ego.graphs, vcount)
          ego.friend.count          = ego.vcount - 1
          ego.friend.tie.count.max  = ego.friend.count*(ego.friend.count-1)/2
          ego.friend.tie.count.real = ego.ecount - ego.friend.count
          ego.density.result        =
                     ego.friend.tie.count.real/ego.friend.tie.count.max
          return(ego.density.result)
 }
ego.density(coauthornetwork.xls.graph)
