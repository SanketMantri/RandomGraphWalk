walk <- function(graph,startnode,totalsteps){
  totalnods <- readline(prompt="Enter number of nodes in the provided graph: ") 
  totalnods <- as.integer(totalnods)
  adjmat<-as_adjacency_matrix(graph)
  
  for(i in 1:totalnods)
  {
    prob<-(10/sum(adjmat[i,]))/10
    
    for(j in 1:totalnods)
    {
      if(adjmat[i,j] == 1)
      {
        adjmat[i,j]<- prob
      }
    }
    
  }
  print(adjmat)
  nxtmove <- startnode
  totalwalk <- NULL
  steps <- totalsteps
  for(i in 1:steps){
    nxtmove<- sample(1:totalnods,1,prob = adjmat[nxtmove,],replace = F)
    totalwalk[i] <- nxtmove
  }
  nodeprob <- NULL
  for(i in 1:totalnods){
    nodeprob[i] <- (sum(totalwalk == i)/steps)
  }
  return(nodeprob)
}
walk(AssignmentGraph,startnode = 2,totalsteps = 1000)