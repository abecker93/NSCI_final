##Load C. elegans csv edgelist and select 100 different highly connected subsets from it

c_tab <- read.table(file='./NSCI_final/C-elegans-frontal.txt', header=F)

unique_nodes <- unique(c_tab[,1])

samples <- as.list(1:1000)

for(i in 1:1000){
  samples[[i]] <- sample(unique_nodes, 31)
}

edgelists <- as.list(1:1000)

for(i in 1:1000){
  samp <- samples[[i]]
  edgelists[[i]] <- c_tab[c_tab[,1]%in%samp&c_tab[,2]%in%samp,]
}

nodelist <- 1:1000
for(i in 1:1000){
  nodelist[i] <-length(unique(edgelists[[i]][,1]))
}

mean(nodelist)
max(nodelist)
min(nodelist)

graph <- graph_from_data_frame(edgelists[[200]])
plot(graph)

for(i in 1:1000){
  fin_edge <- edgelists[[i]]
  colnames(fin_edge) <- c('From', 'To')
  write.csv(fin_edge, file=paste('samples/celegans_samp_', i, '.csv', collapse=''), row.names = F)
}