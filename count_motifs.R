#look at motif structures in purely random specialization networks (count feed-forward loops and feed-back loops)

source('./NSCI_final/find_triangles.R')

motifs_fbl <- as.list(1:2)
motifs_ffl <- as.list(1:6)

motifs_fbl[[1]] <- matrix(c(0,1,0,0,0,1,1,0,0),ncol=3, nrow=3, byrow=T)
motifs_fbl[[2]] <- matrix(c(0,0,1,1,0,0,0,1,0),ncol=3, nrow=3, byrow=T)
motifs_ffl[[1]] <- matrix(c(0,1,1,0,0,0,0,1,0),ncol=3, nrow=3, byrow=T)
motifs_ffl[[2]] <- matrix(c(0,1,1,0,0,1,0,0,0),ncol=3, nrow=3, byrow=T)
motifs_ffl[[3]] <- matrix(c(0,0,0,1,0,0,1,1,0),ncol=3, nrow=3, byrow=T)
motifs_ffl[[4]] <- matrix(c(0,1,0,0,0,0,1,1,0),ncol=3, nrow=3, byrow=T)
motifs_ffl[[5]] <- matrix(c(0,0,1,1,0,1,0,0,0),ncol=3, nrow=3, byrow=T)
motifs_ffl[[6]] <- matrix(c(0,0,0,1,0,1,1,0,0),ncol=3, nrow=3, byrow=T)

files <- list.files('./specRandNets/')

counts_fbl <- 1:length(files)
counts_ffl <- 1:length(files)
counts_nodes <- 1:length(files)
count_tot <- 1:length(files)

for(i in 1:length(files)){
  adj_mat <- as.matrix(read.csv(paste('./specRandNets/',files[i],sep=""), header=F))
  n <- ncol(adj_mat)
  counts_nodes[i] <- n
  triangles <- find_triangles(adj_mat)
  locs <- triangles[[2]]
  count_tot[i] <- triangles[[1]]
  count_fbl <- 0
  count_ffl <- 0
  if(triangles[[1]]>0){
    for(j in 1:triangles[[1]]){
      loc <- locs[j,]
      tri_mat <- adj_mat[loc,loc]
      tri_mat <- unname(tri_mat)
      for(k in 1:2){
        if(identical(tri_mat, motifs_fbl[[k]])){
          count_fbl <- count_fbl+1
        }
      }
      for(k in 1:6){
        if(identical(tri_mat, motifs_ffl[[k]])){
          count_ffl <- count_ffl+1
        }
      }
      print(tri_mat)
    }
  }
  counts_fbl[i] <- count_fbl
  counts_ffl[i] <- count_ffl
  print(paste(round(i/length(files)*100, 2),'%',sep=''))
}

save(counts_nodes, count_tot, counts_fbl, counts_ffl, file='complete_random_counts.RData')

files <- list.files('./samples/')

counts_fbl <- 1:length(files)
counts_ffl <- 1:length(files)
counts_nodes <- 1:length(files)
count_tot <- 1:length(files)

for(i in 1:length(files)){
  edg_lst <- as.matrix(read.csv(paste('./samples/',files[i],sep=""), header=T))
  nodes <- unique(c(unique(edg_lst[,1]), unique(edg_lst[,2])))
  n <- length(nodes)
  adj_mat <- matrix(rep(0, n^2), ncol=n, nrow=n)
  for(j in 1:nrow(edg_lst)){
    a <- which(nodes==edg_lst[j,1])
    b <- which(nodes==edg_lst[j,2])
    adj_mat[a,b] <- 1
  }
  counts_nodes[i] <- n
  triangles <- find_triangles(adj_mat)
  locs <- triangles[[2]]
  count_tot[i] <- triangles[[1]]
  count_fbl <- 0
  count_ffl <- 0
  if(triangles[[1]]>0){
    for(j in 1:triangles[[1]]){
      loc <- locs[j,]
      tri_mat <- adj_mat[loc,loc]
      tri_mat <- unname(tri_mat)
      for(k in 1:2){
        if(identical(tri_mat, motifs_fbl[[k]])){
          count_fbl <- count_fbl+1
        }
      }
      for(k in 1:6){
        if(identical(tri_mat, motifs_ffl[[k]])){
          count_ffl <- count_ffl+1
        }
      }
    }
  }
  counts_fbl[i] <- count_fbl
  counts_ffl[i] <- count_ffl
  print(paste(round(i/length(files)*100, 2),'%',sep=''))
}
