#Function for finding the locations of all triangle motifs within an adjacency matrix

find_triangles <- function(matrix){
  n <- ncol(matrix)
  num_tri <- 0
  tri_list <- matrix(numeric(0), ncol=3, nrow=0)
  for(i in 1:n){
    j_vals <- which(matrix[i,]!=0)
    j_vals <- j_vals[j_vals>i]
    for(j in j_vals){
      k_vals <- which(matrix[j,]!=0)
      k_vals <- k_vals[k_vals>j]
      for(k in k_vals){
        if(matrix[k,i]==1){
          num_tri=num_tri+1
          tri_list <- rbind(tri_list, c(i,j,k))
        }else{
          if(matrix[i,k]==1){
            num_tri=num_tri+1
            tri_list <- rbind(tri_list, c(i,j,k))
          }
        }
      }
    }
  }
  final <- as.list(1:2)
  final[[1]] <- num_tri
  final[[2]] <- tri_list
  return(final)
}
