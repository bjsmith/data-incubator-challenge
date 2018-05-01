d_point_matrix<-matrix(as.integer(NA),m+1,n+1)
prob_point_matrix<-matrix(0,m+1,n+1)

#initial point of origin values: the deviation is zero and the probability of reaching across all possible paths is 1.
prob_point_matrix[1,1]<-1
d_point_matrix[1,1]<-d_point(x = 0,y = 0,m = m,n = n)


get_random_path = function(m,n){
  path=""
  x<-0
  y<-0 #origin
  path_deviation<-0
  for (i in 1:(m+n)){
    #step 1
    if(x<m & y<n){#we could move either way
      if(sample(c(0,1),1)==0){
        x<-x+1
        path<-paste0(path,"X")
        path_deviation<-path_deviation+d_point(x = x,y = y,m = m,n = n)
      }else{
        y<-y+1
        path<-paste0(path,"Y")
        path_deviation<-path_deviation+d_point(x = x,y = y,m = m,n = n)
      }
    }else if (x<m & y==n){#must move in x direction because that's the only one available.
      x<-x+1
      path<-paste0(path,"X")
      path_deviation<-path_deviation+d_point(x = x,y = y,m = m,n = n)
    }else if (x==m & y<n){#must move in y direction because that's the only one available.
      y<-y+1
      path<-paste0(path,"Y")
      path_deviation<-path_deviation+d_point(x = x,y = y,m = m,n = n)
    }else{
      #we've reached the point of origin.
      stop("don't know how we got here.")
    }
  }
  return(data.frame("path"=path,"D"=path_deviation))
}


#get set of paths with probabilities and other statistics.
#this will be large but hopefully not intractable?
#I think we need to recurse.





boot4<-function(m,n,total_iter=10000){
  
  D_sample=rep.int(NA,total_iter)
  path.sample=NULL
  for (i in 1:total_iter){
    path.sample.i<-get_random_path(m,n)
    if(is.null(path.sample)){
      path.sample<-path.sample.i
    }else{
      path.sample<-rbind(path.sample,path.sample.i)
    }
  }
  return(path.sample)
}

#these are empirical values that should suffice to check the analytical solutions.
mean(boot4(2,1,1000)$D)
mean(boot4(2,1,10000)$D)
mean(boot4(11,7,1000)$D)

sd(boot4(11,7,1000)$D)

