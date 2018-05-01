
#let's get our new-improved analytical way to calculate D_path average for all possible paths.
#This is missing the right way to weight each path.
D_Path_Avg<-function(m,n){
  d_path=0
  #iterate through the steps.
  for (i in 1:(m+n)){
    d_i_sum=0
    
    #Calculate the theoretical range of x values we could have if the space wasn't constrained by m,n
    theoretical_x_range=0:i
    #and the actual range of possible x values for x values given the space constraints
    #duplicating the lower and upper bounds; where there's a difference, 
    #that's gonna tell us about a possible set of n,m-constrained paths that end up in a particular place
    #duplicate values will tell us how many different ways there are to end up in a particular spot
    
    possible_x_range=sapply(sapply(theoretical_x_range,function(x){max(x,(i-n))}),function(x){min(x,m)})
    for (x_i in 1:length(possible_x_range)){
      #go through the possible range of x values
      #there's one x value for each point, plus, one extra x value for every set of n-m-constrained paths
      #each of these set of paths has equal probability and shoudl be equally weighted
      x=possible_x_range[x_i]
      #sum up the sum of deviations for all poitns we've reached at this time step
      d_i_sum=d_i_sum+d_point(x=x,y=i-x,m,n)*(1+abs(theoretical_x_range[x_i]-possible_x_range[x_i]))
    }
    #the total deviation across all points for this time step divided by the number of points,
    #counting a point again every time an m,n constraint has led ot that point being arrived at.
    d_i_avg=d_i_sum/sum(1+abs(theoretical_x_range-possible_x_range))
    d_path=d_path+d_i_avg
  }
  return(d_path)
}

#D_Path_Avg(1,1)
#mean(boot2(1,1))
print(D_Path_Avg(2,1))

#Maybe we need a matrix storing (a) the d_point at each point on the grid, and (b) the probability of getting to it
D_Path_Avg(2,2)
D_Path_Avg(11,7)
D_Path_Avg(10,10)
D_Path_Avg(100,100)


#an alternative that works equall well as below.
#here we actually create a pair of matrices and iterate through those matrices.
D_Path_Avg_Iterative<-function(m,n){
  #create matrices to store the deviation and probability of reach each point in the matrix
  d_point_matrix<-matrix(as.integer(NA),m+1,n+1)
  prob_point_matrix<-matrix(0,m+1,n+1)
  #initial point of origin values.
  prob_point_matrix[1,1]<-1
  d_point_matrix[1,1]<-d_point(x = 0,y = 0,m = m,n = n)
  
  #d_point matrix is easy enough: just iterate through x and y nested.
  #probability point matrix...we want to iterate through each point and add the probabilities suggested from the two points that reach out to it.
  #d_point matrix, we do a single list for each i to 0 to m+n,
  #and then within each i, we iterate through each combination of x and y which would be reached at step i.
  #while we're doing this we can iterate through the probability point matrix too.
  for (i in 1:(m+n)){
    #y must be at least i-m and at most m
    for (j in max(0,i-m):min(i,n)){
      x=i-j
      y=j
      d_point_matrix[x+1,y+1]<-d_point(x = x,y = y,m = m,n = n)
      if(x>0) prob_point_matrix[x+1,y+1]<-prob_point_matrix[x+1,y+1]+prob_point_matrix[x,y+1]*(0.5+0.5*(y>=n))
      if(y>0) prob_point_matrix[x+1,y+1]<-prob_point_matrix[x+1,y+1]+prob_point_matrix[x+1,y]*(0.5+0.5*(x>=m))
    }
  }
  #now iterate through the matrix. The iteration process is interesting; we need to move through x and y simultaneously.
  #now the sum is going to be the summed total of the probability times the d_point
  return(sum(d_point_matrix*prob_point_matrix))
}

# print(D_Path_Avg_Iterative(2,1))
# print(D_Path_Avg_Iterative(2,2))
# D_Path_Avg(2,2)
# print(D_Path_Avg_Iterative(1,2))
# D_Path_Avg(1,2)
print(D_Path_Avg_Iterative(2,2))
D_Path_Avg(2,2)
# mean(boot2(2,2))
#could also do this recursively but that might take too much time.

boot3<-function(m,n,total_iter=10000){
  
  D_sample=rep.int(NA,total_iter)
  path.sample=rep("",total_iter)
  for (i in 1:total_iter){
    path=paste0(sample(c(rep("X",m),rep("Y",n))),collapse="")
    D_sample[i]=D_path(m,n,path)
    path.sample[i]<-path
  }
  data.frame("path"=path.sample,"D"=D_sample)
}
# table(boot3(2,2,10000)[,1])/10000
# #ooooh.
# #this method of generating random paths doesn't work, because actually paths along the outside are more likely
# #this method doesn't recognize that.
# 
# D_Path_Avg_Iterative(11,7)

D_Path_Avg_Iterative(2,3)

D2=matrix(as.integer(NA),10,10)
for (m in 1:10){
  for(n in 1:10){
    D2[n,m]=mean(D_Path_Avg_Iterative(m,n))
    cat(".")
  }
}

D3=matrix(as.integer(NA),10,10)
for (m in 1:10){
  for(n in 1:10){
    D3[n,m]=mean(D_Path_Avg_Iterative(m,n))
    cat(".")
  }
}