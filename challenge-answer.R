
d_point=function(x,y,m,n){max(x/m-y/n,y/n-x/m)}



#
#here we actually create a pair of matrices and iterate through those matrices.
D_Path_Avg_Iterative<-function(m,n){
  #create matrices to store the deviation and probability of reaching each point in the matrix
  d_point_matrix<-matrix(as.integer(NA),m+1,n+1)
  DD_matrix<-matrix(as.double(NA),m+1,n+1)
  DD_cumulative_matrix<-matrix(as.double(NA),m+1,n+1)
  prob_point_matrix<-matrix(0,m+1,n+1)
  
  #initial point of origin values: the deviation is zero and the probability of reaching across all possible paths is 1.
  prob_point_matrix[1,1]<-1
  d_point_matrix[1,1]<-d_point(x = 0,y = 0,m = m,n = n)
  
  #d_point matrix is easy enough: just iterate through x and y nested.
  #probability point matrix...we want to iterate through each point and add the probabilities suggested from the two points that reach out to it.
  #d_point matrix, we do a single list for each i to 0 to m+n,
  #and then within each i, we iterate through each combination of x and y which would be reached at step i.
  #while we're doing this we can iterate through the probability point matrix too.
  i_point_i_mean<-rep(as.double(0),m+n)
  #iterate through time steps
  for (i in 1:(m+n)){
    #now for each time step we can specify the possible number of x and y moves the ant could have made
    #the minimum number of y steps is the greater of 0, or the number of time steps minus the maximum x steps (since you'd have run out of x steps and would be forced to make y steps)
    #this will ensure we only process each point once, and that we only process it after processing the steps left and bottom of it.
    for (y in max(0,i-m):min(i,n)){
      #the number of x steps is the number of timesteps minus the number of y steps.
      x=i-y
      #so for this particular point, now that we've identified one, mark its deviation
      d_point_matrix[x+1,y+1]<-d_point(x = x,y = y,m = m,n = n)
      #and then *add* to the probability of reaching this point.
      #the two possible immediate predecessor origins are one point left and one point down
      #so if there is a point left/down
      #then add 0.5 times the probability of having reached that point
      #and if there's a 
      if(x>0) prob_point_matrix[x+1,y+1]<-prob_point_matrix[x+1,y+1]+prob_point_matrix[x,y+1]*(0.5+0.5*(y>=n))
      if(y>0) prob_point_matrix[x+1,y+1]<-prob_point_matrix[x+1,y+1]+prob_point_matrix[x+1,y]*(0.5+0.5*(x>=m))
      #mean of all points at this time step
      i_point_i_mean[i]<-i_point_i_mean[i]+d_point_matrix[x+1,y+1]*prob_point_matrix[x+1,y+1]
    }
  }
  #now iterate through the matrix. The iteration process is interesting; we need to move through x and y simultaneously.
  #now the sum is going to be the summed total of the probability times the d_point
  D_mean<-sum(d_point_matrix*prob_point_matrix)
  
  return(D_mean)
}



formatC(D_Path_Avg_Iterative(11,7),digits=10)
formatC(D_Path_Avg_Iterative(23,31),digits=10)
