get_all_paths = function(m,n){
  
  path_deviation<-0
  path_list<-c("")
  path_probability_list<-c(1)
  for (i in 1:(m+n)){
    path_list_i<-rep(as.character(NA),length(path_list)*2)#
    path_probability_list_i<-rep(as.double(NA),length(path_list)*2)#
    
    #step 1
    #iterate through the possible paths that could have been taken up to this time point.
    for (j in 1:length(path_list)){
      path<-path_list[j]
      path_probability<-path_probability_list[j]
      newlist_pos<-min(which(is.na(path_probability_list_i)))
      x<-sum(attr(gregexpr("X",path)[[1]],"match.length"))
      y<-sum(attr(gregexpr("Y",path)[[1]],"match.length"))
      #which position are we in our new list?
      #now generate a new path in the pathlist for each of the paths that exist now.
      if(x<m & y<n){#we could move either way
        path_list_i[newlist_pos]<-paste0(path,"X")
        path_probability_list_i[newlist_pos]<-path_probability*0.5
        path_list_i[newlist_pos+1]<-paste0(path,"Y")
        path_probability_list_i[newlist_pos+1]<-path_probability*0.5
      }else if(x<m & y==n){
        path_list_i[newlist_pos]<-paste0(path,"X")
        path_probability_list_i[newlist_pos]<-path_probability
      }else if(x==m & y<n){
        path_list_i[newlist_pos]<-paste0(path,"Y")
        path_probability_list_i[newlist_pos]<-path_probability
      }
    }
    #trim for unused spaces.
    path_list<-path_list_i[!is.na(path_list_i)]
    path_probability_list<-path_probability_list_i[!is.na(path_probability_list_i)]
  }
  return(data.frame("path"=path_list,"path_probability"=path_probability_list))
}


res<-get_all_paths(11,7)
