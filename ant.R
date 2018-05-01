m=11
n=7
d_point=function(x,y,m,n){max(x/m-y/n,y/n-x/m)}

#analytical approach to gain some insight into this.
#path should be an m+n length character string specifying whether we move along the x or y axis at each point in time.
#e.g., path="XXXYYY"
D_path=function(m,n,path){
  #make sure this is valid path.
  path_xcount=sum(attr(gregexpr("X",path,ignore.case=FALSE)[[1]],"match.length"))
  path_ycount=sum(attr(gregexpr("Y",path,ignore.case=FALSE)[[1]],"match.length"))
  stopifnot(path_xcount==m)
  stopifnot(path_ycount==n)
  stopifnot(path_ycount+path_xcount==nchar(path))
  
  #iterate through the path
  x=0
  y=0
  D=d_point(x,y,m,n)#this will always be zero.
  for (move_id in 1:nchar(path)){
    #what's this turn's move?
    move=substr(path,move_id,move_id)
    if(move=="X"){
      x=x+1
    }else if (move=="Y"){
      y=y+1
    }else{
      #this should not be reached unless there's a bug in the code above.
      stop("path contained one or more illegal values. Path should only contain X or Y")
    }
    D=D+d_point(x,y,m,n)
  }
  return(D)
}


#path should be an m+n length character string specifying whether we move along the x or y axis at each point in time.
path="XY"
D_path(m=1,n=1,path)

D_path(m=2,n=2,path="XXYY")

#Largest possible deviations
D_path(m=11,n=7,path=paste0(c(rep("X",11),rep("Y",7)),collapse=""))
D_path(m=11,n=7,path=paste0(c(rep("Y",7),rep("X",11)),collapse=""))
#Doesn't matter whether we go along x or go along y; all that matters is deviaiotn from the central point.

D_path(m=5,n=4,path="XYXYXYXYX")
D_path(m=5,n=4,path="XYXYXYXXY")
D_path(m=5,n=4,path="XXXXXYYYY")

#max deviation is always half of m+n
for (m in sample(1:100,10)){
  for (n in sample(1:100,10)){
    D=D_path(m=m,n=n,path=paste0(c(rep("X",m),rep("Y",n)),collapse=""))
    print(D/(m+n))
  }
}
D_path(m=11,n=7,path=paste0(c(rep("X",11),rep("Y",7)),collapse=""))



#observations to help simplify this:
#Deviation of start point will always be zero because x,y==0
#Deviation of end point will alwasy be zero because x/m, y/n==0
#Doesn't matter whether we go along x or go along y; all that matters is deviation from the central point.
#max deviation is always half of m+n

#so what's the mean of D when m==11, n==7
#let's do 1000 random samples.
m=11
n=7
total_iter=10000
D_sample=rep.int(NA,total_iter)
for (i in 1:total_iter){
  path=paste0(sample(c(rep("X",m),rep("Y",n))),collapse="")
  D_sample[i]=D_path(m,n,path)
}
mean(D_sample)
#we have an empirical result, but it varies a lot, so I can't get the precise value from this.
#There is probably an analytical solution to this problem that gives us a precise answer.
#could we work it out from induction?

boot<-function(m,n){
  total_iter=10000
  D_sample=rep.int(NA,total_iter)
  for (j in 1:10){
    for (i in 1:total_iter){
      path=paste0(sample(c(rep("X",m),rep("Y",n))),collapse="")
      D_sample[i]=D_path(m,n,path)
    }
    print(mean(D_sample))
  }
}

m=1
n=1
boot(m,n)
#it's 1

boot(m=1,n=2)
#1+1/3

boot(m=1,n=3)
#1+2/3

boot(m=2,n=2)
#1+1/3
#interesting, this isn't more than boot(m=1,n=2)

boot2<-function(m,n){
  total_iter=10000
  D_sample=rep.int(NA,total_iter)
  for (i in 1:total_iter){
    path=paste0(sample(c(rep("X",m),rep("Y",n))),collapse="")
    D_sample[i]=D_path(m,n,path)
  }
  D_sample
}

#alright let's get really aggressive here.
D=matrix(as.integer(NA),10,10)
for (m in 1:10){
  for(n in 1:10){
    D[n,m]=mean(boot2(m,n))
    cat(".")
  }
}
cat("\n")
D
#so there isn't a clear and obvious simple function that stands out but...

#more equal sizes of the array reduces the size (because they can't deviate as much)
diag(D)

#So let's try a naive analytical approach.

#first, the 1x1 has only one point along it.
D_path(1,1,"XY")

#The 2x2 has 6 possible paths
D_path(2,2,"XXYY")
D_path(2,2,"XYXY")
D_path(2,2,"XYYX")
D_path(2,2,"YXXY")
D_path(2,2,"YXYX")
D_path(2,2,"YYXX")

#The 2x1 has 3
D_path(2,1,"XXY")
D_path(2,1,"XYX")
D_path(2,1,"YXX")

