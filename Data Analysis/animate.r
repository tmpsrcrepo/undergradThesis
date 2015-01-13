getcorners1=function(mydata) {
  
  # Find min/max for each dimension
  min_X = min(mydata[,1])
  max_X = max(mydata[,1])
  min_Y = min(mydata[,2])
  max_Y = max(mydata[,2])
  min_Z = min(mydata[,3])
  max_Z = max(mydata[,3])
  
  # Use min/max points to form the corners of the graphing box.
  c= matrix(c(
    min_X, min_Y, min_Z,
    min_X, min_Y, max_Z,
    min_X, max_Y, min_Z,
    min_X, max_Y, max_Z,
    max_X, min_Y, min_Z,
    max_X, min_Y, max_Z,
    max_X, max_Y, min_Z,
    max_X, max_Y, max_Z),
    byrow=TRUE, nrow=8, dimnames=list(NULL,c('x','y','z')));
  
  return(c);
}
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
animate1=function(const=0,mydata,jindex=1:3,starta=1,stopb=dim(mydata)[1],p.Per=5,sleep=0,sindex=4 ) {
  corner.pts=getcorners1(mydata);
  plot3d(corner.pts,col='white');
  aspect3d("iso");
  colarray <- c('red', 'orange', 'yellow', 'green', 'blue', 'purple');
 
  i<-1;
  testit(20)
  while ( i < stopb ) {
    points3d(mydata[i,jindex],col= colarray[const+mydata[i,sindex]],xlab='Right Hand X', ylab='Right Hand Y', zlab='Right Hand Z');
   ## if(i-100>starta){
     #points3d(mydata[i-100,jindex],col='white')
  #}
    i<-i+2;
    if( sleep>0 ) Sys.sleep(sleep) # Additional sleep to slow down frame rate
  }
}