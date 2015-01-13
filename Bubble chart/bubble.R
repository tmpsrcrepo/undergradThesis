setwd("C:/Users/Xiaoqian/Desktop/Motion Capture/")
chart <-read.csv("bubble/expert_bubble.csv")
Y<-table(chart$y)
Z<-table(chart$z)
ZY<-table(chart$zy,chart$v.states)
ZY
state<-table(chart$v.states)
twoD<-table(chart$y,chart$z)
twoD[2,1]
summary(chart$yzstate)
threeD<-table(chart$y,chart$z,chart$v.states)
threeD[3,1,1]
df=data.frame()
row=1;
for(i in 1:4){
  for(j in 1:4){
    for(k in 1:4){
     
      df[row,1] = i;
      df[row,2]= j;
      df[row,3]= k;
      df[row,4]=threeD[i,j,k];
      row=row+1;
    }
  }
}
(df[,3])
colarray <- c('red', 'orange', 'yellow', 'green');
name<-c("y54,p19", "y51,p18","y50,p16","y41,p8","y33,p0.7");
symbols(df[,1],df[,2],bg=colarray[df[,3]],circles=df[,4],main="bubble chart_expert",xlab="y",ylab="z")
legend("bottomleft",legend=name,lwd = 1,col=colarray,seg.len=0.4)