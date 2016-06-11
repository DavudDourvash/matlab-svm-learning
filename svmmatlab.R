####data preparing####
dt<- read.csv(file = "H:\\since95\\Dayche-2\\Projects\\Data Mining Algorithms\\DMA-data\\Sugarcane-2.csv")
n<-dim(dt)[1]
x<-dt[, -c(1)]
y<-as.numeric(vector(length = n))
for(i in 1:n){
  if( dt[i, 30] >= 70) y[i]=1
  else y[i]=-1
}
classA<-which(y==1)
classB<-which(y==-1)
plot(x[classA,1],x[classA,2],  col = "red")
points(x[classB,1],x[classB,2],  col = "blue", type = "p")

