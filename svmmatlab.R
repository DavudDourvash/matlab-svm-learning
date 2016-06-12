                              ####requirements####
    ##install.packages("quadprog")


        ###load data####
    library(quadprog)
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

####design svm####
c<-10
H<-matrix(data=0, nrow = n, ncol = n)
  for(i in 1:n){
    for(j in i:n){
      H[i,j]<-y[i]*y[j]*sum(t(x[i, ])*x[j, ])
      H[j,i]<-H[i,j]
    }
  }

          f<-matrix(data = -1, nrow = n, ncol = 1)
        lb<-matrix(data = 0, nrow = n, ncol = 1)
      ub<-c*matrix(data = 1, nrow = n, ncol = 1)
    Aeq=y
  beq=0


  alpha<-solve.QP(H, Aeq, f, beq, lb, factorized = TRUE)
  alpha$solution
  
  Almostzero<-abs(alpha$solution)<max(abs(alpha$solution))/1e5
  alpha$solution[Almostzero]=0
  
  

####plot results####
    plot(x[classA,1],x[classA,2],  col = "red", xlab = "x1", ylab = "x2", cex.main=2,cex.lab=1)
    points(x[classB,1],x[classB,2],  col = "blue", pch=22)
    legend(x=0.6, y=0.6, c("classA", "classB"), pch = c(1,22), col = c("red", "blue"))
