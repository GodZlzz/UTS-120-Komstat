#1
#coba bikin faktorial
Fr <- function (x) {
  if (x==0) return(1)
  hasil<-1
  for (i in 1:x){
    hasil<-hasil*i
  }
  return(hasil)
}
Fr(5)
Fr(4) 
#cari fungsi pdf dan hasilnya
pdf <- function(x,lam){
  if (x<0 || lam<0) stop("x dan lambda harus >=0")
  hasil <- (exp(-lam))*(lam^x)/Fr(x)
  return(hasil)
}
pdf(3,10)
pdf(0,0)
pdf(-1,-2)

cdf <- function(x,lam){
  total <- 0
  for (k in 0:x){
    total <- total + pdf(k, lam)
  }
  return(total)
}

cdf(3,10)

#kata katanya bang
cat ("Pdf untuk x=3, dan lamda = 10 adalah:", pdf(3,10),"\n")
cat ("Cdf untuk x=3, dan lamda = 10 adalah:", cdf(3,10),"\n")

#2a
tabel <- matrix(c(41,9,32,18), ncol=2,nrow=2, byrow = TRUE)
rownames(tabel)= c("Metode A", "Metode B")
colnames(tabel)= c("Lulus","Tidak lulus")
total <- sum(tabel)

#fungsi joint prob
joint_prob <- function(tabel){
  hasil <- matrix(0, nrow=nrow(tabel),ncol=ncol(tabel))
  for (i in 1:nrow(tabel)){
    for (j in 1:ncol(tabel)){
      hasil [i,j]<-tabel[i,j]/sum(tabel)
  colnames(hasil)=colnames(tabel)
  rownames(hasil)=rownames(tabel)
    }
  }
  return(hasil)
}
jp <- joint_prob(tabel)
jp

#2b1
b1 = function(tabel){
  hasil <- matrix(0, nrow=nrow(tabel), ncol=ncol(tabel))
  for (i in 1:nrow(tabel)){
    total_baris <- sum(tabel[i, ])
    for (j in 1:ncol(tabel)){
      hasil[i,j]<-tabel[i,j]/total_baris
    }
  }
  return(hasil)
}
cp <- b1(tabel)
cp
#2b2
b2 = function(tabel){
  hasil<-matrix(0, nrow=nrow(tabel),ncol=ncol(tabel))
  for (j in 1: ncol(tabel)){
    total_kolom = sum(tabel[ ,j])
    for (i in 1:nrow(tabel)){
      hasil[i,j]<-tabel[i,j]/total_kolom
    }
  }
  return(hasil)
}

cp2 <-b2(tabel)
cp2

#3
iris
#Petal length
hist(iris$Petal.Length, main= "Ini adalah histogram data Petal Length",
     xlab="Petal Length",
     col="blue")
#3b
plot(iris$Sepal.Length,iris$Petal.Length, type<-"p",
     main = "Sepal vs Petal",
     xlab = "Sepal",
     ylab = "Petal",
     col="red")

cor(iris$Sepal.Length, iris$Petal.Length)

#3c
boxplot(data=iris, Sepal.Length~Species,
        main = "ini adalah boxplot",
        ylab = "Sepal.Length")

barplot(iris$Sepal.Length,iris$Species,
        main="kimay")

barplot(iris$Species)
#3d
plot(iris$Sepal.Length, iris$Petal.Length,
     col = iris$Species,
     pch=19,
     xlab = "Sepal Length", ylab="Petal Length",
     main = "Sepal vs Petal Length by Species")
legend("topleft", legend=levels(iris$Species),
       col = 1:3,pch =19)
