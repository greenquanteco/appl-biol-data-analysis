library(rgl)
set.seed(42)
N  <- rnorm(60, 15, 2)
pH <- rnorm(60, 6, 1)
M  <- rnorm(60, 26, 3)
plot3d(N, pH, M,
  type="p", 
  pch=16, size=6)

N2 <- scale(N)
pH2 <- scale(pH)
M2 <- scale(M)

plot3d(N, pH, M,
  type="n",
  xlim=c(-2.6, 2.01), ylim=c(-2, 2.9), zlim=c(-2.2, 2.1),
  xlab="X", ylab="Y", zlab="Z")

for(i in 1:60){
  segments3d(x=c(0, N2[i]), y=c(0, pH2[i]), z=c(0, M2[i]))
}
par3d(zoom=0.9)
rgl.snapshot("C:/_courses/appl-biol-data-analysis/images/multi-distance-vis01.png")


dx <- data.frame(N2=N2, pH2=pH2, M2=M2)
dx$k <- ifelse(dx$pH2 >= 0, "a", "b")
dx <- dx[order(dx$k),]
rownames(dx) <- NULL

flag1 <- which(dx$k == "a")
flag2 <- which(dx$k == "b")

combo1 <- combn(flag1, 2)
combo2 <- combn(flag2, 2)
combo3 <- combn(1:nrow(dx), 2)
for(i in 1:ncol(combo3)){
  if(dx$k[combo3[1,i]] == dx$k[combo3[2,i]]){
    combo3[,i] <- NA
  }
}
flag3 <- apply(combo3, 2, function(x){length(which(is.na(x)))})
combo3 <- combo3[,which(flag3 == 0)]


plot3d(N, pH, M,
  type="n",
  xlim=c(-2.6, 2.01), ylim=c(-2, 2.9), zlim=c(-2.2, 2.1),
  xlab="X", ylab="Y", zlab="Z"
)
for(i in 1:60){
  segments3d(x=c(0, N2[i]), y=c(0, pH2[i]), z=c(0, M2[i]))
}
for(i in 1:ncol(combo1)){
  segments3d(
    x=c(dx$N2[combo1[1,i]], dx$N2[combo1[2,i]]),
    y=c(dx$pH2[combo1[1,i]], dx$pH2[combo1[2,i]]),
    z=c(dx$M2[combo1[1,i]], dx$M2[combo1[2,i]]),
    color="red", alpha=0.15)
}
for(i in 1:ncol(combo2)){
  segments3d(
    x=c(dx$N2[combo2[1,i]], dx$N2[combo2[2,i]]),
    y=c(dx$pH2[combo2[1,i]], dx$pH2[combo2[2,i]]),
    z=c(dx$M2[combo2[1,i]], dx$M2[combo2[2,i]]),
    color="blue", alpha=0.15)
}
par3d(zoom=0.9)
rgl.snapshot("C:/_courses/appl-biol-data-analysis/images/multi-distance-vis03.png")


plot3d(N, pH, M,
  type="n",
  xlim=c(-2.6, 2.01), ylim=c(-2, 2.9), zlim=c(-2.2, 2.1),
  xlab="X", ylab="Y", zlab="Z"
)
for(i in 1:60){
  segments3d(x=c(0, N2[i]), y=c(0, pH2[i]), z=c(0, M2[i]))
}
for(i in 1:ncol(combo1)){
  segments3d(
    x=c(dx$N2[combo1[1,i]], dx$N2[combo1[2,i]]),
    y=c(dx$pH2[combo1[1,i]], dx$pH2[combo1[2,i]]),
    z=c(dx$M2[combo1[1,i]], dx$M2[combo1[2,i]]),
    color="black", alpha=0.15)
}
for(i in 1:ncol(combo2)){
  segments3d(
    x=c(dx$N2[combo2[1,i]], dx$N2[combo2[2,i]]),
    y=c(dx$pH2[combo2[1,i]], dx$pH2[combo2[2,i]]),
    z=c(dx$M2[combo2[1,i]], dx$M2[combo2[2,i]]),
    color="black", alpha=0.15)
}
par3d(zoom=0.9)
rgl.snapshot("C:/_courses/appl-biol-data-analysis/images/multi-distance-vis02.png")


