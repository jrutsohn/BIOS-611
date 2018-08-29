setwd("/Users/jrutsohn/Documents/Graduate/UNC/BIOS611")
Y1 = NULL
Y2 = NULL
Z = NULL
U1 = NULL
U2 = NULL
U = NULL
k = 0
t = 0
for (i in 1:20000){
  U1[i] <- runif(1,0,1)
  U2[i] <- runif(1,0,1)
  Y1[i] <- -log(U1[i])
  Y2[i] <- -log(U2[i])
  if (Y2[i] >= ((Y1[i] - 1)**2)/2){
    t = t + 1
    U[i] <- runif(1,0,1)
    if (U[i] <= 1/2){
      Z[i] = Y1[i]
    }
    else if (U[i] > 1/2){
      Z[i] = -Y1[i]
    }
  }
  else{
    k = k + 1
  }
}

hist(Z, prob=TRUE, ylim=c(0,0.4))
curve(dnorm(x), col="Red", add=TRUE)
mean(Z)
var(Z)
t/(t+k)

N = 19999
Q = NULL
Mu = NULL
gQ = NULL
Sigma.sq = NULL
for (j in 1:N){
  Q[j] <- runif(1,1,2)
  gQ[j] = Q[j]*((exp(-Q[j]**2)/2)/sqrt(2*pi))
  Mu[j] = mean(gQ)
  Sigma.sq[j] = var(gQ)
}
p = (1:19999)
plot(p, Mu)
plot(p, Mu)
plot(p,Sigma.sq)
gQ <- Q*((exp(-Q**2)/2)/sqrt(2*pi))
theta = sum(1<Q & Q<2)/N
theta
Sigma.sq[N]